module Main exposing (..)

import Browser
import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Item as CI
import Date as D
import Dict exposing (Dict)
import Element as E
import Element.Border as EBorder
import Element.Input as EI
import Element.Font as EF
import Html exposing (div, text)
import Html.Attributes exposing (style)
import Http
import Json.Decode as JD
import Json.Decode.Pipeline exposing (optional, required, requiredAt, optionalAt)
import Task
import Svg



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- TYPES from MLB schema
-- Game Information


type alias GameInfo =
    List Play


type alias Play =
    { result : PlayResult
    , homeTeamWinProbability : Float
    , awayTeamWinProbability : Float
    , homeTeamWinProbabilityAdded : Float
    , aboutInning : Int
    , aboutHalfInning : String
    }


type alias PlayResult =
    { playType : String
    , event : String
    , eventType : String
    , description : String
    , awayScore : Int
    , homeScore : Int
    }



-- Schedule of Games


type alias Schedule =
    { totalItems : Int
    , dates : List GameDay
    }


type alias GameDay =
    { date : String
    , totalItems : Int
    , games : List Game
    }


type alias Game =
    { gamePk : Int
    , gameGuid : String
    , teams : GameTeams
    }


type alias GameTeams =
    { away : TeamSide
    , home : TeamSide
    }


type alias TeamSide =
    { team : Team
    , seriesNumber : Int
    }


type alias Team =
    { id : Int
    , name : String
    }



-- MODEL


type alias Model =
    { selectedGame : Maybe Game
    , schedule : DecodedSchedule
    , gameInfo : DecodedGameInfo
    , hoveringOnGraph : List (CI.One Datum CI.Dot)
    , gameDate : D.Date
    }


type DecodedSchedule
    = Failure
    | Loading
    | Success Schedule


type DecodedGameInfo
    = InfoFailure
    | NoInfo
    | InfoSuccess GameInfo


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Nothing
        Loading
        NoInfo
        []
        (D.fromOrdinalDate 2024 1)
      {- , Cmd.batch [ getTodaysGames
         , D.today |> Task.perform NewDate
          ]
      -}
    , D.today |> Task.perform NewDate
    )


type Msg
    = NewGame Game
    | GotSchedule (Result Http.Error Schedule)
    | GotGameInfo (Result Http.Error GameInfo)
    | OnHover (List (CI.One Datum CI.Dot))
    | NewDate D.Date



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame g ->
            ( { model | selectedGame = Just g }
            , getGameInfo g
            )

        GotSchedule result ->
            case result of
                Ok s ->
                    ( { model | schedule = Success s }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | schedule = Failure }
                    , Cmd.none
                    )

        GotGameInfo result ->
            case result of
                Ok info ->
                    ( { model | gameInfo = InfoSuccess info }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | gameInfo = InfoFailure }
                    , Cmd.none
                    )

        OnHover hov ->
            ( { model | hoveringOnGraph = hov }
            , Cmd.none
            )

        NewDate d ->
            ( { model
                | gameDate = d
                , selectedGame = Nothing
                , gameInfo = NoInfo
              }
            , getSchedule d
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "MLB Game Data View"
    , body =
        [ E.layout [ E.padding 20
                   , E.width E.fill
                   ] <|
            E.column [ E.spacing 10
                     , E.paddingXY 50 5
                     , E.width E.fill
                     , EF.size 32
                     , EF.family [ EF.typeface "Helvetica", EF.sansSerif ]
                     ]
                [ E.row [ E.spacing 5
                        , E.paddingXY 35 5
                        , E.width E.fill
                        ]
                    [ EI.button
                        [ E.padding 3
                        , EBorder.width 2
                        , EBorder.rounded 6
                        ]
                        { onPress = Just (NewDate <| D.add D.Days -1 model.gameDate)
                        , label = E.text "➖"
                        }
                    , E.text <| D.toIsoString model.gameDate
                    , EI.button
                        [ E.padding 3
                        , EBorder.width 2
                        , EBorder.rounded 6
                        ]
                        { onPress = Just (NewDate <| D.add D.Days 1 model.gameDate)
                        , label = E.text "➕"
                        }
                    ]
                , case model.schedule of
                    Loading ->
                        E.text "Loading..."

                    Failure ->
                        E.text "Data acquisition failed for some reason."

                    Success s ->
                        EI.radio
                            [ E.padding 10
                            , E.spacing 5
                            ]
                            { onChange = NewGame
                            , selected = model.selectedGame
                            , label = EI.labelHidden "Select a game:"
                            , options =
                                List.map2 EI.option
                                    (allGames s)
                                    (List.map E.text <| allGameMatchups s)
                            }
                , case model.gameInfo of
                    NoInfo ->
                        E.text "Please select a game."

                    InfoFailure ->
                        E.text "Game info failed to load. Try again later."

                    InfoSuccess _ ->
                        E.row [ E.spacing 10 ]
                            [ E.text "Win Probability and Events"
                            , EI.button
                                [ E.padding 3
                                , EBorder.width 2
                                , EBorder.rounded 6
                                ]
                                { onPress = Maybe.map NewGame model.selectedGame

                                {- onPress = case model.selectedGame of
                                   Nothing -> Nothing
                                   Just g -> NewGame g
                                -}
                                , label = E.text "Refresh"
                                }
                            ]

                {- , ( case model.gameInfo of
                       NoInfo -> E.text "No game info yet."
                       InfoFailure -> E.text "Failed to retrieve MLB data. Game not started yet?"
                       InfoSuccess info -> E.text <|
                         "Number of plays = " ++ String.fromInt (List.length info)
                   )
                -}
                , E.html <|
                    div
                        [ style "width" "100%"

                        -- , style "background-color" "black"
                        , style "padding" "20px"
                        ]
                        [ C.chart
                            [ CA.height 600
                            , CA.width 800
                            , CA.domain [ CA.lowest 0 CA.exactly, CA.highest 100 CA.exactly ]
                            , CE.onMouseMove OnHover (CE.getNearest CI.dots)
                            , CE.onMouseLeave (OnHover [])
                            ]
                            (case model.gameInfo of
                                NoInfo ->
                                    [ C.yAxis [], C.yTicks [] ]

                                InfoFailure ->
                                    [ C.yAxis [], C.yTicks [] ]

                                InfoSuccess info ->
                                    [ {- C.xAxis []
                                         , C.xTicks []
                                         , C.xLabels []
                                         ,
                                      -}
                                      C.yAxis []
                                    , C.yTicks []
                                    , C.yLabels [ CA.moveDown 0
                                                , CA.fontSize 16
                                                ]
                                    , C.series .x
                                        [ C.named ( homeTeamName model.selectedGame )
                                          <|
                                            C.interpolated .y
                                              [ CA.monotone, CA.width 2 ]
                                              [ CA.circle, CA.size 40, CA.opacity 0.2, CA.borderWidth 1 ]
                                        , C.named ( awayTeamName model.selectedGame )
                                          <|
                                            C.interpolated .z
                                                [ CA.monotone, CA.width 2 ]
                                                [ CA.circle, CA.size 40, CA.opacity 0.2, CA.borderWidth 1 ]
                                        ]
                                        (toData info)
                                    , C.legendsAt .min
                                        .max
                                        [ CA.column
                                        , CA.moveUp 0
                                        , CA.moveRight 10
                                        , CA.spacing 10
                                        ]
                                        [ CA.width 25
                                        , CA.fontSize 12
                                        ]
                                    , C.each model.hoveringOnGraph <|
                                        \_ item ->
                                            [ C.tooltip item
                                                [ CA.onTopOrBottom ]
                                                [ style "width" "200px"
                                                , style "text-wrap" "wrap"
                                                , style "font-size" "70%"
                                                ]
                                                [ text <| (CI.getData item).desc
                                                      ++ "\n" ++ "Inning: " ++ (CI.getData item).halfInn ++ " "
                                                      ++ String.fromInt (CI.getData item).inn
                                                      ++ "\n" ++ ( awayTeamName model.selectedGame )
                                                      ++ " " ++ String.fromInt (CI.getData item).aScore
                                                      ++ ", " ++ ( homeTeamName model.selectedGame )
                                                      ++ " " ++ String.fromInt (CI.getData item).hScore
                                                ]
                                            ]
                                    , C.eachDot <| \p dot ->
                                        [ C.label
                                              [ CA.moveDown 4,  CA.fontSize 14, CA.color "black" ]
                                              [ Svg.text (String.fromInt <| if ( CI.getName dot ) == ( homeTeamName model.selectedGame ) then
                                                                                (CI.getData dot).hScore
                                                                            else
                                                                                (CI.getData dot).aScore
                                                         ) 
                                              ]
                                              (CI.getCenter p dot)
                                        ]
                                    ]
                            )
                        ]
                ]

        -- end column
        ]

    -- end body?
    }


homeTeamName : Maybe Game -> String
homeTeamName mg =
   (case mg of
      Just g ->
          case
              Dict.get
                  g.teams.home.team.id
                  shortTeamName
          of
              Just s ->
                  s
              Nothing ->
                  "Home"
      Nothing ->
          "Home"
   )

awayTeamName : Maybe Game -> String
awayTeamName mg =
   (case mg of
      Just g ->
          case
              Dict.get
                  g.teams.away.team.id
                  shortTeamName
          of
              Just s ->
                  s
              Nothing ->
                  "Away"
      Nothing ->
          "Away"
   )


{-
   allGamePks : Schedule -> List Int
   allGamePks s =
     let
       listofgamedays = s.dates
       listoflistofgames = List.map (\gd -> gd.games ) listofgamedays
       listofgames = List.concat listoflistofgames
     in
       List.map ( \g -> g.gamePk ) listofgames
-}
-- EXTRACT DATA FROM DECODED DATA


type alias Datum =
    { x : Float
    , y : Float
    , z : Float
    , desc : String
    , delta : Float
    , inn : Int
    , halfInn : String
    , aScore : Int
    , hScore : Int
    }


andMap : List a -> List ( a -> b ) -> List b
andMap = List.map2 (|>)

toData : GameInfo -> List Datum
toData info =
     List.repeat ( List.length info ) Datum
      |> andMap (List.map toFloat <| List.range 1 (List.length info))
      |> andMap (List.map (\p -> p.homeTeamWinProbability) info)
      |> andMap (List.map (\p -> p.awayTeamWinProbability) info)
      |> andMap (List.map (\p -> p.result.description) info)
      |> andMap (List.map (\p -> p.homeTeamWinProbabilityAdded) info)
      |> andMap (List.map (\p -> p.aboutInning) info)
      |> andMap (List.map (\p -> p.aboutHalfInning) info)
      |> andMap (List.map (\p -> p.result.awayScore) info)
      |> andMap (List.map (\p -> p.result.homeScore) info)

{-
toData : GameInfo -> List Datum
toData info =
    List.map6 Datum -- no such thing as map6
       (List.map toFloat <| List.range 1 (List.length info
       (List.map (\p -> p.homeTeamWinProbability) info)
       (List.map (\p -> p.awayTeamWinProbability) info)
       (List.map (\p -> p.result.description) info)
       (List.map (\p -> p.homeTeamWinProbabilityAdded) info)
       (List.map (\p -> String.fromInt p.aboutInning) info)
-}


allGames : Schedule -> List Game
allGames s =
    s.dates
        |> List.map (\gd -> gd.games)
        |> List.concat


allGamePks : Schedule -> List Int
allGamePks s =
    s.dates
        |> List.map (\gd -> gd.games)
        |> List.concat
        |> List.map (\g -> g.gamePk)


allGameMatchups : Schedule -> List String
allGameMatchups s =
    s.dates
        |> List.map (\gd -> gd.games)
        |> List.concat
        |> List.map (\g -> g.teams)
        |> List.map (\gt -> gt.away.team.name ++ " at " ++ gt.home.team.name)



-- HTTP


getGameInfo : Game -> Cmd Msg
getGameInfo game =
    Http.get
        { url =
            "https://statsapi.mlb.com/api/v1/game/"
                ++ String.fromInt game.gamePk
                ++ "/winProbability"
        , expect = Http.expectJson GotGameInfo gameInfoDecoder
        }


gameInfoDecoder : JD.Decoder GameInfo
gameInfoDecoder =
    JD.list playDecoder


playDecoder : JD.Decoder Play
playDecoder =
    JD.succeed Play
        |> required "result" playResultDecoder
        |> required "homeTeamWinProbability" JD.float
        |> required "awayTeamWinProbability" JD.float
        |> required "homeTeamWinProbabilityAdded" JD.float
--        |> requiredAt ["about", "inning"] JD.int
        |> optionalAt ["about", "inning"] JD.int 0  -- not sure it needs to be optional
        |> optionalAt ["about", "halfInning"] JD.string ""  -- not sure it needs to be optional



{-
   playResultDecoder : JD.Decoder PlayResult
   playResultDecoder =
     JD.map4 PlayResult
       ( JD.field "type" JD.string )
       ( JD.field "event" JD.string )
       ( JD.field "eventType" JD.string )
       ( JD.field "description" JD.string )
-}


playResultDecoder : JD.Decoder PlayResult
playResultDecoder =
    JD.succeed PlayResult
        |> required "type" JD.string
        |> optional "event" JD.string "In Progress"
        |> optional "eventType" JD.string "in progress"
        |> optional "description" JD.string "Play description is not yet available."
        |> required "awayScore" JD.int
        |> required "homeScore" JD.int


getTodaysGames : Cmd Msg
getTodaysGames =
    Http.get
        --    { url = "https://statsapi.mlb.com/api/v1/schedule/games/?sportId=1" -- current day
        { url = "https://statsapi.mlb.com/api/v1/schedule/games/?sportId=1&startDate=2024-06-28&endDate=2024-06-28"
        , expect = Http.expectJson GotSchedule scheduleDecoder
        }


getSchedule : D.Date -> Cmd Msg
getSchedule d =
    Http.get
        { url =
            "https://statsapi.mlb.com/api/v1/schedule/games/?sportId=1&startDate="
                ++ D.toIsoString d
                ++ "&endDate="
                ++ D.toIsoString d
        , expect = Http.expectJson GotSchedule scheduleDecoder
        }


scheduleDecoder : JD.Decoder Schedule
scheduleDecoder =
    JD.map2 Schedule
        (JD.field "totalItems" JD.int)
        (JD.field "dates" (JD.list gameDayDecoder))


gameDayDecoder : JD.Decoder GameDay
gameDayDecoder =
    JD.map3 GameDay
        (JD.field "date" JD.string)
        (JD.field "totalItems" JD.int)
        (JD.field "games" (JD.list gameDecoder))


gameDecoder : JD.Decoder Game
gameDecoder =
    JD.map3 Game
        (JD.field "gamePk" JD.int)
        (JD.field "gameGuid" JD.string)
        (JD.field "teams" gameTeamsDecoder)


gameTeamsDecoder : JD.Decoder GameTeams
gameTeamsDecoder =
    JD.map2 GameTeams
        (JD.field "away" teamSideDecoder)
        (JD.field "home" teamSideDecoder)


teamSideDecoder : JD.Decoder TeamSide
teamSideDecoder =
    JD.map2 TeamSide
        (JD.field "team" teamDecoder)
        (JD.field "seriesNumber" JD.int)


teamDecoder : JD.Decoder Team
teamDecoder =
    JD.map2 Team
        (JD.field "id" JD.int)
        (JD.field "name" JD.string)


shortTeamName : Dict Int String
shortTeamName =
    Dict.fromList
        [ ( 108, "Angels" )
        , ( 109, "DBacks" )
        , ( 110, "Orioles" )
        , ( 111, "BoSox" )
        , ( 112, "Cubs" )
        , ( 113, "Reds" )
        , ( 114, "Guardians" )
        , ( 115, "Rockies" )
        , ( 116, "Tigers" )
        , ( 117, "Astros" )
        , ( 118, "Royals" )
        , ( 119, "Dodgers" )
        , ( 120, "Nats" )
        , ( 121, "Mets" )
        , ( 133, "As" )
        , ( 134, "Pirates" )
        , ( 135, "Padres" )
        , ( 136, "Mariners" )
        , ( 137, "Giants" )
        , ( 138, "Cards" )
        , ( 139, "Rays" )
        , ( 140, "Rangers" )
        , ( 141, "Jays" )
        , ( 142, "Twins" )
        , ( 143, "Phillies" )
        , ( 144, "Braves" )
        , ( 145, "ChiSox" )
        , ( 146, "Marlins" )
        , ( 147, "Yankees" )
        , ( 158, "Brewers" )
        ]
