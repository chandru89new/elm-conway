module Main exposing (..)

import Array as Ar
import Board as B
import Browser
import Html as H
import Html.Attributes as Attr
import Html.Events as Ev
import Random
import Time


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { board : B.Board
    , size : Int
    , generation : Int
    , civilizationState : CivilizationState
    , runs : List Int
    }


type CivilizationState
    = Stopped
    | Running


type Msg
    = NoOp
    | GenerateBoard (List B.Status)
    | GenerateCleanSlate
    | ChangeSizeOfBoard Int
    | ResetBoard
    | BoardInteraction B.Msg
    | Next
    | Start
    | Pause


init : () -> ( Model, Cmd Msg )
init _ =
    let
        size =
            20
    in
    ( { board =
            Ar.empty
      , size = size
      , generation = 0
      , civilizationState = Stopped
      , runs = []
      }
    , Random.generate GenerateBoard (B.randomList size)
    )



-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GenerateCleanSlate ->
            ( { model
                | board = B.emptyBoardGenerator model.size
                , generation = 0
                , runs = []
                , civilizationState = Stopped
              }
            , Cmd.none
            )

        ResetBoard ->
            ( { model | board = Ar.empty, generation = 0, civilizationState = Stopped, runs = [] }, Random.generate GenerateBoard (B.randomList model.size) )

        GenerateBoard list ->
            ( { model | board = B.boardGenerator model.size list }, Cmd.none )

        ChangeSizeOfBoard size ->
            ( { model
                | size = size
                , civilizationState = Stopped
                , generation = 0
              }
            , Random.generate GenerateBoard (B.randomList size)
            )

        BoardInteraction boardMsg ->
            case boardMsg of
                B.Clicked position ->
                    let
                        newBoard =
                            Ar.map
                                (\row ->
                                    Ar.map
                                        (\cell ->
                                            if cell.position == position then
                                                { cell
                                                    | status =
                                                        if cell.status == B.Alive then
                                                            B.Dead

                                                        else
                                                            B.Alive
                                                }

                                            else
                                                cell
                                        )
                                        row
                                )
                                model.board
                    in
                    ( { model | board = newBoard }, Cmd.none )

        Next ->
            let
                hasEnded =
                    B.hasCivilizationCollapsed model.board

                newBoard =
                    if hasEnded then
                        model.board

                    else
                        B.getNextGenerationOfBoard model.board
            in
            if hasEnded then
                ( { model | civilizationState = Stopped, runs = model.generation :: model.runs }, Cmd.none )

            else
                ( { model
                    | board = newBoard
                    , generation =
                        model.generation + 1
                  }
                , Cmd.none
                )

        Start ->
            if B.hasCivilizationCollapsed model.board then
                ( { model | civilizationState = Running, generation = 0 }
                , Random.generate GenerateBoard (B.randomList model.size)
                )

            else
                ( { model | civilizationState = Running }, Cmd.none )

        Pause ->
            ( { model | civilizationState = Stopped }, Cmd.none )



-- view


view : Model -> H.Html Msg
view model =
    H.div
        (B.convertToStyle
            [ ( "display", "flex" )
            , ( "flex-direction", "column" )
            , ( "margin", "10rem auto" )
            , ( "max-width", "700px" )
            , ( "gap", "2rem" )
            ]
        )
        [ H.div [ Attr.class "text-4xl" ] [ H.text "Conway's Game of Life" ]
        , H.map BoardInteraction <| B.viewBoard model.board
        , H.div [] [ H.text <| "Generation: " ++ String.fromInt model.generation ]
        , H.div []
            [ H.span [] [ H.text "Size: " ]
            , H.input
                [ Attr.type_ "number"
                , Ev.onInput (\str -> ChangeSizeOfBoard (Maybe.withDefault 10 <| String.toInt str))
                , Attr.value (String.fromInt model.size)
                ]
                []
            ]
        , H.div
            (B.convertToStyle
                [ ( "display", "flex" )
                , ( "gap", "1rem" )
                , ( "flex", "1 1 auto" )

                -- , ( "justify-content", "space-between" )
                , ( "align-items", "center" )
                ]
            )
            [ H.button [ Ev.onClick ResetBoard ] [ H.text "Reset" ]
            , H.button [ Ev.onClick GenerateCleanSlate ] [ H.text "Blank" ]
            , H.button
                [ Ev.onClick
                    (if model.civilizationState == Running then
                        Pause

                     else
                        Start
                    )
                ]
                [ H.text
                    (if model.civilizationState == Running then
                        "Pause"

                     else
                        "Start"
                    )
                ]
            , H.button
                [ Ev.onClick Next
                , Attr.disabled (B.hasCivilizationCollapsed model.board || (model.civilizationState == Running))
                ]
                [ H.text "Step >>" ]
            ]
        , H.div []
            [ H.ul []
                (List.map
                    (\run -> H.li [] [ H.text <| String.fromInt run ])
                    model.runs
                )
            ]
        ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.civilizationState of
        Running ->
            Time.every 100 (\_ -> Next)

        _ ->
            Sub.none
