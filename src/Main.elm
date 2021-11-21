module Main exposing (..)

import Board exposing (Cell, Status, boardGenerator, convertToStyle, randomList, viewBoard)
import Browser
import Html as H
import Html.Attributes as Attr
import Html.Events as Ev
import List.Extra as List
import Random


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { board : List (List Cell)
    , size : Int
    , generation : Int
    }


type Msg
    = NoOp
    | GenerateBoard (List Status)
    | ChangeSizeOfBoard Int
    | ResetBoard
    | BoardInteraction Board.Msg
    | Next


init : () -> ( Model, Cmd Msg )
init _ =
    let
        size =
            10
    in
    ( { board =
            []
      , size = size
      , generation = 0
      }
    , Random.generate GenerateBoard (randomList size)
    )



-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ResetBoard ->
            ( { board = [], size = 10, generation = 0 }, Random.generate GenerateBoard (randomList 10) )

        GenerateBoard list ->
            ( { model | board = boardGenerator model.size list }, Cmd.none )

        ChangeSizeOfBoard size ->
            ( { model
                | size = size
              }
            , Random.generate GenerateBoard (randomList size)
            )

        BoardInteraction boardMsg ->
            case boardMsg of
                Board.Clicked position ->
                    let
                        newBoard =
                            List.map
                                (\row ->
                                    List.map
                                        (\cell ->
                                            if cell.position == position then
                                                { cell
                                                    | status =
                                                        if cell.status == Board.Alive then
                                                            Board.Dead

                                                        else
                                                            Board.Alive
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
                    model.board
                        |> List.concat
                        |> List.any (\cell -> cell.status == Board.Alive)
                        |> not

                newBoard =
                    if hasEnded then
                        model.board

                    else
                        model.board
                            |> List.map (List.map (Board.generateNextGenerationForCell model.board))
            in
            if hasEnded then
                ( model, Cmd.none )

            else
                ( { model
                    | board = newBoard
                    , generation =
                        model.generation + 1
                  }
                , Cmd.none
                )



-- view


view : Model -> H.Html Msg
view model =
    H.div
        (convertToStyle
            [ ( "display", "flex" )
            , ( "flex-direction", "column" )
            , ( "margin", "10rem auto" )
            , ( "max-width", "500px" )
            , ( "gap", "2rem" )
            ]
        )
        [ H.map BoardInteraction <| viewBoard model.board
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
            (convertToStyle
                [ ( "display", "flex" )
                , ( "gap", "2rem" )
                , ( "flex", "1 1 50%" )
                , ( "justify-content", "space-between" )
                , ( "align-items", "center" )
                ]
            )
            [ H.button [ Ev.onClick ResetBoard ] [ H.text "Reset" ]
            , H.button [ Ev.onClick Next, Attr.autofocus True ] [ H.text "Step >>" ]
            ]
        ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
