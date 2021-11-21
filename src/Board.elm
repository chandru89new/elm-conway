module Board exposing (..)

import Html as H
import Html.Attributes as Attr
import Html.Events as Ev
import List.Extra as List
import Random as Rand


convertToStyle : List ( String, String ) -> List (H.Attribute msg)
convertToStyle =
    List.map (\( key, value ) -> Attr.style key value)


type alias Cell =
    { status : Status
    , position : ( RowId, CellId )
    }


type alias Board =
    List (List Cell)


type Status
    = Alive
    | Dead


type alias RowId =
    Int


type alias CellId =
    Int


type Msg
    = Clicked Position


type alias Position =
    ( Int, Int )


viewCell : Cell -> H.Html Msg
viewCell cell =
    H.span
        (convertToStyle
            [ ( "width", "16px" )
            , ( "height", "16px" )
            , ( "background"
              , if cell.status == Alive then
                    "black"

                else
                    "white"
              )
            , ( "border", "1px solid rgba(0,0,0,0.2)" )
            , ( "display", "inline-block" )
            ]
            ++ [ Ev.onClick (Clicked cell.position)
               ]
        )
        []


viewBoard : List (List Cell) -> H.Html Msg
viewBoard board =
    H.div
        (convertToStyle
            [ ( "line-height", "0" )
            ]
        )
        (List.map
            viewRow
            board
        )


viewRow : List Cell -> H.Html Msg
viewRow row =
    H.div
        (convertToStyle
            [ ( "margin", "0" )
            , ( "padding", "0" )
            ]
        )
        (List.map viewCell row)


randomBoolGenerator : Rand.Generator Status
randomBoolGenerator =
    Rand.weighted ( 50, Dead ) [ ( 50, Alive ) ]


randomList : Int -> Rand.Generator (List Status)
randomList size =
    Rand.list (size * size) randomBoolGenerator


boardGenerator : Int -> List Status -> List (List Cell)
boardGenerator size listOfCellStatuses =
    listOfCellStatuses
        |> List.groupsOf size
        |> List.indexedMap
            (\rowId row ->
                List.indexedMap
                    (\cellId status ->
                        { status = status
                        , position = ( rowId, cellId )
                        }
                    )
                    row
            )


neighbors : Position -> List Position
neighbors ( row, cell ) =
    [ ( row - 1, cell - 1 )
    , ( row - 1, cell )
    , ( row - 1, cell + 1 )
    , ( row, cell - 1 )
    , ( row, cell + 1 )
    , ( row + 1, cell - 1 )
    , ( row + 1, cell )
    , ( row + 1, cell + 1 )
    ]


generateNextGenerationForCell : Board -> Cell -> Cell
generateNextGenerationForCell board cell =
    let
        neighboringCells =
            List.map
                (List.filter (\cell_ -> List.member cell_.position (neighbors cell.position)))
                board
                |> List.concat

        aliveNeighbors =
            List.length <| List.filter (\c -> c.status == Alive) neighboringCells

        getNextGenForAliveCell =
            if aliveNeighbors == 2 then
                Alive

            else
                Dead

        getNextGenForDeadCell =
            if aliveNeighbors >= 3 then
                Alive

            else
                Dead
    in
    { cell
        | status =
            if cell.status == Alive then
                getNextGenForAliveCell

            else
                getNextGenForDeadCell
    }
