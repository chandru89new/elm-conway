module Board exposing (..)

import Array
import Array.Extra as Array
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
    Array.Array (Array.Array Cell)


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


viewCell : Int -> Cell -> H.Html Msg
viewCell size cell =
    let
        cellSize =
            String.fromInt (250 // size) ++ "px"
    in
    H.span
        (convertToStyle
            [ ( "width", cellSize )
            , ( "height", cellSize )
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


viewBoard : Board -> H.Html Msg
viewBoard board =
    let
        board_ =
            Array.toList
                (Array.map
                    (\row -> Array.toList row)
                    board
                )

        size =
            Array.length board
    in
    H.div
        (convertToStyle
            [ ( "line-height", "0" )
            ]
        )
        (List.map
            (viewRow
                size
            )
            board_
        )


viewRow : Int -> List Cell -> H.Html Msg
viewRow size row =
    H.div
        (convertToStyle
            [ ( "margin", "0" )
            , ( "padding", "0" )
            ]
        )
        (List.map (viewCell size) row)


randomBoolGenerator : Rand.Generator Status
randomBoolGenerator =
    Rand.weighted ( 70, Dead ) [ ( 30, Alive ) ]


randomList : Int -> Rand.Generator (List Status)
randomList size =
    Rand.list (size * size) randomBoolGenerator


boardGenerator : Int -> List Status -> Board
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
        |> List.map (\row -> Array.fromList row)
        |> Array.fromList


getNeighboringCells : Board -> Cell -> Array.Array Cell
getNeighboringCells board { position } =
    let
        ( row, cell ) =
            position

        currRowFromBoard =
            Array.get row board |> Maybe.withDefault Array.empty

        prevRowFromBoard =
            Array.get (row - 1) board |> Maybe.withDefault Array.empty

        nextRowFromBoard =
            Array.get (row + 1) board |> Maybe.withDefault Array.empty

        adjacentCellGetter =
            Array.fromList [ Array.get (cell - 1), Array.get cell, Array.get (cell + 1) ]

        adjacentCellsPrevRow : Array.Array Cell
        adjacentCellsPrevRow =
            Array.map (\cellGetter -> cellGetter prevRowFromBoard) adjacentCellGetter
                |> Array.foldl reducer Array.empty

        reducer : Maybe Cell -> Array.Array Cell -> Array.Array Cell
        reducer maybeCell res =
            case maybeCell of
                Just c ->
                    Array.push c res

                Nothing ->
                    res

        adjacentCellsNextRow : Array.Array Cell
        adjacentCellsNextRow =
            Array.map (\cellGetter -> cellGetter nextRowFromBoard) adjacentCellGetter
                |> Array.foldl reducer Array.empty

        adjacentCellsSameRow : Array.Array Cell
        adjacentCellsSameRow =
            Array.map (\cellGetter -> cellGetter currRowFromBoard)
                (Array.fromList [ Array.get (cell - 1), Array.get (cell + 1) ])
                |> Array.foldl reducer Array.empty
    in
    Array.append adjacentCellsSameRow adjacentCellsNextRow
        |> Array.append adjacentCellsPrevRow


getNewStatusOfCell : Cell -> Board -> Status
getNewStatusOfCell cell board =
    let
        neighboringCells =
            getNeighboringCells board cell

        numberOfAliveCells =
            Array.length <| Array.filter isAlive neighboringCells

        isAlive c =
            c.status == Alive
    in
    case cell.status of
        Alive ->
            if numberOfAliveCells == 2 then
                Alive

            else
                Dead

        Dead ->
            if numberOfAliveCells >= 3 then
                Alive

            else
                Dead


getNextGenerationOfBoard : Board -> Board
getNextGenerationOfBoard board =
    Array.map
        (\row ->
            Array.map
                (\cell -> { cell | status = getNewStatusOfCell cell board })
                row
        )
        board


hasCivilizationCollapsed : Board -> Bool
hasCivilizationCollapsed board =
    let
        rowsAsBool : Array.Array Bool
        rowsAsBool =
            Array.map
                (\row ->
                    Array.any
                        (\cell -> cell.status == Alive)
                        row
                )
                board
    in
    Array.any (\v -> v == True) rowsAsBool
        |> not



-- neighbors : Position -> List Position
-- neighbors ( row, cell ) =
--     [ ( row - 1, cell - 1 )
--     , ( row - 1, cell )
--     , ( row - 1, cell + 1 )
--     , ( row, cell - 1 )
--     , ( row, cell + 1 )
--     , ( row + 1, cell - 1 )
--     , ( row + 1, cell )
--     , ( row + 1, cell + 1 )
--     ]
-- generateNextGenerationForCell : Board -> Cell -> Cell
-- generateNextGenerationForCell board cell =
--     let
--         neighboringCells =
--             List.map
--                 (List.filter (\cell_ -> List.member cell_.position (neighbors cell.position)))
--                 board
--                 |> List.concat
--         aliveNeighbors =
--             List.length <| List.filter (\c -> c.status == Alive) neighboringCells
--         getNextGenForAliveCell =
--             if aliveNeighbors == 2 then
--                 Alive
--             else
--                 Dead
--         getNextGenForDeadCell =
--             if aliveNeighbors >= 3 then
--                 Alive
--             else
--                 Dead
--     in
--     { cell
--         | status =
--             if cell.status == Alive then
--                 getNextGenForAliveCell
--             else
--                 getNextGenForDeadCell
--     }
