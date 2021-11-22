module Board exposing (..)

import Array
import Array.Extra as Array
import Html as H
import Html.Attributes as Attr
import Html.Events as Ev
import List.Extra as List
import Random as Rand
import Random.Extra as Rand


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
            String.fromInt (400 // size) ++ "px"
    in
    H.span
        (Attr.class
            (if cell.status == Alive then
                "bg-red-500"

             else
                "bg-white"
            )
            :: (convertToStyle
                    [ ( "width", cellSize )
                    , ( "height", cellSize )
                    , ( "border", "1px solid rgba(0,0,0,0.2)" )
                    , ( "display", "inline-block" )
                    ]
                    ++ [ Ev.onClick (Clicked cell.position)
                       ]
               )
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


emptyBoardGenerator : Int -> Board
emptyBoardGenerator size =
    List.range 0 (size * size - 1)
        |> List.map
            (\id ->
                let
                    row =
                        id // size

                    cell =
                        modBy size id
                in
                Cell Dead ( row, cell )
            )
        |> List.groupsOf size
        |> List.map Array.fromList
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
            if numberOfAliveCells >= 2 && numberOfAliveCells <= 3 then
                Alive

            else
                Dead

        Dead ->
            if numberOfAliveCells == 3 then
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


randomCellGenerator : Position -> Rand.Generator Cell
randomCellGenerator position =
    Rand.map
        (\randStatus ->
            { position = position
            , status = randStatus
            }
        )
        (Rand.weighted ( 80, Dead ) [ ( 20, Alive ) ])


randomBoardGenerator : Int -> Rand.Generator (List Cell)
randomBoardGenerator size =
    List.range 0 (size * size - 1)
        |> List.map
            (\id ->
                let
                    row =
                        id // size

                    cell =
                        modBy size id
                in
                randomCellGenerator ( row, cell )
            )
        |> Rand.sequence
