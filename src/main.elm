module Main exposing (Board, Cell, GameState(..), HorizPos(..), Model, Player(..), Position, Value(..), VertPos(..), allPositions, cellAttributes, cellTxt, columns, css, filterByCol, filterByRow, hPosToStr, htmlFrom, init, isAllO, isAllX, main, markForPlayer, playerToStr, rows, stateStr, update, updatePlayer, updateState, vPosToStr, valToStr, view)

import Browser
import Html exposing (Attribute, Html, br, caption, div, table, tbody, td, text, thead, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Tuple


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type VertPos
    = Top
    | Middle
    | Bottom


type HorizPos
    = Left
    | Center
    | Right


type alias Position =
    ( VertPos, HorizPos )


type Value
    = X
    | O
    | Empty


type alias Cell =
    { position : Position, value : Value }


type alias Row =
    List Cell


type alias Board =
    List Cell


type Player
    = PlayerX
    | PlayerO


type GameState
    = Active
    | Tie
    | Won Player


type alias Model =
    { board : Board
    , currentPlayer : Player
    , gameState : GameState
    }



-- INIT


rows : List VertPos
rows =
    [ Top, Middle, Bottom ]


columns : List HorizPos
columns =
    [ Left, Center, Right ]


allPositions : List Position
allPositions =
    columns
        |> List.map (\r -> List.map (\c -> ( c, r )) rows)
        |> List.concat


init : Model
init =
    { board = List.map (\p -> Cell p Empty) allPositions
    , currentPlayer = PlayerX
    , gameState = Active
    }



-- VIEW


view : Model -> Html Position
view model =
    div []
        [ Html.node "style" [] [ text css ]
        , table []
            [ caption [] [ text "TicTacToe" ]
            , tbody [] (htmlFrom model.board)
            , tr []
                [ td [ colspan 3, align "center" ]
                    [ text <| stateStr model ]
                ]
            ]
        ]


htmlFrom : Board -> List (Html Position)
htmlFrom board =
    rows
        |> List.map (\r -> filterByRow r board)
        |> List.map makeRowHtml


filterByRow : VertPos -> Board -> Row
filterByRow pos board =
    List.filter (\cell -> Tuple.first cell.position == pos) board


filterByCol : HorizPos -> Board -> Row
filterByCol pos board =
    List.filter (\cell -> Tuple.second cell.position == pos) board


makeRowHtml : Row -> Html Position
makeRowHtml row =
    tr [] (List.map makeCellHtml row)


makeCellHtml : Cell -> Html Position
makeCellHtml cell =
    td (cellAttributes cell) (cellTxt cell)


cellAttributes : Cell -> List (Attribute Position)
cellAttributes cell =
    let
        ( v, h ) =
            cell.position
    in
    [ onClick cell.position, class (vPosToStr v ++ " " ++ hPosToStr h) ]


cellTxt : Cell -> List (Html Position)
cellTxt cell =
    [ text <| valToStr cell.value ]


stateStr : Model -> String
stateStr model =
    case model.gameState of
        Active ->
            playerToStr model.currentPlayer ++ "'s turn."

        Tie ->
            "It's a tie :("

        Won winningPlayer ->
            playerToStr winningPlayer ++ " wins !!"


valToStr : Value -> String
valToStr val =
    case val of
        X ->
            "X"

        O ->
            "O"

        Empty ->
            " - "


vPosToStr : VertPos -> String
vPosToStr p =
    case p of
        Top ->
            "Top"

        Middle ->
            "Middle"

        Bottom ->
            "Bottom"


hPosToStr : HorizPos -> String
hPosToStr p =
    case p of
        Left ->
            "Left"

        Center ->
            "Center"

        Right ->
            "Right"


playerToStr : Player -> String
playerToStr p =
    case p of
        PlayerX ->
            "Player X"

        PlayerO ->
            "Player O"


css : String
css =
    """
    table { border-spacing:0px; }
    td { padding: 10px; text-align: center}
    .Center {
        border-left: .1em solid black;
        border-right: .1em solid black;
    }
    .Middle {
        border-top: .1em solid black;
        border-bottom: .1em solid black;
    }
    """



-- UPDATE


update : Position -> Model -> Model
update clkPos model =
    let
        updatedBoard =
            updateCell clkPos model

        clkPosIsEmpty =
            model.board |> List.member { position = clkPos, value = Empty }
    in
    if clkPosIsEmpty && model.gameState == Active then
        { model
            | board = updatedBoard
            , currentPlayer = updatePlayer model.currentPlayer
            , gameState = updateState updatedBoard
        }

    else
        model


updateCell : Position -> Model -> Board
updateCell clkPos model =
    List.map
        (\cell ->
            if cell.position == clkPos then
                { position = cell.position
                , value = markForPlayer model.currentPlayer
                }

            else
                cell
        )
        model.board


updateState : Board -> GameState
updateState board =
    let
        hLines =
            rows
                |> List.map (\row -> filterByRow row board)

        vLines =
            columns
                |> List.map (\col -> filterByCol col board)

        diagonals =
            [ List.filter (\c -> c.position == ( Top, Right ) || c.position == ( Middle, Center ) || c.position == ( Bottom, Left )) board
            , List.filter (\c -> c.position == ( Bottom, Right ) || c.position == ( Middle, Center ) || c.position == ( Top, Left )) board
            ]

        posWins =
            List.concat [ hLines, vLines, diagonals ]
    in
    if List.any (\line -> isAllX line) posWins then
        Won PlayerX

    else if List.any (\line -> isAllO line) posWins then
        Won PlayerO

    else if List.any (\line -> hasEmpty line) posWins then
        Active

    else
        Tie


isAllX : List Cell -> Bool
isAllX line =
    List.all (\c -> c.value == X) line


isAllO : List Cell -> Bool
isAllO line =
    List.all (\c -> c.value == O) line


hasEmpty : List Cell -> Bool
hasEmpty line =
    List.any (\c -> c.value == Empty) line


markForPlayer : Player -> Value
markForPlayer player =
    case player of
        PlayerX ->
            X

        PlayerO ->
            O


updatePlayer : Player -> Player
updatePlayer player =
    case player of
        PlayerX ->
            PlayerO

        PlayerO ->
            PlayerX
