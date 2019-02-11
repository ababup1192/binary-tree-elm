module Main exposing
    ( Model
    , Msg(..)
    , Tree(..)
    , empty
    , init
    , main
    , treeView
    , update
    , view
    )

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Http exposing (Error(..))
import Json.Decode as Decode
import Random



-- ---------------------------
-- MODEL
-- ---------------------------


type alias Model =
    { insertValue : String
    , tree : Tree
    , actionCount : Int
    , searchValue : String
    , deleteValue : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { insertValue = "", tree = Empty, actionCount = 0, searchValue = "", deleteValue = "" }, Cmd.none )



-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg
    = UpdateInsertValue String
    | Insert
    | RandomInsert
    | GenerateInsertNumbers (List Int)
    | UpdateSearchValue String
    | Search
    | UpdateDeleteValue String
    | Delete
    | Clear


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ insertValue, tree, actionCount, searchValue, deleteValue } as model) =
    case msg of
        UpdateInsertValue v ->
            ( { model | insertValue = v }, Cmd.none )

        Insert ->
            case String.toInt insertValue of
                Just iv ->
                    ( { model
                        | tree = clearTree tree |> insert iv
                        , insertValue = ""
                        , actionCount = actionCount + 1
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        RandomInsert ->
            ( model, Random.generate GenerateInsertNumbers <| Random.list 10 (Random.int 0 30) )

        GenerateInsertNumbers nums ->
            ( { model | tree = List.foldl (\num t -> insert num t) Empty nums |> clearTree }, Cmd.none )

        UpdateSearchValue v ->
            ( { model | searchValue = v }, Cmd.none )

        Search ->
            case String.toInt searchValue of
                Just iv ->
                    ( { model
                        | tree = clearTree tree |> search iv
                        , actionCount = actionCount + 1
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        UpdateDeleteValue v ->
            ( { model | deleteValue = v }, Cmd.none )

        Delete ->
            case String.toInt deleteValue of
                Just iv ->
                    ( { model
                        | tree = clearTree tree |> delete iv
                        , deleteValue = ""
                        , actionCount = actionCount + 1
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        Clear ->
            ( { model | tree = Empty }, Cmd.none )



-- ---------------------------
-- VIEW
-- ---------------------------


insert : Int -> Tree -> Tree
insert v tree =
    case tree of
        Node left currentNodeV right ->
            let
                stepNodeV =
                    { currentNodeV | isVisit = True }
            in
            if v < currentNodeV.v then
                Node (insert v left) stepNodeV right

            else
                Node left stepNodeV (insert v right)

        Empty ->
            Node Empty (NodeValue v True) Empty


search : Int -> Tree -> Tree
search v tree =
    case tree of
        Node left currentNodeV right ->
            let
                stepNodeV =
                    { currentNodeV | isVisit = True }
            in
            if v == currentNodeV.v then
                Node left stepNodeV right

            else if v < currentNodeV.v then
                Node (search v left) stepNodeV right

            else
                Node left stepNodeV (search v right)

        Empty ->
            Empty


findMax : Tree -> ( Tree, NodeValue )
findMax tree =
    case tree of
        Node left currentNV Empty ->
            ( left, currentNV )

        Node _ _ right ->
            findMax right

        Empty ->
            ( Empty, NodeValue -1 False )


replaceLeft : NodeValue -> Tree -> Tree -> Tree
replaceLeft nv maxLeftTree tree =
    case tree of
        Node left currentNV right ->
            if nv.v == currentNV.v then
                maxLeftTree

            else
                Node
                    left
                    currentNV
                    (replaceLeft nv maxLeftTree right)

        Empty ->
            Empty


delete : Int -> Tree -> Tree
delete v tree =
    let
        delete_ t =
            case t of
                Node Empty lnv Empty ->
                    -- 末端だった場合
                    if v == lnv.v then
                        Empty

                    else
                        t

                _ ->
                    delete v t
    in
    case tree of
        Node left currentNV Empty ->
            -- 削除ノードの子が左だけだった場合
            if v == currentNV.v then
                left

            else
                Node (delete_ left) currentNV Empty

        Node Empty currentNV right ->
            -- 削除ノードの子が右だけだった場合
            if v == currentNV.v then
                right

            else
                Node Empty currentNV (delete_ right)

        Node left currentNV right ->
            if v == currentNV.v then
                let
                    ( maxLeftTree, maxNV ) =
                        findMax left
                in
                Node (replaceLeft maxNV maxLeftTree left) maxNV right

            else if v < currentNV.v then
                Node (delete_ left) currentNV right

            else
                Node (delete_ left) currentNV (delete_ right)

        Empty ->
            Empty


treeMap : (NodeValue -> NodeValue) -> Tree -> Tree
treeMap f tree =
    case tree of
        Node left nodeValue right ->
            Node (treeMap f left) (f nodeValue) (treeMap f right)

        Empty ->
            Empty


clearTree : Tree -> Tree
clearTree tree =
    treeMap
        (\nodeValue -> { nodeValue | isVisit = False })
        tree


type alias NodeValue =
    { v : Int
    , isVisit : Bool
    }


type Tree
    = Node Tree NodeValue Tree
    | Empty


empty : Int -> Tree
empty v =
    Node Empty (NodeValue v False) Empty


treeView : Int -> Int -> Tree -> Html Msg
treeView actionCount step tree =
    let
        keyString =
            String.fromInt actionCount ++ "-" ++ String.fromInt step
    in
    case tree of
        Node left { v, isVisit } right ->
            Keyed.node "li"
                []
                [ ( keyString ++ "-a"
                  , a
                        [ class <|
                            if isVisit then
                                "flash-" ++ String.fromInt step

                            else
                                ""
                        , href "#"
                        ]
                        [ text <| String.fromInt v ]
                  )
                , ( keyString ++ "-ul"
                  , ul []
                        [ treeView actionCount (step + 1) left
                        , treeView actionCount (step + 1) right
                        ]
                  )
                ]

        Empty ->
            li []
                [ a [ href "#" ] [ text "" ]
                ]


view : Model -> Html Msg
view { tree, insertValue, actionCount, searchValue, deleteValue } =
    div [ class "container" ]
        [ div [ class "pure-form" ]
            [ input
                [ type_ "number"
                , placeholder "num"
                , value searchValue
                , onInput UpdateSearchValue
                ]
                []
            , a [ class "pure-button button", href "#", onClick Search ]
                [ i [ class "fas fa-search" ] []
                , text "Search"
                ]
            ]
        , div [ class "pure-form" ]
            [ input
                [ type_ "number"
                , placeholder "num"
                , value deleteValue
                , onInput UpdateDeleteValue
                ]
                []
            , a [ class "pure-button button", href "#", onClick Delete ]
                [ i [ class "fas fa-trash" ] []
                , text "Delete"
                ]
            , a [ class "pure-button button", href "#", onClick Clear ]
                [ i [ class "fas fa-ban" ] []
                , text "Clear"
                ]
            ]
        , div [ class "pure-form" ]
            [ input
                [ type_ "number"
                , placeholder "num"
                , value insertValue
                , onInput UpdateInsertValue
                ]
                []
            , a [ class "pure-button button", href "#", onClick Insert ]
                [ i [ class "fas fa-angle-double-right" ] []
                , text "Insert"
                ]
            , a [ class "pure-button button", href "#", onClick RandomInsert ]
                [ i [ class "fas fa-random" ] []
                , text "Random"
                ]
            ]
        , div [ class "tree" ]
            [ ul []
                [ treeView actionCount 0 tree
                ]
            ]
        ]



{-

-}
-- ---------------------------
-- MAIN
-- ---------------------------


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view =
            \m ->
                { title = "BinaryTree Editor"
                , body = [ view m ]
                }
        , subscriptions = \_ -> Sub.none
        }
