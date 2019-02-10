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



-- ---------------------------
-- MODEL
-- ---------------------------


type alias Model =
    { insertValue : String
    , tree : Tree
    , insertCount : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { insertValue = "", tree = Empty, insertCount = 0 }, Cmd.none )



-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg
    = UpdateInsertValue String
    | Insert


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ insertValue, tree, insertCount } as model) =
    case msg of
        UpdateInsertValue v ->
            ( { model | insertValue = v }, Cmd.none )

        Insert ->
            case String.toInt insertValue of
                Just iv ->
                    ( { model
                        | tree = clearTree tree |> insert iv
                        , insertValue = ""
                        , insertCount = insertCount + 1
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )



-- ---------------------------
-- VIEW
-- ---------------------------


insert : Int -> Tree -> Tree
insert v tree =
    case tree of
        Node left currentNodeV right ->
            let
                stepNodeV =
                    { currentNodeV | isFlash = True }
            in
            if v < currentNodeV.v then
                Node (insert v left) stepNodeV right

            else
                Node left stepNodeV (insert v right)

        Empty ->
            empty v


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
        (\nodeValue -> { nodeValue | isFlash = False })
        tree


type alias NodeValue =
    { v : Int
    , isFlash : Bool
    }


type Tree
    = Node Tree NodeValue Tree
    | Empty


empty : Int -> Tree
empty v =
    Node Empty (NodeValue v False) Empty


treeView : Int -> Int -> Tree -> Html Msg
treeView insertCount step tree =
    let
        keyString =
            String.fromInt insertCount ++ "-" ++ String.fromInt step
    in
    case tree of
        Node left { v, isFlash } right ->
            Keyed.node "li"
                []
                [ ( keyString ++ "-a"
                  , a
                        [ class <|
                            if isFlash then
                                "flash-" ++ String.fromInt step

                            else
                                ""
                        , href "#"
                        ]
                        [ text <| String.fromInt v ]
                  )
                , ( keyString ++ "-ul"
                  , ul []
                        [ treeView insertCount (step + 1) left
                        , treeView insertCount (step + 1) right
                        ]
                  )
                ]

        Empty ->
            li []
                [ a [ href "#" ] [ text "" ]
                ]


view : Model -> Html Msg
view { tree, insertValue, insertCount } =
    div [ class "container" ]
        [ div [ class "pure-form" ]
            [ input
                [ class "search"
                , type_ "number"
                , placeholder "num"
                , value insertValue
                , onInput UpdateInsertValue
                ]
                []
            , a [ class "pure-button", href "#", onClick Insert ]
                [ i [ class "fas fa-angle-double-right" ] []
                , text "Insert"
                ]
            ]
        , div [ class "tree" ]
            [ ul []
                [ treeView insertCount 0 tree
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
