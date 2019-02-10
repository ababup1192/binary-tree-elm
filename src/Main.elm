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
import Http exposing (Error(..))
import Json.Decode as Decode



-- ---------------------------
-- MODEL
-- ---------------------------


type alias Model =
    { insertValue : String
    , tree : Tree
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { insertValue = "", tree = Empty }, Cmd.none )



-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg
    = UpdateInsertValue String
    | Insert


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ insertValue, tree } as model) =
    case msg of
        UpdateInsertValue v ->
            ( { model | insertValue = v }, Cmd.none )

        Insert ->
            ( { model | tree = insert (Maybe.withDefault -1 (String.toInt insertValue)) tree, insertValue = "" }, Cmd.none )



-- ---------------------------
-- VIEW
-- ---------------------------


insert : Int -> Tree -> Tree
insert v tree =
    case tree of
        Node left currentV right ->
            if v < currentV then
                Node (insert v left) currentV right

            else
                Node left currentV (insert v right)

        Empty ->
            empty v


type Tree
    = Node Tree Int Tree
    | Empty


empty : Int -> Tree
empty v =
    Node Empty v Empty


treeView : Tree -> Html Msg
treeView tree =
    case tree of
        Node left v right ->
            li []
                [ a [ href "#" ]
                    [ text <| String.fromInt v ]
                , ul []
                    [ treeView left
                    , treeView right
                    ]
                ]

        Empty ->
            li []
                [ a [ href "#" ] [ text "" ]
                ]


view : Model -> Html Msg
view { tree, insertValue } =
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
                [ treeView tree
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
