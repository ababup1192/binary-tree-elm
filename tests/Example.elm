module Example exposing (treeViewTest)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


treeViewTest : Test
treeViewTest =
    describe "treeView"
        [ describe "Emptyのとき"
            [ test "Emptyのときは、aタグの中が空文字である。" <|
                \() ->
                    Empty
                        |> treeView
                        |> Query.fromHtml
                        |> Query.find [ Selector.tag "a" ]
                        |> Query.has [ Selector.text "" ]
            ]
        , describe "子を持たないノードのとき" <|
            let
                tree =
                    Node Empty 1 Empty
            in
            [ test "根の数は一つである。" <|
                \() ->
                    tree
                        |> treeView
                        |> Query.fromHtml
                        |> Query.findAll [ Selector.tag "a" ]
                        |> Query.first
                        |> Query.has [ Selector.text "1" ]
            , test "左右の子は値を持たない。" <|
                \() ->
                    tree
                        |> treeView
                        |> Query.fromHtml
                        |> Query.find [ Selector.tag "ul" ]
                        |> Query.findAll [ Selector.tag "a", Selector.text "" ]
                        |> Query.count (Expect.equal 2)
            ]
        , describe "左に子を持つノードのとき" <|
            let
                tree =
                    Node (empty 1) 2 Empty
            in
            [ test "根の数は一つである。" <|
                \() ->
                    tree
                        |> treeView
                        |> Query.fromHtml
                        |> Query.findAll [ Selector.tag "a" ]
                        |> Query.first
                        |> Query.has [ Selector.text "2" ]
            , test "左の子は値を持つ。" <|
                \() ->
                    tree
                        |> treeView
                        |> Query.fromHtml
                        |> Query.findAll [ Selector.tag "ul" ]
                        |> Query.first
                        |> Query.findAll [ Selector.tag "li" ]
                        |> Query.first
                        |> Query.has [ Selector.tag "a", Selector.text "1" ]
            ]
        , describe "左右に子を持つノードのとき" <|
            let
                tree =
                    Node (empty 1) 2 (empty 3)
            in
            [ test "根の数は一つである。" <|
                \() ->
                    tree
                        |> treeView
                        |> Query.fromHtml
                        |> Query.findAll [ Selector.tag "a" ]
                        |> Query.first
                        |> Query.has [ Selector.text "2" ]
            , test "左の子は値を持つ。" <|
                \() ->
                    tree
                        |> treeView
                        |> Query.fromHtml
                        |> Query.findAll [ Selector.tag "ul" ]
                        |> Query.first
                        |> Query.findAll [ Selector.tag "li" ]
                        |> Query.first
                        |> Query.has [ Selector.tag "a", Selector.text "1" ]
            , test "右の子は値を持つ。" <|
                \() ->
                    tree
                        |> treeView
                        |> Query.fromHtml
                        |> Query.findAll [ Selector.tag "ul" ]
                        |> Query.first
                        |> Query.findAll [ Selector.tag "li" ]
                        -- 左の兄弟, 左の子2つ, の次。(厳しいのでいい感じのテストにしたい)
                        |> Query.index 3
                        |> Query.has [ Selector.tag "a", Selector.text "3" ]
            ]
        ]
