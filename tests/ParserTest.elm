module ParserTest exposing (..)

import Expect
import Main exposing (..)
import Parser
import Test exposing (..)


routeParserTest : Test
routeParserTest =
    describe "routeParser"
        [ test "a simple route" <|
            \_ ->
                "api_v2_account_members GET /api/v2/accounts/:account_id/members(.:format)"
                    |> Parser.run routeParser
                    |> Expect.equal
                        (Ok
                            { prefix = Just "api_v2_account_members"
                            , verb = Get
                            , uri =
                                [ Verbatim "api"
                                , Verbatim "v2"
                                , Verbatim "accounts"
                                , Resource "account_id"
                                , Verbatim "members"
                                ]
                            , optional =
                                Just <|
                                    [ Dot
                                    , Resource "format"
                                    ]
                            }
                        )
        ]


verbParserTest : Test
verbParserTest =
    describe "verbParser"
        [ test "Get" <|
            \_ ->
                "GET"
                    |> Parser.run verbParser
                    |> Expect.equal (Ok Get)
        , test "Post" <|
            \_ ->
                "POST"
                    |> Parser.run verbParser
                    |> Expect.equal (Ok Post)
        , test "Patch" <|
            \_ ->
                "PATCH"
                    |> Parser.run verbParser
                    |> Expect.equal (Ok Patch)
        , test "Put" <|
            \_ ->
                "PUT"
                    |> Parser.run verbParser
                    |> Expect.equal (Ok Put)
        , test "Delete" <|
            \_ ->
                "DELETE"
                    |> Parser.run verbParser
                    |> Expect.equal (Ok Delete)
        ]


uriParserTest : Test
uriParserTest =
    describe "uriParser"
        [ test "only Verbatims" <|
            \_ ->
                "/api/v2/accounts/"
                    |> Parser.run uriParser
                    |> Expect.equal
                        (Ok
                            [ Verbatim "api"
                            , Verbatim "v2"
                            , Verbatim "accounts"
                            ]
                        )
        , test "one Verbatim with on Resource" <|
            \_ ->
                "/api/v2/accounts/:account_id"
                    |> Parser.run uriParser
                    |> Expect.equal
                        (Ok
                            [ Verbatim "api"
                            , Verbatim "v2"
                            , Verbatim "accounts"
                            , Resource "account_id"
                            ]
                        )
        , test "Verbatim, Resource, Verbatim" <|
            \_ ->
                "/api/v2/accounts/:account_id/members"
                    |> Parser.run uriParser
                    |> Expect.equal
                        (Ok
                            [ Verbatim "api"
                            , Verbatim "v2"
                            , Verbatim "accounts"
                            , Resource "account_id"
                            , Verbatim "members"
                            ]
                        )
        , test "Verbatim, Resource, Verbatim, Optional (Verbatim, Resource)" <|
            \_ ->
                "/api/v2/accounts/:account_id/members(.:format)"
                    |> Parser.run uriParser
                    |> Expect.equal
                        (Ok
                            [ Verbatim "api"
                            , Verbatim "v2"
                            , Verbatim "accounts"
                            , Resource "account_id"
                            , Verbatim "members"
                            ]
                        )
        ]


uriPartParserTest : Test
uriPartParserTest =
    describe "uriPartParser"
        [ test "Verbatim" <|
            \_ ->
                "api"
                    |> Parser.run uriPartParser
                    |> Expect.equal (Ok (Verbatim "api"))
        , test "Resource" <|
            \_ ->
                ":account_id"
                    |> Parser.run uriPartParser
                    |> Expect.equal (Ok (Resource "account_id"))
        ]
