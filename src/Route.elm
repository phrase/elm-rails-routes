module Route
    exposing
        ( Route
        , asElm
        , parse
        )

import Char
import Parser exposing ((|.), (|=), Parser)
import String.Extra as String


type Route
    = Route RouteData


type alias RouteData =
    { prefix : String
    , verb : Verb
    , uri : Uri
    , optional : Maybe Uri
    }


type Verb
    = Get
    | Post
    | Patch
    | Put
    | Delete


type alias Uri =
    List UriPart


type UriPart
    = Verbatim String
    | Resource String
    | Dot


{-| Parse strings like `"edit_api_v2_project_locale_translation GET
/api/v2/projects/:project_id/locales/:locale_id/translations/:id/edit(.:format)
api/v2/translations#edit {:format=>"json"}"`.
-}
parse : String -> Maybe Route
parse line =
    line
        |> Parser.run routeParser
        |> Result.toMaybe


{-| Generate the Elm code for a route path helper function
-}
asElm : Route -> String
asElm (Route { prefix, verb, uri }) =
    let
        indent string =
            string
                |> String.split "\n"
                |> List.map (\line -> "    " ++ line)
                |> String.join "\n"

        extractResources uriParts =
            uriParts
                |> List.map
                    (\uriPart ->
                        case uriPart of
                            Verbatim _ ->
                                []

                            Resource name ->
                                List.singleton name

                            Dot ->
                                []
                    )
                |> List.concat

        name =
            prefix
                |> String.camelize
                |> String.decapitalize

        resources =
            extractResources uri

        path =
            [ "[ "
            , uri
                |> List.map
                    (\uriPart ->
                        case uriPart of
                            Verbatim text ->
                                "\"" ++ text ++ "\""

                            Resource name ->
                                [ "Http.encodeUri"
                                , name
                                ]
                                    |> String.join " "

                            Dot ->
                                "\".\""
                    )
                |> String.join "\n, "
            , "\n]\n"
            , indent "|> String.join \"/\"\n|> String.append \"/\""
            ]
                |> String.concat
    in
    [ [ name
      , "String"
            |> List.repeat (1 + List.length resources)
            |> String.join " -> "
      ]
        |> String.join " : "
    , [ [ name ]
      , resources
      , [ "=" ]
      ]
        |> List.concat
        |> String.join " "
    , indent path
    ]
        |> String.join "\n"



---- PARSER


routeParser : Parser Route
routeParser =
    Parser.map Route <|
        Parser.succeed RouteData
            |. spaces
            |= Parser.delayedCommitMap (\name _ -> name)
                nameParser
                (Parser.succeed ())
            |. spaces
            |= verbParser
            |. spaces
            |= uriParser
            |= Parser.oneOf
                [ Parser.succeed Just
                    |. Parser.symbol "("
                    |= actualUriParser
                    |. Parser.symbol ")"
                , Parser.succeed Nothing
                ]


verbParser : Parser Verb
verbParser =
    let
        matchKeyword string constructor =
            Parser.keyword string
                |> Parser.map (always constructor)
    in
    Parser.oneOf
        [ matchKeyword "GET" Get
        , matchKeyword "POST" Post
        , matchKeyword "PATCH" Patch
        , matchKeyword "PUT" Put
        , matchKeyword "DELETE" Delete
        ]


uriParser : Parser Uri
uriParser =
    Parser.succeed identity
        |. Parser.symbol "/"
        |= actualUriParser


actualUriParser : Parser Uri
actualUriParser =
    Parser.succeed identity
        |= Parser.andThen
            (\uriPart ->
                uriParserHelp [ uriPart ]
            )
            uriPartParser


uriParserHelp : List UriPart -> Parser (List UriPart)
uriParserHelp uriParts =
    Parser.succeed identity
        |= Parser.oneOf
            [ Parser.delayedCommit (Parser.symbol "/") <|
                Parser.andThen
                    (\uriPart ->
                        uriParserHelp (uriPart :: uriParts)
                    )
                    uriPartParser
            , Parser.andThen
                (\uriPart ->
                    uriParserHelp (uriPart :: uriParts)
                )
                uriPartParser
            , Parser.succeed (List.reverse uriParts)
            ]


uriPartParser : Parser UriPart
uriPartParser =
    Parser.oneOf
        [ Parser.delayedCommit (Parser.symbol ":") <|
            Parser.map Resource <|
                nameParser
        , Parser.map (\_ -> Dot) <|
            Parser.symbol "."
        , Parser.map Verbatim nameParser
        ]



---- PARSER HELPER


spaces : Parser ()
spaces =
    Parser.ignore Parser.zeroOrMore (\c -> c == ' ')


nameParser : Parser String
nameParser =
    Parser.source <|
        Parser.ignore Parser.oneOrMore <|
            \c ->
                Char.isLower c
                    || Char.isDigit c
                    || (c == '_')
