port module Main exposing (..)

import Char
import Json.Decode
import Parser exposing ((|.), (|=), Parser)
import Platform
import String.Extra as String


main : Program Never {} String
main =
    Platform.program
        { init = ( {}, Cmd.none )
        , update = update
        , subscriptions = \_ -> read identity
        }


port read : (String -> msg) -> Sub msg


port print : String -> Cmd msg


update : String -> {} -> ( {}, Cmd String )
update routes model =
    ( model
    , [ [ "module PA.Routes exposing (..)"
        , "import Http"
        ]
            |> String.join "\n\n"
      , routes
            |> String.split "\n"
            |> List.filterMap (Parser.run routeParser >> Result.toMaybe)
            |> List.filter (\{ prefix } -> prefix /= Nothing)
            |> List.map generateRouteHelper
            |> String.join "\n\n\n"
      ]
        |> String.join "\n\n\n"
        |> print
    )



---- MODEL


type alias Route =
    { prefix : Maybe String
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


routeParser : Parser Route
routeParser =
    Parser.succeed Route
        |. spaces
        |= Parser.oneOf
            [ Parser.delayedCommitMap (\name _ -> Just name)
                nameParser
                (Parser.succeed ())
            , Parser.succeed Nothing
            ]
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



---- GENERATE


generateRouteHelper : Route -> String
generateRouteHelper { prefix, verb, uri } =
    case prefix of
        Nothing ->
            Debug.crash "no prefix given"

        Just prefix ->
            let
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


indent string =
    string
        |> String.split "\n"
        |> List.map (\line -> "    " ++ line)
        |> String.join "\n"


extractResources : Uri -> List String
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
