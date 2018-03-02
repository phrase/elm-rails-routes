module Main exposing (..)

import Char
import Html exposing (Html)
import Parser exposing ((|.), (|=), Parser)
import String.Extra as String


main =
    Html.beginnerProgram
        { model = init
        , update = update
        , view = view
        }


init =
    {}


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
                    String.camelize prefix

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



---- UPDATE


update msg model =
    model



---- VIEW


view model =
    Html.pre []
        [ Html.code []
            [ [ [ "module PA.Routes exposing (..)"
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
                |> Html.text
            ]
        ]



---- DATA


routes =
    """
                                                                                    github_api_v2_hooks POST       /api/v2/hooks/github(.:format)                                                                                                                                                                     api/v2/hooks#github {:format=>/json/}
                                                                                         api_v2_formats GET        /api/v2/formats(.:format)                                                                                                                                                                          api/v2/formats#index {:format=>"json"}
                                                                                            api_v2_user GET        /api/v2/user(.:format)                                                                                                                                                                             api/v2/users#show {:format=>"json"}
                                                                                 api_v2_account_members GET        /api/v2/accounts/:account_id/members(.:format)                                                                                                                                                     api/v2/members#index {:format=>"json"}
                                                                                  api_v2_account_member GET        /api/v2/accounts/:account_id/members/:id(.:format)                                                                                                                                                 api/v2/members#show {:format=>"json"}
                                                                                                        PATCH      /api/v2/accounts/:account_id/members/:id(.:format)                                                                                                                                                 api/v2/members#update {:format=>"json"}
                                                                                                        PUT        /api/v2/accounts/:account_id/members/:id(.:format)                                                                                                                                                 api/v2/members#update {:format=>"json"}
                                                                                                        DELETE     /api/v2/accounts/:account_id/members/:id(.:format)                                                                                                                                                 api/v2/members#destroy {:format=>"json"}
                                                                       resend_api_v2_account_invitation POST       /api/v2/accounts/:account_id/invitations/:id/resend(.:format)                                                                                                                                      api/v2/invitations#resend {:format=>"json"}
                                                                             api_v2_account_invitations GET        /api/v2/accounts/:account_id/invitations(.:format)                                                                                                                                                 api/v2/invitations#index {:format=>"json"}
                                                                                                        POST       /api/v2/accounts/:account_id/invitations(.:format)                                                                                                                                                 api/v2/invitations#create {:format=>"json"}
                                                                          new_api_v2_account_invitation GET        /api/v2/accounts/:account_id/invitations/new(.:format)                                                                                                                                             api/v2/invitations#new {:format=>"json"}
                                                                         edit_api_v2_account_invitation GET        /api/v2/accounts/:account_id/invitations/:id/edit(.:format)                                                                                                                                        api/v2/invitations#edit {:format=>"json"}
                                                                              api_v2_account_invitation GET        /api/v2/accounts/:account_id/invitations/:id(.:format)                                                                                                                                             api/v2/invitations#show {:format=>"json"}
                                                                                                        PATCH      /api/v2/accounts/:account_id/invitations/:id(.:format)                                                                                                                                             api/v2/invitations#update {:format=>"json"}
                                                                                                        PUT        /api/v2/accounts/:account_id/invitations/:id(.:format)                                                                                                                                             api/v2/invitations#update {:format=>"json"}
                                                                                                        DELETE     /api/v2/accounts/:account_id/invitations/:id(.:format)                                                                                                                                             api/v2/invitations#destroy {:format=>"json"}
                                       api_v2_account_glossary_glossary_term_glossary_term_translations POST       /api/v2/accounts/:account_id/glossaries/:glossary_id/terms/:glossary_term_id/translations(.:format)                                                                                                api/v2/glossary_term_translations#create {:format=>"json"}
                                        api_v2_account_glossary_glossary_term_glossary_term_translation PATCH      /api/v2/accounts/:account_id/glossaries/:glossary_id/terms/:glossary_term_id/translations/:id(.:format)                                                                                            api/v2/glossary_term_translations#update {:format=>"json"}
                                                                                                        PUT        /api/v2/accounts/:account_id/glossaries/:glossary_id/terms/:glossary_term_id/translations/:id(.:format)                                                                                            api/v2/glossary_term_translations#update {:format=>"json"}
                                                                                                        DELETE     /api/v2/accounts/:account_id/glossaries/:glossary_id/terms/:glossary_term_id/translations/:id(.:format)                                                                                            api/v2/glossary_term_translations#destroy {:format=>"json"}
                                                                 api_v2_account_glossary_glossary_terms GET        /api/v2/accounts/:account_id/glossaries/:glossary_id/terms(.:format)                                                                                                                               api/v2/glossary_terms#index {:format=>"json"}
                                                                                                        POST       /api/v2/accounts/:account_id/glossaries/:glossary_id/terms(.:format)                                                                                                                               api/v2/glossary_terms#create {:format=>"json"}
                                                                  api_v2_account_glossary_glossary_term GET        /api/v2/accounts/:account_id/glossaries/:glossary_id/terms/:id(.:format)                                                                                                                           api/v2/glossary_terms#show {:format=>"json"}
                                                                                                        PATCH      /api/v2/accounts/:account_id/glossaries/:glossary_id/terms/:id(.:format)                                                                                                                           api/v2/glossary_terms#update {:format=>"json"}
                                                                                                        PUT        /api/v2/accounts/:account_id/glossaries/:glossary_id/terms/:id(.:format)                                                                                                                           api/v2/glossary_terms#update {:format=>"json"}
                                                                                                        DELETE     /api/v2/accounts/:account_id/glossaries/:glossary_id/terms/:id(.:format)                                                                                                                           api/v2/glossary_terms#destroy {:format=>"json"}
                                                                              api_v2_account_glossaries GET        /api/v2/accounts/:account_id/glossaries(.:format)                                                                                                                                                  api/v2/glossaries#index {:format=>"json"}
                                                                                                        POST       /api/v2/accounts/:account_id/glossaries(.:format)                                                                                                                                                  api/v2/glossaries#create {:format=>"json"}
                                                                                api_v2_account_glossary GET        /api/v2/accounts/:account_id/glossaries/:id(.:format)                                                                                                                                              api/v2/glossaries#show {:format=>"json"}
                                                                                                        PATCH      /api/v2/accounts/:account_id/glossaries/:id(.:format)                                                                                                                                              api/v2/glossaries#update {:format=>"json"}
                                                                                                        PUT        /api/v2/accounts/:account_id/glossaries/:id(.:format)                                                                                                                                              api/v2/glossaries#update {:format=>"json"}
                                                                                                        DELETE     /api/v2/accounts/:account_id/glossaries/:id(.:format)                                                                                                                                              api/v2/glossaries#destroy {:format=>"json"}
                                                                                        api_v2_accounts GET        /api/v2/accounts(.:format)                                                                                                                                                                         api/v2/accounts#index {:format=>"json"}
                                                                                         api_v2_account GET        /api/v2/accounts/:id(.:format)                                                                                                                                                                     api/v2/accounts#show {:format=>"json"}
                                                                                  api_v2_authorizations GET        /api/v2/authorizations(.:format)                                                                                                                                                                   api/v2/authorizations#index {:format=>"json"}
                                                                                                        POST       /api/v2/authorizations(.:format)                                                                                                                                                                   api/v2/authorizations#create {:format=>"json"}
                                                                               new_api_v2_authorization GET        /api/v2/authorizations/new(.:format)                                                                                                                                                               api/v2/authorizations#new {:format=>"json"}
                                                                              edit_api_v2_authorization GET        /api/v2/authorizations/:id/edit(.:format)                                                                                                                                                          api/v2/authorizations#edit {:format=>"json"}
                                                                                   api_v2_authorization GET        /api/v2/authorizations/:id(.:format)                                                                                                                                                               api/v2/authorizations#show {:format=>"json"}
                                                                                                        PATCH      /api/v2/authorizations/:id(.:format)                                                                                                                                                               api/v2/authorizations#update {:format=>"json"}
                                                                                                        PUT        /api/v2/authorizations/:id(.:format)                                                                                                                                                               api/v2/authorizations#update {:format=>"json"}
                                                                                                        DELETE     /api/v2/authorizations/:id(.:format)                                                                                                                                                               api/v2/authorizations#destroy {:format=>"json"}
                                                                             api_v2_oauth_authorization GET|POST   /api/v2/authorizations/oauth/:provider(.:format)                                                                                                                                                   api/v2/authorizations#oauth {:format=>/html/, :provider=>/github|facebook|twitter|linkedin|google_oauth2/}
                                                                     complete_api_v2_project_job_locale POST       /api/v2/projects/:project_id/jobs/:job_id/locales/:id/complete(.:format)                                                                                                                           api/v2/jobs/locales#complete {:format=>"json"}
                                                                             api_v2_project_job_locales GET        /api/v2/projects/:project_id/jobs/:job_id/locales(.:format)                                                                                                                                        api/v2/jobs/locales#index {:format=>"json"}
                                                                                                        POST       /api/v2/projects/:project_id/jobs/:job_id/locales(.:format)                                                                                                                                        api/v2/jobs/locales#create {:format=>"json"}
                                                                          new_api_v2_project_job_locale GET        /api/v2/projects/:project_id/jobs/:job_id/locales/new(.:format)                                                                                                                                    api/v2/jobs/locales#new {:format=>"json"}
                                                                         edit_api_v2_project_job_locale GET        /api/v2/projects/:project_id/jobs/:job_id/locales/:id/edit(.:format)                                                                                                                               api/v2/jobs/locales#edit {:format=>"json"}
                                                                              api_v2_project_job_locale GET        /api/v2/projects/:project_id/jobs/:job_id/locales/:id(.:format)                                                                                                                                    api/v2/jobs/locales#show {:format=>"json"}
                                                                                                        PATCH      /api/v2/projects/:project_id/jobs/:job_id/locales/:id(.:format)                                                                                                                                    api/v2/jobs/locales#update {:format=>"json"}
                                                                                                        PUT        /api/v2/projects/:project_id/jobs/:job_id/locales/:id(.:format)                                                                                                                                    api/v2/jobs/locales#update {:format=>"json"}
                                                                                                        DELETE     /api/v2/projects/:project_id/jobs/:job_id/locales/:id(.:format)                                                                                                                                    api/v2/jobs/locales#destroy {:format=>"json"}
                                                                               start_api_v2_project_job POST       /api/v2/projects/:project_id/jobs/:id/start(.:format)                                                                                                                                              api/v2/jobs#start {:format=>"json"}
                                                                            complete_api_v2_project_job POST       /api/v2/projects/:project_id/jobs/:id/complete(.:format)                                                                                                                                           api/v2/jobs#complete {:format=>"json"}
                                                                                keys_api_v2_project_job POST       /api/v2/projects/:project_id/jobs/:id/keys(.:format)                                                                                                                                               api/v2/jobs#keys {:format=>"json"}
                                                                                                        DELETE     /api/v2/projects/:project_id/jobs/:id/keys(.:format)                                                                                                                                               api/v2/jobs#remove_keys {:format=>"json"}
                                                                                    api_v2_project_jobs GET        /api/v2/projects/:project_id/jobs(.:format)                                                                                                                                                        api/v2/jobs#index {:format=>"json"}
                                                                                                        POST       /api/v2/projects/:project_id/jobs(.:format)                                                                                                                                                        api/v2/jobs#create {:format=>"json"}
                                                                                 new_api_v2_project_job GET        /api/v2/projects/:project_id/jobs/new(.:format)                                                                                                                                                    api/v2/jobs#new {:format=>"json"}
                                                                                edit_api_v2_project_job GET        /api/v2/projects/:project_id/jobs/:id/edit(.:format)                                                                                                                                               api/v2/jobs#edit {:format=>"json"}
                                                                                     api_v2_project_job GET        /api/v2/projects/:project_id/jobs/:id(.:format)                                                                                                                                                    api/v2/jobs#show {:format=>"json"}
                                                                                                        PATCH      /api/v2/projects/:project_id/jobs/:id(.:format)                                                                                                                                                    api/v2/jobs#update {:format=>"json"}
                                                                                                        PUT        /api/v2/projects/:project_id/jobs/:id(.:format)                                                                                                                                                    api/v2/jobs#update {:format=>"json"}
                                                                                                        DELETE     /api/v2/projects/:project_id/jobs/:id(.:format)                                                                                                                                                    api/v2/jobs#destroy {:format=>"json"}
                                                                     search_api_v2_project_translations POST       /api/v2/projects/:project_id/translations/search(.:format)                                                                                                                                         api/v2/translations#index {:format=>"json"}
                                                                     verify_api_v2_project_translations PATCH      /api/v2/projects/:project_id/translations/verify(.:format)                                                                                                                                         api/v2/translations#verify {:format=>"json"}
                                                                   unverify_api_v2_project_translations PATCH      /api/v2/projects/:project_id/translations/unverify(.:format)                                                                                                                                       api/v2/translations#unverify {:format=>"json"}
                                                                    exclude_api_v2_project_translations PATCH      /api/v2/projects/:project_id/translations/exclude(.:format)                                                                                                                                        api/v2/translations#exclude {:format=>"json"}
                                                                    include_api_v2_project_translations PATCH      /api/v2/projects/:project_id/translations/include(.:format)                                                                                                                                        api/v2/translations#include {:format=>"json"}
                                                           machine_translate_api_v2_project_translation PATCH      /api/v2/projects/:project_id/translations/:id/machine_translate(.:format)                                                                                                                          api/v2/translations#machine_translate {:format=>"json"}
                                                                    api_v2_project_translation_versions GET        /api/v2/projects/:project_id/translations/:translation_id/versions(.:format)                                                                                                                       api/v2/translation_versions#index {:format=>"json"}
                                                                     api_v2_project_translation_version GET        /api/v2/projects/:project_id/translations/:translation_id/versions/:id(.:format)                                                                                                                   api/v2/translation_versions#show {:format=>"json"}
                                                                            api_v2_project_translations GET        /api/v2/projects/:project_id/translations(.:format)                                                                                                                                                api/v2/translations#index {:format=>"json"}
                                                                                                        POST       /api/v2/projects/:project_id/translations(.:format)                                                                                                                                                api/v2/translations#create {:format=>"json"}
                                                                         new_api_v2_project_translation GET        /api/v2/projects/:project_id/translations/new(.:format)                                                                                                                                            api/v2/translations#new {:format=>"json"}
                                                                        edit_api_v2_project_translation GET        /api/v2/projects/:project_id/translations/:id/edit(.:format)                                                                                                                                       api/v2/translations#edit {:format=>"json"}
                                                                             api_v2_project_translation GET        /api/v2/projects/:project_id/translations/:id(.:format)                                                                                                                                            api/v2/translations#show {:format=>"json"}
                                                                                                        PATCH      /api/v2/projects/:project_id/translations/:id(.:format)                                                                                                                                            api/v2/translations#update {:format=>"json"}
                                                                                                        PUT        /api/v2/projects/:project_id/translations/:id(.:format)                                                                                                                                            api/v2/translations#update {:format=>"json"}
                                                                                                        DELETE     /api/v2/projects/:project_id/translations/:id(.:format)                                                                                                                                            api/v2/translations#destroy {:format=>"json"}
                                                                                   api_v2_project_badge GET        /api/v2/projects/:project_id/badge(.:format)                                                                                                                                                       api/v2/badges#show {:format=>"svg"}
                                                              search_api_v2_project_locale_translations POST       /api/v2/projects/:project_id/locales/:locale_id/translations/search(.:format)                                                                                                                      api/v2/translations#index {:format=>"json"}
                                                              verify_api_v2_project_locale_translations PATCH      /api/v2/projects/:project_id/locales/:locale_id/translations/verify(.:format)                                                                                                                      api/v2/translations#verify {:format=>"json"}
                                                            unverify_api_v2_project_locale_translations PATCH      /api/v2/projects/:project_id/locales/:locale_id/translations/unverify(.:format)                                                                                                                    api/v2/translations#unverify {:format=>"json"}
                                                             exclude_api_v2_project_locale_translations PATCH      /api/v2/projects/:project_id/locales/:locale_id/translations/exclude(.:format)                                                                                                                     api/v2/translations#exclude {:format=>"json"}
                                                             include_api_v2_project_locale_translations PATCH      /api/v2/projects/:project_id/locales/:locale_id/translations/include(.:format)                                                                                                                     api/v2/translations#include {:format=>"json"}
                                                    machine_translate_api_v2_project_locale_translation PATCH      /api/v2/projects/:project_id/locales/:locale_id/translations/:id/machine_translate(.:format)                                                                                                       api/v2/translations#machine_translate {:format=>"json"}
                                                             api_v2_project_locale_translation_versions GET        /api/v2/projects/:project_id/locales/:locale_id/translations/:translation_id/versions(.:format)                                                                                                    api/v2/translation_versions#index {:format=>"json"}
                                                              api_v2_project_locale_translation_version GET        /api/v2/projects/:project_id/locales/:locale_id/translations/:translation_id/versions/:id(.:format)                                                                                                api/v2/translation_versions#show {:format=>"json"}
                                                                     api_v2_project_locale_translations GET        /api/v2/projects/:project_id/locales/:locale_id/translations(.:format)                                                                                                                             api/v2/translations#index {:format=>"json"}
                                                                                                        POST       /api/v2/projects/:project_id/locales/:locale_id/translations(.:format)                                                                                                                             api/v2/translations#create {:format=>"json"}
                                                                  new_api_v2_project_locale_translation GET        /api/v2/projects/:project_id/locales/:locale_id/translations/new(.:format)                                                                                                                         api/v2/translations#new {:format=>"json"}
                                                                 edit_api_v2_project_locale_translation GET        /api/v2/projects/:project_id/locales/:locale_id/translations/:id/edit(.:format)                                                                                                                    api/v2/translations#edit {:format=>"json"}
                                                                      api_v2_project_locale_translation GET        /api/v2/projects/:project_id/locales/:locale_id/translations/:id(.:format)                                                                                                                         api/v2/translations#show {:format=>"json"}
                                                                                                        PATCH      /api/v2/projects/:project_id/locales/:locale_id/translations/:id(.:format)                                                                                                                         api/v2/translations#update {:format=>"json"}
                                                                                                        PUT        /api/v2/projects/:project_id/locales/:locale_id/translations/:id(.:format)                                                                                                                         api/v2/translations#update {:format=>"json"}
                                                                                                        DELETE     /api/v2/projects/:project_id/locales/:locale_id/translations/:id(.:format)                                                                                                                         api/v2/translations#destroy {:format=>"json"}
                                                                         download_api_v2_project_locale GET        /api/v2/projects/:project_id/locales/:id/download(.:format)                                                                                                                                        api/v2/locales#download {:format=>"json"}
                                                                                 api_v2_project_locales GET        /api/v2/projects/:project_id/locales(.:format)                                                                                                                                                     api/v2/locales#index {:format=>"json"}
                                                                                                        POST       /api/v2/projects/:project_id/locales(.:format)                                                                                                                                                     api/v2/locales#create {:format=>"json"}
                                                                              new_api_v2_project_locale GET        /api/v2/projects/:project_id/locales/new(.:format)                                                                                                                                                 api/v2/locales#new {:format=>"json"}
                                                                             edit_api_v2_project_locale GET        /api/v2/projects/:project_id/locales/:id/edit(.:format)                                                                                                                                            api/v2/locales#edit {:format=>"json"}
                                                                                  api_v2_project_locale GET        /api/v2/projects/:project_id/locales/:id(.:format)                                                                                                                                                 api/v2/locales#show {:format=>"json"}
                                                                                                        PATCH      /api/v2/projects/:project_id/locales/:id(.:format)                                                                                                                                                 api/v2/locales#update {:format=>"json"}
                                                                                                        PUT        /api/v2/projects/:project_id/locales/:id(.:format)                                                                                                                                                 api/v2/locales#update {:format=>"json"}
                                                                                                        DELETE     /api/v2/projects/:project_id/locales/:id(.:format)                                                                                                                                                 api/v2/locales#destroy {:format=>"json"}
                                                                                    api_v2_project_tags GET        /api/v2/projects/:project_id/tags(.:format)                                                                                                                                                        api/v2/tags#index {:format=>"json"}
                                                                                                        POST       /api/v2/projects/:project_id/tags(.:format)                                                                                                                                                        api/v2/tags#create {:format=>"json"}
                                                                                 new_api_v2_project_tag GET        /api/v2/projects/:project_id/tags/new(.:format)                                                                                                                                                    api/v2/tags#new {:format=>"json"}
                                                                                edit_api_v2_project_tag GET        /api/v2/projects/:project_id/tags/:id/edit(.:format)                                                                                                                                               api/v2/tags#edit {:format=>"json"}
                                                                                     api_v2_project_tag GET        /api/v2/projects/:project_id/tags/:id(.:format)                                                                                                                                                    api/v2/tags#show {:format=>"json"}
                                                                                                        DELETE     /api/v2/projects/:project_id/tags/:id(.:format)                                                                                                                                                    api/v2/tags#destroy {:format=>"json"}
                                                                             api_v2_project_styleguides GET        /api/v2/projects/:project_id/styleguides(.:format)                                                                                                                                                 api/v2/styleguides#index {:format=>"json"}
                                                                                                        POST       /api/v2/projects/:project_id/styleguides(.:format)                                                                                                                                                 api/v2/styleguides#create {:format=>"json"}
                                                                          new_api_v2_project_styleguide GET        /api/v2/projects/:project_id/styleguides/new(.:format)                                                                                                                                             api/v2/styleguides#new {:format=>"json"}
                                                                         edit_api_v2_project_styleguide GET        /api/v2/projects/:project_id/styleguides/:id/edit(.:format)                                                                                                                                        api/v2/styleguides#edit {:format=>"json"}
                                                                              api_v2_project_styleguide GET        /api/v2/projects/:project_id/styleguides/:id(.:format)                                                                                                                                             api/v2/styleguides#show {:format=>"json"}
                                                                                                        PATCH      /api/v2/projects/:project_id/styleguides/:id(.:format)                                                                                                                                             api/v2/styleguides#update {:format=>"json"}
                                                                                                        PUT        /api/v2/projects/:project_id/styleguides/:id(.:format)                                                                                                                                             api/v2/styleguides#update {:format=>"json"}
                                                                                                        DELETE     /api/v2/projects/:project_id/styleguides/:id(.:format)                                                                                                                                             api/v2/styleguides#destroy {:format=>"json"}
                                                                            test_api_v2_project_webhook POST       /api/v2/projects/:project_id/webhooks/:id/test(.:format)                                                                                                                                           api/v2/webhooks#test {:format=>"json"}
                                                                                api_v2_project_webhooks GET        /api/v2/projects/:project_id/webhooks(.:format)                                                                                                                                                    api/v2/webhooks#index {:format=>"json"}
                                                                                                        POST       /api/v2/projects/:project_id/webhooks(.:format)                                                                                                                                                    api/v2/webhooks#create {:format=>"json"}
                                                                             new_api_v2_project_webhook GET        /api/v2/projects/:project_id/webhooks/new(.:format)                                                                                                                                                api/v2/webhooks#new {:format=>"json"}
                                                                            edit_api_v2_project_webhook GET        /api/v2/projects/:project_id/webhooks/:id/edit(.:format)                                                                                                                                           api/v2/webhooks#edit {:format=>"json"}
                                                                                 api_v2_project_webhook GET        /api/v2/projects/:project_id/webhooks/:id(.:format)                                                                                                                                                api/v2/webhooks#show {:format=>"json"}
                                                                                                        PATCH      /api/v2/projects/:project_id/webhooks/:id(.:format)                                                                                                                                                api/v2/webhooks#update {:format=>"json"}
                                                                                                        PUT        /api/v2/projects/:project_id/webhooks/:id(.:format)                                                                                                                                                api/v2/webhooks#update {:format=>"json"}
                                                                                                        DELETE     /api/v2/projects/:project_id/webhooks/:id(.:format)                                                                                                                                                api/v2/webhooks#destroy {:format=>"json"}
                                                                        read_api_v2_project_key_comment PATCH      /api/v2/projects/:project_id/keys/:key_id/comments/:id/read(.:format)                                                                                                                              api/v2/comments#mark_as_read {:format=>"json"}
                                                                                                        DELETE     /api/v2/projects/:project_id/keys/:key_id/comments/:id/read(.:format)                                                                                                                              api/v2/comments#mark_as_unread {:format=>"json"}
                                                                                                        GET        /api/v2/projects/:project_id/keys/:key_id/comments/:id/read(.:format)                                                                                                                              api/v2/comments#read {:format=>"json"}
                                                                            api_v2_project_key_comments GET        /api/v2/projects/:project_id/keys/:key_id/comments(.:format)                                                                                                                                       api/v2/comments#index {:format=>"json"}
                                                                                                        POST       /api/v2/projects/:project_id/keys/:key_id/comments(.:format)                                                                                                                                       api/v2/comments#create {:format=>"json"}
                                                                         new_api_v2_project_key_comment GET        /api/v2/projects/:project_id/keys/:key_id/comments/new(.:format)                                                                                                                                   api/v2/comments#new {:format=>"json"}
                                                                        edit_api_v2_project_key_comment GET        /api/v2/projects/:project_id/keys/:key_id/comments/:id/edit(.:format)                                                                                                                              api/v2/comments#edit {:format=>"json"}
                                                                             api_v2_project_key_comment GET        /api/v2/projects/:project_id/keys/:key_id/comments/:id(.:format)                                                                                                                                   api/v2/comments#show {:format=>"json"}
                                                                                                        PATCH      /api/v2/projects/:project_id/keys/:key_id/comments/:id(.:format)                                                                                                                                   api/v2/comments#update {:format=>"json"}
                                                                                                        PUT        /api/v2/projects/:project_id/keys/:key_id/comments/:id(.:format)                                                                                                                                   api/v2/comments#update {:format=>"json"}
                                                                                                        DELETE     /api/v2/projects/:project_id/keys/:key_id/comments/:id(.:format)                                                                                                                                   api/v2/comments#destroy {:format=>"json"}
                                                                 search_api_v2_project_key_translations POST       /api/v2/projects/:project_id/keys/:key_id/translations/search(.:format)                                                                                                                            api/v2/translations#index {:format=>"json"}
                                                                 verify_api_v2_project_key_translations PATCH      /api/v2/projects/:project_id/keys/:key_id/translations/verify(.:format)                                                                                                                            api/v2/translations#verify {:format=>"json"}
                                                               unverify_api_v2_project_key_translations PATCH      /api/v2/projects/:project_id/keys/:key_id/translations/unverify(.:format)                                                                                                                          api/v2/translations#unverify {:format=>"json"}
                                                                exclude_api_v2_project_key_translations PATCH      /api/v2/projects/:project_id/keys/:key_id/translations/exclude(.:format)                                                                                                                           api/v2/translations#exclude {:format=>"json"}
                                                                include_api_v2_project_key_translations PATCH      /api/v2/projects/:project_id/keys/:key_id/translations/include(.:format)                                                                                                                           api/v2/translations#include {:format=>"json"}
                                                       machine_translate_api_v2_project_key_translation PATCH      /api/v2/projects/:project_id/keys/:key_id/translations/:id/machine_translate(.:format)                                                                                                             api/v2/translations#machine_translate {:format=>"json"}
                                                                api_v2_project_key_translation_versions GET        /api/v2/projects/:project_id/keys/:key_id/translations/:translation_id/versions(.:format)                                                                                                          api/v2/translation_versions#index {:format=>"json"}
                                                                 api_v2_project_key_translation_version GET        /api/v2/projects/:project_id/keys/:key_id/translations/:translation_id/versions/:id(.:format)                                                                                                      api/v2/translation_versions#show {:format=>"json"}
                                                                        api_v2_project_key_translations GET        /api/v2/projects/:project_id/keys/:key_id/translations(.:format)                                                                                                                                   api/v2/translations#index {:format=>"json"}
                                                                                                        POST       /api/v2/projects/:project_id/keys/:key_id/translations(.:format)                                                                                                                                   api/v2/translations#create {:format=>"json"}
                                                                     new_api_v2_project_key_translation GET        /api/v2/projects/:project_id/keys/:key_id/translations/new(.:format)                                                                                                                               api/v2/translations#new {:format=>"json"}
                                                                    edit_api_v2_project_key_translation GET        /api/v2/projects/:project_id/keys/:key_id/translations/:id/edit(.:format)                                                                                                                          api/v2/translations#edit {:format=>"json"}
                                                                         api_v2_project_key_translation GET        /api/v2/projects/:project_id/keys/:key_id/translations/:id(.:format)                                                                                                                               api/v2/translations#show {:format=>"json"}
                                                                                                        PATCH      /api/v2/projects/:project_id/keys/:key_id/translations/:id(.:format)                                                                                                                               api/v2/translations#update {:format=>"json"}
                                                                                                        PUT        /api/v2/projects/:project_id/keys/:key_id/translations/:id(.:format)                                                                                                                               api/v2/translations#update {:format=>"json"}
                                                                                                        DELETE     /api/v2/projects/:project_id/keys/:key_id/translations/:id(.:format)                                                                                                                               api/v2/translations#destroy {:format=>"json"}
                                                                             search_api_v2_project_keys POST       /api/v2/projects/:project_id/keys/search(.:format)                                                                                                                                                 api/v2/translation_keys#index {:format=>"json"}
                                                                                tag_api_v2_project_keys PATCH      /api/v2/projects/:project_id/keys/tag(.:format)                                                                                                                                                    api/v2/translation_keys#tag {:format=>"json"}
                                                                              untag_api_v2_project_keys PATCH      /api/v2/projects/:project_id/keys/untag(.:format)                                                                                                                                                  api/v2/translation_keys#untag {:format=>"json"}
                                                                                    api_v2_project_keys DELETE     /api/v2/projects/:project_id/keys(.:format)                                                                                                                                                        api/v2/translation_keys#destroy_collection {:format=>"json"}
                                                                                                        GET        /api/v2/projects/:project_id/keys(.:format)                                                                                                                                                        api/v2/translation_keys#index {:format=>"json"}
                                                                                                        POST       /api/v2/projects/:project_id/keys(.:format)                                                                                                                                                        api/v2/translation_keys#create {:format=>"json"}
                                                                                 new_api_v2_project_key GET        /api/v2/projects/:project_id/keys/new(.:format)                                                                                                                                                    api/v2/translation_keys#new {:format=>"json"}
                                                                                edit_api_v2_project_key GET        /api/v2/projects/:project_id/keys/:id/edit(.:format)                                                                                                                                               api/v2/translation_keys#edit {:format=>"json"}
                                                                                     api_v2_project_key GET        /api/v2/projects/:project_id/keys/:id(.:format)                                                                                                                                                    api/v2/translation_keys#show {:format=>"json"}
                                                                                                        PATCH      /api/v2/projects/:project_id/keys/:id(.:format)                                                                                                                                                    api/v2/translation_keys#update {:format=>"json"}
                                                                                                        PUT        /api/v2/projects/:project_id/keys/:id(.:format)                                                                                                                                                    api/v2/translation_keys#update {:format=>"json"}
                                                                                                        DELETE     /api/v2/projects/:project_id/keys/:id(.:format)                                                                                                                                                    api/v2/translation_keys#destroy {:format=>"json"}
                                                                        api_v2_project_blacklisted_keys GET        /api/v2/projects/:project_id/blacklisted_keys(.:format)                                                                                                                                            api/v2/blacklisted_keys#index {:format=>"json"}
                                                                                                        POST       /api/v2/projects/:project_id/blacklisted_keys(.:format)                                                                                                                                            api/v2/blacklisted_keys#create {:format=>"json"}
                                                                     new_api_v2_project_blacklisted_key GET        /api/v2/projects/:project_id/blacklisted_keys/new(.:format)                                                                                                                                        api/v2/blacklisted_keys#new {:format=>"json"}
                                                                    edit_api_v2_project_blacklisted_key GET        /api/v2/projects/:project_id/blacklisted_keys/:id/edit(.:format)                                                                                                                                   api/v2/blacklisted_keys#edit {:format=>"json"}
                                                                         api_v2_project_blacklisted_key GET        /api/v2/projects/:project_id/blacklisted_keys/:id(.:format)                                                                                                                                        api/v2/blacklisted_keys#show {:format=>"json"}
                                                                                                        PATCH      /api/v2/projects/:project_id/blacklisted_keys/:id(.:format)                                                                                                                                        api/v2/blacklisted_keys#update {:format=>"json"}
                                                                                                        PUT        /api/v2/projects/:project_id/blacklisted_keys/:id(.:format)                                                                                                                                        api/v2/blacklisted_keys#update {:format=>"json"}
                                                                                                        DELETE     /api/v2/projects/:project_id/blacklisted_keys/:id(.:format)                                                                                                                                        api/v2/blacklisted_keys#destroy {:format=>"json"}
                                                                           confirm_api_v2_project_order PATCH      /api/v2/projects/:project_id/orders/:id/confirm(.:format)                                                                                                                                          api/v2/translation_orders#confirm {:format=>"json"}
                                                                                  api_v2_project_orders GET        /api/v2/projects/:project_id/orders(.:format)                                                                                                                                                      api/v2/translation_orders#index {:format=>"json"}
                                                                                                        POST       /api/v2/projects/:project_id/orders(.:format)                                                                                                                                                      api/v2/translation_orders#create {:format=>"json"}
                                                                                   api_v2_project_order GET        /api/v2/projects/:project_id/orders/:id(.:format)                                                                                                                                                  api/v2/translation_orders#show {:format=>"json"}
                                                                                                        DELETE     /api/v2/projects/:project_id/orders/:id(.:format)                                                                                                                                                  api/v2/translation_orders#destroy {:format=>"json"}
                                                                                 api_v2_project_uploads GET        /api/v2/projects/:project_id/uploads(.:format)                                                                                                                                                     api/v2/uploads#index {:format=>"json"}
                                                                                                        POST       /api/v2/projects/:project_id/uploads(.:format)                                                                                                                                                     api/v2/uploads#create {:format=>"json"}
                                                                                  api_v2_project_upload GET        /api/v2/projects/:project_id/uploads/:id(.:format)                                                                                                                                                 api/v2/uploads#show {:format=>"json"}
                                                                                 api_v2_project_members GET        /api/v2/projects/:project_id/members(.:format)                                                                                                                                                     api/v2/members#index {:format=>"json"}
                                                                          compare_api_v2_project_branch GET        /api/v2/projects/:project_id/branches/:id/compare(.:format)                                                                                                                                        api/v2/branches#compare {:format=>"json"}
                                                                            merge_api_v2_project_branch PATCH      /api/v2/projects/:project_id/branches/:id/merge(.:format)                                                                                                                                          api/v2/branches#merge {:format=>"json"}
                                                                                api_v2_project_branches GET        /api/v2/projects/:project_id/branches(.:format)                                                                                                                                                    api/v2/branches#index {:format=>"json"}
                                                                                                        POST       /api/v2/projects/:project_id/branches(.:format)                                                                                                                                                    api/v2/branches#create {:format=>"json"}
                                                                                  api_v2_project_branch GET        /api/v2/projects/:project_id/branches/:id(.:format)                                                                                                                                                api/v2/branches#show {:format=>"json"}
                                                                                                        PATCH      /api/v2/projects/:project_id/branches/:id(.:format)                                                                                                                                                api/v2/branches#update {:format=>"json"}
                                                                                                        PUT        /api/v2/projects/:project_id/branches/:id(.:format)                                                                                                                                                api/v2/branches#update {:format=>"json"}
                                                                                                        DELETE     /api/v2/projects/:project_id/branches/:id(.:format)                                                                                                                                                api/v2/branches#destroy {:format=>"json"}
                                                                                        api_v2_projects GET        /api/v2/projects(.:format)                                                                                                                                                                         api/v2/projects#index {:format=>"json"}
                                                                                                        POST       /api/v2/projects(.:format)                                                                                                                                                                         api/v2/projects#create {:format=>"json"}
                                                                                     new_api_v2_project GET        /api/v2/projects/new(.:format)                                                                                                                                                                     api/v2/projects#new {:format=>"json"}
                                                                                    edit_api_v2_project GET        /api/v2/projects/:id/edit(.:format)                                                                                                                                                                api/v2/projects#edit {:format=>"json"}
                                                                                         api_v2_project GET        /api/v2/projects/:id(.:format)                                                                                                                                                                     api/v2/projects#show {:format=>"json"}
                                                                                                        PATCH      /api/v2/projects/:id(.:format)                                                                                                                                                                     api/v2/projects#update {:format=>"json"}
                                                                                                        PUT        /api/v2/projects/:id(.:format)                                                                                                                                                                     api/v2/projects#update {:format=>"json"}
                                                                                                        DELETE     /api/v2/projects/:id(.:format)                                                                                                                                                                     api/v2/projects#destroy {:format=>"json"}
                                                                            internal_api_project_upload GET        /internal/api/projects/:project_id/uploads/:id(.:format)                                                                                                                                           api/v2/uploads#show {:format=>"json", :to=>"api/v2/uploads#show"}
                                                                           internal_api_project_members GET        /internal/api/projects/:project_id/members(.:format)                                                                                                                                               api/v2/members#index {:format=>"json"}
                                                                       internal_api_project_job_members GET        /internal/api/projects/:project_id/jobs/:job_id/members(.:format)                                                                                                                                  api/v2/members#index {:format=>"json"}
                                                                              internal_api_project_jobs GET        /internal/api/projects/:project_id/jobs(.:format)                                                                                                                                                  api/v2/jobs#index {:format=>"json"}
                                                                                                        POST       /internal/api/projects/:project_id/jobs(.:format)                                                                                                                                                  api/v2/jobs#create {:format=>"json"}
                                                                           new_internal_api_project_job GET        /internal/api/projects/:project_id/jobs/new(.:format)                                                                                                                                              api/v2/jobs#new {:format=>"json"}
                                                                          edit_internal_api_project_job GET        /internal/api/projects/:project_id/jobs/:id/edit(.:format)                                                                                                                                         api/v2/jobs#edit {:format=>"json"}
                                                                               internal_api_project_job GET        /internal/api/projects/:project_id/jobs/:id(.:format)                                                                                                                                              api/v2/jobs#show {:format=>"json"}
                                                                                                        PATCH      /internal/api/projects/:project_id/jobs/:id(.:format)                                                                                                                                              api/v2/jobs#update {:format=>"json"}
                                                                                                        PUT        /internal/api/projects/:project_id/jobs/:id(.:format)                                                                                                                                              api/v2/jobs#update {:format=>"json"}
                                                                                                        DELETE     /internal/api/projects/:project_id/jobs/:id(.:format)                                                                                                                                              api/v2/jobs#destroy {:format=>"json"}
                                                                              internal_api_project_tags GET        /internal/api/projects/:project_id/tags(.:format)                                                                                                                                                  api/v2/tags#index {:format=>"json"}
                                                                           internal_api_project_locales GET        /internal/api/projects/:project_id/locales(.:format)                                                                                                                                               api/v2/locales#index {:format=>"json"}
                                                                 search_count_internal_api_project_keys GET        /internal/api/projects/:project_id/keys/search_count(.:format)                                                                                                                                     api/v2/translation_keys#search_count {:format=>"json"}
                                                                       search_internal_api_project_keys POST       /internal/api/projects/:project_id/keys/search(.:format)                                                                                                                                           api/v2/translation_keys#index {:format=>"json"}
                                                                              internal_api_project_keys GET        /internal/api/projects/:project_id/keys(.:format)                                                                                                                                                  api/v2/translation_keys#index {:format=>"json"}
                                                                      merge_internal_api_project_branch PATCH      /internal/api/projects/:project_id/branches/:id/merge(.:format)                                                                                                                                    api/v2/branches#merge {:format=>"json"}
                                                                    compare_internal_api_project_branch GET        /internal/api/projects/:project_id/branches/:id/compare(.:format)                                                                                                                                  api/v2/branches#compare {:format=>"json"}
                                                                          internal_api_project_branches GET        /internal/api/projects/:project_id/branches(.:format)                                                                                                                                              api/v2/branches#index {:format=>"json"}
                                                                                                        POST       /internal/api/projects/:project_id/branches(.:format)                                                                                                                                              api/v2/branches#create {:format=>"json"}
                                                                            internal_api_project_branch GET        /internal/api/projects/:project_id/branches/:id(.:format)                                                                                                                                          api/v2/branches#show {:format=>"json"}
                                                                                                        DELETE     /internal/api/projects/:project_id/branches/:id(.:format)                                                                                                                                          api/v2/branches#destroy {:format=>"json"}
                                                                                  internal_api_projects GET        /internal/api/projects(.:format)                                                                                                                                                                   projects#index {:format=>"json"}
                                                                                                        POST       /internal/api/projects(.:format)                                                                                                                                                                   projects#create {:format=>"json"}
                                                                               new_internal_api_project GET        /internal/api/projects/new(.:format)                                                                                                                                                               projects#new {:format=>"json"}
                                                                              edit_internal_api_project GET        /internal/api/projects/:id/edit(.:format)                                                                                                                                                          projects#edit {:format=>"json"}
                                                                                   internal_api_project GET        /internal/api/projects/:id(.:format)                                                                                                                                                               projects#show {:format=>"json"}
                                                                                                        PATCH      /internal/api/projects/:id(.:format)                                                                                                                                                               projects#update {:format=>"json"}
                                                                                                        PUT        /internal/api/projects/:id(.:format)                                                                                                                                                               projects#update {:format=>"json"}
                                                                                                        DELETE     /internal/api/projects/:id(.:format)                                                                                                                                                               projects#destroy {:format=>"json"}
                                                                  username_validation_internal_api_user POST       /internal/api/user/username_validation(.:format)                                                                                                                                                   api/v2/users#username_validation {:format=>"json"}
                                                                                  new_internal_api_user GET        /internal/api/user/new(.:format)                                                                                                                                                                   users#new {:format=>"json"}
                                                                                 edit_internal_api_user GET        /internal/api/user/edit(.:format)                                                                                                                                                                  users#edit {:format=>"json"}
                                                                                      internal_api_user GET        /internal/api/user(.:format)                                                                                                                                                                       users#show {:format=>"json"}
                                                                                                        PATCH      /internal/api/user(.:format)                                                                                                                                                                       users#update {:format=>"json"}
                                                                                                        PUT        /internal/api/user(.:format)                                                                                                                                                                       users#update {:format=>"json"}
                                                                                                        DELETE     /internal/api/user(.:format)                                                                                                                                                                       users#destroy {:format=>"json"}
                                                                                                        POST       /internal/api/user(.:format)                                                                                                                                                                       users#create {:format=>"json"}
"""
