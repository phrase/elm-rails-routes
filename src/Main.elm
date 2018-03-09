module Main exposing (..)

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Platform
import Ports
import Route exposing (Route)
import Scope exposing (Scope)


type alias Config =
    { moduleName : String
    , modulePrefix : List String
    , include : Scope
    , exclude : Scope
    }


main : Program Value Config String
main =
    Platform.programWithFlags
        { init = init
        , update = update
        , subscriptions = \_ -> Ports.read identity
        }


init : Value -> ( Config, Cmd String )
init value =
    case Decode.decodeValue configDecoder value of
        Err errorMsg ->
            Debug.crash errorMsg

        Ok config ->
            ( config
            , Cmd.none
            )


update : String -> Config -> ( Config, Cmd String )
update routes config =
    ( config
    , [ ( "path"
        , config.modulePrefix
            |> List.map Encode.string
            |> Encode.list
        )
      , ( "filename"
        , (config.moduleName ++ ".elm")
            |> Encode.string
        )
      , ( "content"
        , [ [ "module PA.Routes exposing (..)"
            , "import Http"
            ]
                |> String.join "\n\n"
          , routes
                |> String.split "\n"
                |> List.filterMap Route.parse
                |> List.filterMap
                    (Route.filter
                        { include = config.include
                        , exclude = config.exclude
                        }
                    )
                |> List.map Route.asElm
                |> String.join "\n\n\n"
          ]
            |> String.join "\n\n\n"
            |> Encode.string
        )
      ]
        |> Encode.object
        |> Ports.write
    )



---- DECODER


configDecoder : Decoder Config
configDecoder =
    Decode.succeed Config
        |> Decode.required "module-name" Decode.string
        |> Decode.required "module-prefix"
            (Decode.string
                |> Decode.map
                    (\rawModulePrefix ->
                        case rawModulePrefix of
                            "" ->
                                []

                            _ ->
                                String.split "." rawModulePrefix
                    )
            )
        |> Decode.required "include" Scope.decoder
        |> Decode.required "exclude" Scope.decoder
