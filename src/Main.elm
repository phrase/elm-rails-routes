module Main exposing (..)

import Json.Decode
import Platform
import Ports
import Route exposing (Route)


main : Program Never {} String
main =
    Platform.program
        { init = ( {}, Cmd.none )
        , update = update
        , subscriptions = \_ -> Ports.read identity
        }


update : String -> {} -> ( {}, Cmd String )
update routes model =
    ( model
    , [ [ "module PA.Routes exposing (..)"
        , "import Http"
        ]
            |> String.join "\n\n"
      , routes
            |> String.split "\n"
            |> List.filterMap Route.parse
            |> List.map Route.asElm
            |> String.join "\n\n\n"
      ]
        |> String.join "\n\n\n"
        |> Ports.write
    )
