port module Ports
    exposing
        ( read
        , write
        )

import Json.Decode exposing (Value)


port read : (String -> msg) -> Sub msg


port write : Value -> Cmd msg
