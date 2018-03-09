port module Ports
    exposing
        ( read
        , write
        )


port read : (String -> msg) -> Sub msg


port write : String -> Cmd msg
