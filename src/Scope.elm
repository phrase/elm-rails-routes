module Scope
    exposing
        ( Path
        , PathElement
            ( Anything
            , Verbatim
            )
        , Scope
        , contains
        , decoder
        )

import Json.Decode as Decode exposing (Decoder)


type Scope
    = Everything
    | Scope (List ( String, Scope ))


type alias Path =
    List PathElement


type PathElement
    = Verbatim String
    | Anything


{-| Is the path contained in the scope?
-}
contains : Path -> Scope -> Bool
contains path scope =
    case scope of
        Everything ->
            True

        Scope subScopes ->
            case path of
                [] ->
                    False

                pathElement :: rest ->
                    case pathElement of
                        Anything ->
                            subScopes
                                |> List.any (Tuple.second >> contains rest)

                        Verbatim pathElementName ->
                            subScopes
                                |> List.any
                                    (\( subScopeName, subScope ) ->
                                        (pathElementName == subScopeName)
                                            && contains rest subScope
                                    )



---- DECODER


decoder : Decoder Scope
decoder =
    Decode.oneOf
        [ Decode.keyValuePairs
            (Decode.lazy (\_ -> decoder))
            |> Decode.map Scope
        , Decode.string
            |> Decode.andThen
                (\name ->
                    case name of
                        "*" ->
                            Decode.succeed Everything

                        _ ->
                            Decode.fail "expecting \"*\""
                )
        ]
