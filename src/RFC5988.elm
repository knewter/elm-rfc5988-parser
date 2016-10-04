module RFC5988 exposing (rfc5988, rfc5988s, Link, emptyLink)

import Dict exposing (Dict)
import Combine exposing (parse, Parser, string, map, succeed, between, regex, sepBy, andThen)
import Combine.Infix exposing ((<*), (*>))


emptyLink : Link
emptyLink =
    { context = ""
    , target = ""
    , relationType = ""
    , targetAttributes = Dict.empty
    }


type alias IRI =
    String


{-| Defined here: https://mnot.github.io/I-D/rfc5988bis/
-}
type alias Link =
    { context : IRI
    , target : IRI
    , relationType : String
    , targetAttributes : Dict String String
    }


rfc5988s : Parser (List Link)
rfc5988s =
    sepBy (whitespace *> string "," <* whitespace) rfc5988


rfc5988 : Parser Link
rfc5988 =
    let
        mergeParameter : ( String, String ) -> Link -> Link
        mergeParameter ( key, value ) link =
            case key of
                "rel" ->
                    { link | relationType = value }

                _ ->
                    { link | targetAttributes = mergeTargetAttribute ( key, value ) link.targetAttributes }

        mergeParameters : List ( String, String ) -> Link -> Link
        mergeParameters params link =
            params
                |> List.foldl mergeParameter link

        mergeTargetAttribute : ( String, String ) -> Dict String String -> Dict String String
        mergeTargetAttribute ( key, value ) acc =
            Dict.insert key value acc

        updateTargetAttributes : Link -> Parser Link
        updateTargetAttributes link =
            sepBy (string ";") linkParam
                |> map (\keyVals -> mergeParameters keyVals link)
    in
        (carets (regex "[^>]*")
            |> map (\uri -> { emptyLink | target = uri })
        )
            <* string ";"
            `andThen` updateTargetAttributes


linkParam : Parser ( String, String )
linkParam =
    whitespace
        *> regex "[a-zA-Z]*"
        <* string "="
        `andThen`
            (\key ->
                string "\""
                    *> regex "[a-zA-Z]*"
                    <* string "\""
                    |> map (\value -> ( key, value ))
            )


whitespace : Parser String
whitespace =
    regex "[ \t\x0D\n]*"


carets : Parser res -> Parser res
carets =
    between (string "<") (string ">")
