module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Combine exposing (parse, Parser, string, map, succeed, between, regex, sepBy, andThen)
import Combine.Infix exposing ((<*), (*>))
import Dict exposing (Dict)


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


all : Test
all =
    describe "Parsing RFC5988 Link Headers"
        [ test "Parsing a basic link header value" <|
            \() ->
                Expect.equal (parse rfc5988 "<http://urbit.org>; rel=\"start\"") ( Ok basicLink, { input = "", position = 31 } )
        , test "Parsing a link with some additional target attributes" <|
            \() ->
                Expect.equal (parse rfc5988 "<http://urbit.org>; rel=\"start\"; borg=\"unimatrixzero\"") ( Ok { basicLink | targetAttributes = Dict.insert "borg" "unimatrixzero" basicLink.targetAttributes }, { input = "", position = 53 } )
        , test "Parsing a list of links" <|
            \() ->
                Expect.equal (parse rfc5988s listOfLinksString) ( Ok listOfLinks, { input = "", position = 152 } )
        ]


listOfLinksString : String
listOfLinksString =
    "<http://www.example.com/users?page=2>; rel=\"next\", <http://www.example.com/users?page=1>; rel=\"first\", <http://www.example.com/users?page=2>; rel=\"last\""


listOfLinks : List Link
listOfLinks =
    [ { emptyLink | target = "http://www.example.com/users?page=2", relationType = "next" }
    , { emptyLink | target = "http://www.example.com/users?page=1", relationType = "first" }
    , { emptyLink | target = "http://www.example.com/users?page=2", relationType = "last" }
    ]


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


emptyLink : Link
emptyLink =
    { context = ""
    , target = ""
    , relationType = ""
    , targetAttributes = Dict.empty
    }


basicLink : Link
basicLink =
    { context = ""
    , target = "http://urbit.org"
    , relationType = "start"
    , targetAttributes = Dict.empty
    }
