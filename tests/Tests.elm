module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Combine exposing (parse, Parser, string, map, succeed)
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
                Expect.equal (parse rfc5988 "<http://urbit.org>; rel=\"start\"") ( Ok basicLink, { input = "", position = 17 } )
        ]


rfc5988 : Parser Link
rfc5988 =
    succeed basicLink


basicLink : Link
basicLink =
    { context = ""
    , target = "http://urbit.org"
    , relationType = "start"
    , targetAttributes = Dict.empty
    }
