module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Combine exposing (parse)
import Dict exposing (Dict)
import RFC5988 exposing (rfc5988, rfc5988s, Link, emptyLink)


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


basicLink : Link
basicLink =
    { context = ""
    , target = "http://urbit.org"
    , relationType = "start"
    , targetAttributes = Dict.empty
    }
