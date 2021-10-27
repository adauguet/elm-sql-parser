module ExprTests exposing (..)

import Expect
import Parser exposing (run)
import SQL.Expr exposing (Expr(..), format, parser)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "expressions"
        [ describe "parsing"
            [ test "5 > 4" <|
                \_ -> Expect.equal (run parser "5 > 4") (Ok (GreaterThan (Int 5) (Int 4)))
            , test "LEN(name)" <|
                \_ -> Expect.equal (run parser "LEN(name)") (Ok (Len (Variable "name")))
            , test "LEN(name) > 4" <|
                \_ -> Expect.equal (run parser "LEN(name) > 4") (Ok (GreaterThan (Len (Variable "name")) (Int 4)))
            , test "count(name)" <|
                \_ -> Expect.equal (run parser "count(name)") (Ok (Function "count" (Variable "name")))
            , test "(LEN(name) > 4)" <|
                \_ -> Expect.equal (run parser "(LEN(name) > 4)") (Ok (Parenthesis (GreaterThan (Len (Variable "name")) (Int 4))))
            , test "(count(name) > 4)" <|
                \_ -> Expect.equal (run parser "(count(name) > 4)") (Ok (Parenthesis (GreaterThan (Function "count" (Variable "name")) (Int 4))))
            , test "0xa5" <|
                \_ -> Expect.equal (run parser "0xa5") (Ok (Hex 0xA5))
            , test ".2" <|
                \_ -> Expect.equal (run parser ".2") (Ok (Float 0.2))
            , test "1.2" <|
                \_ -> Expect.equal (run parser "1.2") (Ok (Float 1.2))
            , test "1.2e4" <|
                \_ -> Expect.equal (run parser "1.2e4") (Ok (Float 1.2e4))
            , test "1.2e-4" <|
                \_ -> Expect.equal (run parser "1.2e-4") (Ok (Float 1.2e-4))
            , test "1.2E-4" <|
                \_ -> Expect.equal (run parser "1.2E-4") (Ok (Float 1.2e-4))
            , test "'name'" <|
                \_ -> Expect.equal (run parser "'name'") (Ok (String "name"))
            , test "X'53514C697465'" <|
                \_ -> Expect.equal (run parser "X'53514C697465'") (Ok (Blob "53514C697465"))
            , test "NULL" <|
                \_ -> Expect.equal (run parser "NULL") (Ok Null)
            , test "+5" <|
                \_ -> Expect.equal (run parser "+5") (Ok (Pos (Int 5)))
            , test "-5.4" <|
                \_ -> Expect.equal (run parser "-5.4") (Ok (Neg (Float 5.4)))
            , test "~name" <|
                \_ -> Expect.equal (run parser "~name") (Ok (Tilde (Variable "name")))
            , test "4 COLLATE name" <|
                \_ -> Expect.equal (run parser "4 COLLATE name") (Ok (Collate "name" (Int 4)))
            ]
        , describe "formatting"
            [ test "5 > 4" <|
                \_ -> Expect.equal (format (GreaterThan (Int 5) (Int 4))) "5 > 4"
            , test "LEN(name)" <|
                \_ -> Expect.equal (format (Len (Variable "name"))) "LEN(name)"
            , test "LEN(name) > 4" <|
                \_ -> Expect.equal (format (GreaterThan (Len (Variable "name")) (Int 4))) "LEN(name) > 4"
            , test "(LEN(name) > 4)" <|
                \_ -> Expect.equal (format (Parenthesis (GreaterThan (Len (Variable "name")) (Int 4)))) "(LEN(name) > 4)"
            , test "0x01" <|
                \_ -> Expect.equal (format (Hex 0x01)) "0x01"
            , test "'name'" <|
                \_ -> Expect.equal (format (String "name")) "'name'"
            ]
        ]
