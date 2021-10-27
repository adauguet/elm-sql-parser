module SQL.Expr exposing (..)

import Char exposing (isAlpha, isHexDigit)
import Hex
import Parser exposing ((|.), (|=), Parser, backtrackable, chompWhile, float, getChompedString, int, keyword, map, oneOf, spaces, succeed, symbol, token)
import Pratt exposing (Config, infixLeft, literal, prefix)
import Pratt.Extra exposing (postfix2)
import Set


type Expr
    = Float Float
    | Int Int
    | Hex Int
    | String String
    | Blob String
    | Null
    | Variable String
    | LowerThan Expr Expr
    | GreaterThan Expr Expr
    | LowerOrEqualThan Expr Expr
    | GreaterOrEqualThan Expr Expr
    | Len Expr
    | Function String Expr
    | Parenthesis Expr
    | Pos Expr
    | Neg Expr
    | Tilde Expr
    | Concat Expr Expr
    | Collate String Expr


parser : Parser Expr
parser =
    Pratt.expression
        { oneOf =
            [ literal number
            , literal string
            , literal blob
            , literal null
            , fLen
            , backtrackable << function
            , literal variable
            , parenthesis
            , prefix 0 (symbol "+") Pos
            , prefix 0 (symbol "-") Neg
            , prefix 0 (symbol "~") Tilde
            ]
        , andThenOneOf =
            [ infixLeft 1 (symbol "<") LowerThan
            , infixLeft 1 (symbol ">") GreaterThan
            , infixLeft 1 (symbol "<=") LowerOrEqualThan
            , infixLeft 1 (symbol ">=") GreaterOrEqualThan
            , infixLeft 3 (symbol "||") Concat
            , postfix2 2
                (succeed identity
                    |. keyword "COLLATE"
                    |. spaces
                    |= getChompedString (token "name")
                )
                Collate
            ]
        , spaces = spaces
        }


number : Parser Expr
number =
    Parser.number
        { int = Just Int
        , hex = Just Hex
        , octal = Nothing
        , binary = Nothing
        , float = Just Float
        }


string : Parser Expr
string =
    succeed String
        |. symbol "'"
        |= (getChompedString <| chompWhile (\c -> c /= '\''))
        |. symbol "'"


blob : Parser Expr
blob =
    succeed Blob
        |. oneOf [ symbol "X", symbol "x" ]
        |. symbol "'"
        |= getChompedString (chompWhile isHexDigit)
        |. symbol "'"


null : Parser Expr
null =
    succeed Null |. keyword "NULL"


variable : Parser Expr
variable =
    map Variable <|
        Parser.variable
            { start = isAlpha
            , inner = isAlpha
            , reserved = Set.empty
            }


fLen : Config Expr -> Parser Expr
fLen config =
    succeed Len
        |. keyword "LEN"
        |. symbol "("
        |= Pratt.subExpression 0 config
        |. symbol ")"


function : Config Expr -> Parser Expr
function config =
    succeed Function
        |= Parser.variable { start = isAlpha, inner = isAlpha, reserved = Set.empty }
        |. symbol "("
        |= Pratt.subExpression 0 config
        |. symbol ")"


parenthesis : Config Expr -> Parser Expr
parenthesis config =
    succeed Parenthesis
        |. symbol "("
        |= Pratt.subExpression 0 config
        |. symbol ")"


format : Expr -> String
format expr =
    case expr of
        Float float ->
            String.fromFloat float

        Int int ->
            String.fromInt int

        Hex hex ->
            let
                roundEvenUp : Int -> Int
                roundEvenUp int =
                    int + modBy 2 int

                hexString =
                    Hex.toString hex
            in
            "0x" ++ String.padLeft (roundEvenUp (String.length hexString)) '0' (Hex.toString hex)

        String s ->
            "'" ++ s ++ "'"

        Blob s ->
            "X'" ++ s ++ "'"

        Null ->
            "NULL"

        Variable name ->
            name

        LowerThan a b ->
            format a ++ " < " ++ format b

        GreaterThan a b ->
            format a ++ " > " ++ format b

        LowerOrEqualThan a b ->
            format a ++ " <= " ++ format b

        GreaterOrEqualThan a b ->
            format a ++ " >= " ++ format b

        Len e ->
            "LEN(" ++ format e ++ ")"

        Function name e ->
            name ++ "(" ++ format e ++ ")"

        Parenthesis e ->
            "(" ++ format e ++ ")"

        Pos e ->
            "+" ++ format e

        Neg e ->
            "-" ++ format e

        Tilde e ->
            "~" ++ format e

        Concat a b ->
            format a ++ " || " ++ format b

        Collate n e ->
            format e ++ " COLLATE " ++ n
