module SQL.Parser exposing
    ( Column
    , CreateTable
    , Type(..)
    , column
    , columnConstraint
    , createTable
    , default
    , schema
    )

import Char exposing (isAlpha)
import Parser exposing ((|.), (|=), Nestable(..), Parser, Trailing(..), backtrackable, chompWhile, getChompedString, int, keyword, oneOf, sequence, spaces, succeed, symbol, variable)
import Set exposing (Set)


type alias CreateTable =
    { ifNotExist : Bool
    , schema : Maybe String
    , name : String
    , columns : List Column
    }


type alias Column =
    { name : String
    , type_ : Type
    , identity : Maybe ( Int, Int )
    , isNotNull : Bool
    , constraint : Maybe String
    , isPrimaryKey : Bool
    , default : Maybe String
    , columnReferences : Maybe String
    }


type Type
    = Int
    | Text
    | Varchar (Maybe Int)
    | CharacterVarying Int
    | Other String


createTable : Parser CreateTable
createTable =
    succeed CreateTable
        |. spaces
        |. keyword "CREATE TABLE"
        |. spaces
        |= tableIfNotExists
        |. spaces
        |= schema
        |= tableName
        |. spaces
        |= columns
        |. symbol ";"


columns : Parser (List Column)
columns =
    sequence
        { start = "("
        , separator = ","
        , end = ")"
        , spaces = spaces
        , item = column
        , trailing = Forbidden
        }


tableIfNotExists : Parser Bool
tableIfNotExists =
    bool <| keyword "IF NOT EXISTS"


bool : Parser () -> Parser Bool
bool p =
    oneOf
        [ succeed True |. p
        , succeed False
        ]


tableName : Parser String
tableName =
    let
        name =
            variable
                { start = isAlpha
                , inner = isAlpha
                , reserved = Set.empty
                }
    in
    oneOf
        [ quoted "[" "]" name
        , name
        ]


schema : Parser (Maybe String)
schema =
    oneOf
        [ backtrackable <|
            succeed Just
                |= variable
                    { start = Char.isAlpha
                    , inner = Char.isAlpha
                    , reserved = postgreSQLReservedKeywords
                    }
                |. symbol "."
        , succeed Nothing
        ]


column : Parser Column
column =
    succeed Column
        |= columnName
        |. spaces
        |= columnType
        |. spaces
        |= columnIdentity
        |. spaces
        |= columnIsNotNull
        |. spaces
        |= columnConstraint
        |. spaces
        |= columnIsPrimaryKey
        |. spaces
        |= default
        |. spaces
        |= columnReferences
        |. spaces


columnName : Parser String
columnName =
    let
        isColumnNameChar c =
            isAlpha c || c == '_'

        name =
            variable
                { start = isAlpha
                , inner = isColumnNameChar
                , reserved = Set.fromList []
                }
    in
    oneOf
        [ quoted "'" "'" name
        , quoted "`" "`" name
        , quoted "[" "]" name
        , quoted "\"" "\"" name
        , name
        ]


quoted : String -> String -> Parser String -> Parser String
quoted start end stringParser =
    succeed identity
        |. symbol start
        |= stringParser
        |. symbol end


columnType : Parser Type
columnType =
    oneOf
        [ succeed Int |. keyword "INT"
        , succeed Text |. keyword "TEXT"
        , succeed Varchar
            |. keyword "VARCHAR"
            |= oneOf
                [ succeed Just
                    |. symbol "("
                    |= int
                    |. symbol ")"
                , succeed Nothing
                ]
        , succeed CharacterVarying
            |. keyword "CHARACTER VARYING"
            |. symbol "("
            |= int
            |. symbol ")"
        , succeed Other
            |= variable
                { start = isAlpha
                , inner = isAlpha
                , reserved = Set.empty
                }
        ]


columnIsNotNull : Parser Bool
columnIsNotNull =
    bool <| keyword "NOT NULL"


columnIsPrimaryKey : Parser Bool
columnIsPrimaryKey =
    bool <| keyword "PRIMARY KEY"



-- type ConflictClause
--     = Rollback
--     | Abort
--     | Fail
--     | Ignore
--     | Replace
-- conflictClause : Parser ConflictClause
-- conflictClause =
--     succeed identity
--         |. keyword "ON CONFLICT"
--         |. spaces
--         |= oneOf
--             [ succeed Rollback |. keyword "ROLLBACK"
--             , succeed Abort |. keyword "ABORT"
--             , succeed Fail |. keyword "FAIL"
--             , succeed Ignore |. keyword "IGNORE"
--             , succeed Replace |. keyword "REPLACE"
--             ]


columnConstraint : Parser (Maybe String)
columnConstraint =
    let
        isConstraintChar c =
            isAlpha c || c == '_'
    in
    oneOf
        [ succeed Just
            |. keyword "CONSTRAINT"
            |. spaces
            |= variable { start = isAlpha, inner = isConstraintChar, reserved = Set.empty }
        , succeed Nothing
        ]


default : Parser (Maybe String)
default =
    oneOf
        [ succeed Just
            |. keyword "DEFAULT"
            |. spaces
            |. symbol "'"
            |= getChompedString (chompWhile (\c -> c /= '\''))
            |. symbol "'"
        , succeed Nothing
        ]


columnIdentity : Parser (Maybe ( Int, Int ))
columnIdentity =
    oneOf
        [ succeed (\start increment -> Just ( start, increment ))
            |. keyword "identity"
            |. symbol "("
            |. spaces
            |= int
            |. spaces
            |. symbol ","
            |. spaces
            |= int
            |. spaces
            |. symbol ")"
        , succeed Nothing
        ]


columnReferences : Parser (Maybe String)
columnReferences =
    let
        isReferencesChar c =
            isAlpha c || c == '.'
    in
    oneOf
        [ succeed Just
            |. keyword "REFERENCES"
            |. spaces
            |= variable
                { start = isAlpha
                , inner = isReferencesChar
                , reserved = Set.empty
                }
        , succeed Nothing
        ]


postgreSQLReservedKeywords : Set String
postgreSQLReservedKeywords =
    Set.fromList
        [ "ALL"
        , "ANALYSE"
        , "ANALYZE"
        , "AND"
        , "ANY"
        , "ARRAY"
        , "AS"
        , "ASC"
        , "ASYMMETRIC"
        , "AUTHORIZATION"
        , "BETWEEN"
        , "BINARY"
        , "BOTH"
        , "CASE"
        , "CAST"
        , "CHECK"
        , "COLLATE"
        , "COLUMN"
        , "CONSTRAINT"
        , "CREATE"
        , "CROSS"
        , "CURRENT_DATE"
        , "CURRENT_ROLE"
        , "CURRENT_TIME"
        , "CURRENT_TIMESTAMP"
        , "CURRENT_USER"
        , "DEFAULT"
        , "DEFERRABLE"
        , "DESC"
        , "DISTINCT"
        , "DO"
        , "ELSE"
        , "END"
        , "EXCEPT"
        , "FOR"
        , "FOREIGN"
        , "FREEZE"
        , "FROM"
        , "FULL"
        , "GRANT"
        , "GROUP"
        , "HAVING"
        , "ILIKE"
        , "IN"
        , "INITIALLY"
        , "INNER"
        , "INTERSECT"
        , "INTO"
        , "IS"
        , "ISNULL"
        , "JOIN"
        , "LEADING"
        , "LEFT"
        , "LIKE"
        , "LIMIT"
        , "LOCALTIME"
        , "LOCALTIMESTAMP"
        , "NATURAL"
        , "NEW"
        , "NOT"
        , "NOTNULL"
        , "NULL"
        , "OFF"
        , "OFFSET"
        , "OLD"
        , "ON"
        , "ONLY"
        , "OR"
        , "ORDER"
        , "OUTER"
        , "OVERLAPS"
        , "PLACING"
        , "PRIMARY"
        , "REFERENCES"
        , "RIGHT"
        , "SELECT"
        , "SESSION_USER"
        , "SIMILAR"
        , "SOME"
        , "SYMMETRIC"
        , "TABLE"
        , "THEN"
        , "TO"
        , "TRAILING"
        , "UNION"
        , "UNIQUE"
        , "USER"
        , "USING"
        , "VERBOSE"
        , "WHEN"
        , "WHERE"
        , "FALSE"
        , "TRUE"
        ]
