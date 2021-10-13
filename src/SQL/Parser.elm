module SQL.Parser exposing (..)

import Char exposing (isAlpha)
import Parser exposing ((|.), (|=), Parser, Trailing(..), backtrackable, int, keyword, map, oneOf, sequence, spaces, succeed, symbol, variable)
import Set exposing (Set)


type alias CreateTable =
    { schema : Maybe String
    , name : String
    , columns : List Column
    }


type alias Column =
    { name : String
    , type_ : Type
    , isNotNull : Bool
    , isPrimaryKey : Bool
    }


type Type
    = Int
    | Text
    | CharacterVarying Int
    | Other String


createTable : Parser CreateTable
createTable =
    succeed CreateTable
        |. spaces
        |. keyword "CREATE TABLE"
        |. spaces
        |. tableIfNotExists
        |. spaces
        |= schema
        |= tableName
        |. spaces
        |= sequence
            { start = "("
            , separator = ","
            , end = ")"
            , spaces = spaces
            , item = column
            , trailing = Forbidden
            }
        |. symbol ";"


tableIfNotExists : Parser Bool
tableIfNotExists =
    oneOf
        [ succeed True |. keyword "IF NOT EXISTS"
        , succeed False
        ]


tableName : Parser String
tableName =
    variable
        { start = isAlpha
        , inner = isAlpha
        , reserved = Set.empty
        }


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
        |= columnIsNotNull
        |. spaces
        |= columnIsPrimaryKey


columnName : Parser String
columnName =
    let
        name =
            variable
                { start = isAlpha
                , inner = isAlpha
                , reserved = Set.fromList []
                }
    in
    oneOf
        [ quoted "'" "'" name
        , quoted "`" "`" name
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
        [ map (\_ -> Int) <| keyword "INT"
        , map (\_ -> Text) <| keyword "TEXT"
        , map (\count -> CharacterVarying count) <|
            (succeed identity
                |. keyword "CHARACTER VARYING"
                |. symbol "("
                |= int
                |. symbol ")"
            )
        , map (\typeName -> Other typeName) <|
            variable
                { start = isAlpha
                , inner = isAlpha
                , reserved = Set.empty
                }
        ]


columnIsNotNull : Parser Bool
columnIsNotNull =
    oneOf
        [ succeed True |. keyword "NOT NULL"
        , succeed False
        ]


columnIsPrimaryKey : Parser Bool
columnIsPrimaryKey =
    oneOf
        [ succeed True |. keyword "PRIMARY KEY"
        , succeed False
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
