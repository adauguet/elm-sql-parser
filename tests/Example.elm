module Example exposing (..)

import Expect
import Parser exposing (run)
import SQL.Parser exposing (Column, CreateTable, Type(..), column, createTable, schema)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "main suite test"
        [ test "parsing a column" <|
            \_ -> Expect.equal (run column "name TEXT") (Ok <| Column "name" Text False False)
        , test "parsing a column with ticks" <|
            \_ -> Expect.equal (run column "'id' INT") (Ok <| Column "id" Int False False)
        , test "parsing a not null column" <|
            \_ -> Expect.equal (run column "count INT NOT NULL") (Ok <| Column "count" Int True False)
        , test "parsing a CREATE TABLE statement" <|
            \_ ->
                let
                    statement =
                        """CREATE TABLE users (id INT, name VARCHAR);"""
                in
                Expect.equal (run createTable statement)
                    (Ok <|
                        CreateTable
                            Nothing
                            "users"
                            [ Column "id" Int False False
                            , Column "name" (Other "VARCHAR") False False
                            ]
                    )
        , test "with schema" <| \_ -> Expect.equal (run schema "public.users") (Ok <| Just "public")
        , test "without schema" <| \_ -> Expect.equal (run schema "users") (Ok Nothing)
        , test "parsing a complex CREATE TABLE statement" <|
            \_ ->
                let
                    statement =
                        """
                        CREATE TABLE IF NOT EXISTS public.users(
                            `id` INT NOT NULL PRIMARY KEY,
                            `name` CHARACTER VARYING(255)
                            );
                        """
                in
                Expect.equal (run createTable statement)
                    (Ok <|
                        CreateTable (Just "public")
                            "users"
                            [ Column "id" Int True True
                            , Column "name" (CharacterVarying 255) False False
                            ]
                    )
        ]
