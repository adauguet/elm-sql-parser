module Example exposing (suite)

import Expect
import Parser exposing (run)
import SQL.Parser exposing (Column, CreateTable, Type(..), column, columnConstraint, createTable, default, schema)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "main suite test"
        [ test "parsing a column" <|
            \_ -> Expect.equal (run column "name TEXT") (Ok <| Column "name" Text Nothing False Nothing False Nothing Nothing)
        , test "parsing a column with ticks" <|
            \_ -> Expect.equal (run column "'id' INT") (Ok <| Column "id" Int Nothing False Nothing False Nothing Nothing)
        , test "parsing a not null column" <|
            \_ -> Expect.equal (run column "count INT NOT NULL") (Ok <| Column "count" Int Nothing True Nothing False Nothing Nothing)
        , test "parsing a CREATE TABLE statement" <|
            \_ ->
                let
                    statement =
                        "CREATE TABLE users (id INT, name VARCHAR);"
                in
                Expect.equal (run createTable statement)
                    (Ok <|
                        CreateTable
                            False
                            Nothing
                            "users"
                            [ Column "id" Int Nothing False Nothing False Nothing Nothing
                            , Column "name" (Varchar Nothing) Nothing False Nothing False Nothing Nothing
                            ]
                    )
        , test "with schema" <| \_ -> Expect.equal (run schema "public.users") (Ok <| Just "public")
        , test "without schema" <| \_ -> Expect.equal (run schema "users") (Ok Nothing)
        , test "default clause" <| \_ -> Expect.equal (run default "DEFAULT 'no name'") (Ok (Just "no name"))
        , test "parsing a complex CREATE TABLE statement" <|
            \_ ->
                let
                    statement =
                        """
                        CREATE TABLE IF NOT EXISTS public.users(
                            `id` INT NOT NULL PRIMARY KEY,
                            `name` CHARACTER VARYING(255) DEFAULT 'no name'
                            );
                        """
                in
                Expect.equal (run createTable statement)
                    (Ok <|
                        CreateTable True
                            (Just "public")
                            "users"
                            [ Column "id" Int Nothing True Nothing True Nothing Nothing
                            , Column "name" (CharacterVarying 255) Nothing False Nothing False (Just "no name") Nothing
                            ]
                    )
        , test "column constraint" <|
            \_ -> Expect.equal (run columnConstraint "CONSTRAINT users_pk") (Ok <| Just "users_pk")
        , test "with constraints" <|
            \_ ->
                let
                    statement =
                        """
                        CREATE TABLE [Users] (
                            [id] INT identity(1, 1) NOT NULL CONSTRAINT users_pk PRIMARY KEY,
                            "name" VARCHAR(255),
                            "profile_id" INT CONSTRAINT users_profile_fk REFERENCES public.profiles.id
                        );
                        """
                in
                Expect.equal (run createTable statement)
                    (Ok <|
                        CreateTable False
                            Nothing
                            "Users"
                            [ Column "id" Int (Just ( 1, 1 )) True (Just "users_pk") True Nothing Nothing
                            , Column "name" (Varchar (Just 255)) Nothing False Nothing False Nothing Nothing
                            , Column "profile_id" Int Nothing False (Just "users_profile_fk") False Nothing (Just "public.profiles.id")
                            ]
                    )
        ]
