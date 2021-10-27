module Pratt.Advanced.Extra exposing (postfix2_)

import Parser.Advanced exposing (Parser, map)
import Pratt.Advanced exposing (Config)


postfix2_ : Int -> Parser c x a -> (a -> e -> e) -> Config c x e -> ( Int, e -> Parser c x e )
postfix2_ precedence operator apply _ =
    ( precedence
    , \left -> map (\a -> apply a left) operator
    )
