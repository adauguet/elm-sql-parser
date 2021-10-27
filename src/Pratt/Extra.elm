module Pratt.Extra exposing (..)

import Parser exposing (Parser)
import Pratt exposing (Config)
import Pratt.Advanced.Extra as Advanced


postfix2 : Int -> Parser a -> (a -> expr -> expr) -> Config expr -> ( Int, expr -> Parser expr )
postfix2 =
    Advanced.postfix2_
