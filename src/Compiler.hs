module Compiler where

import Parser (parseExpr)
import PrettyPrinter

compile :: String -> String
compile code = either id pretty ast
    where ast = parseExpr code