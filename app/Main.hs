module Main where

import System.Environment
import PointFree
import Expr

main :: IO ()
main = do
    { a:_ <- getArgs
    ; print (normalize (pointfree (read a)))
    }
