module Main where

import Control.Monad (when)
import System.Posix.IO

main :: IO()
main = do
    (char, count) <- fdRead stdInput 1
    when (count == 1 && char /= "q" ) main