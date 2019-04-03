module Main where

import System.Posix.IO

main :: IO()
main = do
    (char, count) <- fdRead stdInput 1
    if count == 1 then
        main
    else return ()