module Main where

import NWA

main :: IO ()
main = case new (State 1) [] [(State 1)] of
    (Left err) -> putStrLn err
    (Right auto) -> case exec auto "" of
        (Left err) -> putStrLn err
        (Right b) -> putStrLn (show b)
