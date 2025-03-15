module Main where

import qualified Data.Text as T

main :: IO ()
main = do
    let text = T.pack "Hello, World!"
    putStrLn $ T.unpack text