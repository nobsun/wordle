module Interact
    ( interactWithPrompt
    ) where

import System.IO.Unsafe
import System.Console.Haskeline

interactWithPrompt :: String -> String -> ([String] -> [String]) -> IO ()
interactWithPrompt prompt quit f 
    = putStr . unlines . f =<< loop
    where
        loop :: IO [String]
        loop = unsafeInterleaveIO $ do
            { minput <- runInputT defaultSettings (getInputLine prompt)
            ; case minput of
                Nothing   -> return []
                Just line
                    | line == quit -> return []
                    | otherwise    -> (line :) <$> loop
            }
