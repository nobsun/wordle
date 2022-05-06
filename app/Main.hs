module Main where

import System.Environment ( getArgs )
import Data.List.Extra
import Wordle ( wordle )
import Interact ( interactWithPrompt )

main :: IO ()
main =  interactWithPrompt "? " ":quit" . wordle 
    =<< list (readFile "/usr/share/dict/words") (const . readFile)
    =<< getArgs
