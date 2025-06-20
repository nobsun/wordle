{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE BangPatterns #-}
module Wordle
    ( wordle
    ) where

import Data.Bool
import Data.Char
import Data.Maybe
import Interact

wordle :: String -> [String] -> [String]
wordle extra = mapMaybe output . eval . initial extra

data MachineState
    = MachineState 
    { inChan :: [String]
    , output :: Maybe String
    , innerState :: InnerState
    }

type InnerState = Dict
type Dict = [String]

initial :: String -> [String] -> MachineState
initial extra inputs
    = MachineState
    { inChan = inputs
    , output = Nothing
    , innerState = filter (all isAscii)
                 . filter (all isLetter)
                 . filter (all isLower)
                 . filter ((5 ==) . length) 
                 . lines 
                 $ extra
    }

eval :: MachineState -> [MachineState]
eval state = state : rests
    where
        rests | isFinal state = []
              | otherwise     = eval (step state)

isFinal :: MachineState -> Bool
isFinal state = case state of
    MachineState { inChan = [] } -> True
    _                            -> False

step :: MachineState -> MachineState
step state = case state of
    MachineState { inChan = i : is
                 , innerState = dict
                 } -> state { inChan = is
                            , innerState = dict'  
                            , output = msg
                            }
                        where
                            (dict', msg) = case words i of
                                guess:pattern:_
                                    | all (`elem` "bgy") pattern 
                                      && length pattern >= 5 -> (dict'', ok)
                                    | otherwise -> (dict, ng)
                                    where
                                        ng    = Just $ "Error"
                                                     ++ ": pattern contains other char than \"bgy\" or too short"
                                        ok    = Just $ unlines dict'
                                        dict'' = buildFilter guess pattern dict
                                _ -> (dict, Just "pleas input both guess and pattern")

buildFilter :: String -> String -> [String] -> [String]
buildFilter _     ""      = id
buildFilter guess pattern = filter p
    where
        p w = pattern == matchPattern guess w

matchPattern :: String -> String -> String
matchPattern guess target
    = case zipWith phi guess target of
        str1 -> zipWith (psi str1) guess str1
    where
        phi x y = bool y ' ' (x == y)
        psi zs x y
            | y == ' '     = 'g'
            | x `elem` zs  = 'y'
            | otherwise    = 'b'
