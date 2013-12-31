module Brainless.Debug
( debug
, summarize
, trace
) where

import Data.List
import Brainless.Interpret

data Debug = Debug ([Brainfuck], [Brainfuck]) Condition

debug :: String -> String -> Debug
debug = (debug' .) . brainfuck

debug' :: Brainfuck -> Debug
debug' = let dbg ss b = either (Debug (ss,[b])) (dbg $ b:ss) $ step b in dbg []

summarize :: Debug -> String
summarize (Debug (s0,s1) es) =
  "Program traverses " ++ show nstates ++ " states and " ++ finish ++ ".\n" ++
  "Final output: " ++ show out
  where
    lastState = last s1
    nstates   = length s0 + length s1
    out       = reverse $ output lastState
    lastCmd   = show . succ . length . fst $ prog lastState
    finish    = (++ lastCmd) $ case es of
      EOP                -> "ends normally after command "
      EOF                -> "hits EOF at command "
      OOB                -> "accesses an out-of-bounds cell at command "
      UnmatchedDelimiter -> "encounters an unmatched loop delimiter at command "

trace :: Debug -> String
trace (Debug (s0,s1) e) =
  intercalate "\n" . (header:) . (++end) . map showState $ reverse s0 ++ s1
  where header = "CMD (#) | MEM (#) | OUTPUT"
        end = case e of EOP -> []
                        EOF -> ["ERROR: end of input"]
                        OOB -> ["ERROR: out-of-bounds cell access"]
                        UnmatchedDelimiter ->
                          ["ERROR: unmatched loop delimiter"]

showState :: Brainfuck -> String
showState (BF (mp, m0:_) p _ _ o) = intercalate " | " $
  [cmd, show m0 ++ " (" ++ show (length mp) ++ ")", show (reverse o)]
  where cmd = case p of
          (_,[])    -> "<final>"
          (pp,p0:_) -> show p0 ++ " (" ++ show (succ $ length pp) ++ ")"

