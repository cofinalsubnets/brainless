module Brainless.Compile
( MemType(..)
, CellType(..)
, CompileFlags(..)
, defaultFlags
, compile
) where

data MemType = Static Int | Ring Int | Dynamic
data CellType = Char | WChar | Int | Long
data CompileFlags = C { memType :: MemType, cellType :: CellType }

defaultFlags :: CompileFlags
defaultFlags = C (Static 30000) Char

compile :: CompileFlags -> String -> String
compile cf = super cf . base

base :: String -> String
base s = let insts = filter isInst s 
             indents = indent 1 insts
             cs = map compileInst insts
         in unlines $ zipWith (++) indents cs

indent :: Int -> String -> [String]
indent _ [] = []
indent n s = replicate (2*n0) ' ' : indent n1 (tail s)
  where (n0,n1) = case head s of '[' -> (n,n+1)
                                 ']' -> (n-1,n-1)
                                 _   -> (n,n)

compileInst :: Char -> String
compileInst c = case c of
  '>' -> "movr;"
  '<' -> "movl;"
  '+' -> "++(*i);"
  '-' -> "--(*i);"
  '.' -> "putchar(*i);"
  ',' -> "*i = getchar();"
  '[' -> "while (*i) {"
  ']' -> "}"
  _   -> ""

isInst :: Char -> Bool
isInst = (`elem` "<>+-.,[]")

super :: CompileFlags -> String -> String
super (C mt ct) insts = unlines [includes, start, insts, end]
  where
    start = unlines $ case mt of

      Static n ->
        [ "#define movr ++i"
        , "#define movl --i"
        , ctype ++ " d[" ++ show n ++ "];"
        , ctype ++ " *i = d;"
        , "int main() {"
        ]

      Ring n ->
        [ "#define MAXSIZE " ++ show (n-1)
        , "#define movr _mv(1)"
        , "#define movl _mv(-1)"
        , ctype ++ " d[" ++ show n ++ "];"
        , ctype ++ " *i = d;"
        , "void _mv(int);"
        , "void _mv(int n) {"
        , "  i+=n;"
        , "  if ((i-d) >= MAXSIZE) i=0;"
        , "  else if ((i-d) < 0) i=d+MAXSIZE;"
        , "}"
        , "int main() {"
        ]

      Dynamic ->
        [ "#define movr _movr()"
        , "#define movl --i"
        , "unsigned int size = 256;"
        , ctype ++ " *d, *i;"
        , "void _movr();"
        , "void _movr() {"
        , "  int pos = i-d;"
        , "  if (pos == size-1) {"
        , "    " ++ ctype ++ " *t = malloc(2 * size * sizeof(" ++ ctype ++ "));"
        , "    memcpy(t, d, size * sizeof(" ++ ctype ++ "));"
        , "    memset(t+size, 0, size * sizeof(" ++ ctype ++ "));"
        , "    free(d);"
        , "    d = t;"
        , "    size += size;"
        , "  }"
        , "  i = d + pos + 1;"
        , "}"
        , "int main() {"
        , "  d = (" ++ ctype ++ "*) malloc(size * sizeof(" ++ ctype ++ "));"
        , "  i = d;"
        ]

    ctype = case ct of
      Char -> "char"
      WChar -> "wchar_t"
      Int -> "int"
      Long -> "long"

    includes = unlines $ common ++ memS ++ cellS
      where common = ["#include <stdio.h>" , "#include <stdlib.h>"]
            cellS  = case ct of {WChar   -> ["#include <wchar.h>"];  _ -> []}
            memS   = case mt of {Dynamic -> ["#include <string.h>"]; _ -> []}

    end = "  return 0;\n}"
 
