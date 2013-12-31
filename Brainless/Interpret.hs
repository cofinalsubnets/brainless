module Brainless.Interpret
( Brainfuck(..)
, Condition(..)
, brainfuck
, interpret
, run
, step
, command
) where

type Z a = ([a],[a])
data Brainfuck = BF { mem    :: Z Char
                    , prog   :: Z Char
                    , lstack :: [Z Char]
                    , input  :: String
                    , output :: String
                    }
data Condition = OOB | UnmatchedDelimiter | EOF | EOP

brainfuck :: String -> String -> Brainfuck
brainfuck p i = BF ([], repeat '\0') ("", p) [] i ""

interpret :: String -> String -> Either Condition String
interpret = (run .) . brainfuck

run :: Brainfuck -> Either Condition String
run b = case step b of Right b' -> run b'
                       Left EOP -> return . reverse $ output b
                       Left err -> Left err

step :: Brainfuck -> Either Condition Brainfuck
step b = case prog b of
  (_,[]) -> Left EOP
  (_,c:_) -> do
    b' <- command c b
    p' <- right $ prog b'
    return b' {prog=p'}

command :: Char -> Brainfuck -> Either Condition Brainfuck
command cmd = case cmd of
  '<' -> moveLeft
  '>' -> moveRight
  '+' -> incr
  '-' -> decr
  ',' -> bfread
  '.' -> bfwrite
  '[' -> startLoop
  ']' -> endLoop
  _   -> return

  where
    moveLeft  b = left  (mem b) >>= \m' -> return b {mem=m'}
    moveRight b = right (mem b) >>= \m' -> return b {mem=m'}

    incr bf@BF{mem=(a,b:c)} = return bf {mem=(a,succ b:c)}
    decr bf@BF{mem=(a,b:c)} = return bf {mem=(a,pred b:c)}

    bfread bf@BF{mem=(a,_:c), input=h:t} = return bf{mem=(a,h:c), input=t}
    bfread _ = Left EOF

    bfwrite bf@BF{mem=(_,m:_)} = return bf {output=m:output bf}

    startLoop bf@BF{mem=(_,m:_)} = skipLoop (prog bf) >>= \p ->
      if m == '\0' then return bf { prog = p }
                   else return bf {lstack = prog bf : lstack bf}

    endLoop bf@BF{mem=(_,m:_)} = case lstack bf of
      [] -> Left UnmatchedDelimiter
      (h:hs) -> return $ if m == '\0' then bf { lstack = hs }
                                      else bf { prog = h }
      
    skipLoop = skip (-1)
      where skip _ (_,[]) = Left UnmatchedDelimiter
            skip n p@(_,c:_) = case c of
              ']' -> if n == 0 then return p else right p >>= skip (n-1)
              '[' -> right p >>= skip (n+1)
              _   -> right p >>= skip n

right :: Z a -> Either Condition (Z a)
right (ls,r0:rs) = return (r0:ls,rs)
right _          = Left OOB

left :: Z a -> Either Condition (Z a)
left (l0:ls,rs) = return (ls,l0:rs)
left _          = Left OOB

