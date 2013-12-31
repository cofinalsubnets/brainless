import Brainless.Interpret
import Brainless.Debug
import Brainless.Compile
import System.Console.GetOpt
import System.Environment
import System.Exit
import Data.List

data Options = O { cflags    :: CompileFlags
                 , infile    :: FilePath
                 , outfile   :: Maybe FilePath
                 , doCompile :: Bool
                 , doSummary :: Bool
                 , doTrace   :: Bool
                 }

main :: IO ()
main = getArgs >>= parseArgs >>= execute

parseArgs :: [String] -> IO Options
parseArgs args = case getOpt Permute options args of
  (os,[f],[]) -> return $ foldr ($) defaultOptions{infile = f} os
  (_,_,es) -> mapM_ putStr es >> printUsage >> exitFailure

printUsage :: IO ()
printUsage = do
  pn <- getProgName
  let header = "Usage: " ++ pn ++ " [OPTIONS] INFILE"
  putStr $ usageInfo header options

defaultOptions :: Options
defaultOptions = O defaultFlags "" Nothing False False False

execute :: Options -> IO ()
execute o
 | doCompile o = readFile inf >>= writeFile outf . compile (cflags o)
 | doSummary o = dbg summarize
 | doTrace o   = dbg trace
 | otherwise   = getData
             >>= either (const exitFailure) putStr . run . uncurry brainfuck
  where
    inf     = infile o
    outf    = maybe (outfileFor inf) id $ outfile o
    dbg r   = getData >>= putStrLn . r . uncurry debug
    getData = readFile inf >>= \p -> getContents >>= \i -> return (p,i)


outfileFor :: FilePath -> FilePath
outfileFor inf = basename ++ ".c"
  where
    hts = zip (inits inf) (tails inf)
    basename = case filter (isPrefixOf "." . snd) hts of
      [] -> inf
      hts' -> fst $ last hts'

options :: [OptDescr (Options -> Options)]
options = [ Option "e" ["execute"]
            (NoArg id)
            "execute input file (default)"
          , Option "c" ["compile"]
            (NoArg (\o -> o{doCompile = True}))
            "compile input file to C"
          , Option "o" ["out"]
            (ReqArg (\n o -> o{outfile = Just n}) "FILE")
            "write compiled program to FILE"
          , Option "s" ["summary"]
            (NoArg (\o -> o{doSummary = True}))
            "output program summary"
          , Option "t" ["trace"]
            (NoArg (\o -> o{doTrace = True}))
            "output program trace"
          , Option "" ["ring"]
            (ReqArg (\n o -> o{cflags = (cflags o){memType = Ring (read n)}})
             "SIZE")
            "use static ring buffer of size SIZE"
          , Option "" ["static"]
            (ReqArg (\n o -> o{cflags = (cflags o){memType = Static (read n)}})
             "SIZE")
            "use SIZE static cells (default with SIZE=3000)"
          , Option "" ["dynamic"]
            (NoArg (\o -> o{cflags = (cflags o){memType = Dynamic}}))
            "use dynamically allocated cells"
          , Option "" ["char"]
            (NoArg (\o -> o{cflags = (cflags o){cellType = Char}}))
            "use cell type `char' (default)"
          , Option "" ["wchar"]
            (NoArg (\o -> o{cflags = (cflags o){cellType = WChar}}))
            "use cell type `wchar_t'"
          , Option "" ["int"]
            (NoArg (\o -> o{cflags = (cflags o){cellType = Int}}))
            "use cell type `int'"
          , Option "" ["long"]
            (NoArg (\o -> o{cflags = (cflags o){cellType = Long}}))
            "use cell type `long'"
          ]

