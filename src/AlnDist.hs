import System.Environment (getArgs,getProgName)
import System.IO
import Phylo.Alignment
import Phylo.Tree
import Phylo.Alignment.Dist
import Text.Printf
import System.Console.GetOpt
import Numeric
import Control.Monad
import Data.List (unlines)

data Options = Options  { optVersion    :: Bool
                        , optFunc       :: ListAlignment -> ListAlignment -> Either String [(Int,Int)]
                        , sumFunc       :: ListAlignment -> ListAlignment -> Either String [(Int,Int)] -> IO ()
}

options :: [ OptDescr (Options -> IO Options) ]
options = [ Option ['p'] ["pos"] (NoArg (\opt-> return opt {optFunc = safeCompare homGapDist})) "Homolgy distance with gaps labelled by position (d_pos)",
            Option ['n'] ["ssp"] (NoArg (\opt -> return opt {optFunc = safeCompare hom0Dist})) "Symmetrised Sum-Of-Pairs (d_SSP)",
            Option ['s'] ["simple"] (NoArg (\opt -> return opt {optFunc = safeCompare homDist})) "Homology distance (default, d_simple)",
            Option ['t'] ["tree"] (ReqArg (\arg opt -> do treeIO <- (liftM readBiNewickTree) (readFile arg)
                                                          let tree = case treeIO of
                                                                       Right t -> t
                                                                       Left err -> error err
                                                          return $ opt {optFunc = (safeTreeCompare homTreeDist tree)}) "TREE" )
            "Homology distance with tree-labelled gaps (d_evol)",
            Option ['a'] ["all-seqs"] (NoArg (\opt -> return opt {sumFunc = seqwiseDistance})) "Output distance for each sequence"
          ]
safeCompare :: (ListAlignment -> ListAlignment -> [(Int,Int)]) -> ListAlignment -> ListAlignment -> Either String [(Int,Int)]
safeCompare dist aln1 aln2 = case compatibleAlignments aln1 aln2 of 
                                        False -> Left "Incompatible alignments"
                                        True -> Right $ dist aln1 aln2

safeTreeCompare dist tree aln1 aln2 = case (compatible tree aln1) of
                                        False -> Left "Tree is incompatible with first alignment"
                                        True -> case (compatible tree aln2) of 
                                                False -> Left "Tree is incompatible with second alignment"
                                                True -> safeCompare (dist tree) aln1 aln2


startOptions = Options {optVersion = False,
                        optFunc = safeCompare homDist,
                        sumFunc = summariseDistance }

summariseDistance first second out = case out of
                                  Left err -> hPutStrLn stderr err
                                  Right list -> putStrLn $ (show n) ++ " / " ++ (show d) ++ " = " ++ (show ((fromIntegral n)/(fromIntegral d))) 
                                                where (d,n) = sumList list
seqwiseDistance first second out = case out of
                                  Left err -> hPutStrLn stderr err
                                  Right list -> putStrLn $ (unlines seqBySeq) ++ (show n) ++ " / " ++ (show d) ++ " = " ++ (show ((fromIntegral n)/(fromIntegral d))) 
                                                where (d,n) = sumList list
                                                      seqNames = Phylo.Alignment.names first
                                                      seqBySeq = map (\(nam,(i,j)) -> nam ++ " " ++ (show j) ++ " / " ++ (show i) ++ " = " ++ (show ((fromIntegral j)/(fromIntegral i)))) $ zip seqNames list



sumList = foldr (\(i,j) (i2,j2)-> (i+i2,j+j2)) (0,0)

main = do args <- getArgs
          let (actions, nonOptions, errors)=getOpt Permute options args
          opts <- foldl (>>=) (return startOptions) actions
          let Options {optFunc = f, sumFunc = sF} = opts
          alignments <- mapM readAln nonOptions
          me <- getProgName
          let preamble = unlines ["MetAl: Metrics for Multiple Sequence Alignments.",
                                  "Usage: metal <options> alignment1 alignment2",
                                  "This is free software, see LICENCE for details",
                                  "Options:"]
          case alignments of 
               (x:y:[]) -> sF x y (f x y) 
               _        -> hPutStrLn stderr (usageInfo preamble options)



readAln :: String -> IO ListAlignment
readAln x = do rawa <- parseAlignmentFile parseFasta x
               return $ case rawa of 
                          Right aln -> aln
                          Left err -> error err 
