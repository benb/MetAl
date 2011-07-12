import System.Environment (getArgs,getProgName)
import System.IO
import Phylo.Alignment
import Phylo.Tree
import Phylo.Alignment.Dist
import Text.Printf
import System.Console.GetOpt
import Numeric
import Control.Monad
import Data.List (unlines,intercalate)
import Text.JSON
import Debug.Trace

data Options = Options  { optVersion    :: Bool
                        , optFunc       :: ListAlignment -> ListAlignment -> Either String [[(Int,Int)]]
                        , sumFunc       :: ListAlignment -> ListAlignment -> Either String [[(Int,Int)]] -> IO ()
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
            Option ['a'] ["all-seqs"] (NoArg (\opt -> return opt {sumFunc = seqwiseDistance})) "Output distance for each sequence",
            Option ['c'] ["all-sites"] (ReqArg (\arg opt -> return opt {sumFunc = basewiseDistance arg}) "CSV") "Output CSV with sitewise distances (implies -a)",
            Option ['j'] ["json"] (NoArg (\opt -> return opt {sumFunc = jsonDistance})) "Output all distances to JSON format"
          ]
safeCompare :: (ListAlignment -> ListAlignment -> [[(Int,Int)]]) -> ListAlignment -> ListAlignment -> Either String [[(Int,Int)]]
safeCompare dist aln1 aln2 = case compatibleAlignments aln1 aln2 of 
                                        False -> trace ((show aln1) ++ "\n\n" ++ (show aln2)) Left "Incompatible alignments"
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
                                                where (d,n) = totalDistList list

seqwiseDistance :: ListAlignment -> ListAlignment -> Either String [[(Int,Int)]] -> IO ()
seqwiseDistance first second out = case out of
                                  Left err -> hPutStrLn stderr err
                                  Right list -> putStrLn $ (unlines seqBySeq) ++ (show n) ++ " / " ++ (show d) ++ " = " ++ (show ((fromIntegral n)/(fromIntegral d))) 
                                                where (d,n) = totalDistList list
                                                      seqNames = Phylo.Alignment.names first
                                                      seqBySeq = map (\(nam,(i,j)) -> nam ++ " " ++ (show j) ++ " / " ++ (show i) ++ " = " ++ (show ((fromIntegral j)/(fromIntegral i)))) $ zip seqNames (reverse $ map summariseDistList list)


basewiseDistance :: String -> ListAlignment -> ListAlignment -> Either String [[(Int,Int)]] -> IO ()
basewiseDistance filename first second out = do outFile <- openFile filename WriteMode
                                                case out of
                                                        Left err -> hPutStrLn stderr err
                                                        Right list -> do dumpCSV seqNames list outFile
                                                                         hClose outFile
                                                                         putStrLn $ (unlines seqBySeq) ++ (show n) ++ " / " ++ (show d) ++ " = " ++ (show ((fromIntegral n)/(fromIntegral d))) 
                                                                             where (d,n) = totalDistList list
                                                                                   seqNames = Phylo.Alignment.names first
                                                                                   seqBySeq = map (\(nam,(i,j)) -> nam ++ " " ++ (show j) ++ " / " ++ (show i) ++ " = " ++ (show ((fromIntegral j)/(fromIntegral i)))) $ zip seqNames (reverse $ map summariseDistList list)

data Distances = Distances [String] [[(Int,Int)]] deriving (Show,Eq)

instance JSON  Distances where
                showJSON (Distances names dists) = showJSON ("Distances",names,dists)
                readJSON x = Error "Unimplemented"

jsonDistance :: ListAlignment -> ListAlignment -> Either String [[(Int,Int)]] -> IO ()
jsonDistance first second out = case out of
                                        Left err -> hPutStrLn stderr err
                                        Right list -> putStrLn $ encode $ showJSON (showJSON total,showJSON first,showJSON second, showJSON (Distances seqNames (reverse (map reverse list))))
                                                               where (d,n) = totalDistList list
                                                                     total::(String,Double,Double,Double)
                                                                     total = ("total",fromIntegral n,fromIntegral d,(fromIntegral n)/(fromIntegral d))
                                                                     seqNames = Phylo.Alignment.names first
                                                                     seqBySeq = map (\(nam,(i,j)) -> nam ++ " " ++ (show j) ++ " / " ++ (show i) ++ " = " ++ (show ((fromIntegral j)/(fromIntegral i)))) $ zip seqNames (reverse $ map summariseDistList list)



dumpCSV seqNames list handle = hPutStrLn handle $ unlines $ map (\(name,l2)->name ++ "," ++ intercalate "," (reverse $ map toStr l2)) $ zip seqNames (reverse list)
                                where toStr (i,j) = show ((fromIntegral j)/(fromIntegral i))

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
readAln x = do rawa <- parseAlignmentFile parseUniversal x
               return $ case rawa of 
                          Right aln -> aln
                          Left err -> error err 
