{-Copyright 2011 Ben Blackburne 

    This file is part of MetAl.

    MetAl is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    MetAl is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with MetAl.  If not, see <http://www.gnu.org/licenses/>.

-}

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
                        , ignoreNames   :: Bool
                        , preprocFunc      :: ListAlignment -> ListAlignment
                        , optFunc       :: ListAlignment -> ListAlignment -> [[(Int,Int)]]
                        , sumFunc       :: ListAlignment -> ListAlignment -> Either String [[(Int,Int)]] -> IO ()
                        , compareFunc     :: (ListAlignment -> ListAlignment -> [[(Int,Int)]]) -> ListAlignment -> ListAlignment -> Either String [[(Int,Int)]]
}

options :: [ OptDescr (Options -> IO Options) ]
options = [ Option ['p'] ["pos"] (NoArg (\opt-> return opt {optFunc = homGapDist})) "Homolgy distance with gaps labelled by position (default, d_pos)",
            Option ['n'] ["ssp"] (NoArg (\opt -> return opt {optFunc = hom0Dist, preprocFunc = identity})) "Symmetrised Sum-Of-Pairs (d_SSP)",
            Option ['s'] ["simple"] (NoArg (\opt -> return opt {optFunc = homDist, preprocFunc = identity})) "Homology distance (d_simple)",
            Option ['t'] ["tree"] (ReqArg (\arg opt -> do treeIO <- (liftM readBiNewickTree) (readFile arg)
                                                          let tree = case treeIO of
                                                                       Right t -> t
                                                                       Left err -> error err
                                                          return $ opt {optFunc = (safeTreeCompare homTreeDist tree)}) "TREE" )
            "Homology distance with tree-labelled gaps (d_evol)",
            Option ['a'] ["all-seqs"] (NoArg (\opt -> return opt {sumFunc = seqwiseDistance})) "Output distance for each sequence",
            Option ['c'] ["all-sites"] (ReqArg (\arg opt -> return opt {sumFunc = basewiseDistance arg}) "CSV") "Output CSV with sitewise distances (implies -a)",
            Option ['j'] ["json"] (NoArg (\opt -> return opt {sumFunc = jsonDistance})) "Output all distances to JSON format",
            Option ['f'] ["force"] (NoArg (\opt -> return opt {compareFunc=unsafeCompare})) "Force comparsion of possibly incompatible alignments",
            Option [] ["ignore-names"] (NoArg (\opt -> return opt {ignoreNames = True})) "Ignore the names in the alignment files (rely on order in file)",
            Option [] ["seqorder"] (NoArg (\opt -> return opt {preprocFunc = sortAlignmentBySeq})) "Reorder non-overlapping columns on alignments sorted by sequence content, not name",
            Option [] ["nameorder"] (NoArg (\opt -> return opt {preprocFunc = sortAlignment})) "Reorder non-overlapping columns on alignments sorted by name, not sequence content (default)"
          ]

safeCompare :: (ListAlignment -> ListAlignment -> [[(Int,Int)]]) -> ListAlignment -> ListAlignment -> Either String [[(Int,Int)]]
safeCompare dist aln1 aln2 = case incompatibilities aln1 aln2 of 
                                        (Incompat (fatal,err):errs) -> Left err
                                        [] -> Right $ dist aln1 aln2
unsafeCompare dist aln1 aln2 = case (filter (\(Incompat (fatal,err)) -> fatal ) $ incompatibilities aln1 aln2) of 
                                        (Incompat (fatal,err):errs) -> Left err
                                        [] -> Right $ dist aln1 aln2


safeTreeCompare dist tree aln1 aln2 = case (compatible tree aln1) of
                                          False -> error "Tree is incompatible with first alignment"
                                          True -> case (compatible tree aln2) of 
                                                False -> error "Tree is incompatible with second alignment"
                                                True -> (dist tree) aln1 aln2


startOptions = Options {optVersion = False,
                        ignoreNames = False,
                        optFunc = homGapDist,
                        preprocFunc = sortAlignment,
                        sumFunc = summariseDistance,
                        compareFunc = safeCompare }

identity x = x

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

reassignNames ((ListAlignment names seqs cols):(ListAlignment _ seqs' cols'):xs) | (length seqs) == (length seqs')  = (ListAlignment names seqs cols):(ListAlignment names seqs' cols'):xs 
                                                                                 | otherwise = error $ "ERROR: alignment files have " ++ (show $ length seqs) ++ " and " ++ (show $ length seqs') ++ " sequences, respectively"

main = do args <- getArgs
          let (actions, nonOptions, errors)=getOpt Permute options args
          opts <- foldl (>>=) (return startOptions) actions
          let Options {optFunc = optF, ignoreNames = iG, sumFunc = sF, compareFunc = cF, preprocFunc = pp} = opts
          let f = cF optF
          alignments' <- mapM readAln nonOptions
          let alignments = case iG of 
                                False -> map pp alignments'
                                True -> map pp $ reassignNames alignments'
          me <- getProgName
          let preamble = unlines ["MetAl 1.0.3: Metrics for Multiple Sequence Alignments.",
                                  "Usage: metal <options> alignment1 alignment2",
                                  "This is free software, see COPYING for details",
                                  "Options:"]
          case alignments of 
               (x:y:[]) -> sF x y (f x y) 
               _        -> hPutStrLn stderr (usageInfo preamble options)



readAln :: String -> IO ListAlignment
readAln x = do rawa <- parseAlignmentFile parseUniversal x
               return $ case rawa of 
                          Right aln -> aln
                          Left err -> error $ "Error parsing alignment " ++ x ++ ": " ++ err 
