{-Copyright 2011 Ben Blackburne                                                                                                                                                                                                               
                                                                                                                                                                                                                                              
  This program is free software: you can redistribute it and/or modify                                                                                                                                                                        
  it under the terms of the GNU General Public License as published by                                                                                                                                                                        
  the Free Software Foundation, either version 3 of the License, or                                                                                                                                                                           
  (at your option) any later version.                                                                                                                                                                                                         
                                                                                                                                                                                                                                              
  This program is distributed in the hope that it will be useful,                                                                                                                                                                             
  but WITHOUT ANY WARRANTY; without even the implied warranty of                                                                                                                                                                              
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                                                                                                                                                                               
  GNU General Public License for more details.                                                                                                                                                                                                
                                                                                                                                                                                                                                              
  You should have received a copy of the GNU General Public License                                                                                                                                                                           
  along with this program.  If not, see <http://www.gnu.org/licenses/>.                                                                                                                                                                       
                                                                                                                                                                                                                                              
-}              
import System.Environment (getArgs)
import Phylo.Alignment
import Phylo.Tree
import Phylo.Alignment.DistSet
import Text.Printf
import System.Console.GetOpt
import Numeric
import Control.Monad

main = do args <- getArgs
          ans <- parseCommand args 
          putStrLn ans
          return ()
 
parseCommand :: [String] -> IO String
parseCommand ("-g":xs) = Main.diff homGapDist xs
parseCommand ("-n":xs) = Main.diff hom0Dist xs
parseCommand ("-t":xs) = diffTree homTreeDist xs
parseCommand xs = Main.diff homDist xs

diffTree :: (Node->ListAlignment->ListAlignment->(Int,Int)) -> [String] -> IO String
diffTree dist (z:x:y:xs) = do rawa <- parseAlignmentFile parseUniversal x
                              rawb <- parseAlignmentFile parseUniversal y
                              let a = fmap sortAlignment rawa
                              let b = fmap sortAlignment rawb
                              treeStr <- readFile z
                              let t = readBiNewickTree treeStr
                              return $ goTree dist ((liftM2 compatible) t a) ((liftM2 compatible) t b) t a b

diffTree dist x = return usage

usage = "Usage: alndist <-t> <tree> <-g> <-n> <fasta1> <fasta2>"

diff :: (ListAlignment -> ListAlignment -> (Int,Int)) -> [String] -> IO String
diff dist (x:y:xs) = do rawa <- parseAlignmentFile parseUniversal x
                        rawb <- parseAlignmentFile parseUniversal y
                        let a = fmap sortAlignment rawa
                        let b = fmap sortAlignment rawb
                        let ans = (liftM2 dist) a b
                        case ans of 
                                (Right (numPairs,numDiff)) -> return $ (show numDiff) ++ " / " ++ (show numPairs) ++ " = " ++ (show ((fromIntegral numDiff)/(fromIntegral numPairs)))
                                (Left err) -> return err

diff dist x = return usage


goTree :: (Node->ListAlignment->ListAlignment->(Int,Int)) -> Either String Bool  -> Either String Bool -> Either String Node -> Either String ListAlignment -> Either String ListAlignment -> String
goTree dist (Right False) x t a b = "Tree is incompatible with first alignment"
goTree dist (Right True) (Right False) t a b = "Tree is incompatible with second alignment"
goTree dist (Right True) (Right True) (Right t) (Right a) (Right b) = (show numDiff) ++ " / " ++ (show numPairs) ++ " = " ++ (show ((fromIntegral numDiff)/(fromIntegral numPairs))) 
                                                                   where numPairs = fst ans
                                                                         numDiff = snd ans
                                                                         ans = dist t a b 
goTree dist x y (Left err) a b = err
goTree dist x y t (Left err) b = err
