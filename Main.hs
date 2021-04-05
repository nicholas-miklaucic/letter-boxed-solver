module Main where

import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Set as Set
import Debug.Trace
import System.IO

type Corpus = [String]

type Puzzle = [String]

makePuzzle :: String -> Puzzle
makePuzzle = chunksOf 3

getRow :: Puzzle -> Char -> Maybe Int
getRow puz char = findIndex (char `elem`) puz

validSeq :: [String] -> Bool
validSeq [] = True
validSeq words = all (uncurry connects) (zip words $ tail words)
  where
    connects :: String -> String -> Bool
    connects "" _ = False
    connects _ "" = False
    connects w1 w2 = last w1 == head w2

validLetters :: Puzzle -> [String] -> Bool
validLetters _ [] = True
validLetters puz ws = all (uncurry $ isSeq puz) (zip letters $ tail letters)
  where
    letters :: String
    letters = head ws ++ concatMap tail (tail ws)
    isSeq :: Puzzle -> Char -> Char -> Bool
    isSeq puz c1 c2 = i1 /= i2 && isJust i1 && isJust i2
      where
        i1 = getRow puz c1
        i2 = getRow puz c2

isValid :: Puzzle -> [String] -> Bool
isValid puz words = validSeq words && validLetters puz words

-- eliminates % suffixes and ! prefixes, uncommon/disallowed words
readCorpus :: String -> Corpus
readCorpus file = filter noNote $ words file
  where
    noNote :: String -> Bool
    noNote ('!' : _) = False
    noNote "" = False
    noNote x = '%' /= last x

numNew :: Puzzle -> [String] -> String -> Int
numNew puz soFar new = - (Set.size $ Set.union lettersSoFar newLetters)
  where
    lettersSoFar :: Set.Set Char
    lettersSoFar = Set.difference (Set.fromList (concat puz)) (Set.fromList (concat soFar))
    newLetters :: Set.Set Char
    newLetters = Set.fromList new

naiveSolve :: Puzzle -> Corpus -> Int -> [[String]]
naiveSolve puz corp maxLen = solveInner puz (filter (\x -> isValid puz [x]) corp) maxLen []
  where
    solveInner :: Puzzle -> Corpus -> Int -> [String] -> [[String]]
    solveInner puz corp maxLen soFar
      | length soFar > maxLen = []                                                                
      | (Set.fromList (concat soFar) == Set.fromList (concat puz)) && isValid puz soFar = [soFar] 
      | not $ isValid puz soFar = []                                                              
      | otherwise = concatMap tryNext $ sortOn (numNew puz soFar) candidates                      
      where
        candidates
          | null soFar = corp                                           
          | otherwise = filter (\x -> last (last soFar) == head x) corp 
        tryNext :: String -> [[String]]
        tryNext next = solveInner puz corp maxLen (soFar ++ [next])

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine        

main :: IO ()
main = do
  corp <- readCorpus . map toUpper <$> readFile "2of12inf.txt"
  puz <- makePuzzle <$> prompt "Enter the puzzle: "
  numWords <- read <$> prompt "Number of words: "
  showSol <- prompt "Show solutions? (Y/N) "
  let firstSolves = take 10 $ naiveSolve puz corp numWords
  if showSol == "Y"
    then putStrLn $ "Solutions: " ++ (show firstSolves)
    else putStrLn $ "# of solutions: " ++ (show $ length firstSolves)
