module Main where

import Control.Monad (forever)
import Data.Char (toLower, toUpper)
import Data.Maybe (isJust)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import System.IO
import Helpers

minWordLength :: Int
minWordLength = 4

maxWordLength :: Int
maxWordLength = 6

newtype WordList =
  WordList [String]
  deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
          in  l >= minWordLength &&
              l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0 , length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

freshPuzzle :: String
            -> Helpers.Puzzle
freshPuzzle word = (Puzzle word (nothings word) [])
  where nothings a = fmap (\x -> Nothing) a

gameOver :: Helpers.Puzzle
         -> IO ()
gameOver puzzle@(Puzzle wordToGuess filledInSoFar guessed) =
  if (numGuessesLeft puzzle) == 0 then
    do putStrLn $ "You lose!"
       putStrLn $ "The word was " ++ (fmap toUpper wordToGuess)
       exitSuccess
  else return ()

gameWin :: Helpers.Puzzle
        -> IO ()
gameWin (Puzzle wordToGuess filledInSoFar _) =
  if all isJust filledInSoFar then
      do putStrLn $ "Win!"
         putStrLn $ "You got " ++ (fmap toUpper wordToGuess)
         exitSuccess
  else return ()

runGame :: Helpers.Puzzle
        -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ show puzzle
  putStr "Guess: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn $ "A single character plz!"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn $ "Hangman!"
  werd <- randomWord'
  let puzzle = freshPuzzle (fmap toLower werd)
  runGame puzzle
