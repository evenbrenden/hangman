module Main where

import Control.Monad (forever)
import Data.Char (toLower, toUpper)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import System.IO

minWordLength :: Int
minWordLength = 4

maxWordLength :: Int
maxWordLength = 6

maxGuesses :: Int
maxGuesses = maxWordLength

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

data Puzzle =
  Puzzle String [Maybe Char] [Char]

instance Show Puzzle
  where
    show (Puzzle _ discovered guesses) =
      (intersperse ' ' $ fmap renderPuzzleChar discovered) ++ " " ++ (guessesString guesses)
      where
        guessesString x
          | length x == 0 = ""
          | otherwise = "(" ++ x ++ ")"

freshPuzzle :: String
            -> Puzzle
freshPuzzle word = (Puzzle word (nothings word) [])
  where nothings a = fmap (\x -> Nothing) a

renderPuzzleChar :: (Maybe Char)
                 -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just s) = s

charInWord :: Puzzle
           -> Char
           -> Bool
charInWord (Puzzle word _ _) guess = elem guess word

alreadyGuessed :: Puzzle
               -> Char
               -> Bool
alreadyGuessed (Puzzle _ _ guessed) guess = elem guess guessed

fillInCharacter :: Puzzle
                -> Char
                -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c : s)
  where zipper guessed wordChar guessChar =
          if wordChar == guessed
          then Just wordChar
          else guessChar
        newFilledInSoFar =
          zipWith (zipper c)
            word filledInSoFar

numGuessesLeft :: Puzzle
               -> Int
numGuessesLeft puzzle = maxGuesses - (numIncorrectGuesses puzzle)
  where
    numIncorrectGuesses (Puzzle _ filledInSoFar guessed) =
      length guessed - length (filter isJust filledInSoFar)

handleGuess :: Puzzle
            -> Char
            -> IO Puzzle
handleGuess puzzle guess = do
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn $ "You already guessed " ++ [guess]
      return puzzle
    (True, _) -> do
      putStrLn $ "Score!"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn $ "Fail!"
      putStrLn $ show ((numGuessesLeft puzzle) - 1) ++ " guesses left"
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle
         -> IO ()
gameOver puzzle@(Puzzle wordToGuess filledInSoFar guessed) =
  if (numGuessesLeft puzzle) == 0 then
    do putStrLn $ "You lose!"
       putStrLn $ "The word was " ++ (fmap toUpper wordToGuess)
       exitSuccess
  else return ()

gameWin :: Puzzle
        -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
      do putStrLn $ "Win!"
         exitSuccess
  else return ()

runGame :: Puzzle
        -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ show puzzle
  putStr "Guess: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn $ "A single character plz"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn $ "Hangman!"
  werd <- randomWord'
  let puzzle = freshPuzzle (fmap toLower werd)
  runGame puzzle
