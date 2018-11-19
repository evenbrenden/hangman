module Helpers where

import Data.Maybe (isJust)
import Data.List (intersperse)

maxGuesses :: Int
maxGuesses = 10

data Puzzle =
  Puzzle String [Maybe Char] [Char]
  deriving Eq

instance Show Puzzle
  where
    show (Puzzle _ discovered guesses) =
      (intersperse ' ' $ fmap renderPuzzleChar discovered) ++ " " ++ (guessesString guesses)
      where
        guessesString x
          | length x == 0 = ""
          | otherwise = "(" ++ x ++ ")"

renderPuzzleChar :: (Maybe Char)
                 -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just s) = s

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

charInWord :: Puzzle
           -> Char
           -> Bool
charInWord (Puzzle word _ _) guess = elem guess word

alreadyGuessed :: Puzzle
               -> Char
               -> Bool
alreadyGuessed (Puzzle _ _ guessed) guess = elem guess guessed

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

