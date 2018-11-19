module Main where

import Test.Hspec
import Helpers
import System.IO.Unsafe

main :: IO ()
main = hspec $ do
  describe "fillInCharacter" $ do
    it "Wrong guess" $ do
      let beforePuzzle = Puzzle "word" [Nothing, Just 'o', Nothing, Just 'd'] "x"
      let afterPuzzle = Puzzle "word" [Nothing, Just 'o', Nothing, Just 'd'] "yx"
      (fillInCharacter beforePuzzle 'y') `shouldBe` afterPuzzle
    it "Already guessed" $ do
      let beforePuzzle = Puzzle "word" [Nothing, Just 'o', Nothing, Just 'd'] "x"
      let afterPuzzle = Puzzle "word" [Nothing, Just 'o', Nothing, Just 'd'] "xx"
      (fillInCharacter beforePuzzle 'x') `shouldBe` afterPuzzle
    it "Right guess" $ do
      let beforePuzzle = Puzzle "word" [Nothing, Just 'o', Nothing, Just 'd'] "x"
      let afterPuzzle = Puzzle "word" [Just 'w', Just 'o', Nothing, Just 'd'] "wx"
      (fillInCharacter beforePuzzle 'w') `shouldBe` afterPuzzle
  describe "handleGuess" $ do
    it "Wrong guess" $ do
      let beforePuzzle = Puzzle "word" [Nothing, Just 'o', Nothing, Just 'd'] "x"
      let afterPuzzle = Puzzle "word" [Nothing, Just 'o', Nothing, Just 'd'] "yx"
      let result = unsafePerformIO (handleGuess beforePuzzle 'y')
      result `shouldBe` afterPuzzle
    it "Already guessed" $ do
      let beforePuzzle = Puzzle "word" [Nothing, Just 'o', Nothing, Just 'd'] "x"
      let afterPuzzle = beforePuzzle
      let result = unsafePerformIO (handleGuess beforePuzzle 'x')
      result `shouldBe` afterPuzzle
    it "Right guess" $ do
      let beforePuzzle = Puzzle "word" [Nothing, Just 'o', Nothing, Just 'd'] "x"
      let afterPuzzle = Puzzle "word" [Just 'w', Just 'o', Nothing, Just 'd'] "wx"
      let result = unsafePerformIO (handleGuess beforePuzzle 'w')
      result `shouldBe` afterPuzzle

