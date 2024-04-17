-- Wordle game in Haskell

import System.Random
import Data.List
import Data.Maybe

-- wordlist.txt contains all 12947 5 letter words accepted by Wordle
wordlistFile :: String
wordlistFile = "wordlist.txt"
-- answerlist.txt contains all 2309 5 letter words that are potential answers in Wordle (this is a subset of wordlist.txt that excludes too obscure words, plurals, etc.)
answerlistFile :: String
answerlistFile = "answerlist.txt"
-- wordlist.txt and answerlist.txt are both taken from https://dagshub.com/arjvik/wordle-wordlist

-- number of guesses allowed
maxGuesses :: Int
maxGuesses = 6

-- print_intro prints the introduction to the game
print_intro :: IO ()
print_intro = do
  putStrLn "Guess the word!"
  putStrLn ("You have " ++ show maxGuesses ++ " guesses to guess the word.")
  putStrLn "The word is a 5-letter word, picked from a list of common English words (should be the same as the official Wordle word list)."
  putStrLn "Enter a guess after each prompt."
  putStrLn "Good luck!"
  putStrLn ""

-- load_wordlist loads a list of words from a file
load_wordlist :: String -> IO [String]
load_wordlist filename = do
  contents <- readFile filename -- readFile reads a file and returns its contents as a string
  return (lines contents) -- lines splits a string into a list of strings at newline characters

-- pick_solution picks a random word from a list of words
pick_solution :: [String] -> IO String
pick_solution wordlist = do
  index <- randomRIO (0, length wordlist - 1) -- randomRIO generates a random number in the IO monad
  return (wordlist !! index) -- !! is the list indexing operator

-- the game state
data GameState = GameState
  { guesses :: [String] -- list of guesses
  , numGuesses :: Int -- number of guesses
  , answer :: String -- the answer
  , wordlist :: [String] -- the word list
  }

-- initialState is the initial game state
initialState :: [String] -> String -> GameState
initialState wordlist answer = GameState
  { guesses = []
  , numGuesses = 0
  , answer = answer
  , wordlist = wordlist
  }

-- toUpper converts a string to uppercase (all prints in the terminal are uppercase to look better)
toUpper :: String -> String
toUpper = map (\c -> if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c)

-- returnFeedbackChar returns the feedback for a single character in a guess (🟩 or 🟨 or ⬜)
returnFeedbackChar :: String -> String -> Char -> Int -> Char
returnFeedbackChar answer guess guessChar guessIndex
  | answer !! guessIndex == guessChar = '🟩' -- !! returns the Char at index in answer
  | elem guessChar answer = '🟨' -- elem checks if guessChar is in answer
  | otherwise = '⬜' -- otherwise is a synonym for True

-- returnFeedbackWord returns the feedback for 1 guess (the list of colors 🟨🟩⬜ for each character in the guess)
returnFeedbackWord :: String -> String -> String
returnFeedbackWord answer guess = zipWith (returnFeedbackChar answer guess) guess [0..] -- zipWith applies returnFeedbackChar to each character in guess, along with its index

-- returnGuess returns a guess with its feedback alongside
returnGuess :: String -> String -> String
returnGuess answer guess = returnFeedbackWord answer guess ++ " : " ++ toUpper guess

-- returnGuesses returns all guesses with their feedback alongside
returnGuesses :: [String] -> String -> String
returnGuesses guesses answer = 
  let guessesString = intercalate "\n" (map (returnGuess answer) guesses) -- intercalate concatenates a list of strings with a separator, 
      remainingGuesses = intercalate "\n" (replicate (maxGuesses - length guesses) (replicate 5 '⬛')) -- replicate creates a list of the same element repeated n times
  in if null guesses then remainingGuesses else guessesString ++ "\n" ++ remainingGuesses -- if there are no guesses, only show the remaining guesses to avoid an extra newline

-- Define a new data type Result with two data constructors: Error and Good
data Result = Error String | Good ()

-- checkGuess checks if a guess is valid
checkGuess :: GameState -> String -> Result
checkGuess state guess
  | length guess /= 5 = Error "Guess must be 5 characters long."
  | not (all (`elem` (['a'..'z'] ++ ['A'..'Z'])) guess) = Error "Guess must contain only letters."
  | not (elem guess (wordlist state)) = Error "This Guess is not in the list of accepted words."
  | elem guess (guesses state) = Error "Guess must not be repeated."
  | otherwise = Good ()

-- gameLoop is the main game loop
gameLoop :: GameState -> IO ()
gameLoop state = do
  putStrLn "\n"
  putStrLn ( "=========================" )
  putStrLn ( returnGuesses (reverse (guesses state)) (answer state) ) -- reverse is used to show the most recent guesses last
  putStrLn ( "Guesses left: " ++ show (maxGuesses - numGuesses state) ++ "/" ++ show maxGuesses ) -- show converts an Int to a String
  case () of -- test if last guess is the answer
    _ | not (null (guesses state)) && head (guesses state) == answer state -> do
      putStrLn "\n"
      putStrLn "\tYOU WIN!"
      putStrLn "\n"
      putStrLn ( "The word was: " ++ toUpper (answer state) )
      putStrLn ( "=========================" )
    _ | numGuesses state >= maxGuesses -> do
      putStrLn "\n"
      putStrLn "\tYOU LOST!"
      putStrLn "\n"
      putStrLn ( "The word was: " ++ toUpper (answer state) )
      putStrLn ( "=========================" )
    _ -> do
      putStrLn "Enter a guess: "
      guess <- getLine
      putStrLn ( "=========================" )
      -- TODO : check if guess is a valid 5-letter word (ie : 5 characters, all lowercase letters, in the wordlist and not already guessed)
      case checkGuess state guess of
        Error err -> do
          putStrLn "\n"
          putStrLn "Invalid guess:"
          putStrLn err
          gameLoop state
        Good () -> do
          let state' = state { guesses = guess : guesses state, numGuesses = numGuesses state + 1 } -- { } is used to update a record
          gameLoop state'

-- main function
main :: IO ()
main = do
  print_intro -- print the introduction
  wordlist <- load_wordlist wordlistFile -- load the word list
  answerlist <- load_wordlist answerlistFile -- load the answer list
  answer <- pick_solution answerlist -- pick a random word from the answer list
  let state = initialState wordlist answer -- initialize the game state

  -- print answer for debugging
  putStrLn ( "FOR DEBUGING : Answer: " ++ toUpper answer)

  gameLoop state -- start the game loop
