module EvilHangman where

import qualified Data.Map as Map
import Data.Tuple.HT
import Data.List
import Data.Char

(f `on` g) x y = f (g x) (g y)

displayFrom :: String -> String -> String
displayFrom guessed word = map df word
  where df ch = if elem ch guessed then ch else '-'
        
wordClasses :: String -> [String] -> Map.Map String [String]
wordClasses guessed words = foldr (\w -> Map.insertWith (++) (displayFrom guessed w) [w]) Map.empty words

maxWordClass guessed words = maximumBy (compare `on` thd3) $ map (\(c,l) -> (c,l,length l)) $ Map.toList $ wordClasses guessed words

turn _ words 0 = do
  putStrLn "You lose, sucker!"
  putStrLn $ "The word was " ++ (head words)
turn guessed words left = do
  putStrLn "" 
  putStrLn $ "You have " ++ (show left) ++ " turns left" 
  putStrLn "Enter a character, or \"guessed\" to see what letters have already been guessed"
--  putStrLn $ "There are " ++ (show $ length words) ++ " English words consistent with the game state"
  nextGuess <- fmap (map toLower) getLine
  case nextGuess of
    [x]  
      | x `notElem` guessed -> do
        let (code,newwords,len) = maxWordClass (x:guessed) words
        putStrLn $ "The word now looks like " ++ code
        if '-' `notElem` code 
          then putStrLn "You Win!"
          else do
          putStrLn $ if x`elem`code then "You found one!" else "Try again!"
          turn (x:guessed) newwords (if x`elem`code then  left else left-1)
    "guessed" -> do
      putStrLn $ "You have gussed the characters " ++ (intersperse ',' guessed)
      turn guessed words left
    _ -> do
      putStrLn "Please enter just one character that has not been guessed before"
      turn guessed words left


playGame word_len turns = do
  words <- fmap ((map (map toLower)).(filter (all isLetter)).(filter ((==word_len).length)).lines) $ readFile "/usr/share/dict/words"
  turn "" words turns
    
           

