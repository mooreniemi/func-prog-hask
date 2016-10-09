import Data.Random
import Data.Random.Extras (choice)

getwords :: FilePath -> IO [String]
getwords path = do contents <- readFile path
                   return (lines contents)

check :: String -> String -> Char -> (Bool,String)
check word display c
  = (c `elem` word, [if x==c then c else y | (x,y) <-zip word display])

turn :: String -> String -> Int -> IO ()
turn word display n =
  do if n==0
       then putStrLn ("You ran out of guesses. The word was: " ++ word)
       else if word==display
              then putStrLn ("You win! The word was: " ++ word)
              else mkguess word display n

mkguess :: String -> String -> Int -> IO()
mkguess word display n =
  do putStrLn (display ++ " " ++ take n (repeat '*'))
     putStr " Enter your guess: "
     q <- getLine
     let (correct, display') = check word display (q!!0)
     let n' = if correct then n else n-1
     turn word display' n'

starman :: Int -> IO ()
starman n =
  do words <- getwords "/usr/share/dict/words"
     word <- sample $ choice words
     turn word ['-' | x <- word] n
