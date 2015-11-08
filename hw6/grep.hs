{-# LANGUAGE FlexibleContexts #-}
import System.Environment
import System.Directory
import Data.List
import Data.IORef
import Data.Array.IO
import Control.Monad

grep :: Eq a => [a] -> [[a]] -> [[a]]
grep substr text = filter (isInfixOf substr) text

appCycle :: IOArray Int String -> IOArray Int String -> IO ()
appCycle arr newArr = do
  putStrLn "Edit(E Number), Write to file(W OutputFileName), Quit(Q)"
  line <- getLine
  proceed line arr newArr

proceed :: String -> IOArray Int String -> IOArray Int String -> IO ()
proceed [x] arr newArr
  | x == 'Q' = return ()
proceed (x:xs) arr newArr
  | x == 'E' = edit (read xs :: Int) newArr >> appCycle arr newArr
  | x == 'W' = write (tail xs) arr newArr >> appCycle arr newArr
proceed x arr newArr = putStrLn "Unknown command" >> appCycle arr newArr

edit :: Int -> IOArray Int String -> IO ()
edit index newArr = do
  writeArray newArr index =<< getLine

msgNewFile :: FilePath -> String
msgNewFile file = "File " ++ file ++ " created, all changes saved"

msgExistingFile :: String
msgExistingFile = "Changes appended to existing file"

write :: FilePath -> IOArray Int String -> IOArray Int String -> IO ()
write file arr newArr = do
    exists <- doesFileExist file
    if (exists) then
      putStrLn (msgExistingFile)
    else            
      putStrLn (msgNewFile file) >> writeFile file ""
    bounds <- getBounds arr    
    forM_ [1..snd bounds] $ \i -> do
      s1 <- readArray newArr i
      s2 <- readArray arr i
      if compare s1 s2 /= EQ then
        appendFile file (s1 ++ "\n")
      else
        return ()

main = do
  args <- getArgs
  file <- readFile (args !! 1)
  let result = grep (args !! 0) $ lines file
  let len = length result
  arr <- newListArray (1 , len)  result :: IO (IOArray Int String)  
  newArr <- mapArray id arr
  forM_ [1..len] $ \i -> do
    s <- readArray arr i
    putStrLn $ show i ++ ") " ++ s
  appCycle arr newArr  
