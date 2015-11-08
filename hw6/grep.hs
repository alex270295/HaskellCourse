{-# LANGUAGE FlexibleContexts #-}
import System.Environment
import Data.List
import Data.IORef
import Data.Array.IO
import Control.Monad

run substr text = filter (isInfixOf substr) text

createArray list arr = forM_ [1..(length list)] $ \i -> 
                        writeArray arr i (list !! (i-1))

appCycle arr newArr len = do
  putStrLn "Edit(E Number), Write to file(W OutputFileName), Quit(Q)"
  line <- getLine
  proceed line arr newArr len

proceed (x:xs) arr newArr len
  | x == 'E' = edit (read xs :: Int) newArr >> appCycle arr newArr len
  | x == 'W' = write (tail xs) arr newArr len >> appCycle arr newArr len
proceed [x] arr newArr len
  | x == 'Q' = return ()
  | otherwise = putStrLn "Unknown command" >> appCycle arr newArr len


edit index newArr = do
  writeArray newArr index =<< getLine

write file arr newArr len = do
    forM_ [1..len] $ \i -> do
      s1 <- readArray newArr i
      s2 <- readArray arr i
      if compare s1 s2 /= EQ then
        appendFile file (s1 ++ "\n")
      else
        return ()

main = do
  args <- getArgs
  file <- readFile (args !! 0)
  let result = run (args !! 1) $ lines file
  let len = (length result)
  arr <- newArray (1 , len)  "" :: IO (IOArray Int String)
  createArray result arr
  forM_ [1..len] $ \i -> do
    s <- readArray arr i
    putStrLn $ show i ++ ") " ++ s
  newArr <- newArray (1 , len)  "" :: IO (IOArray Int String)
  forM_ [1..len] $ \i -> do
    readArray arr i >>= writeArray newArr i

  
  appCycle arr newArr len

  
