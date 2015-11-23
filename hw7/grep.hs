{-# LANGUAGE FlexibleContexts #-}
import System.Environment
import System.Directory
import Data.List
import Data.IORef
import Data.Array.IO
import Control.Monad

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

data Environment = Environment
  { getFilename :: FilePath
  , getSubst :: String
  }
  deriving (Show)

grep :: String -> [String] -> [String]
grep substr text = filter (isInfixOf substr) text

appCycle :: IOArray Int String -> IOArray Int String -> WriterT [String] (StateT (Integer, Integer) IO) ()
appCycle arr newArr = do
  liftIO $ putStrLn "Edit(E Number), Write to file(W OutputFileName), Quit(Q)"
  line <- liftIO $ getLine
  proceed line arr newArr

proceed :: String -> IOArray Int String -> IOArray Int String -> WriterT [String] (StateT (Integer, Integer) IO) ()
proceed [x] arr newArr
  | x == 'Q' = return ()
proceed (x:xs) arr newArr
  | x == 'E' = do
      edit (read xs :: Int) newArr
      appCycle arr newArr
  | x == 'W' = do    
    write (tail xs) arr newArr
    appCycle arr newArr
proceed x arr newArr = do
  liftIO $ putStrLn "Unknown command"
  appCycle arr newArr

edit :: Int -> IOArray Int String -> WriterT [String] (StateT (EditCounter, WriteCounter) IO) ()
edit index newArr = do  
  line <- liftIO $ getLine
  (count, x) <- get
  put $ (count + 1, x)
  tell (["String at index " ++ show index ++ " now is " ++ line])
  (lBound, rBound) <- liftIO $ getBounds newArr
  if ((index >= lBound) && (index <= rBound)) then
    liftIO $ writeArray newArr index line
  else
    do
      tell ["Invalid index"]
      return ()


msgNewFile :: FilePath -> String
msgNewFile file = "File " ++ file ++ " created, all changes saved"

msgExistingFile :: String
msgExistingFile = "Changes appended to existing file"

write :: FilePath -> IOArray Int String -> IOArray Int String -> WriterT [String] (StateT (EditCounter, WriteCounter) IO) ()
write file arr newArr = do
  (x, count) <- get
  put $ (x, count + 1)
  exists <- liftIO $ doesFileExist file
  if (exists) then
    do
      tell (["Append to file " ++ file])
      liftIO $ putStrLn (msgExistingFile)
  else            
    do
      tell (["Write to existed file " ++ file])      
      liftIO $ putStrLn (msgNewFile file) >> writeFile file ""
  bounds <- liftIO $ getBounds arr    
  forM_ [1..snd bounds] $ \i -> do
    s1 <- liftIO $ readArray newArr i
    s2 <- liftIO $ readArray arr i
    if compare s1 s2 /= EQ then
      do
        tell ["String " ++ show i ++ " was changed."]
        liftIO $ appendFile file (s1 ++ "\n")
    else
      return ()
  tell ["Written or appended"]

type EditCounter = Integer
type WriteCounter = Integer
--run :: FilePath -> String -> IO (((), String), (EditCounter, WriteCounter))
run filename subst = runReaderT runGrep (Environment filename subst)


runGrep :: ReaderT Environment IO [String]
runGrep = do
  env <- ask
  let filename = getFilename env
      subst = getSubst env
  file <- liftIO $ readFile filename
  return $ grep subst $ lines file


main = do
  args <- getArgs
  let subst = args !! 0
      file = args !! 1
  mainWithoutArgs subst file

mainWithoutArgs subst file = do  
  res <- run file subst
  let len = length res
  arr <- newListArray (1, len) res :: IO (IOArray Int String)
  newArr <- mapArray id arr
  tmp <- getElems arr
  putStrLn $ intercalate "\n" $ zipWith (\a b -> show a ++ ") " ++ b) [1..] (tmp) 
  (log, (editCount, writeCount)) <- runStateT (execWriterT $ appCycle arr newArr) (0, 0)
  putStrLn $ "\nLog"
  putStrLn $ intercalate "\n" log
  putStrLn $ "End of log\n"
  putStrLn $ "Stats"
  putStrLn $ "Edit operations: " ++ show editCount
  putStrLn $ "Write operations: " ++ show writeCount


{-  let result = grep subst $ lines file
  let len = length result
  arr <- 
  newArr <- mapArray id arr
  forM_ [1..len] $ \i -> do
    s <- readArray arr i
    putStrLn $ show i ++ ") " ++ s
  appCycle arr newArr
-}