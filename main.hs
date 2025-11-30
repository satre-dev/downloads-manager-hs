import Control.Monad (filterM, forM)
import Data.Char (toLower)
import Data.List (isInfixOf)
import System.Directory
import System.FilePath

data UserChoice = Accept | Reject
  deriving (Show, Eq)

parseChoice :: String -> Maybe UserChoice
parseChoice s = case map toLower s of
  "y" -> Just Accept
  "yes" -> Just Accept
  "n" -> Just Reject
  "no" -> Just Reject
  _ -> Nothing

processChoice :: UserChoice -> [FilePath] -> IO ()
processChoice Accept files = deleteFilesSafe files
processChoice Reject _ = putStrLn "Cancelled"

deleteFilesSafe :: [FilePath] -> IO ()
deleteFilesSafe files = do
  putStrLn "Would delete the following files:"
  mapM_ (\f -> putStrLn $ " 🗑️ " ++ f) files
  putStrLn $ "Total: " ++ show (length files) ++ " files"

isDuplicate :: FilePath -> Bool
isDuplicate path =
  let filename = takeBaseName (takeFileName path)
   in any (`isInfixOf` filename) ["(1)", "(2)", "(3)", "(4)", "(5)", "(6)"]

findDuplicates :: FilePath -> IO [FilePath]
findDuplicates dir = do
  contents <- listDirectory dir
  let files = map (dir </>) contents
  actualFiles <- filterM doesFileExist files
  return $ filter isDuplicate actualFiles

main :: IO ()
main = do
  home <- getHomeDirectory
  let downloadsDir = home </> "Downloads"

  putStrLn $ "Scanning " ++ downloadsDir ++ "..."
  duplicates <- findDuplicates downloadsDir

  if null duplicates
    then putStrLn "No duplicates found!"
    else do
      putStrLn $ "Found " ++ show (length duplicates) ++ " duplicates:"
      mapM_ putStrLn duplicates

      putStrLn "\nDelete these files? [y/n]"
      input <- getLine

      case parseChoice input of
        Just choice -> processChoice choice duplicates
        Nothing -> do
          putStrLn "Invalid input, please enter y/n"
          main
