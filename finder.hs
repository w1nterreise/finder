{-#LANGUAGE OverloadedStrings #-}
import Main.Utf8 (withUtf8)
import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import Data.Time.Clock
import qualified Data.List as L
import System.Directory
import System.FilePath
import Control.Monad
import Control.Exception (SomeException, catch)
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as E
import qualified Data.Text as T

data FinderOptions = FinderOptions { whereFind :: String, what :: String } deriving Generic
instance FromJSON FinderOptions

data File = File { fullName :: String, shortName :: String, dateTime :: UTCTime, content :: String }

search :: [File] -> [String] -> [(String, [File])]
search z ss = map f ss where f x = (x, filter (\y -> L.isInfixOf x (content y)) z)

concatInfo :: File -> String -> String
concatInfo x acc = acc ++ " " ++ shortName x ++ " - " ++ show (dateTime x)

printResult :: [(String, [File])] -> IO [()]
printResult = mapM sr
    where sr x = putStrLn $ fst x ++ " : " ++ (foldr concatInfo "" (snd x))

printNotFound :: [String] -> IO [()]
printNotFound = mapM (putStrLn . ("Не найдено: " ++) . show)

countUnique :: [String] -> [(String, Int)]
countUnique = map (\xs@(x:_) -> (x, length xs)) . L.group . L.sort

printUnique :: [(String, Int)] -> IO [()]
printUnique = mapM (\(f, c) -> putStrLn (f ++ " : " ++ (show c)))

loadFilesToList :: [FilePath] -> IO [String]
loadFilesToList filenames = do
    fileContents <- mapM readFileIfFile filenames
    let validContents = catMaybes fileContents
    return validContents
  where
    readFileIfFile :: FilePath -> IO (Maybe String)
    readFileIfFile filename = do
        result <- catch (Right <$> B.readFile filename) (\(e :: SomeException) -> return (Left e))
        case result of
            Left _ -> return (Just "")
            Right bytes -> case E.decodeUtf8' bytes of
                Left _ -> return (Just "")
                Right text -> return (Just (T.unpack text))

main :: IO [()]
main = withUtf8 $ do

    optionsFile <- BSL.readFile "finder.json"
    let options = decode optionsFile :: Maybe FinderOptions

    let directory = whereFind $ fromJust options
    toFind <- readFile (what $ fromJust options)
    entries <- listDirectory directory

    files <- filterM doesFileExist (map (\x -> directory ++ x) entries)
    let shortNames = map takeFileName files
    dates <- mapM getModificationTime files
    contents <- loadFilesToList files  
    let fs = L.zipWith4 File files shortNames dates contents

    let result = search fs (words toFind)
    let found = filter (\x -> (length $ snd x) > 0) result
    let unq = countUnique $ map (shortName) (foldr (\(_, xs) acc -> acc ++ xs) [] found)
    let nFound = filter f result where f (a, []) = True; f (a, _) = False

    printResult $ L.sortOn (\(_, x:xs) -> shortName x) found
    printUnique unq
    printNotFound $ map fst nFound