import System.IO
import Data.List
import Data.Function
import qualified Data.Char as Char
import qualified Data.Map.Strict as Map


-- loads the text of Frankenstein und does some word count statistics
main = do
    withFile "frankenstein.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        let words = wordList contents
        let wordCounts = Map.toList $ countMap words

        putStrLn $ "Number of words: " ++ show (length words)
        putStrLn $ "Found " ++ show (length wordCounts) ++ " different words"
        putStrLn "Most frequently used words:"
        sequence $ map (putStrLn.show) (take 20 (sortBy (flip compare `on` snd) wordCounts))
        putStrLn "All used words:"
        sequence $ map (putStrLn.show) wordCounts
        )



-- all to lower, removall all but chars, digits, "-" and "'", remove "--"
preprocess :: String -> String
preprocess =
    let
        replaceNonWordCharsWithSpace :: Char -> Char
        replaceNonWordCharsWithSpace c =
            let chars = "-\'0"++['a'..'z']++['1'..'9'] -- '-' and '\'' can be inside word
            in if c `elem` chars then c else ' '

        -- remove "--"
        removeDash :: String -> String
        removeDash [] = []
        removeDash ('-':'-':xs) = ' ' : removeDash xs
        removeDash (x:xs) = x : removeDash xs
    in
    map (replaceNonWordCharsWithSpace . Char.toLower) . removeDash

-- remove non-chars at word beginning and end; ignore empty words
postProcess :: [String] -> [String]
postProcess =
    let
        needsRemoval c = c `elem` "-\'"
        strip = (dropWhile needsRemoval) . (dropWhileEnd needsRemoval)
    in
    filter (/="") . map strip

-- create list of words from string
wordList :: String -> [String]
wordList text = postProcess $ words $ preprocess text

-- turn list of words into map key=word value=count
countMap :: [String] -> Map.Map String Int
countMap =  Map.fromListWith (+) . map (\x -> (x,1))

