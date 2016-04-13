import System.IO
import qualified Data.List as List
import qualified Data.Char as Char

---------------------------------------------------------------------
-- clean the input
---------------------------------------------------------------------


{-
Given a string:
* removes all characters that are not letters or digits, except for ...
    * preserves single-quotes within words and after plural-s (e.g. "susy's", "o'connor", "horses'")
    * preserves dashes within words (e.g. "twenty-four")
* turns sentence-terminating punctuation into sentence termination symbols surrounded by whitespace: '|'
* turns paragraph-separating multi-newlines into paragraph termination symbols surrounded by whitespace: '@'
* turns intra-sentence punctuation into free-standing punctuation characters: -,;:
-}
cleanContents :: String -> String
cleanContents = reverse . List.foldl accumulate ""
    where
        mustEliminate = (flip elem) ",:;\"[]()"
        isSentenceTerminator = (flip elem) ".!?"

        -- "--he ho--twenty-four 'bullshit' - ok! But this \nis a \"test\". \n\nNo? \n\n\n\nIt is, o'connor's boy's horses' foot.--"
        accumulate :: String -> Char -> String          -- the accumulator: (result, pending)
        accumulate result upperC =
            let c = Char.toLower upperC
            in
                if mustEliminate c then
                    ' ' : result                            -- undesirable converts to space
                else
                    if isSentenceTerminator c then          -- sentence terminator converts to " | "
                        case result of
                            (' ' : xs) -> " | " ++ xs
                            _ -> " | " ++ result
                    else
                        case (c, result) of
                            (' ',  '-'  : ' ' : xxs) -> ' ' : xxs     -- suppress " - "
                            ('-',  '-'  : xs) -> ' ' : xs     -- suppress "--"
                            (' ',  ' '  : xs) -> result       -- suppress duplicate spaces
                            -- (' ',  's' : '\'' : xxs) -> c : result      -- preserve single-quote in genetive, e.g. "x's"
                            ('\r', '\n' : xs) -> '\n' : result         -- lifefeed & newline => newline
                            ('\n', '\r' : '\n' : xs) -> case xs of   -- newline after linefeed & newline
                                (' ' : '@' : xxs) -> xxs        -- suppress duplicate paragraph separators
                                (' ' : '|' : xxs) -> " @" ++ xxs -- suppress sentence separators before paragraph separator
                                (' ' : xxs) -> " @ " ++ xxs     -- avoid double space before paragraph separators
                                _ -> " @ " ++ xs                -- emit paragraph separator
                            ('\n', '\n' : xs) -> case xs of   -- newline after newline
                                (' ' : '@' : xxs) -> xxs        -- suppress duplicate paragraph separators
                                (' ' : '|' : xxs) -> " @" ++ xxs -- suppress sentence separators before paragraph separator
                                (' ' : xxs) -> " @ " ++ xxs     -- avoid double space before paragraph separators
                                _ -> " @ " ++ xs                -- emit paragraph separator
                            ('\'',  []) -> result               -- suppress leading single quote at start of text
                            ('\'',  '\n' : xs) -> result         -- suppress leading single quote after newline
                            ('\'',  ' ' : xs) -> result         -- suppress leading single quotes after space
                            (' ',  '\'' : xs) -> case xs of     -- suppress trailing single quotes
                                ('s' : xxs) -> c : result           -- preserve single-quote in genetive, e.g. "xs'"
                                _ -> c : xs                         -- suppress leading single quotes
                            (_,    '\n' : xs) -> c : ' ' : xs -- anything after newline => convert newline to space
                            _ -> c : result




---------------------------------------------------------------------
-- sentences
---------------------------------------------------------------------

-- given cleaned contents, build sentence list and print info about it
printSentenceInfo :: String -> IO ()
printSentenceInfo contents = do
    let
        cleanedContents = cleanContents contents
        sentences = splitIntoSentences cleanedContents
    sequence $ map putStrLn $ map renderSentence $ sentences
    putStrLn $ "-----------------------------------------------"
    putStrLn $ "sentence count: " ++ (show $ length sentences)

type MyWord = String
type Sentence = [MyWord]

-- print out sentence nicely
renderSentence :: Sentence -> String
renderSentence s = unwords s  -- ++ "."


-- given cleaned text, returns list of sentences
splitIntoSentences :: String -> [Sentence]
splitIntoSentences = filter (not . null) . reverse . fst . (foldl accumulate ([], [])) . words
    where
        accumulate :: ([Sentence], [MyWord]) -> MyWord -> ([Sentence], [MyWord])
        accumulate (sentences, wordBuffer) word =
            case word of
                "|" -> ((reverse wordBuffer) : sentences, [])       -- sentence terminator => flush word buffer into sentence store
                "@" -> ((reverse wordBuffer) : sentences, [])       -- paragraph terminator => flush word buffer into sentence store
                _ -> (sentences, word : wordBuffer)                 -- push word into word buffer


main =
    withFile "frankenstein.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        let sentences = splitIntoSentences $ cleanContents contents
        mapM (putStrLn.renderSentence) sentences
        )
