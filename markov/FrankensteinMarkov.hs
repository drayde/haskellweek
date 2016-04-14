import System.IO
import Debug.Trace
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.Char as Char
import qualified System.Random as Random


type MyWord = String
type Sentence = [MyWord]
type MarkovMap = Map.Map (MyWord, MyWord) [(MyWord, Int)]

-- print out sentence nicely
renderSentence :: Sentence -> String
renderSentence s = unwords s  ++ "."


-- given cleaned text, returns list of sentences
splitIntoSentences :: String -> [Sentence]
splitIntoSentences =  map (filter (not.null) . words) . filter (not.null) . lines


-- filter out weird sentences
filterSentences :: [Sentence] -> [Sentence]
filterSentences = filter ((>2) . length)

-- build a markov chain model from a list of sentences
buildMarkovModel :: [Sentence] -> MarkovMap
buildMarkovModel sentences = fixStartValues $ Map.fromListWith (++) transformed
    where
        transformed = map (\((a,b,c),n) -> ((a,b),[(c,n)])) counted
        counted = Map.toList $ Map.fromListWith (+) tuples
        tuples = map (\x -> (x,1)) . concatMap extract $ sentences

        extract :: [MyWord] -> [(MyWord, MyWord, MyWord)]
        extract l = zip3 ("":"":l) ("":l) ll where ll = l++[""]

        -- fine-tune probabilities for first word
        fixStartValues :: MarkovMap -> MarkovMap
        fixStartValues = Map.adjust (map (\(w,c)->(w,min 100 c))) ("","")

-- build a sentence from the markov model
makeSentence :: MarkovMap -> Random.StdGen -> Sentence
makeSentence model rand = reverse $ make "" "" model rand []
    where
        make :: MyWord -> MyWord -> MarkovMap -> Random.StdGen -> Sentence -> Sentence
        make a b model rand out =
            let
                (next,rand') = case Map.lookup (a,b) model of
                    Nothing     -> ("", rand)
                    (Just list) -> chooseWord list rand
            in
                if null next then b:a:out -- end of sentence, add words in pipeline
                             else make b next model rand' (if null a then out else a:out)

        -- from the list, pick one
        chooseWord :: [(MyWord, Int)] -> Random.StdGen -> (MyWord, Random.StdGen)
        chooseWord lst rand =
            let
                sumCount = foldr (\(_,c) acc->acc + c) 0 lst
                (r,rand') = Random.randomR (0, sumCount-1) rand
            in
                --trace (show rand)
                (chooser r lst, rand')

        chooser :: Int -> [(MyWord, Int)] -> MyWord
        chooser n ((w,c):xs) = if n<c then w else chooser (n-c) xs

main =
    withFile "franken_sentences.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        let sentences = filterSentences $ splitIntoSentences contents
        let model = buildMarkovModel sentences

        -- mapM (putStrLn.renderSentence) sentences

        let format2nd (w,c) = w++ "," ++ show c ++ "  "
        -- mapM_ (\((k1,k2),x) -> putStrLn $  k1 ++ "," ++ k2 ++ " -> " ++ concatMap format2nd x) $ Map.toList model

        mapM (\x -> (putStrLn.renderSentence) (makeSentence model (Random.mkStdGen x))) [1..10]
        )
