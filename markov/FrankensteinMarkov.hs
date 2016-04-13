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
splitIntoSentences all =  map toWords (filter (not.null) (lines all) )
    where
        toWords :: String -> Sentence
        toWords line = filter (not.null) $ words line


-- filter out weird sentences
filterSentences :: [Sentence] -> [Sentence]
filterSentences = filter (\x -> 2 < length x)


buildMarkovModel :: [Sentence] -> MarkovMap
buildMarkovModel sentences = Map.fromListWith (++) transformed
    where
        transformed = map (\((a,b,c),n) -> ((a,b),[(c,n)])) counted
        counted = Map.toList $ Map.fromListWith (+) tuples
        tuples = map (\x -> (x,1)) (extract1 sentences)

        extract1 :: [Sentence] -> [(MyWord, MyWord, MyWord)]
        extract1 [] = []
        extract1 (x:xs) = extract2 ("@@":x) ++ extract1 xs

        extract2 :: [MyWord] -> [(MyWord, MyWord, MyWord)]
        extract2 [] = []
        extract2 [x] = []
        extract2 ("@@":x:xs) = ("", "", x) : extract2 ("@":x:xs)
        extract2 ("@":x:y:xs) = ("", x, y) : extract2 (x:y:xs)
        extract2 (x:y:z:xs) = (x, y, z) : extract2 (y:z:xs)
        extract2 (x:y:xs) = [(x, y, "")]


makeSentence :: MarkovMap -> Random.StdGen -> Sentence
makeSentence model rand = reverse $ make "" "" model rand []
    where
        make :: MyWord -> MyWord -> MarkovMap -> Random.StdGen -> Sentence -> Sentence
        make (x:xs) "" model rand out = out
        make a b model rand out =
            let (next,rand') = findNextWord (Map.lookup (a,b) model) rand in
                make b next model rand' (if null a then out else a:out)

        findNextWord :: Maybe [(MyWord, Int)] -> Random.StdGen -> (MyWord, Random.StdGen)
        findNextWord Nothing rand = ("", rand)
        findNextWord (Just []) rand = ("", rand)
        findNextWord (Just list) rand = chooseWord list rand

        chooseWord :: [(MyWord, Int)] -> Random.StdGen -> (MyWord, Random.StdGen)
        chooseWord lst rand =
            let
                sumCount = foldr (\x acc->acc + snd x) 0 lst
                (r,rand') = Random.randomR (0, sumCount*2) rand
            in
                --trace (show rand)
                (chooser r lst, rand')

        chooser :: Int -> [(MyWord, Int)] -> MyWord
        chooser _ [] = ""
        chooser _ [x] = fst x
        chooser n ((w,c):xs) = if n<c then w else chooser (n-c) (xs++[(w,c)])

main =
    withFile "franken_sentences.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        let sentences = filterSentences $ splitIntoSentences contents
        let model = buildMarkovModel sentences
        -- mapM (putStrLn.renderSentence) sentences
        -- sequence_ $ Map.mapWithKey (\k x -> print ( show k ++ " -> " ++ show x)) model

        mapM (\x -> (putStrLn.renderSentence) (makeSentence model (Random.mkStdGen x))) [1..10]
        )
