import Data.List
import Control.Monad
import Control.Monad.Random

-- from http://stackoverflow.com/questions/9266765/how-to-use-monadrandom/
test = do
    g <- getStdGen
    let r = evalRand (afunc 1 2 3) g :: Int
    -- or use runRand if you want to do more random stuff:
    -- let (r,g') = runRand (myFunc 1 2 3) g :: (Double,StdGen)
    print r

afunc x y z = bfunc x y
bfunc x y = cfunc x
cfunc x = do
    ret <- getRandomR (0,10)
    return (ret * x)

---------------------------------------------------------------------


test2 = do
    g <- getStdGen
    print "Testing RandomMonad"
    let r = evalRand (afunc2 1 2 3) g
    print r
    -- or use runRand if you want to do more random stuff:
    -- let (r,g') = runRand (myFunc 1 2 3) g :: (Double,StdGen)

    iotest

    s <- af 1 2 3
    let s' = evalRand s g
    print s'

af :: Int -> Int -> Int -> IO (Rand StdGen String)
af x y z = do
    print "Testing IO monad combined with RandomMonad"
    return $ do
        i <- bfunc2 x y
        return $ show i

iotest :: IO ()
iotest = print "Testing IO monad"

afunc2 :: Int -> Int -> Int -> Rand StdGen String
afunc2 x y z = do
    results <- mapM (\_ -> bfunc2 x y) [1..z]
    return $  concat $  intersperse ";" $ map show results

bfunc2 :: Int -> Int -> Rand StdGen Int
bfunc2 x y = do
    j <- cfunc2 x
    return $ y*j

cfunc2 :: Int -> Rand StdGen Int
cfunc2 x = do
    ret <- getRandomR (0,10)
    return (ret * x)


