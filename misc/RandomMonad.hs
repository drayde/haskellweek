import Data.List
import Control.Monad.Random

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
    let r = evalRand (afunc2 1 2 3) g
    -- or use runRand if you want to do more random stuff:
    -- let (r,g') = runRand (myFunc 1 2 3) g :: (Double,StdGen)
    s <- af 1 2 3
    let s' = evalRand s g
    print r
    print s'

af :: (RandomGen g) => Int -> Int -> Int -> IO (Rand g String)
af x y z = do
    print "jkjhkH"
    return $ do
        i <- bfunc2 x y
        return $ concat $  intersperse "," $ replicate z (show i)

iotest :: IO ()
iotest = print "hjg"

afunc2 :: (RandomGen g) => Int -> Int -> Int -> Rand g String
afunc2 x y z = do
    i <- bfunc2 x y
    return $ concat $  intersperse "," $ replicate z (show i)

bfunc2 :: (RandomGen g) => Int -> Int -> Rand g Int
bfunc2 x y = do
    j <- cfunc2 x
    return $ y*j

cfunc2 :: (RandomGen g) => Int -> Rand g Int
cfunc2 x = do
    ret <- getRandomR (0,10)
    return (ret * x)


