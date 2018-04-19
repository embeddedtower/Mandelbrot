module Julia where

import JuliaIO
import Data.Complex
import Data.Char (isDigit)

import Data.Array.Unboxed

import Control.Monad (forever)
import System.IO (stdout, hFlush)

realAbs :: RealFloat a => Complex a -> a
realAbs = realPart . abs

evaluateAt :: Num a => [a] -> a -> a
evaluateAt as z = sum $ zipWith (\a n -> a * z^n) as [0..]

evalRationalF :: Fractional a => RationalF a -> a -> a
evalRationalF (RationalF n d) z = evaluateAt n z / evaluateAt d z

addPoly :: Num a => [a] -> [a] -> [a]
addPoly as []         = as
addPoly [] bs         = bs
addPoly (a:as) (b:bs) = (a+b) : addPoly as bs

subPoly :: Num a => [a] -> [a] -> [a]
subPoly as []         = as
subPoly [] bs         = map negate bs
subPoly (a:as) (b:bs) = (a-b) : subPoly as bs

multiplyPoly :: Num a => [a] -> [a] -> [a]
multiplyPoly _ []      = []
multiplyPoly [] _      = []
multiplyPoly [a] bs    = map (a*) bs
multiplyPoly (a:as) bs = addPoly (map (a*) bs) (0 : multiplyPoly as bs)

differentiate :: Num a => [a] -> [a]
differentiate []     = []
differentiate (a:as) = zipWith (*) as $ map fromIntegral [1..]

diffRationalF :: Num a => RationalF a -> RationalF a
diffRationalF (RationalF n d) = RationalF dn dd
  where
    n' = differentiate n
    d' = differentiate d
    dn = subPoly (multiplyPoly n' d) (multiplyPoly n d')
    dd = multiplyPoly d d

juliaIterator :: Fractional a => RationalF a -> RationalF a -> a -> [(Int,a,a)]
juliaIterator f f' z =
  iterate (\(n,zk, zk') -> (n+1, evalRationalF f zk, evalRationalF f' zk * zk')) (0,z, 1)

divergentDelta :: (Int, Complex Double, Complex Double) -> Double
divergentDelta (_, zk, zk')
  | rzk  == 0 = 0
  | rzk' == 0 = 0
  | otherwise = rzk * log rzk / rzk'
  where
    rzk  = realAbs zk
    rzk' = realAbs zk'

convergentDeltaSA :: Complex Double -> (Int, Complex Double, Complex Double) -> Double
convergentDeltaSA z0 (_, zk, zk')
  | rzkc == 0 = 0
  | rzk' == 0 = 0
  | otherwise = rzkc * log rzkc / rzk'
  where
    rzkc = realAbs $ zk - z0
    rzk' = realAbs zk'

convergentDelta :: Complex Double -> (Int, Complex Double, Complex Double) -> Double
convergentDelta z0 (_, zk, zk')
  | rzk' == 0 = 0
  | otherwise = rzkc / rzk'
  where
    rzkc = realAbs $ zk - z0
    rzk' = realAbs zk'


estimateJuliaLimit :: Int
                   -> Double
                   -> ((Int, Complex Double, Complex Double) -> Double)
                   -> [(Int, Complex Double, Complex Double)]
                   -> Double
estimateJuliaLimit nmax e delta ((n,zk,zk'):(n1,zk1,zk1'):(n2,zk2,zk2'):zks)
  | n >= nmax      = 0
  | abs (d-d1) < e = d1
  | abs (d-d2) < e = d2
  | otherwise      = estimateJuliaLimit nmax e delta $ (n1,zk1,zk1'):(n2,zk2,zk2'):zks
  where
    d   = delta (n,zk,zk')
    d1  = delta (n,zk1,zk1')
    d2  = delta (n,zk2,zk2')

estimateCycleElement :: Int
                     -> Double
                     -> [(Int, Complex Double, Complex Double)]
                     -> Complex Double
estimateCycleElement n e zks
  | dzk < e   = zk
  | otherwise = estimateCycleElement n e $ tail zks
  where
    (_,zk,_)  = head zks
    (_,zkn,_) = head $ drop n zks
    dzk       = realAbs (zk - zkn)

estimateColor :: Int
              -> Double
              -> ((Int, Complex Double, Complex Double) -> Double)
              -> RationalF (Complex Double)
              -> RationalF (Complex Double)
              -> Complex Double
              -> Int
estimateColor nmax e delta f f' z
  | d < e     = floor $ 1/e
  | otherwise = floor $ 1/d
  where
    d = estimateJuliaLimit nmax e delta $ juliaIterator f f' z

myF1, myF1', myF2, myF2' :: RationalF (Complex Double)
myF1 = RationalF [0 :+ 0, 0 :+ 0, 1 :+ 0] [1 :+ 0]
myF1' = diffRationalF myF1
myF2 = RationalF [(-0.55) :+ 0.16, 0 :+ 0, 0 :+ 0, 1 :+ 0] [1 :+ 0]
myF2' = diffRationalF myF2

main :: IO ()
main = do
  let s      = 2000
      nmax   = 1000
      e      = 0.001
      scale  = (1500 /) . negate $ log e
      cycleN = 3

  forever $ do
    putStr "Enter a complex paramter: "
    hFlush stdout
    c <- parseComplex <$> getLine

    putStr "Enter a side length: "
    hFlush stdout
    side <- read <$> getLine

    let
        num   = addPoly [1 :+ 0] $ multiplyPoly [0 :+ 0, 1 :+ 0, 0 :+ 0, 1 :+ 0] [c]
        denom = [0 :+ 0, 1 :+ 0, 0 :+ 0, 1 :+ 0]
        f   = RationalF num denom
        f'  = diffRationalF f
        z0  = estimateCycleElement cycleN (e/100) $ juliaIterator f f' (1 :+ 0)
    putStrLn $ "Found " ++ show cycleN ++ "-cycle containing " ++ show z0

    let dInverses = mkIntList ((-side,-side), 2*side) s $ estimateColor nmax e (convergentDelta z0) f f'
    imgList s scale "img/julia03.bmp" dInverses
