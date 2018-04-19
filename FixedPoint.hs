module FixedPoint where

import Data.Complex

data PlusInfinity a = Infinity | Finite a          deriving (Eq, Show)
data Attraction   a = SuperAttractor | Attractor a deriving (Eq, Show)
data Behavior       = Convergent | Divergent       deriving (Eq, Show)


realAbs :: RealFloat a => Complex a -> a
realAbs = realPart . abs

findAttractiveCycle :: (Complex Double -> Complex Double)
                    -> Int
                    -> Double
                    -> Int
                    -> Complex Double
                    -> Maybe (Behavior, Complex Double)
findAttractiveCycle f n e nmax z = findCycleWorker nmax zs
  where
    zs = iterate f z
    findCycleWorker m ws
      | m <= 0    = Nothing
      | d < e     = Just (Convergent, w0)
      | rw > 1/e  = Just (Divergent,  w0)
      | otherwise = findCycleWorker (m-1) (tail ws)
      where
        w0 = head ws
        rw = realAbs w0
        d  = realAbs (w0 - head (drop n ws))

diffIterator :: (Complex Double -> Complex Double)
             -> (Complex Double -> Complex Double)
             -> Int
             -> Complex Double
             -> Complex Double
diffIterator f f' n z =
  head . drop n . map snd $ iterate (\(zk, zk') -> (f zk, zk' * f' zk)) (z, 1)

alphaEstimate :: (Complex Double -> Complex Double)
              -> (Complex Double -> Complex Double)
              -> Int
              -> Behavior
              -> Complex Double
              -> Double
alphaEstimate f f' n b z = case b of
  Convergent -> 1 / realAbs (diffIterator f f' n z)
  Divergent  -> realAbs (diffIterator f f' n z)

findAlpha :: (Complex Double -> Complex Double)
          -> (Complex Double -> Complex Double)
          -> Int
          -> Double
          -> Int
          -> Complex Double
          -> Maybe (Behavior, Complex Double, Double)
findAlpha f f' n e nmax z =
  case findAttractiveCycle f n e nmax z of
    Nothing               -> Nothing
    Just (Convergent, z0) -> Just (Convergent, z0, alphaEstimate f f' n Convergent z0)
    Just (Divergent, z0)  -> Just (Divergent,  z0, alphaEstimate f f' n Divergent  z0)

refineAlpha :: (Complex Double -> Complex Double)
            -> (Complex Double -> Complex Double)
            -> Int
            -> Double
            -> Double
            -> Int
            -> Int
            -> (Behavior, Complex Double, Double)
            -> Maybe (PlusInfinity (Complex Double), Attraction Double)
refineAlpha f f' n ez ea nmax stages (b, z0, a0)
  | stages == 0 = case b of
    Divergent  -> Just (Infinity,  SuperAttractor)
    Convergent -> Just (Finite z0, SuperAttractor)
  | otherwise = case findAlpha f f' n (ez/10) nmax (f z0) of
    Nothing -> Nothing
    Just (Convergent, z1, a1)
      | abs (a1 - a0) < ea -> Just (Finite z1, Attractor a1)
      | otherwise          -> refineAlpha f f' n (ez/10) ea nmax (stages-1) (Convergent, z1, a1)
    Just (Divergent, z1, a1)
      | abs (a1 - a0) < ea -> Just (Infinity, Attractor a1)
      | a1 > 1 / ea        -> Just (Infinity, SuperAttractor)
      | otherwise          -> refineAlpha f f' n (ez/10) ea nmax (stages-1) (Divergent, z1, a1)
