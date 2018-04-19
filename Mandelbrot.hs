module Mandelbrot where

import Data.Complex
import Data.Char (isDigit)

import Data.Array.Unboxed

import qualified Data.ByteString as B
import Codec.BMP
import Data.Word

type Mandel       = UArray (Int,Int) Int
data FunctionCode = PolynomialF [Complex Float]
                  | RationalF [Complex Float] [Complex Float] deriving (Eq, Show)
data SetType      = Julia | Mandelbrot deriving (Eq, Show)

isNumeric :: Char -> Bool
isNumeric c = isDigit c || c `elem` "-."

parseComplex :: String -> Complex Float
parseComplex str = r :+ i
  where
    r  = read $ takeWhile isNumeric str
    i0 = dropWhile (not . isNumeric) $ dropWhile isNumeric str
    i  = if null i0
           then 0
           else read $ takeWhile isNumeric i0

parseFunction :: [String] -> FunctionCode
parseFunction strs = case ftype of
  "polynomial" -> PolynomialF cs1
  "rational"   -> RationalF cs1 cs2
  _            -> error "function type: no parse"
  where
    (ftype:cs) = strs
    cs1 = map parseComplex $ takeWhile (/= "") cs
    cs2 = map parseComplex . drop 1 $ dropWhile (/= "") cs

parseInput :: String -> (String, SetType, Float, Int, ((Float,Float), Float), Int, FunctionCode)
parseInput str = (title, st, bnd, nmax, imgBnds, imgDivs, fc)
  where
    (title:st':nmax':bnd':xb':yb':s':divs':fc') = map (drop 2 .dropWhile (/= ':')) $ lines str
    st      = case st' of
      "mandelbrot" -> Mandelbrot
      "julia"      -> Julia
      _            -> error "set type: no parse"
    nmax    = read nmax'
    bnd     = read bnd'
    imgBnds = ((read xb', read yb'), read s')
    imgDivs = read divs'
    fc      = parseFunction fc'

poly :: [Complex Float] -> Complex Float -> Complex Float
poly as x = sum $ zipWith (\a n -> a * x ^ n) as ([0..] :: [Int])

clampedRatio :: Float -> [Complex Float] -> [Complex Float] -> Complex Float -> Complex Float
clampedRatio bnd as bs x =
  if realAbs d < 1 / bnd
    then bnd :+ 0
    else poly as x / d
  where
    d = poly bs x

realAbs :: Complex Float -> Float
realAbs = realPart . abs

mandelbrot :: (Complex Float -> Complex Float) -> Float -> Int -> Complex Float -> Int
mandelbrot f bnd nmax c = mandelbrotWorker 0 0
  where
    mandelbrotWorker x s
      | s == nmax       = 0
      | realAbs x > bnd = s
      | otherwise       = mandelbrotWorker (f x + c) (s+1)

julia :: (Complex Float -> Complex Float) -> Float -> Int -> Complex Float -> Int
julia f bnd nmax c = juliaWorker c 0
  where
    juliaWorker x s
      | s == nmax                  = 0
      | realAbs x > bnd            = s
      | realAbs (x - fx) < 1 / bnd = s
      | otherwise                  = juliaWorker fx $ s+1
      where fx = f x

mandelArray :: (Complex Float -> Complex Float)
            -> Float
            -> Int
            -> ((Float,Float), Float)
            -> Int
            -> Mandel
mandelArray f bnd nmax imgBnds imgDivs =
  array ((0,0), (imgDivs,imgDivs))
    [ ((x,y), mandelbrot f bnd nmax (cx :+ cy))
    | x <- [0..imgDivs]
    , y <- [0..imgDivs]
    , let cx = xb + fromIntegral x * d
    , let cy = yb + fromIntegral y * d
    ]
    where
      ((xb,yb), s) = imgBnds
      d = s / fromIntegral imgDivs

juliaArray :: (Complex Float -> Complex Float)
            -> Float
            -> Int
            -> ((Float,Float), Float)
            -> Int
            -> Mandel
juliaArray f bnd nmax imgBnds imgDivs =
  array ((0,0), (imgDivs,imgDivs))
    [ ((x,y), julia f bnd nmax (cx :+ cy))
    | x <- [0..imgDivs]
    , y <- [0..imgDivs]
    , let cx = xb + fromIntegral x * d
    , let cy = yb + fromIntegral y * d
    ]
    where
      ((xb,yb), s) = imgBnds
      d = s / fromIntegral imgDivs


colorize :: Int -> Int -> [Word8]
colorize smax s = [r, g, b, 255]
  where
    s' = s * div 1000 smax
    b  = fromIntegral $ min 255 s'
    g  = fromIntegral $ min 255 (max (s'-256) 0)
    r  = fromIntegral $ min 255 (max (s'-512) 0)

packArray :: Mandel -> (Int -> [Word8]) -> B.ByteString
packArray m f = B.pack colorList
  where colorList = concat [ f $ m ! (j,i) | i <- [0..x], j <- [0..y] ]
        x         = fst . snd . bounds $ m
        y         = snd . snd . bounds $ m

imgArray :: Mandel -> Int -> FilePath -> IO ()
imgArray m nmax file = do
  let rgba = packArray m (colorize nmax)
  let bmp  = packRGBA32ToBMP ((1+) . snd . snd . bounds $ m) ((1+) . fst . snd . bounds $ m) rgba
  writeBMP file bmp
  return ()

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let (title, st, bnd, nmax, imgBnds, imgDivs, fc) = parseInput raw
      f = case fc of
        (PolynomialF as)  -> poly as
        (RationalF as bs) -> clampedRatio bnd as bs
      set = case st of
        Mandelbrot -> mandelArray f bnd nmax imgBnds imgDivs
        Julia      -> juliaArray  f bnd nmax imgBnds imgDivs
      smax = maximum $ elems set
  imgArray set smax $ "img/" ++ title ++ ".bmp"
