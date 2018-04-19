module JuliaIO where

import Data.Complex
import Data.Char (isDigit)

import Data.Array.Unboxed

import qualified Data.ByteString as B
import Codec.BMP
import Data.Word

data RationalF a = RationalF [a] [a] deriving (Eq, Show)

isNumeric :: Char -> Bool
isNumeric c = isDigit c || c `elem` "-."

parseComplex :: String -> Complex Double
parseComplex str = r :+ i
  where
    r  = read $ takeWhile isNumeric str
    i0 = dropWhile (not . isNumeric) $ dropWhile isNumeric str
    i  = if null i0
           then 0
           else read $ takeWhile isNumeric i0

parseFunction :: [String] -> RationalF (Complex Double)
parseFunction strs = RationalF cs1 cs2
  where
    cs1 = map parseComplex $ takeWhile (/= "") strs
    cs2 = map parseComplex . drop 1 $ dropWhile (/= "") strs

interpolate :: (Int,Int) -> Int -> [Int] -> [Int] -> [Word8]
interpolate (nmin, nmax) n starts ends =
  zipWith (\start end -> fromIntegral $ start + div ((n - nmin) * (end - start)) (nmax - nmin)) starts ends

colorize :: Double -> Int -> [Word8]
colorize scale s
  | s' < 256  = interpolate (0,255) s' color0 color1
  | s' < 512  = interpolate (256,511) s' color1 color2
  | s' < 768  = interpolate (512,767) s' color2 color3
  | otherwise = map fromIntegral [0,0,0,255]
  where
    s' = if s > 0
         then floor $ scale * log (fromIntegral s)
         else 0
    color0 = [0,0,128,255]
    color1 = [0,0,255,255]
    color2 = [0,255,200,255]
    color3 = [255,255,255,255]

mkIntList :: ((Double, Double), Double)
          -> Int
          -> (Complex Double -> Int)
          -> [Int]
mkIntList ((xb,yb), l) steps f =
  [ f z
  | y <- [0..steps]
  , x <- [0..steps]
  , let z = (xb + d * fromIntegral x) :+ (yb + d * fromIntegral y)
  ]
  where
    d = l / fromIntegral steps

packList :: [Int] -> (Int -> [Word8]) -> B.ByteString
packList m f = B.pack $ concatMap f m

imgList :: Int -> Double -> FilePath -> [Int] -> IO ()
imgList steps scale file m = do
  let rgba = packList m (colorize scale)
      bmp  = packRGBA32ToBMP24 (steps+1) (steps+1) rgba
  writeBMP file bmp
