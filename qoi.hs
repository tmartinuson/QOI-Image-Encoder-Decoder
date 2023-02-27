module Main where

import qualified Data.Vector.Unboxed as VU
import Data.Vector
import qualified Control.Applicative as Map
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as SB
import Data.ByteString.Char8 as C
import Control.Concurrent.Async
import Data.Word
import Debug.Trace
import Graphics.Image
import Graphics.Image.Interface
import Data.Bits (Bits(xor, shiftL))
import Control.Concurrent (yield)
import Data.Map as M
import Data.Bits
import Data.Binary.Put
import System.Directory
import System.FilePath
import Data.List as L

-- the input type of pixel
data PixelRaw = PixelRaw Int Int Int
  deriving (Show, Eq)

-- Conversion helper functions for converting a Pixel Raw into its corresponding QOIPixel type
toQOIPixel :: PixelRaw -> QOIPixel
toQOIPixel (PixelRaw r g b) = QOIPixelRaw r g b

toQOIPixelDiffBeeg :: (Int, Int, Int) -> QOIPixel
toQOIPixelDiffBeeg (r, g, b) = QOIPixelDiffBeeg r g b

toQOIPixelDiffSmol :: (Int, Int, Int) -> QOIPixel
toQOIPixelDiffSmol (r, g, b) = QOIPixelDiffSmol r g b

-- QOI internal data types
data QOIPixel =
  QOIPixelRaw Int Int Int |
  QOIPixelDiffSmol Int Int Int |
  QOIPixelDiffBeeg Int Int Int |
  QOIPixelRun Int | -- maybe add prev pix?
  QOIPixelIndex Int
  deriving (Show, Eq)

-- Gets the pixel difference between two pixel raws
getPixelDiff :: PixelRaw -> PixelRaw -> (Int, Int, Int)
getPixelDiff (PixelRaw r1 g1 b1) (PixelRaw r2 g2 b2) = ((r1 - r2), (g1 - g2), (b1 - b2))

-- Hash function for mapping pixel values that are seen previously and indexing them within a Map
hash :: PixelRaw -> Int
hash (PixelRaw r g b) = (r * 3 + g * 5 + b * 7 + 255 * 11) `mod` 64

-- Compares smaller difference between pixels
isSmolDiff :: (Int, Int, Int) -> Bool
isSmolDiff (dr, dg, db) = abs (dr) < 4 &&  abs (dg) < 4 &&  abs (db) < 4

-- Compares larger difference between pixels
isBeegDiff :: (Int, Int, Int) -> Bool
isBeegDiff (dr, dg, db) = abs (dr) < 32 && abs (dg) < 16 && abs (db) < 16

-- Encoding Algorithm
-- prev pix == curr pix               -> QOI_OP_RUN
-- prev pix dif curr pix < smol_diff  -> QOI_OP_DIFF
-- prev pix dif curr pix < big_diff   -> QOI_OP_LUMA
-- curr pix seen before?              -> QOI_OP_INDEX
-- cant have shit in detroit huh      -> QOI_OP_RGB

-- Appends a new QOIPixelRun when there is a run of similar pixels
appendRun :: Data.Vector.Vector QOIPixel -> Int -> Data.Vector.Vector QOIPixel
appendRun out run
  | run > 0 = Data.Vector.snoc out (QOIPixelRun run)
  | otherwise = out

-- first 2 args are the previous pixel and curr pixel.
-- Int is the run length, 0 if none
-- second argument is the map of seen pixels
-- last argument is array that is the array to write to
-- returns an encoded list
processPixels :: Data.Vector.Vector PixelRaw -> Int -> Map Int PixelRaw -> Data.Vector.Vector QOIPixel -> Data.Vector.Vector QOIPixel
processPixels vec run seen out
  | Data.Vector.null vec = out
  | L.length vec == 1 = (appendRun out run)
  | otherwise = do
    let prev = Data.Vector.head vec
    let rest = Data.Vector.tail vec
    let curr = Data.Vector.head rest
    if prev == curr
      then if run >= 62
        then processPixels (Data.Vector.snoc rest curr) 1 seen (Data.Vector.snoc out (QOIPixelRun run))
        else processPixels (Data.Vector.snoc rest curr) (run + 1) seen out
      -- Case 2 if we've seen curr pixel before and indexed it with a hash
      else if member (hash curr) seen
        then processPixels (Data.Vector.snoc rest curr) 0 seen (Data.Vector.snoc (appendRun out run) (QOIPixelIndex (hash curr)))
  --  -- Case 3 is a small difference between the previous pixel
        else if isSmolDiff (getPixelDiff prev curr)
          then processPixels (Data.Vector.snoc rest curr) 0 (M.insert (hash curr) curr seen) (Data.Vector.snoc (appendRun out run) (toQOIPixelDiffSmol (getPixelDiff prev curr)))
  --  -- Case 4 is a larger (but not too large) difference between the previous pixel
        else if isBeegDiff (getPixelDiff prev curr)
         then processPixels (Data.Vector.snoc rest curr) 0 (M.insert (hash curr) curr seen) (Data.Vector.snoc (appendRun out run) (toQOIPixelDiffBeeg (getPixelDiff prev curr)))
  --  -- Case 5 default case we just add it as it is
         else processPixels (Data.Vector.snoc rest curr) 0 (M.insert (hash curr) curr seen) (Data.Vector.snoc (appendRun out run) (toQOIPixel curr))
--processPixels [] _ _ out = out
--processPixels [a] run seen out = (appendRun out run)
--processPixels (prev:curr:rest) run seen out =
--  -- Case 1 where current pixel is the same as the previous pixel
--  if prev == curr
--    then if run >= 62
--      then processPixels (curr:rest) 1 seen (Data.Vector.snoc out QOIPixelRun run)
--      else processPixels (curr:rest) (run + 1) seen out
--    -- Case 2 if we've seen curr pixel before and indexed it with a hash
--    else if member (hash curr) seen
--      then processPixels (curr:rest) 0 seen (Data.Vector.snoc (appendRun out run) QOIPixelIndex (hash curr))
----  -- Case 3 is a small difference between the previous pixel
--      else if isSmolDiff (getPixelDiff prev curr)
--        then processPixels (curr:rest) 0 (M.insert (hash curr) curr seen) (Data.Vector.snoc (appendRun out run) toQOIPixelDiffSmol (getPixelDiff prev curr))
----  -- Case 4 is a larger (but not too large) difference between the previous pixel
--      else if isBeegDiff (getPixelDiff prev curr)
--       then processPixels (curr:rest) 0 (M.insert (hash curr) curr seen) (Data.Vector.snoc (appendRun out run) toQOIPixelDiffBeeg (getPixelDiff prev curr))
----  -- Case 5 default case we just add it as it is
--       else processPixels (curr:rest) 0 (M.insert (hash curr) curr seen) (Data.Vector.snoc (appendRun out run) (toQOIPixel curr))

-- Maps pixels to pixel raws
mapPixels :: [(Pixel RGB Double)] -> [PixelRaw]
mapPixels arr = Prelude.map pixelToTuple arr

-- Convert HIP pixel class to internal QOIPixelRaw
pixelToTuple :: (Pixel RGB Double) -> PixelRaw
pixelToTuple (PixelRGB r g b) = PixelRaw (round $ 255 * (r :: Double)) (round $ 255 * (g :: Double)) (round $ 255 * (b :: Double))

-- Convert QOIPixel into its corresponding byte by QOI spec
encodeQOIPixelToBinaryString :: QOIPixel -> LB.ByteString
encodeQOIPixelToBinaryString (QOIPixelRaw r g b) =
  runPut $ do
    putWord8 254
    putWord8 (fromIntegral r)
    putWord8 (fromIntegral g)
    putWord8 (fromIntegral b)
encodeQOIPixelToBinaryString (QOIPixelDiffSmol dr dg db) =
  runPut $ do
    putWord8 ((1 `shiftL` 6) .|. (((fromIntegral dr) .&. 0x3) `shiftL` 4) .|. (((fromIntegral dg) .&. 0x3) `shiftL` 2) .|. (((fromIntegral db) .&. 0x3)))
encodeQOIPixelToBinaryString (QOIPixelDiffBeeg dr dg db) =
  runPut $ do
    putWord8 ((2 `shiftL` 6) .|. (((fromIntegral dg) .&. 0x3F)))
    let diffDr = dr - dg
    let diffDb = db - dg
    putWord8 ((((fromIntegral diffDr) .&. 0xF) `shiftL` 4) .|. ((fromIntegral diffDb) .&. 0xF))
encodeQOIPixelToBinaryString (QOIPixelRun run) =
  runPut $ do
    putWord8 ((3 `shiftL` 6) .|. (((fromIntegral (run - 1)) .&. 0x3F)))
encodeQOIPixelToBinaryString (QOIPixelIndex index) =
  runPut $ do
    putWord8 ((0 `shiftL` 6) .|. (((fromIntegral index) .&. 0x3F)))

-- Helper function for mapping all QOIPixels to their binary representations above
encodeToBinary :: Data.Vector.Vector QOIPixel -> Data.Vector.Vector LB.ByteString
encodeToBinary arr = Data.Vector.map encodeQOIPixelToBinaryString arr

mapToStrict :: Data.Vector.Vector LB.ByteString -> Data.Vector.Vector SB.ByteString
mapToStrict arr = Data.Vector.map LB.toStrict arr

-- Writes a list of ByteStrings to the given FilePath
writeByteStringListToDisk :: FilePath -> Data.Vector.Vector SB.ByteString -> IO ()
--writeByteStringListToDisk filePath byteStrings = SB.writeFile filePath (C.unwords (Data.Vector.toList byteStrings))
writeByteStringListToDisk filePath byteStrings = SB.writeFile filePath (C.pack "wowoww")

-- Creates the header for a qoi formatted file
createHeader :: Int -> Int -> LB.ByteString
createHeader width height =
  runPut $ do
    putWord8 113 -- q ie. Magic Bytes that spell qoif
    putWord8 111 -- o
    putWord8 105 -- i
    putWord8 102 -- f
    putWord32be (fromIntegral width)
    putWord32be (fromIntegral height)
    putWord8 3 -- RGB channels only supported
    putWord8 0 -- linear channels for colorspace

-- Creates the end marker for a qoi formatted file
createEndMarker :: LB.ByteString
createEndMarker = 
  runPut $ do
    putWord32le 0
    putWord8 0
    putWord8 0
    putWord8 0
    putWord8 1
  
-- Runs the encode part of the program
runEncode :: FilePath -> IO ()
runEncode filePath = do
  let fileTitle = takeBaseName filePath
  image <- readImageRGB VU ("./input/" L.++ filePath)
  Prelude.putStrLn (filePath L.++ ": reading file as image.")
  let pixels = mapPixels (VU.toList (toVector image))
  Prelude.putStrLn (filePath L.++ ": created list of raw pixels.")
  let fst = L.head pixels
  let processedPixels = processPixels (Data.Vector.fromList pixels) 0 (M.singleton (hash fst) fst) (Data.Vector.singleton (toQOIPixel fst))
  Prelude.putStrLn (filePath L.++ ": processed raw pixels into QOI pixels.")
  print (Data.Vector.length processedPixels)
  let binaryEncoding = (Data.Vector.singleton (createHeader (cols image) (rows image))) Data.Vector.++ (encodeToBinary processedPixels) Data.Vector.++ (Data.Vector.singleton createEndMarker)
--  let binLen = length binaryEncoding
--  Prelude.putStrLn "Length of binary string is: " L.++ binLen
  Prelude.putStrLn (filePath L.++ ": encoded QOI pixels into binary.")
  Prelude.putStrLn "starting converting to static byte string!"
--  let static =  LB.toStrict (LB.fromChunks (mapToStrict binaryEncoding))
  let static =  mapToStrict binaryEncoding
  Prelude.putStrLn "converted to static byte string!"
  writeByteStringListToDisk ("./output/" L.++ fileTitle L.++ ".qoi") static
  Prelude.putStrLn (filePath L.++ ": write to disk successful.")

-- Main CLI program - run with "cabal run qoi" and ensure desired pngs are placed under ./input
main :: IO ()
main = do
  Prelude.putStrLn "Welcome to our QOI Image Encoder!"
  Prelude.putStrLn "Please ensure your images (.png) that you would like to be encoded are under ./input."
  Prelude.putStrLn "Would you like to begin y/n?"
  input <- Prelude.getLine
  case input of
    "y" -> do
--        files <- getDirectoryContents "./input"
--        let filePaths = L.filter (".png" `isSuffixOf`) files
        _ <- mapConcurrently runEncode ["test.png"]
        Prelude.putStrLn "Finishing Run..."
    "n" -> do
        Prelude.putStrLn "Exiting QOI Image Encoder"
    _ -> do
        Prelude.putStrLn "Invalid input. Please enter 'y' or 'n'."
        main
