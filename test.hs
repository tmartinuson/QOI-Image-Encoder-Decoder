import Codec.Picture

main :: IO ()
main = do
  -- Load image from file
  image <- readImage "car.jpg"

  -- Extract pixel data from image
  case image of
    Left err -> putStrLn err
    Right (ImageRGBA8 img) -> do
      let pixels = pixelMap pixelToTuple img
      print pixels
    Right _ -> putStrLn "Unsupported image format"

-- Convert pixel to tuple of RGBA values
pixelToTuple :: PixelRGBA8 -> (Int, Int, Int, Int)
pixelToTuple (PixelRGBA8 r g b a) = (fromIntegral r, fromIntegral g, fromIntegral b, fromIntegral a)


mapOverSlice :: Int -> Int -> (a -> a) -> Vector a -> Vector a
mapOverSlice startIndex length mapper input =
  let
    (split1Heading, split1Trail) =
      splitAt startIndex input
    (split2Heading, split2Trail) =
      splitAt (pred length) split1Trail
    in
      split1Heading <>
      map mapper split2Heading <>
      split2Trail


-- QOI file format header
--data Header = Header
--  { hMagic :: MatchASCII "QOI magic" "qoif"
--  , hWidth :: Word32
--  , hHeight :: Word32
--  , hChannels :: Word8
--  , hColorspace :: Word8
--  } deriving (Eq, Show, Generic, Binary)
