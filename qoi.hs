module Main where

import Control.Concurrent.Async
import Codec.Picture

qoi :: Image -> Image
qoi image = do 
    -- TODO write algorithm here

encode :: FilePath -> IO DynamicImage
encode path = do
    file <- readImage path
    let encodedImage = qoi file
    return encodedImage

main :: IO()
main = do
    let temporaryFilePaths = ["test.png"]
    async <-