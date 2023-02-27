# QOI-Image-Encoder-Decoder

## Setup
Cabal is needed to install the dependencies used for this project, you can simply open a command shell within the project folder and type the following (If you have cabal already installed you can skip this step):

```sudo apt install cabal-install```

Once installed you can verify your version with the following:

```cabal --version```

Before installing the dependencies you should run a cabal update like below:

```cabal update```

Next to install the dependencies, run the following command. This will add the required dependencies to run this program:

```cabal install --only-dependencies```

Finally you should be all set to run the program!

## How to run?

To run this program type the following in shell while within the project folder:

```cabal run qoi```

Place any desired 3-channel RGB .pngs within the ./images folder for encoding into .qoi. QOI files will be placed in ./output when finished.

## What is the problem?
PNG images take up too much space and in the modern day of machine learning this is definitely not tolerable. In comes QOI image compressions, which provides a fast lossless image compression from PNG to QOI that is 50x faster and that reduces the size of an image by ~20%.

## What is the something extra?
Our program will take a folder that contains several images and compress them asynchronously in batches to better suit machine learning pipelines.

## What did we learn from doing this?
(This should be written after you have done the work.)What is the bottom-line? Is functional programming suitable for (part-of) the task? Make sure you include the evidence for your claims.

Pixel encoding is quite a challenging task and that is not as simple to come up with a solution as originally thought. This task required a different way of approaching the problem than other types of programming in which conversions between types and manipulating values felt a bit more redundant when using functional programming for this task. The nice thing was that pattern matching was found to be really helpful when it came to identifying different cases for encoding our QOI pixels which is something Haskell definitely excels at.
