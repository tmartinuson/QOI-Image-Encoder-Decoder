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

## What is the problem?
PNG images take up too much space and in the modern day of machine learning this is definitely not tolerable. In comes QOI image compressions, which provides a fast lossless image compression from PNG to QOI that is 50x faster and that reduces the size of an image by ~20%.

## What is the something extra?
Our program will take a folder that contains several images and compress them asynchronously in batches to better suit machine learning pipelines.

## What did we learn from doing this?
(This should be written after you have done the work.)What is the bottom-line? Is functional programming suitable for (part-of) the task? Make sure you include the evidence for your claims.
