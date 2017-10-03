module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.IORef
import Data.Text (Text)
import GHC.Conc
import System.Environment
import System.IO
import System.Random
import Transient.Base
import Transient.Indeterminism
import Transient.Logged
import Transient.Move

import qualified Data.Text as T

-- | Document with count of pages
data PDFDocument = PDFDocument Int
  deriving (Show, Read)

-- | A single page of PDF with its number
data PDFPage = PDFPage Int
  deriving (Show, Read)

-- | PDF page transformed to PNG image
data PDFPagePng = PDFPagePng Int
  deriving (Show, Read)

-- | Recognized text of PDF page
data PDFPageText = PDFPageText Int Text
  deriving (Show, Read)

-- | Split PDF document to pages and generate a task per page
splitDocument :: PDFDocument -> IO [PDFPage]
splitDocument (PDFDocument n) = pure $ PDFPage <$> [0 .. n-1]

-- | Convert PDF page to png image
pageToPng :: PDFPage -> IO PDFPagePng
pageToPng (PDFPage i) = pure $ PDFPagePng i

-- | Convert PNG to text
recognizePng :: PDFPagePng -> IO PDFPageText
recognizePng (PDFPagePng i) = do
  txt <- replicateM 50 $ do
    let ws = ["Lorem", "ipsum", "dolor", "sit", "amet,", "praesent", "ante", "ornare", "eu", "sodales", "suspendisse"]
    i <- randomRIO (0, length ws - 1)
    pure $ ws !! i
  pure $ PDFPageText i $ T.pack $ unwords txt

-- | Run recognition on cluster and colllect results
recognizePDFDocument :: PDFDocument -> Cloud [PDFPageText]
recognizePDFDocument doc = do
  pgs <- lliftIO $ splitDocument doc
  nodes <- local getNodes
  zipWithM recognize (cycle nodes) pgs
  where
    recognize :: Node -> PDFPage -> Cloud PDFPageText
    recognize node pg = runAt node $ local $ do
       png <- liftIO $ pageToPng pg
       liftIO $ recognizePng png

main :: IO ()
main = void $ do
  args <- getArgs
  let localPort = read (args !! 0)
      seedHost  = args !! 1
      seedPort  = read (args !! 2)

      numCalcsNode= 100
  mynode    <- createNode "localhost"  localPort
  seedNode  <- createNode seedHost seedPort
  rresults <- liftIO $ newIORef (0,0)

  runCloudIO $ do
    connect mynode seedNode

    local $ option  "start"  "start the calculation once all the nodes have been started"  :: Cloud String
    let doc = PDFDocument 10
    txts <- recognizePDFDocument doc
    lliftIO $ print txts
