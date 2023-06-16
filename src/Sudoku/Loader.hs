module Sudoku.Loader (loadSudoku) where

import Control.Arrow ((>>>))
import Data.ByteString.Lazy.Char8 (unpack)
import Data.List (transpose)
import Network.HTTP.Client (Response (responseBody), httpLbs, newManager, parseRequest)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Sudoku.Types (Difficulty (..), UnfilledGrid)
import Sudoku.Utils (groupBy, readGrid, ungroup)
import Text.HandsomeSoup (css, parseHtml, (!))
import Text.XML.HXT.Core (runX)

-- | Converts a difficulty to number which can be plugged into the url.
toNumber :: Difficulty -> Int
toNumber Easy = 40
toNumber Medium = 33
toNumber Hard = 26
toNumber Evil = 17

{- | Creates the url for a given difficullty level from which the sudokus get
  loaded.
-}
url :: Difficulty -> String
url d = "https://kjell.haxx.se/sudoku/?visade=" ++ (show $ toNumber d) ++ "&seed=%28random+seed%29&action=Create+a+field&hardchange=0"

-- | Loads a sudoku with a given difficulty level from the internet.
loadSudoku :: Difficulty -> IO (Maybe UnfilledGrid)
loadSudoku d = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest (url d)
  response <- httpLbs request manager
  let doc = parseHtml $ unpack $ responseBody response
  values <- runX $ doc >>> css "input.sgrid" ! "value"
  -- in the html the values are aranged block wise and not row wise
  let transposedValues =
        ungroup
          . ungroup
          . transpose
          . groupBy 3
          . groupBy 9
          . ungroup
          . ungroup
          . transpose
          . groupBy 3
          . groupBy 3
          $ values
  let sudokuString = concat $ map (\v -> if v == "" then '.' : "" else v) transposedValues
  pure (readGrid sudokuString)
