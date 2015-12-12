module Csv.CSVParser (
    parseCSVFile
) where

import           Data.Csv
import           Data.Char
import           Data.Vector              as V
import qualified Data.ByteString.Lazy     as BL
import           Types


parseCSVFile :: String -> CSVDecodeOptions -> IO (Either String (Table Double))

parseCSVFile filePath decOpts = do
    csvData <- BL.readFile filePath
    let decoderOpts = DecodeOptions { decDelimiter = fromIntegral (ord $ delimiter decOpts) }
    let resultString = if hasHeader decOpts
                          then decodeWith decoderOpts HasHeader csvData
                          else decodeWith decoderOpts NoHeader csvData
    case resultString of Left err -> return (Left err)
                         Right v ->  do
                            let result = V.map (\x -> V.map (\y -> read y::Double) x) v
                            return (Right (removeIgnoredColumns result decOpts) )

removeIgnoredColumns :: Table Double -> CSVDecodeOptions -> Table Double

removeIgnoredColumns csvData opts = removeFirstIfNeed (ignoreFirstColumn opts) $ removeLastIfNeed (ignoreLastColumn opts) csvData

removeLastIfNeed :: Bool -> Table Double -> Table Double

removeLastIfNeed need csvData = if need 
                                  then V.map V.init csvData
                                  else csvData

removeFirstIfNeed :: Bool -> Table Double -> Table Double

removeFirstIfNeed need csvData = if need 
                                    then V.map V.tail csvData
                                    else csvData
