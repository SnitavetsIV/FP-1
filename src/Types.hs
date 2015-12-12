module Types (
  CSVDecodeOptions(..),
  ComLineArgs(..),
  --InitParams(..),
  Table
) where


import           Data.Vector              as V


type Table a = V.Vector (V.Vector a)

data CSVDecodeOptions = 
    CSVDecodeOptions {delimiter :: Char,
                      hasHeader :: Bool,
                      ignoreFirstColumn :: Bool,
                      ignoreLastColumn :: Bool
               }
  deriving (Show, Read)


data ComLineArgs =  -- First, we need a datatype
  ComLineArgs {inputFile :: String,
               outputFile :: String,
               metric :: String,
               count :: Int,
               accuracy :: Float,
               initVal :: String
               }
  deriving (Show) -- we will print the values

--data InitParams = 
--    InitParams {matrix :: Table Double,
--                centers :: Vector Int
--                }
