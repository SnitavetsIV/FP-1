import System.Console.ArgParser
import Csv.CSVParser
import Types
import Clasterer.Initializer
import Clasterer.Counter
import System.IO
import System.IO.Error 
import Control.Exception

argsParser -- Then, we define a parser 
  :: ParserSpec ComLineArgs
argsParser = ComLineArgs
  `parsedBy` reqPos "inFile" `Descr` "CSV file you want to parse"
  `andBy` optFlag "console" "outFile" `Descr` "Output file for result of parsing "
  `andBy` optFlag "euclid" "metric" `Descr` "Metric for distant count ('euclid', 'hamming')"
  `andBy` optFlag 3 "count" `Descr` "Count of centers of clasters"
  `andBy` optFlag 0.0001 "accuracy" `Descr` "Accuracy of calculations"
  `andBy` optFlag "matrix" "init" `Descr` "Type of initialization ('matrix', 'centers')"


clearFile :: String -> IO()
clearFile output = writeFile output ""

writeResults :: String -> Table Double -> IO()
writeResults outputFile dataToWrite = do
            if outputFile /= "console" then
                mapM_ (\line -> appendFile outputFile $ (show line) ++ "\n") dataToWrite
            else
                mapM_ print dataToWrite


process :: ComLineArgs -> IO ()
process args = do
  let opts = CSVDecodeOptions {delimiter = ',',
                      hasHeader = False,
                      ignoreFirstColumn = False,
                      ignoreLastColumn = True
               }
  parsedCsv <- parseCSVFile (inputFile args) opts
  case parsedCsv of
    Left s -> putStrLn s
    Right v -> do
      result <- clasterize v args
      clearFile (outputFile args) `catch` handler
      writeResults (outputFile args) result `catch` handler
  return ()

main = do -- We proceed to build an interface and run it:
  interface <- mkApp argsParser
  runApp interface process

handler :: IOError -> IO()
handler e
    | isPermissionError e = putStrLn "You have no permissions for writing file"
    | otherwise = ioError e  
