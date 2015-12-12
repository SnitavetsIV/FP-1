module Clasterer.Initializer (
    initClastererCenters,
    initClastererMatrix
) where

import Types
import Data.Vector  as  V
import System.Random
import System.IO.Unsafe


initClastererCenters :: Table Double -> ComLineArgs -> IO(Table Double)
initClastererCenters parsedData args  = do
  idxes <- getCentersIndexes (count args) parsedData
  return $ V.map (\x -> parsedData ! x) idxes

initClastererMatrix :: Table Double -> ComLineArgs -> IO(Table Double)
initClastererMatrix parsedData args  =  generateM (Prelude.length parsedData) (\x -> createRow (count args))

createRow :: Int -> IO(Vector Double)
createRow count = do 
  gen <- newStdGen
  let idxMas = Prelude.take count (randomRs (0,1) gen::[Double])
  let row = fromList idxMas
  let sum = V.sum row
  let result = V.map (\x -> x / sum) row
  return result


getCentersIndexes :: Int -> Table Double -> IO(Vector Int)
getCentersIndexes count parsedData= do
  gen <- newStdGen
  let idxMas = Prelude.take count (randomRs (0,(Prelude.length parsedData) - 1) gen::[Int])
  let result = fromList idxMas
  return result
