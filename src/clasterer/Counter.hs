module Clasterer.Counter (
    clasterize,
    hammingDist,
    euclidDist,
    calcNorm
) where

import Types
import Data.Vector  as  V
import Clasterer.Initializer
import GHC.Float

-- Main function, which returns result
clasterize :: Table Double -> ComLineArgs -> IO(Table Double)
clasterize parsedData args = do
    let result = case initVal args of "matrix" ->  countWithInitialMatrix parsedData args
                                      "centers" -> countWithInitialCenters parsedData args
    result

-- Functions to count whith different initials
countWithInitialMatrix :: Table Double -> ComLineArgs -> IO(Table Double)
countWithInitialMatrix parsedData args = do
    initialMatrix <- initClastererMatrix parsedData args
    let resultMatrix = calculateBelongsMatrix initialMatrix parsedData args
    return resultMatrix

countWithInitialCenters :: Table Double -> ComLineArgs -> IO(Table Double)
countWithInitialCenters parsedData args = do
    initialCenters <- initClastererCenters parsedData args
    let newMatrix = calculateNewMatrix (metric args) parsedData initialCenters
        resultMatrix = calculateBelongsMatrix newMatrix parsedData args
    return resultMatrix


calculateBelongsMatrix :: Table Double -> Table Double -> ComLineArgs -> Table Double
calculateBelongsMatrix prevMatrix parsedData args = do    
    let centers = findNewCenters parsedData prevMatrix
        newMatrix = calculateNewMatrix (metric args) parsedData centers
    if resultCalculated prevMatrix newMatrix (accuracy args)
        then newMatrix
        else calculateBelongsMatrix newMatrix parsedData args


resultCalculated :: Table Double -> Table Double -> Float -> Bool
resultCalculated oldMatrix newMatrix eps = do
    let norm = calcNorm oldMatrix newMatrix
    norm < (float2Double eps)

calcNorm :: Table Double -> Table Double -> Double
calcNorm oldMatrix newMatrix = V.maximum (V.zipWith (\old new -> V.maximum (V.zipWith (\x y -> abs (x - y)) old new)) oldMatrix newMatrix)

dist :: String -> Vector Double -> Vector Double -> Double
dist metric first second = 
    case metric of "euclid" -> euclidDist first second
                   "hamming" -> hammingDist first second

hammingDist :: Vector Double -> Vector Double -> Double
hammingDist first second = V.sum (V.zipWith (\x y -> abs (x - y)) first second)

euclidDist :: Vector Double -> Vector Double -> Double
euclidDist first second = sqrt (V.sum (V.zipWith (\x y -> (x - y) ** 2) first second))

findNewCenters :: Table Double -> Table Double -> Table Double
findNewCenters parsedData belongMatrix = generate (Prelude.length (belongMatrix ! 0)) (\x -> findNewCenter parsedData belongMatrix x)


findNewCenter :: Table Double -> Table Double -> Int -> Vector Double
findNewCenter parsedData belongMatrix centerIdx = do 
    let column = V.map (\x -> x ! centerIdx) belongMatrix
        sum = V.sum column
        smth = V.zipWith (\obj coef -> V.map (\prizn -> prizn * (coef ** 2) / sum) obj) parsedData column
    V.foldl (\x y -> V.zipWith (+) x y) (V.generate (Prelude.length (parsedData ! 0)) (\x -> 0)) smth


calculateNewMatrix :: String -> Table Double -> Table Double -> Table Double
calculateNewMatrix metric parsedData centers = 
    generate (Prelude.length parsedData) (\i -> generate (Prelude.length centers) (\k -> calculateMatrixElement metric i k parsedData centers))


calculateMatrixElement :: String -> Int -> Int -> Table Double -> Table Double -> Double
calculateMatrixElement metric i k parsedData centers = 
    1 / V.sum (V.map (\center ->  qqq metric (parsedData ! i) (centers ! k) center) centers)

qqq :: String -> Vector Double -> Vector Double -> Vector Double-> Double
qqq metric obj center curCenter = do
    let dist1 = dist metric obj curCenter
    if dist1 == 0
        then 1
        else ((dist metric obj center) / dist1) ** 2
