module Tests where

import Clasterer.Counter
import Test.HUnit
import Test.QuickCheck
import Data.Vector as V

main :: IO()
main = do
    let testHamming = TestCase (do
                                    let v1 = V.fromList [0, 0, 0]
                                        v2 = V.fromList [0, 0, 0]
                                    assertEqual "hamming [0, 0, 0] [0, 0, 0]" 0 (hammingDist v1 v2)
                                    let v1 = V.fromList [1, 2, 3]
                                        v2 = V.fromList [1, 2, 3]
                                    assertEqual "hamming [1, 2, 3] [1, 2, 3]" 0 (hammingDist v1 v2)
                                    let v1 = V.fromList [1, 2, 3]
                                        v2 = V.fromList [4, 4, 4]
                                    assertEqual "hamming [1, 2, 3] [4, 4, 4]" 6 (hammingDist v1 v2)
                                )
    let testEuclid = TestCase (do
                                    let v1 = V.fromList [0, 0, 0]
                                        v2 = V.fromList [0, 0, 0]
                                    assertEqual "euclid [0, 0, 0] [0, 0, 0]" 0 (euclidDist v1 v2)
                                    let v1 = V.fromList [1, 2, 3]
                                        v2 = V.fromList [1, 2, 3]
                                    assertEqual "euclid [1, 2, 3] [1, 2, 3]" 0 (euclidDist v1 v2)
                                    let v1 = V.fromList [1, 2, 3]
                                        v2 = V.fromList [3, 4, 4]
                                    assertEqual "euclid [1, 2, 3] [3, 4, 4]" 3 (euclidDist v1 v2)
                                )
    let testNorma = TestCase (do
                                let v1 = V.fromList [0, 0, 0]
                                    v2 = V.fromList [1, 2, 3]                                    
                                    v3 = V.fromList [4, 5, 6]
                                let t1 = V.fromList [v1, v1]
                                    t2 = V.fromList [v1, v1]
                                assertEqual "calcNorm 1" 0 (calcNorm t1 t2)
                                let t1 = V.fromList [v1, v1]
                                    t2 = V.fromList [v1, v2]
                                assertEqual "calcNorm 2" 3 (calcNorm t1 t2)
                                let t1 = V.fromList [v1, v1]
                                    t2 = V.fromList [v3, v1]
                                assertEqual "calcNorm 3" 6 (calcNorm t1 t2)
                            )
        tests = TestList [testHamming, testEuclid, testNorma]
    runTestTT tests
    return ()
