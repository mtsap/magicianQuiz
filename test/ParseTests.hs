{-# LANGUAGE QuasiQuotes #-}

module ParseTests (
  testGoodInput,
  testBadInput1,
  testBadInput2,
  testBadInput3,
) where

import Magician
import Test.HUnit
import Text.RawString.QQ

test1 = TestCase (assertEqual "for (add 3)," 5 (4 + 1))

goodInput =
  [r| 15
2.423929917008996 20.187139309438546 2
19.39788132776695 14.174570106439353 1
-1.3175678970133191 10.019351994529405 6|]

testGoodInput =
  TestCase
    ( assertEqual
        "parseInput works as expected"
        (Just 15, Just [Node{xCoord = 2423.93, yCoord = 20187.139, nodeReward = 2}, Node{xCoord = 19397.88, yCoord = 14174.57, nodeReward = 1}, Node{xCoord = -1317.568, yCoord = 10019.352, nodeReward = 6}])
        (parseInput (goodInput))
    )

badInput1 =
  [r| 
2.423929917008996 20.187139309438546 2
19.39788132776695 14.174570106439353 1
-1.3175678970133191 10.019351994529405 6|]

testBadInput1 =
  TestCase
    ( assertEqual
        "testBadInput1"
        (Nothing, Just [Node{xCoord = 2423.93, yCoord = 20187.139, nodeReward = 2}, Node{xCoord = 19397.88, yCoord = 14174.57, nodeReward = 1}, Node{xCoord = -1317.568, yCoord = 10019.352, nodeReward = 6}])
        (parseInput (badInput1))
    )

badInput2 =
  [r| 15
2.423929917008996 20.187139309438546 
19.39788132776695 14.174570106439353 1
-1.3175678970133191 10.019351994529405 6|]

testBadInput2 =
  TestCase
    ( assertEqual
        "testBadInput2"
        (Just 15, Nothing)
        (parseInput (badInput2))
    )

badInput3 =
  [r| 15
2.423929917008996 20.187139309438546 a
19.39788132776695 14.174570106439353 1
-1.3175678970133191 10.019351994529405 6|]

testBadInput3 =
  TestCase
    ( assertEqual
        "testBadInput3"
        (Just 15, Nothing)
        (parseInput (badInput3))
    )
