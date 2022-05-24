module NodeTests (
  nodeTest1,
  nodeTest2,
  nodeFromListTest1,
  nodeFromListTest2,
  nodeFromListTest3,
) where

import Magician
import Test.HUnit
import Text.RawString.QQ

nodeTest1 =
  TestCase
    ( assertEqual
        "mkNode happy path"
        (Just (Node{xCoord = 0.0, yCoord = 0.0, nodeReward = 0}))
        (mkNode "0" "0" "0")
    )
nodeTest2 = TestCase (assertEqual "mkNode that fails" (Nothing) (mkNode "0" "0" "a"))

nodeFromListTest1 =
  TestCase
    ( assertEqual
        "nodeFromList happy path"
        (Just (Node{xCoord = 0.0, yCoord = 14440.0, nodeReward = 0}))
        (nodeFromList ["0", "14.44", "0"])
    )

nodeFromListTest2 =
  TestCase
    ( assertEqual
        "nodeFromList fail 2"
        (Nothing)
        (nodeFromList ["0", "0", "a"])
    )

nodeFromListTest3 =
  TestCase
    ( assertEqual
        "nodeFromList fail 3"
        (Nothing)
        (nodeFromList ["0", "0", "0", "df"])
    )
