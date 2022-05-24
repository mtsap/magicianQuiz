module SolveTests (
  solve1,
  testResult1,
) where

import Magician
import Test.HUnit

nodeA = Node 15000 0 60
nodeB = Node (-7500) 0 30
nodeC = Node (-7505) 0 10
nodeD = Node (-7510) 0 10

-- Tavel time
--            30mins                                                          60mins
--NodeD-NodeC-NodeB-------------------Home------------------------------------NodeA
-- 10     10   10                       0                                       60
-- For time limit 120 our best choice is to visit just NodeA

patients = [nodeA, nodeB, nodeC, nodeD]
solution = solve patients home 121

solve1 =
  TestCase
    ( assertEqual
        "solve1"
        [(120.0, 60), (60.08, 50), (60.120003, 50), (60.120003, 50)]
        solution
    )
testResult1 =
  TestCase
    ( assertEqual
        "solve1"
        60
        (maximum $ map snd solution)
    )
