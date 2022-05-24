import NodeTests
import ParseTests
import SolveTests
import Test.HUnit

parseTests = TestList [testGoodInput, testBadInput1, testBadInput2, testBadInput3]

nodeTests = TestList [nodeTest1, nodeTest2, nodeFromListTest1, nodeFromListTest2, nodeFromListTest3]

solveTests = TestList [solve1, testResult1]

main :: IO ()
main = do
  runTestTT parseTests
  runTestTT nodeTests
  runTestTT solveTests
  putStrLn "Test finished"
