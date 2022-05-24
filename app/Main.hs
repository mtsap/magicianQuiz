import Control.Exception
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Magician
import System.Environment
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  inputOrExc <- try (readFile $ head args) :: IO (Either SomeException String)
  case inputOrExc of
    Left e -> do
      print e
      print "Something went wrong. Exiting"
      exitWith (ExitFailure 1)
    Right input -> do
      let (maybeNumOfPatients, maybeNodes) = parseInput input
      if isNothing maybeNodes || isNothing maybeNumOfPatients
        then do
          print "Parsing Failed. Exiting"
          exitWith (ExitFailure 1)
        else do
          let patients = fromJust maybeNodes
          let nodes = home : patients

          -- let trips = getAllTrips nodes
          -- let tripsByNode = getTripMap nodes trips

          -- -- calulcates the travel path based on the immediate reward
          -- -- We do that for all the possible first visits
          -- let tours = map (\x -> travel tripsByNode [mkTrip home x] x) patients

          -- -- gets the finalResult for the all the tours
          -- let results = map (calculateTrip timeLimit) tours

          -- print the maximum reward
          print . maximum $ (map snd $ (solve patients home timeLimit))
          exitSuccess
