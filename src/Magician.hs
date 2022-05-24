module Magician (
  speed,
  timeLimit,
  mkTrip,
  mkNode,
  nodeFromList,
  calcDistance,
  calcTravelTime,
  magicianTravelTime,
  head',
  parseInput,
  pairOfDifferentNodes,
  getAllTrips,
  getTripMap,
  addTripToMap,
  findNextMostProfitableNode,
  travel,
  calculateTrip,
  solve,
  Node (..),
  Trip (..),
  home,
) where

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord
import qualified GHC.Base as Nodes
import Text.Read

type Meters = Float
type Minutes = Float

-- | The speed of the Magician 15km\h = 250 m/min
speed :: Float
speed = 250

-- | 8h time limit in minutes
timeLimit :: Minutes
timeLimit = 8.0 * 60

-- | Data the represent the nodes we have to visit
data Node = Node
  { xCoord :: Meters
  , yCoord :: Meters
  , nodeReward :: Int
  }
  deriving (Show, Eq, Ord)

{- | Data representing a trip from node start to node end.
 tripReward is the reward we receive upon arring at the end and
 curing the patent.
 travelTime is the minutes we need to go from one place to the other.
 relativeReward the reward/minutes.
-}
data Trip = Trip
  { start :: Node
  , end :: Node
  , tripReward :: Int
  , travelTime :: Minutes
  , relativeReward :: Float
  }
  deriving (Show, Eq)

-- | The Node of the Castle
home :: Node
home =
  Node
    { xCoord = 0
    , yCoord = 0
    , nodeReward = 0
    }

mkNode :: String -> String -> String -> Maybe Node
mkNode x y r =
  let coordx = readMaybe x :: Maybe Meters
      coordy = readMaybe y :: Maybe Meters
      reward = readMaybe r :: Maybe Int
   in Node <$> fmap (1000 *) coordx <*> fmap (1000 *) coordy <*> reward

nodeFromList :: [String] -> Maybe Node
nodeFromList [a, b, c] = mkNode a b c
nodeFromList _ = Nothing

-- | Euclidean distance between two nodes
calcDistance :: Node -> Node -> Meters
calcDistance a b = sqrt ((yCoord b - yCoord a) ** 2 + (xCoord b - xCoord a) ** 2)

-- | Calculate the travel time between two nodes
calcTravelTime :: Float -> Node -> Node -> Minutes
calcTravelTime mySpeed a b = calcDistance a b / mySpeed

-- | Calculate the travel time between two nodes for the speed of the magician
magicianTravelTime :: Node -> Node -> Minutes
magicianTravelTime = calcTravelTime speed

mkTrip :: Node -> Node -> Trip
mkTrip nodeA nodeB =
  let travelTime = magicianTravelTime nodeA nodeB
   in Trip nodeA nodeB (nodeReward nodeB) travelTime (fromIntegral (nodeReward nodeB) / travelTime)

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x : xs) = Just x

parseInput :: String -> (Maybe Int, Maybe [Node])
parseInput input =
  let inputLines = lines input
      numOfPatients = head' inputLines >>= readMaybe
      nodes = sequenceA $ map (nodeFromList . words) (tail inputLines)
   in (numOfPatients, nodes)

-- | Get a list of all pairs of as apart from those for which a == b
pairOfDifferentNodes :: Eq a => [a] -> [(a, a)]
pairOfDifferentNodes l = [(x, y) | x <- l, y <- l, x /= y]

-- Get all the Trips except for the one that have the same start and end
getAllTrips :: [Node] -> [Trip]
getAllTrips nodes = uncurry mkTrip <$> pairOfDifferentNodes nodes

-- Makes a Map that has as key a node and as value a list of all possible trips from that node
-- without trips form a to a
getTripMap :: [Node] -> [Trip] -> Map.Map Node [Trip]
getTripMap nodes trips =
  let emptyMap = Map.fromList $ map (\x -> (x, [])) nodes :: Map.Map Node [Trip]
   in foldl' addTripToMap emptyMap trips

addTripToMap :: Map.Map Node [Trip] -> Trip -> Map.Map Node [Trip]
addTripToMap nodeMap trip = Map.insertWith (++) (start trip) [trip] nodeMap

{- | Given a Map of all the available trips for each Node, our previously visited nodes
 and the inital Node, it gives back a List of the visited Nodes.
 The next node is the one with the highest relativeReward that we have
 not already visited.
-}
travel :: Map.Map Node [Trip] -> [Trip] -> Node -> [Trip]
travel nodeMap history currentNode =
  case findNextMostProfitableNode history currentNode nodeMap of
    Nothing -> history
    Just nextTrip -> travel nodeMap (nextTrip : history) (end nextTrip)

{- | Given our previously visited nodes
 the Node and a Map of all the available trips for each Node,
 it gives back a List of the visited Nodes.
 The next node is the one with the highest relativeReward that we have
 not already visited.
-}
findNextMostProfitableNode :: [Trip] -> Node -> Map.Map Node [Trip] -> Maybe Trip
findNextMostProfitableNode history currentNode nodeMap =
  case Map.lookup currentNode nodeMap of
    Nothing -> Nothing
    Just trips -> head' $ filter (\x -> (end x) `notElem` (map end history)) $ sortOn (Data.Ord.Down . relativeReward) trips

-- Given a timeLimit a list of trips it finds the longest sub trip
-- that takes less then timeLimit to complete.
-- For evey sub trip it also add a trip back to home before
-- calculating the total time. When the longest possible trip is found
-- it calculates the reward and returns the results
calculateTrip :: Minutes -> [Trip] -> (Minutes, Int)
calculateTrip limit trips = case head' trips of
  Nothing -> (0, 0)
  Just trip ->
    let fullTour = reverse $ mkTrip (end trip) (Node 0 0 0) : trips
        fullTourTime = foldl' (+) 0 $ map travelTime fullTour
        fullTourReward = foldl' (+) 0 $ map tripReward fullTour
        result =
          if fullTourTime > limit
            then calculateTrip limit (tail trips)
            else (fullTourTime, fullTourReward)
     in result

solve :: [Node] -> Node -> Minutes -> [(Minutes, Int)]
solve patients home timeLimit =
  let nodes = home : patients
      trips = getAllTrips nodes
      tripsByNode = getTripMap nodes trips
      -- calulcates the travel path based on the immediate reward
      -- We do that for all the possible first visits
      tours = map (\x -> travel tripsByNode [mkTrip home x] x) patients
   in -- gets the finalResult for the all the tours
      map (calculateTrip timeLimit) tours
