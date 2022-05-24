## Design of the solution 

### General description 
So this is can be seen as the the Travelling Salesman Problem with some extra
constraints and a lot of magic.

In contrary to the TSP where the salesman has to travel the all the nodes
in the shortest amount of time, our magician tries to maximize his total reward.
It would make sense for our magician to try to 
visit all, or as many nodes as possible, if they all had the same reward. 
But since each node has a different reward it makes sense to try
to maximize the based on the reward from each node. And since each node 
has a visit cost it makes even more sense to try to maximize the 
reward/cost for each visited node.

## Firest approach

The first attempt at solving it is by trying to maximize the immidate 
relative relative reward for each trip (Nearest neighbor heuristic search).

To model the problem we have the following type:

We have N nodes (patients). Each node is of the type:

```data Node = Node
  { xCoord :: Meters
  , yCoord :: Meters
  , nodeReward :: Int
  }
```
xCoord and yCoord are the coordinates of the node and the nodeReward 
is the reward.

From all the nodes that we parse we can now 
calculate all the possible trips. Trips have the following type: 

```
data Trip = Trip
  { start :: Node
  , end :: Node
  , tripReward :: Int
  , travelTime :: Minutes
  , relativeReward :: Float
  }
```
Each trip has a start and an end Node, a tripReward that is the reward of the 
destination Node, a travelTime which is the time it takes the magician to go 
from start to end, and finally the relativeReward which is how much
we are gonna get paid per minute of travel.

To calculate the tour for the day we try to maximize the relativeReward at each step. 
Starting from the castle node (0,0) we pick our next step by finding the 
node with the highest relativeReward from the ones we havn't visited yet.
When there are no more nodes to pick, because they are all visited, we add a trip 
back home and we have the full tour. 

Now we just have to make sure that we dont get stranded out of our castle 
before the time limit expires. To do that, we calculate the duration of the full
tour. If we are over the timelimit, we remove the last trip that is not the 
trip back home and we try again. When we are within the time limit we can
calculate the reward we will get back.

To be sure that we don't get lured by a first visit with a really high
relative reward, we also repeat the previous steps for all the possible
first visits. After calculating all the tours we can select the one with 
the maximum reward.

This method has the advantage that is it really fast. 
It's biggets disadvantage is that it can produce a suboptimal solution.
We can say that the magician is greedy and lazy
since he doesn't want to think much.

The most computation heavy part of of this solution is calculating the next  Node
to visit. This is done by the following function. 
```findNextMostProfitableNode :: [Trip] -> Node -> Map.Map Node [Trip] -> Maybe Trip
findNextMostProfitableNode history currentNode nodeMap =
  case Map.lookup currentNode nodeMap of
    Nothing -> Nothing
    Just trips -> head' $ filter (\x -> (end x) `notElem` (map end history)) $ sortOn (Data.Ord.Down . relativeReward) trips
```
The map that we use holds all the available trips for each of the nodes in our
graph that are used as the key.

For example a graph with 3 Nodes A, B and C would give us the following map:
```
NodeA: [
  Trip (Node A Node B  10 10 1)
  Trip (Node A Node C  10  5 2)
],
NodeB: [
  Trip (Node B Node A  10 10 1)
  Trip (Node B Node C  20  5 2)
],
```
By using a map we have fast access to the available Trips ( O(logn) ) and 
then we have to find the next move among at worst N trips ( where N the number
of the Nodes).


## Next iteration
If I had to continue working on the problem I would try to represent all the 
possible tours in a Tree. For example 

```data MyTree a = MyEmptyNode
              | MyFilledNode a [(MyTree a)]```

Lets say that we have 4 Nodes in our graph.
Our tree would look like this:
```
NodeA -> NodeB -> NodeC -> NodeD
                  NodeD -> NodeC
         NodeC -> NodeB -> NodeD
                  NodeD -> NodeB
         NodeD -> NodeB -> NodeC
                  NodeC -> NodeB
```
The data that I would put into each node would look somthing like this

```
data NodeState = NodeState
{ 
      node :: Node -- The coordinates and reward of that node
      path :: [Node] -- The already visited nodes till this node. 
    , pathReward :: Int -- The already collected reward
    , pathTime :: the time elapsed for that path
}
```
While generating each Node we can alrady see if we have gone over the time limit
so we dont have to continue calculating this path.

After calculating all the valid paths we can get all the leafs and find the 
path with the max reward.

This is a brute force aproach. 
In the worst case scenario we would have to calculate all of them and thus
having time complexity of O(n!) but still it would work for smaller graphs
and it sounds like a cool problem to try to solve.


