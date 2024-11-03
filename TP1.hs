import qualified Data.List
--import qualified Data.Array
--import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

-- This function recieves the RoadMap, concatenates it in a list of Cities using concatMap and removes the duplicated cities using the function nub from Data.List
cities :: RoadMap -> [City]
cities rm = Data.List.nub (concatMap (\(a,b,_) -> [a,b]) rm)

-- This function recieves the RoadMap and two cities,
-- it uses the function any from Data.List to return True
-- if those cities are adjacent in the RoadMap and False otherwise.
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent rm c1 c2 = Data.List.any (\(a, b, _) -> (a == c1 && b == c2) || (a == c2 && b == c1)) rm

-- This funcion recieves the RoadMap and two cities and returns the distance.
-- It does that by using the function find from Data.List to find the connection between the two cities and
-- the function fmap to return only the distance
distance :: RoadMap -> City -> City -> Maybe Distance
distance rm c1 c2 =  fmap (\(_, _, d) -> d) (Data.List.find (\(a, b, d) -> (a == c1 && b == c2) || (a == c2 && b == c1)) rm)

-- This function recieves the RoapMap and a City and returns a list of tuples
-- with the adjacent cities and the respective distance. It does that by filtering
-- every connection related to the City passed by argument using the function filter from Data.List.!!
-- The it uses the funcion map to correct to correct the output.
adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent rm c = map (\(a, b, d) -> if a == c then (b, d) else (a, d))
                 (Data.List.filter (\(a, b, _) -> a == c || b == c) rm)

-- This function recieves a RoadMap and a Path and returns the distance of that Path.
-- Ir does that by calling the function recursively. The first step return 0 if the path is empty.
-- Them we calculate the distance between the first and second element using the previous function that we created.
-- After that we apply the function pathDistance recursivelly with the second element onwards.
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [_] = Just 0
pathDistance rm (x:y:ys) = do
  d <- distance rm x y
  rest <- pathDistance rm (y:ys)
  return (d+rest)

-- This function recieves the RoadMap and returns the list of cities that have the most connections.
-- It does that building a list called cityConnections that is composed of tuples with the city and
-- the length of the result of the adjacent function that we created previouslly. Them we also create
-- a maxconnect that recieves the maximum value of the second position of the tuples in the cityConnections list.
rome :: RoadMap -> [City]
rome rm = let
  cityConnections = [(city, length (adjacent rm city)) | city <- cities rm]
  maxconnect = maximum (map snd cityConnections)
  in ([city | (city, count) <- cityConnections, count == maxconnect])

-- A basic implementation of a dfs that Recieves the RoadMap and two lists of cities and returns one of them
dfs :: RoadMap -> [City] -> [City] -> [City]
dfs rm [] visited = visited
dfs rm (c:cs) visited
  | c `elem` visited = dfs rm cs visited
  |otherwise = dfs rm (cs ++ map fst (adjacent rm c)) (c:visited)

-- This function basically starts the dfs with a list with the selected city and a void list.
-- It aims to return all the cities reachable by the city in the argument
reachable :: RoadMap -> City -> [City]
reachable rm start = dfs rm [start] []

-- This function uses the last two functions to test if the list of allCities, that contains all the nodes of the RoadMap passed in the argument
-- is equal to the list of the nodes reachable trough the first city in the list.
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected rm =
  let allCities = cities rm
      startCity = head allCities
      reachableFromStart = reachable rm startCity
  in null allCities || Data.List.sort allCities == Data.List.sort reachableFromStart

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath rm start end
  | start == end = [[]]
  | otherwise = dijkstra [(start, 0, [start])] []
  where
    -- Algoritmo de Dijkstra
    dijkstra :: [(City, Distance, [City])] -> [(City, Distance)] -> [Path]
    dijkstra [] _ = []
    dijkstra ((currentCity, currentDist, path):queue) visited
      | currentCity == end = collectShortestPaths currentDist path queue visited
      | currentCity `elem` map fst visited = dijkstra queue visited
      | otherwise =
          let adj = adjacent rm currentCity
              newEntries = [ (neighbor, currentDist + dist, path ++ [neighbor]) 
                           | (neighbor, dist) <- adj
                           , neighbor `notElem` map fst visited
                           ]
              newQueue = Data.List.sortBy (\(_, d1, _) (_, d2, _) -> compare d1 d2) (queue ++ newEntries)
          in dijkstra newQueue ((currentCity, currentDist) : visited)

    collectShortestPaths minDist path queue visited =
      let allPaths = dijkstra queue visited
          minPaths = filter (\p -> case pathDistance rm p of
                                     Just d  -> d == minDist
                                     Nothing -> False) allPaths
      in path : minPaths


travelSales :: RoadMap -> Path
travelSales = undefined

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]
