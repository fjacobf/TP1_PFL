import qualified Data.List
--import qualified Data.Array
--import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

cities :: RoadMap -> [City]
cities rm = Data.List.sort (Data.List.nub (concatMap (\(a,b,_) -> [a,b]) rm))

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent rm c1 c2 = Data.List.any (\(a, b, _) -> (a == c1 && b == c2) || (a == c2 && b == c1)) rm

distance :: RoadMap -> City -> City -> Maybe Distance
distance rm c1 c2 =  fmap (\(_, _, d) -> d) (Data.List.find (\(a, b, d) -> (a == c1 && b == c2) || (a == c2 && b == c1)) rm)

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent rm c = map (\(a, b, d) -> if a == c then (b, d) else (a, d))
                 (Data.List.filter (\(a, b, _) -> a == c || b == c) rm)

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [_] = Just 0
pathDistance rm (x:y:ys) = do
  d <- distance rm x y
  rest <- pathDistance rm (y:ys)
  return (d+rest)

rome :: RoadMap -> [City]
rome rm = let
  cityConnections = [(city, length (adjacent rm city)) | city <- cities rm]
  maxconnect = maximum (map snd cityConnections)
  in ([city | (city, count) <- cityConnections, count == maxconnect])

neighborsC :: RoadMap -> City -> [City]
neighborsC rm c =  map (\(a, b, d) -> if a == c then b else a)
                 (Data.List.filter (\(a, b, _) -> a == c || b == c) rm)


dfs :: RoadMap -> [City] -> [City] -> [City]
dfs rm [] visited = visited
dfs rm (c:cs) visited
  | c `elem` visited = dfs rm cs visited
  |otherwise = dfs rm (cs ++ neighborsC rm c) (c:visited)

reachable :: RoadMap -> City -> [City]
reachable rm start = dfs rm [start] []

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected rm =
  let allCities = cities rm
      startCity = head allCities
      reachableFromStart = reachable rm startCity
  in null allCities || Data.List.sort allCities == Data.List.sort reachableFromStart

shortestPath :: RoadMap -> City -> City -> ([City], Distance)
shortestPath rm start end
  | start == end = ([], 0)
  | otherwise = dijkstra [(start, 0, [start])] [] -- (cidade, distância, caminho)
  where
    -- Algoritmo de Dijkstra
    dijkstra :: [(City, Distance, [City])] -> [(City, Distance)] -> ([City], Distance)
    dijkstra [] _ = ([], 0)  -- Caso não seja possível encontrar um caminho
    dijkstra ((currentCity, currentDist, path):queue) visited
      | currentCity == end = (path, currentDist) -- Encontrou o destino
      | currentCity `elem` map fst visited = dijkstra queue visited -- Ignora se já foi visitada
      | otherwise =
          let adj = adjacent rm currentCity -- Cidades vizinhas
              newEntries = [ (neighbor, currentDist + dist, path ++ [neighbor]) 
                           | (neighbor, dist) <- adj
                           , neighbor `notElem` map fst visited
                           ]
              newQueue = Data.List.sortBy (\(_, d1, _) (_, d2, _) -> compare d1 d2) (queue ++ newEntries)
          in dijkstra newQueue ((currentCity, currentDist) : visited)

-- shortestPath :: RoadMap -> City -> City -> [Path]
-- shortestPath = undefined

travelSales :: RoadMap -> Path
travelSales = undefined

-- tspBruteForce :: RoadMap -> Path
-- tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]
