-- | A wrapper around the types and functions from "Data.Graph" to make programming with them less painful. Also
-- implements some extra useful goodies such as 'successors' and 'sccGraph', and improves the documentation of
-- the behaviour of some functions.
--
-- As it wraps "Data.Graph", this module only supports directed graphs with unlabelled edges.
--
-- Incorporates code from the 'containers' package which is (c) The University of Glasgow 2002 and based
-- on code described in:
--
--   /Lazy Depth-First Search and Linear Graph Algorithms in Haskell/,
--   by David King and John Launchbury
module Data.Graph.Wrapper (
    Edge, Graph,
    
    vertex,
    
    fromListSimple, fromList, fromListBy, fromVerticesEdges,
    
    vertices, edges, successors,
    
    outdegree, indegree,
    
    transpose,
    
    reachableVertices, hasPath,
    
    topologicalSort,
    
    SCC(..), stronglyConnectedComponents, sccGraph
  ) where

import Control.Arrow (second)

import Data.Array
import qualified Data.Graph as G
import Data.List (sortBy, mapAccumL)
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Map as M
import Data.Ord
import qualified Data.Set as S

import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable


fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c


-- | An edge from the first vertex to the second
type Edge i = (i, i)

-- | A directed graph
data Graph i v = G {
    graph :: G.Graph,
    indexGVertexArray :: Array G.Vertex i,
    gVertexVertexArray :: Array G.Vertex v
  }

instance (Ord i, Show i, Show v) => Show (Graph i v) where
    show g = "fromVerticesEdges " ++ show ([(i, vertex g i) | i <- vertices g]) ++ " " ++ show (edges g)

instance Functor (Graph i) where
    fmap f g = g { gVertexVertexArray = fmap f (gVertexVertexArray g) }

instance Foldable.Foldable (Graph i) where
    foldMap f g = Foldable.foldMap f (gVertexVertexArray g)

instance Traversable.Traversable (Graph i) where
    traverse f g = fmap (\gvva -> g { gVertexVertexArray = gvva }) (Traversable.traverse f (gVertexVertexArray g))


indexGVertex :: Ord i => Graph i v -> i -> G.Vertex
indexGVertex g i = indexGVertex' (indexGVertexArray g) i

gVertexIndex :: Graph i v -> G.Vertex -> i
gVertexIndex g gv = indexGVertexArray g ! gv

gVertexVertex :: Graph i v -> G.Vertex -> v
gVertexVertex g gv = gVertexVertexArray g ! gv

-- | Retrieve data associated with the vertex
vertex :: Ord i => Graph i v -> i -> v
vertex g = gVertexVertex g . indexGVertex g


-- | Construct a 'Graph' where the vertex data double up as the indices.
--
-- Unlike 'Data.Graph.graphFromEdges', vertex data that is listed as edges that are not actually themselves
-- present in the input list are reported as an error.
fromListSimple :: Ord v => [(v, [v])] -> Graph v v
fromListSimple = fromListBy id

-- | Construct a 'Graph' that contains the given vertex data, linked up according to the supplied key extraction
-- function and edge list.
--
-- Unlike 'Data.Graph.graphFromEdges', indexes in the edge list that do not correspond to the index of some item in the
-- input list are reported as an error.
fromListBy :: Ord i => (v -> i) -> [(v, [i])] -> Graph i v
fromListBy f vertices = fromList [(f v, v, is) | (v, is) <- vertices]

-- | Construct a 'Graph' directly from a list of vertices (and vertex data).
--
-- If either end of an 'Edge' does not correspond to a supplied vertex, an error will be raised.
fromVerticesEdges :: Ord i => [(i, v)] -> [Edge i] -> Graph i v
fromVerticesEdges vertices edges | M.null final_edges_map = fromList done_vertices
                                 | otherwise              = error "fromVerticesEdges: some edges originated from non-existant vertices"
  where
    (final_edges_map, done_vertices) = mapAccumL accum (M.fromListWith (++) (map (second return) edges)) vertices
    accum edges_map (i, v) = case M.updateLookupWithKey (\_ _ -> Nothing) i edges_map of (mb_is, edges_map) -> (edges_map, (i, v, fromMaybe [] mb_is))

-- | Construct a 'Graph' that contains the given vertex data, linked up according to the supplied index and edge list.
--
-- Unlike 'Data.Graph.graphFromEdges', indexes in the edge list that do not correspond to the index of some item in the
-- input list are reported as an error.
fromList :: Ord i => [(i, v, [i])] -> Graph i v
fromList vertices = G graph key_map vertex_map
  where
    max_v           = length vertices - 1
    bounds0         = (0, max_v) :: (G.Vertex, G.Vertex)
    sorted_vertices = sortBy (comparing fst3) vertices
  
    graph       = array bounds0 $ [0..] `zip` map (map (indexGVertex' key_map) . thd3) sorted_vertices
    key_map     = array bounds0 $ [0..] `zip` map fst3                                 sorted_vertices
    vertex_map  = array bounds0 $ [0..] `zip` map snd3                                 sorted_vertices

indexGVertex' :: Ord i => Array G.Vertex i -> i -> G.Vertex
indexGVertex' key_map k = go 0 (snd (bounds key_map))
  where
    go a b | a > b = error "Data.Graph.Wrapper.fromList: one of the edges of a vertex pointed to a vertex that was not supplied in the input"
           | otherwise = case compare k (key_map ! mid) of
                           LT -> go a (mid - 1)
                           EQ -> mid
                           GT -> go (mid + 1) b
     where mid = (a + b) `div` 2

-- | Exhaustive list of vertices in the graph
vertices :: Graph i v -> [i]
vertices g = map (gVertexIndex g) $ G.vertices (graph g)

-- | Exhaustive list of edges in the graph
edges :: Graph i v -> [Edge i]
edges g = map (\(x, y) -> (gVertexIndex g x, gVertexIndex g y)) $ G.edges (graph g)

-- | Find the vertices we can reach from a vertex with the given indentity
successors :: Ord i => Graph i v -> i -> [i]
successors g i = map (gVertexIndex g) (graph g ! indexGVertex g i)

-- | Number of edges going out of the vertex.
--
-- It is worth sharing a partial application of 'outdegree' to the 'Graph' argument if you intend to query
-- for the outdegrees of a number of vertices.
outdegree :: Ord i => Graph i v -> i -> Int
outdegree g = \i -> outdegrees ! indexGVertex g i
  where outdegrees = G.outdegree (graph g)

-- | Number of edges going in to the vertex.
--
-- It is worth sharing a partial application of 'indegree' to the 'Graph' argument if you intend to query
-- for the indegrees of a number of vertices.
indegree :: Ord i => Graph i v -> i -> Int
indegree g = \i -> indegrees ! indexGVertex g i
  where indegrees = G.indegree (graph g)

-- | The graph formed by flipping all the edges, so edges from i to j now go from j to i
transpose :: Graph i v -> Graph i v
transpose g = g { graph = G.transposeG (graph g) }

-- | Topological sort of of the graph (<http://en.wikipedia.org/wiki/Topological_sort>). If the graph is acyclic,
-- vertices will only appear in the list once all of those vertices with arrows to them have already appeared.
--
-- Vertex i precedes j in the output whenever j is reachable from i but not vice versa.
topologicalSort :: Graph i v -> [i]
topologicalSort g = map (gVertexIndex g) $ G.topSort (graph g)

-- | List all of the vertices reachable from the given starting point
reachableVertices :: Ord i => Graph i v -> i -> [i]
reachableVertices g = map (gVertexIndex g) . G.reachable (graph g) . indexGVertex g

-- | Is the second vertex reachable by following edges from the first vertex?
hasPath :: Ord i => Graph i v -> Edge i -> Bool
hasPath g (i1, i2) = G.path (graph g) (indexGVertex g i1) (indexGVertex g i2)


data SCC i = AcyclicSCC i
           | CyclicSCC [i]
           deriving (Show)

instance Functor SCC where
    fmap f (AcyclicSCC v) = AcyclicSCC (f v)
    fmap f (CyclicSCC vs) = CyclicSCC (map f vs)

instance Foldable.Foldable SCC where
    foldMap f (AcyclicSCC v) = f v
    foldMap f (CyclicSCC vs) = Foldable.foldMap f vs

instance Traversable.Traversable SCC where
    traverse f (AcyclicSCC v) = fmap AcyclicSCC (f v)
    traverse f (CyclicSCC vs) = fmap CyclicSCC (Traversable.traverse f vs)

-- | Strongly connected components (<http://en.wikipedia.org/wiki/Strongly_connected_component>).
--
-- The SCCs are listed in a *reverse topological order*. That is to say, any edges *to* a node in the SCC
-- originate either *from*:
--
--   1) Within the SCC itself (in the case of a 'CyclicSCC' only)
--   2) Or from a node in a SCC later on in the list
--
-- Vertex i strictly precedes j in the output whenever i is reachable from j but not vice versa.
-- Vertex i occurs in the same SCC as j whenever both i is reachable from j and j is reachable from i.
stronglyConnectedComponents :: Graph i v -> [SCC i]
stronglyConnectedComponents g = map decode forest
  where
    forest = G.scc (graph g)
    decode (G.Node v []) | mentions_itself v = CyclicSCC [gVertexIndex g v]
                         | otherwise         = AcyclicSCC (gVertexIndex g v)
    decode other = CyclicSCC (dec other [])
      where dec (G.Node v ts) vs = gVertexIndex g v : foldr dec vs ts
    
    mentions_itself v = v `elem` (graph g ! v)

-- | The graph formed by the strongly connected components of the input graph. Each node in the resulting
-- graph is indexed by the set of vertex indices from the input graph that it contains.
sccGraph :: Ord i => Graph i v -> Graph (S.Set i) (M.Map i v)
sccGraph g = fromList nodes'
  where
    -- As we consume the SCCs, we accumulate a Map i (S.Set i) that tells us which SCC any given index belongs to.
    -- When we do a lookup, it is sufficient to look in the map accumulated so far because nodes that are successors
    -- of a SCC must occur to the *left* of it in the list.
    (_final_i2scc_i, nodes') = mapAccumL go M.empty (stronglyConnectedComponents g)
    
    --go :: M.Map i (S.Set i) -> SCC i -> (M.Map i (S.Set i), (S.Set i, M.Map i v, [S.Set i]))
    go i2scc_i scc = (i2scc_i', (scc_i,
                                 Foldable.foldMap (\i -> M.singleton i (vertex g i)) scc,
                                 Foldable.foldMap (\i -> map (fromJust . (`M.lookup` i2scc_i')) (successors g i)) scc))
      where
        -- The mechanism by which we index the new graph -- the set of indexes of its components
        scc_i = Foldable.foldMap S.singleton scc
        i2scc_i' = i2scc_i `M.union` Foldable.foldMap (\i -> M.singleton i scc_i) scc
