-- | Exposes things that are considered to be too unstable for inclusion in the exports of "Data.Graph.Wrapper".
--
-- Use of this module should be avoided as it will change frequently and changes to this module alone will not necessarily
-- follow the Package Versioning Policy.
{-# OPTIONS_HADDOCK not-home #-}
module Data.Graph.Wrapper.Internal (
    Graph(..)
  ) where

import Data.Array
import qualified Data.Graph as G

-- | A directed graph
data Graph i v = G {
    graph :: G.Graph,
    indexGVertexArray :: Array G.Vertex i,
    gVertexVertexArray :: Array G.Vertex v
  }

