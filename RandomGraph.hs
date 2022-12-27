module RandomGraph where

import qualified Data.IntMap.Strict as IMap
import qualified Data.IntSet as ISet

import Data.IORef

import qualified Data.List as List

import System.Random

type Graph = [(Int, Int)]

randomGraph :: Int -> Int -> Graph
randomGraph nodes edges = map correct $ take edges $ zip froms tos
	where
		froms = randomRs (0, nodes-1) $ mkStdGen 1234512345
		tos = randomRs (0, nodes-2) $ mkStdGen 5432154321
		correct (f,t) = (f, if t >= f then t + 1 else t)

triangles :: Graph -> [(Int, Int, Int)]
triangles g = concatMap tris g
	where
		fs = IMap.unionsWith ISet.union [IMap.singleton f (ISet.singleton t) | (f, t) <- g]
		ts = IMap.unionsWith ISet.union [IMap.singleton t (ISet.singleton f) | (f, t) <- g]
		tris (f, t) = [(f, t, b) | b <- ISet.toList both]
			where
				ff = IMap.findWithDefault ISet.empty f fs
				ft = IMap.findWithDefault ISet.empty t ts
				both = ISet.intersection ff ft

