{-# LANGUAGE BangPatterns, PatternGuards #-}

module Main where

import Control.Monad
import Control.Monad.Trans

import Data.Bits

import qualified Data.List as List

import qualified Data.Set as Set

import System.Environment
import System.Exit

import Text.Printf

import ROBDD
import RandomGraph

usage :: IO ()
usage = do
	pn <- getProgName
	putStrLn $ "usage: " ++ pn ++ " number-of-nodes number-of-edges log2-bloom-filter-size"
	exitFailure

readInt :: String -> Maybe Int
readInt s = case reads s of
	(i,""):_ -> Just i
	_ -> Nothing

{-
Here's the query:

CREATE TABLE g(f INT, t INT);
INSERT INTO g
VALUES
        (1, 2), (1, 3), (1, 4), (2, 4), (2, 5),
        (3, 4), (3, 6), (3, 7), (4, 5), (4, 7),
        (4, 8), (5, 8), (6, 7), (7, 8);
SELECT
    g1.f AS a, g1.t AS b, g2.t AS c
FROM
    g AS g1, g AS g2, g AS g3
WHERE
    g1.t = g2.f AND g2.t = g3.t AND g1.f = g3.f;

-}
scanIteration :: ID -> Int -> Graph -> ROBDDM (Graph, ID)
scanIteration prevRoot bitsPerVar graph = do
	loop [] idFalse idFalse idFalse graph
	where
		mask = shiftL 1 bitsPerVar - 1
		hash i = xor (shiftR i bitsPerVar) i .&. mask
		hashBits n i = [ if o then v else negate v | (o, v) <- zip bits vars]
			where
				h = hash i
				bits = map odd $ take bitsPerVar $ iterate (flip div 2) h
				vars = map (+ (n * bitsPerVar)) [1..]
		loop !pass ab bc ac [] = do
			liftIO $ putStrLn $ show (length pass) ++ " edges pass scan test"
			abbc <- logicAND ab bc
			newRoot <- logicAND abbc ac
			return (pass, newRoot)
		loop !pass g1 g2 g3 ((f,t):fts) = do
			let	g1f = hashBits 2 f
				g1t = hashBits 0 t
				g2f = hashBits 0 f
				g2t = hashBits 1 t
				g3f = hashBits 2 f
				g3t = hashBits 1 t
				g1vs = g1f ++ g1t
				g2vs = g2f ++ g2t
				g3vs = g3f ++ g3t
			i1 <- andVars g1vs >>= logicAND prevRoot
			i2 <- andVars g2vs >>= logicAND prevRoot
			i3 <- andVars g3vs >>= logicAND prevRoot
			let	drop = i1 == idFalse && i2 == idFalse && i3 == idFalse
				pass' = if drop
					then pass
					else (f,t) : pass
			when False $ liftIO $ do
				putStrLn $ "f: "++show f++", t: "++show t
				--putStrLn $ "g1vs: "++show g1vs
				--putStrLn $ "g2vs: "++show g2vs
				--putStrLn $ "g3vs: "++show g3vs
				putStrLn $ "drop: " ++ show drop
			g1 <- logicOR g1 i1
			g2 <- logicOR g2 i2
			g3 <- logicOR g3 i3
			pass' `seq` loop pass' g1 g2 g3 fts

run :: Int -> Int -> Int -> IO ()
run nn ne bb' = do
	let	bb3 = div (bb' + 2) 3
		bb = bb3 * 3
		bfs = 2 ^ bb
		graph = randomGraph nn ne
		tgs = triangles graph
		tgsEdges = Set.fromList $ concatMap (\(a,b,c) -> [(a,b), (b, c), (a, c)]) tgs
		inTris = filter (flip Set.member tgsEdges) graph
	putStrLn $ "running test for " ++ show nn ++ " nodes and " ++ show ne ++ " edges in graph and Bloom filter size of " ++ show bfs ++ " bits."
	when (bb /= bb') $ putStrLn $ "Bloom filter size was rounded up to " ++ show bb ++ " from " ++ show bb' ++ " to be divisible by 3."
	let	p = fromIntegral ne / fromIntegral nn / fromIntegral (nn - 1)
		expectedTriangles = product [ fromIntegral i * p | i <- [nn,nn-1,nn-2]] :: Double
	putStrLn $ printf "expected number of triangles %.3g" expectedTriangles
	putStrLn $ printf "actual number of triangles is %d, number of edges that occur in traingles %d" (length tgs) (length inTris)
	evalROBDDM $ runScans idTrue bb3 graph
	where
		runScans prevID bitsPerVar graph = do
			liftIO $ putStrLn $ "scanning with " ++ show prevID
			(edgesFiltered, id') <- scanIteration prevID bitsPerVar graph
			liftIO $ putStrLn $ "scan result is " ++ show id'
			if id' == prevID
				then do
					liftIO $ do
						putStrLn "stopped"
						putStrLn $ "original graph triangles, sorted: " ++ show (List.sort $ triangles graph)
						putStrLn $ "triangles from filtered edges, sorted: " ++ show (List.sort $ triangles edgesFiltered)
				else runScans id' bitsPerVar graph
main = do
	args <- getArgs
	case args of
		[snn, sne, sbb]
			| Just nn <- readInt snn
			, Just ne <- readInt sne
			, Just bb <- readInt sbb -> run nn ne bb
		_ -> usage
