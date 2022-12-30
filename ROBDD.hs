{-# LANGUAGE BangPatterns #-}

module ROBDD where

import Control.Applicative

import Control.Monad
import Control.Monad.State

import qualified Data.List as List

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data Node = Node !Int !ID !ID
	deriving (Eq, Ord, Show)

newtype ID = ID Int
	deriving (Eq, Ord, Show)

data CacheOp = CacheBin BinOp !ID !ID
	deriving (Eq, Ord, Show)

data BinOp = LogicOR | LogicAND
	deriving (Eq, Ord, Show)

data ROBDDS = ROBDDS
	{ robddsCounter		:: !Int
	, robddsNodeToId	:: !(Map.Map Node ID)
	, robddsIdToNode	:: !(Map.Map ID Node)
	, robddsCachedOp	:: !(Map.Map CacheOp ID)
	}
	deriving (Show)

type ROBDDM a = StateT ROBDDS IO a

emptyROBDDS :: ROBDDS
emptyROBDDS = ROBDDS
        { robddsCounter         = 2	-- skip constants.
        , robddsNodeToId        = Map.empty
        , robddsIdToNode        = Map.empty
        , robddsCachedOp        = Map.empty
        }

evalROBDDM :: ROBDDM a -> IO a
evalROBDDM act = do
	evalStateT act emptyROBDDS

id0, id1, idFalse, idTrue :: ID
id0 = ID 0
id1 = ID 1
idFalse = id0
idTrue = id1

getID :: Node -> ROBDDM ID
getID node@(Node var a b)
	| a == b = return a -- reduce rule.
	| otherwise = do
		mbId <- Map.lookup node . robddsNodeToId <$> get
		case mbId of
			Just id -> return id
			Nothing -> do
				when False $ do
					let	check i
							| i == idTrue || i == idFalse = return ()
							| otherwise = do
								ni@(Node v _ _) <- getNode i
								when (v <= var) $ error $ "order violation, trying to register " ++ show node ++ " eferring to "++show i++ " which is "++show ni
					check a
					check b
				i <- (+ 1) . robddsCounter <$> get
				let	id = ID i
				modify' $ \rs -> rs
						{ robddsCounter = i
						, robddsNodeToId	= Map.insert node id $ robddsNodeToId rs
						, robddsIdToNode	= Map.insert id node $ robddsIdToNode rs
						}
				return id

getNode :: ID -> ROBDDM Node
getNode a = do
	Map.findWithDefault (error $ "can't find node for " ++ show a) a . robddsIdToNode <$> get

_cacheBin :: BinOp -> ID -> ID -> ROBDDM ID -> ROBDDM ID
_cacheBin binOp a b act = do
	let	key = CacheBin binOp a b
	mbCached <- Map.lookup key . robddsCachedOp <$> get
	case mbCached of
		Just i -> return i
		Nothing -> do
			i <- act
			modify' $ \rs -> rs { robddsCachedOp = Map.insert key i $ robddsCachedOp rs }
			return i

logicOR :: ID -> ID -> ROBDDM ID
logicOR a b
	| a == b = return a
	| a == idFalse = return b
	| b == idFalse = return a
	| a == idTrue = return idTrue
	| b == idTrue = return idTrue
	| otherwise = _cacheBin LogicOR a b $ do
		Node va fa ta <- getNode a
		Node vb fb tb <- getNode b
		case compare va vb of
			EQ -> do
				f <- logicOR fa fb
				t <- logicOR ta tb
				getID $ Node va f t
			LT -> do
				f <- logicOR fa b
				t <- logicOR ta b
				getID $ Node va f t
			GT -> do
				f <- logicOR a fb
				t <- logicOR a tb
				getID $ Node vb f t

logicAND :: ID -> ID -> ROBDDM ID
logicAND a b
	| a == b = return a
	| a == idFalse = return idFalse
	| b == idFalse = return idFalse
	| a == idTrue = return b
	| b == idTrue = return a
	| otherwise = _cacheBin LogicAND a b $ do
		Node va fa ta <- getNode a
		Node vb fb tb <- getNode b
		case compare va vb of
			EQ -> do
				f <- logicAND fa fb
				t <- logicAND ta tb
				getID $ Node va f t
			LT -> do
				f <- logicAND fa b
				t <- logicAND ta b
				getID $ Node va f t
			GT -> do
				f <- logicAND a fb
				t <- logicAND a tb
				getID $ Node vb f t

andVars :: [Int] -> ROBDDM ID
andVars vs = loop sortedVS idTrue
	where
		sortedVS = reverse $ map snd $ List.sort $ zip (map abs vs) vs
		loop [] id = return id
		loop (v:vs) id
			| v == 0 = error "zero var index"
			| otherwise = do
				id <- getID $ if v < 0
					then Node (abs v) idFalse id
					else Node (abs v) id idFalse
				loop vs id

size :: ID -> ROBDDM Int
size id
	| id == idFalse || id == idTrue = return 0
	| otherwise = count (Set.singleton id) (Set.singleton id)
	where
		count :: Set.Set ID -> Set.Set ID -> ROBDDM Int
		count visited front
			| Set.null front = return $ Set.size visited
			| otherwise = do
				nodes <- Map.elems . flip Map.intersection (Map.fromSet (const ()) front) . robddsIdToNode <$> get
				let	nodeIDs (Node _ a b) = [a, b]
					ids = Set.delete idFalse $ Set.delete idTrue $ Set.fromList $ concatMap nodeIDs nodes
					newVisited = Set.union ids visited
					newFront = Set.difference ids visited
				count newVisited newFront
					

collectGarbage :: Set.Set ID -> ROBDDM ()
collectGarbage roots' = do
	idToNode <- robddsIdToNode <$> get
	let	roots'' = Map.fromSet (const ()) roots'
		roots = Map.intersection idToNode roots''
	reachable <- reach roots roots
	let	nodes = Map.fromList [(node, id) | (id, node) <- Map.toList reachable]
	modify' $ \rs -> rs
			{ robddsIdToNode = reachable
			, robddsNodeToId = nodes
			, robddsCachedOp = Map.empty
			}
	where
		reach :: Map.Map ID Node -> Map.Map ID Node -> ROBDDM (Map.Map ID Node)
		reach roots front = do
			toNode <- robddsIdToNode <$> get
			toId <- robddsNodeToId <$> get
			let	ns = Map.unions $ map nodeToIds $ Map.elems front
				unseen = Map.difference ns roots
				front' = Map.intersection toNode unseen
			if Map.null front'
				then return roots
				else reach (Map.union roots front') front'
		nodeToIds (Node _ a b) = Map.fromList [(a,()), (b,())]

