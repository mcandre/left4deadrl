{-# LANGUAGE NoImplicitPrelude #-}

-- A Left4Dead Roguelike
--
-- Andrew Pennebaker

import HsCharm
import Prelude hiding (lookup)
import Maybe (fromJust)
import Control.Monad (when, replicateM)
import Data.List (find, delete, sortBy)
import Data.List.Utils (join)
import Data.Map (Map, empty, insert, lookup, assocs)
import Random (randomRIO)

pick :: [a] -> IO a
pick xs = (randomRIO (0, length xs - 1)) >>= (return . (xs !!))

type Pos = (Int, Int)

data Game = Game {
		dungeon :: [[Cell]],
		messages :: [String],
		rogueLoc :: Pos,
		safehouseExitLoc :: Pos,
		safehouseEntranceLoc :: Pos
	}

-- Assumes each row is the same length
width :: Game -> Int
width = length . (!! 0) . dungeon

height :: Game -> Int
height = length . dungeon

data Cell = Cell {
		tile :: Tile,
		occupant :: Maybe Monster
	}

data Tile = Tile {
		tileName :: String,

		-- ASCII symbol
		tileSymbol :: String,

		-- From 0.0 (easily traversed) to 1.0 (impassible)
		terrain :: Float
	}

data Monster = Monster {
		monsterName :: String,

		-- ASCII symbol
		monsterSymbol :: String,

		-- Attack points
		ap :: Int,

		-- Health points
		hp :: Int
	}

instance Show Tile where
	show = tileSymbol

instance Show Monster where
	show = monsterSymbol

instance Show Cell where
	show c = case occupant c of
		Nothing -> (show . tile) c
		Just m -> show m

messageSpace :: Int
messageSpace = 3

voicemail :: Game -> [String]
voicemail = take (messageSpace - 1) . messages

space = Tile {
		tileName = "floor",
		tileSymbol = " ",
		terrain = 0.0
	}

emptyCell = Cell {
		tile = space,
		occupant = Nothing
	}

emptySpace = emptyCell {
		tile = space
	}

wall = Tile {
		tileName = "wall",
		tileSymbol = "#",
		terrain = 1.0
	}

emptyWall = emptyCell {
		tile = wall
	}

-- Player starts here
safehouseExit = Tile {
		tileName = "safehouse exit",
		tileSymbol = "]",
		terrain = 0.0
	}

emptySafehouseExit = emptyCell {
		tile = safehouseExit
	}

-- Player must get here
safehouseEntrance = Tile {
		tileName = "safehouse entrance",
		tileSymbol = "[",
		terrain = 0.0
	}

emptySafehouseEntrance = emptyCell {
		tile = safehouseEntrance
	}

defaultRogue = Monster {
		monsterName = "rogue",
		monsterSymbol = "@",
		ap = 1,
		hp = 10
	}

zombie = Monster {
		monsterName = "zombie",
		monsterSymbol = "z",
		ap = 1,
		hp = 1
	}

withinBounds :: Game -> Pos -> Bool
withinBounds g (x, y) = (x >= 0) && (x <= width g - 1) && (y >= 0) && (y <= height g - 1)

-- Assumes x and y are within bounds.
getCell :: Game -> Pos -> Cell
getCell g (x, y) = ((dungeon g) !! y) !! x

-- Assumes x and y are within bounds.
putCell :: Game -> Pos -> Cell -> Game
putCell g (x, y) c = g { dungeon = (rowsBefore ++ [curRow'] ++ rowsAfter) }
	where
		rows = dungeon g
		(rowsBefore, curRow:rowsAfter) = splitAt y rows
		(colsBefore, _:colsAfter) = splitAt x curRow
		curRow' = colsBefore ++ [c] ++ colsAfter

moveOccupant :: Game -> Pos -> Pos -> Game
moveOccupant g a b = g''
	where
		aCell = getCell g a
		bCell = getCell g b
		bCell' = bCell { occupant = occupant aCell }
		aCell' = aCell { occupant = Nothing }
		g' = putCell g b bCell'
		g'' = putCell g' a aCell'

move :: Game -> Key -> IO Game
move g k = do
	let (x, y) = rogueLoc g

	let (x', y') = case k of
		KeyUp -> (x, y - 1)
		KeyDown -> (x, y + 1)
		KeyRight -> (x + 1, y)
		KeyLeft -> (x - 1, y)

	if withinBounds g (x', y')
		then do
			let b = getCell g (x', y')

			case occupant b of
				-- Monster in the way
				Just m -> do
					return $ strike g (rogueLoc g) (x', y')

				-- Nothing in the way
				_ -> if passable g (x', y')
					then do
						-- Move rogue from cell a to cell b.
						let g' = moveOccupant g (x, y) (x', y')
						return $ g' { rogueLoc = (x', y') }
					else do
						let t = tile b
						return $ g { messages = ("There is a " ++ tileName t ++ " in the way."):(voicemail g) }
		else
			return $ g { messages = "Edge of the world.":(voicemail g) }

blotMessages :: Game -> [String] -> IO ()
blotMessages g ms = blotMessages' ms (height g + messageSpace - 1)
	where
		blotMessages' :: [String] -> Int -> IO ()
		blotMessages' [] _ = return ()
		blotMessages' (m:ms) row = do
			moveCursor 0 row
			hCenterString m
			blotMessages' ms (row - 1)

blotDungeon :: [[Cell]] -> IO ()
blotDungeon g = do
	moveCursor 0 0
	(blotString . join "\n" . (map (join "" . (map show)))) g

win :: Game -> Bool
win g = (safehouseEntranceLoc g) == (rogueLoc g)

lose :: Game -> Bool
lose g = ((0 ==) . hp . fromJust . occupant . getCell g . rogueLoc) g

blotRecap :: Game -> String -> IO ()
blotRecap g s = do
	clearScreen
	vCenterString $ s ++ " Play again (y/n)?"

	k <- getKey

	case k of
		KeyY -> restart
		KeyN -> return ()
		KeyQ -> return ()
		KeyEscape -> return ()
		_ -> blotRecap g s

strike :: Game -> Pos -> Pos -> Game
strike g a b = g'
	where
		aCell = getCell g a
		bCell = getCell g b
		m1 = fromJust $ occupant aCell
		m2 = fromJust $ occupant bCell
		m2' = m2 { hp = hp m2 - ap m1 }
		g' = if (hp m2' <= 0) && (monsterName m2' /= "rogue")
			then
				-- Delete monster.
				putCell (g { messages = ("Killed a " ++ monsterName m2' ++ "."):(voicemail g) }) b (bCell { occupant = Nothing })
			else
				-- Reinstert monster.
				placeMonster (g { messages = ("Hit a " ++ monsterName m2' ++ "."):(voicemail g) }) b m2'

passable :: Game -> Pos -> Bool
passable g p = (not (occupied c)) && (0.0 == (terrain (tile c)))
	where
		c = getCell g p

dist :: Pos -> Pos -> Int
dist (x1, y1) (x2, y2) = floor $ sqrt $ fromIntegral $ dx * dx + dy * dy -- (x2 - x1) + (y2 - y1) 
	where
		dx = x1 - x2
		dy = y1 - y2

-- Ignore corner adjacencies
neighbors :: Game -> Pos -> [Pos]
neighbors g (x, y) = filter (withinBounds g) [
		(x + 1, y),
		(x - 1, y),
		(x, y + 1),
		(x, y - 1)
	]

-- Greedy pathfinding
cheapPath :: Game -> Pos -> Pos -> Maybe [Pos]
cheapPath g s e
	| 1 == dist s e = Just [s, e]
	| otherwise = Just [s, b, e]
	where
		b = head $ sortBy (\a b -> compare (dist a e) (dist b e)) $ filter (passable g) $ neighbors g s

data AStar = AStar {
		game :: Game,
		start :: Pos,
		end :: Pos,
		closed :: [Pos],
		open :: [Pos],
		cameFrom :: Map Pos Pos,
		gScores :: Map Pos Int,
		hScores :: Map Pos Int,
		fScores :: Map Pos Int
	}

-- From Wikipedia
-- http://en.wikipedia.org/wiki/A*_search_algorithm
path :: Game -> Pos -> Pos -> Maybe [Pos]
path g s e = path' AStar {
		game = g,
		start = s,
		end = e,
		closed = [],
		open = [s],
		cameFrom = empty,
		gScores = insert s 0 empty,
		hScores = insert s (h s e) empty,
		fScores = insert s (h s e) empty
	}
	where
		path' :: AStar -> Maybe [Pos]
		path' astar
			| null o = Nothing
			| x == e = Just $ reconstructPath cF (fromJust $ lookup e cF)
			| otherwise = path' astar''
				where
					g = game astar
					o = open astar
					c = closed astar
					e = end astar
					cF = cameFrom astar

					x = fst $ head $ sortBy (\(k1, v1) (k2, v2) -> compare v1 v2) $ assocs (fScores astar)

					-- Move x from open to closed.
					open' = delete x o
					closed' = c ++ [x]

					astar' = astar { closed = closed', open = open' }
					astar'' = consider astar' x $ filter (\p -> p `notElem` closed' && passable g p) (neighbors g x)

		reconstructPath :: Map Pos Pos -> Pos -> [Pos]
		reconstructPath cF p = case lookup p cF of
			Just p' -> (reconstructPath cF p) ++ [p']
			_ -> [p]

		consider :: AStar -> Pos -> [Pos] -> AStar
		consider astar _ [] = astar
		consider astar a (b:bs) = consider astar' a bs
			where
				e = end astar
				o = open astar
				cF = cameFrom astar
				gS = gScores astar
				hS = hScores astar
				fS = fScores astar

				tentativeGScore = (fromJust $ lookup a gS) + (dist a b)

				open' = if b `notElem` o
					then o ++ [b]
					else o

				tentativeIsBetter = (b `notElem` o) || (tentativeGScore < (fromJust $ lookup b gS))

				cF' = insert b a cF
				gS' = insert b tentativeGScore gS
				hS' = insert b (h b e) hS
				fS' = insert b ((fromJust $ lookup b gS') + (fromJust $ lookup b hS')) fS

				astar' = if tentativeIsBetter
					then astar {
								open = open',
								cameFrom = cF',
								gScores = gS',
								hScores = hS',
								fScores = fS'
							}
					else astar

		-- Manhattan
		h :: Pos -> Pos -> Int
		h = dist

-- Let each monster respond.
respond :: Game -> [Pos] -> IO Game
respond g [] = return g
respond g (a:as) = do
	let b = rogueLoc g

	let m = fromJust $ occupant $ getCell g a

	case cheapPath g a b of
		Just (_:p:ps) -> do
			-- Monster is next to rogue.
			if length ps == 0
				then do
					let g' = (strike g a b) { messages = ("You were struck by a " ++ monsterName m ++ "."):(voicemail g) }
					respond g' as
				-- Monster is away from rogue.
				else do
					-- Move monster one step along path.
					let g' = (moveOccupant g a p) { messages = ("A " ++ monsterName m ++ " moved closer to you."):(voicemail g) }
					respond g' as
		-- No path. Monster will sit.
		_ -> respond (g { messages = ("A " ++ monsterName m ++ " sat down."):(voicemail g) }) as

occupied :: Cell -> Bool
occupied c = case occupant c of
	Just _ -> True
	_ -> False

-- The location of every monster that is not a rogue
monsters :: Game -> [Pos]
monsters g = [
		(x, y)
		|
		x <- [0 .. width g - 1],
		y <- [0 .. height g - 1],
		occupied $ getCell g (x, y),
		(x, y) /= rogueLoc g
	]

loop :: Game -> IO ()
loop g = do
	case (win g, lose g) of
		(True, _) -> blotRecap g "You made it!"
		(_, True) -> blotRecap g "You were overwhelmed."
		_ -> do
			-- Display dungeon
			blotDungeon (dungeon g)

			-- Clear messages
			blotMessages g (replicate 3 $ join "" $ replicate (width g) " ")

			-- Display messages
			blotMessages g (reverse $ messages g)

			k <- getKey

			when (k `notElem` [KeyEscape, KeyQ])
				(do
					g' <- if k `elem` [KeyUp, KeyDown, KeyRight, KeyLeft] then
							move g k
						else
							return g

					blotMessages g ["Pathfinding..."]

					g'' <- respond g' (monsters g')
					
					loop g'')

					-- loop g')

generateRow :: Int -> IO [Cell]
generateRow w = replicateM w (pick (emptyWall:(replicate 10 emptySpace)))

generateSafehouseRow :: Int -> IO [Cell]
generateSafehouseRow w = do
	cells <- replicateM (w - 2) (pick (emptyWall:(replicate 10 emptySpace)))

	return $ emptySafehouseEntrance:(cells ++ [emptySafehouseExit])

generateDungeon :: Int -> Int -> IO [[Cell]]
generateDungeon w h = do
	as <- replicateM ((h - 1) `div` 2) (generateRow w)
	b <- generateSafehouseRow w
	cs <- replicateM ((h - 1) `div` 2) (generateRow w)

	return $ as ++ (b:cs)

commonZombies :: Game -> Int
commonZombies g = 2 -- width g `div` 10

placeMonster :: Game -> Pos -> Monster -> Game
placeMonster g (x, y) r = putCell g (x, y) c'
	where
		c = getCell g (x, y)
		c' = c { occupant = Just r }

placeMonsters :: Game -> [Monster] -> IO Game
placeMonsters g [] = return g
placeMonsters g (m:ms) = do
	x <- pick [0 .. (width g - 1)]
	y <- pick [0 .. (height g - 1)]

	-- If cell is occupied or impassible, reroll.
	if not (passable g (x, y))
		then placeMonsters g (m:ms)
		else do
			let g' = placeMonster g (x, y) m
			placeMonsters g' ms

newGame :: IO Game
newGame = do
	w <- getWidth
	h <- getHeight

	-- Reserve space for messages
	let (w', h') = (w, h - messageSpace)

	-- Shrink screen for pathfinding testing
	let (w'', h'') = (w' `div` 4, h' `div` 4)

	let exitLoc = (w'' - 1, h'' `div` 2)
	let rLoc = exitLoc
	let entranceLoc = (0, h'' `div` 2)

	d <- generateDungeon w'' h''

	let g = Game {
			dungeon = d,
			messages = [],
			rogueLoc = rLoc,
			safehouseExitLoc = exitLoc,
			safehouseEntranceLoc = entranceLoc
		}

	let g' = placeMonster g rLoc defaultRogue

	return g'

	placeMonsters g' (replicate (commonZombies g) zombie)

restart :: IO ()
restart = newGame >>= loop

main :: IO ()
main = do
	startCharm

	restart

	endCharm