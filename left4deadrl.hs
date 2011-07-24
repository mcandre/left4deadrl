-- A Left4Dead Roguelike
--
-- Andrew Pennebaker

import HsCharm
import Maybe (fromJust)
import Control.Monad (when, replicateM)
import Data.List (find, delete, sortBy)
import Data.List.Utils (join)
import Data.HashMap hiding (map)
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

-- -- Assumes x and y are within bounds.
-- attack :: Game -> Pos -> IO Game
-- attack g (x, y) = do
-- 	let c = getCell g (x, y)
-- 	let t = tile c
-- 	let o = occupant c
-- 
-- 	case o of
-- 		-- Monster
-- 		Just m -> do
-- 			let m' = m { hp = hp m - 1 }
-- 			let c' = if hp m' <= 0
-- 				then
-- 					c { occupant = Nothing }
-- 				else
-- 					c { occupant = Just m' }
-- 
-- 			let g' = putCell g (x, y) c'
-- 			return $ g' { messages = ("You hit a " ++ monsterName m ++ "."):(voicemail g) }
-- 		-- Tile
-- 		_ -> return $ g { messages = ("You hit a " ++ tileName t ++ ""):(voicemail g) }

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
				_ -> do
					let t = tile b

					case terrain t of
						0.0 -> do
							-- Move rogue from cell a to cell b.
							let g' = moveOccupant g (x, y) (x', y')
							return $ g' { rogueLoc = (x', y') }
						1.0 -> return $ g { messages = ("There is a " ++ tileName t ++ " in the way."):(voicemail g) }
		else
			return $ g { messages = "Edge of the world.":(voicemail g) }

blotMessages :: [String] -> Int -> IO ()
blotMessages [] _ = return ()
blotMessages (m:ms) row = do
	moveCursor 0 row
	hCenterString m
	blotMessages ms (row - 1)

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

-- -- Ignore corner adjacencies
-- adjacencies :: Game -> Pos -> [Pos]
-- adjacencies g (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
-- 
-- manhattan :: Pos -> Pos -> Int
-- manhattan = floor $ sqrt $ fromIntegral $ dx * dx + dy * dy
-- 	where
-- 		dx = fst p1 - fst p2
-- 		dy = snd p1 - snd p2
-- 
-- -- Based on A* Pathfinding for Beginners
-- -- http://www.policyalmanac.org/games/aStarTutorial.htm
-- path :: Game -> Pos -> Pos -> [Pos]
-- path g a b = path' g a b [] [a] (HashMap Pos Pos) (insert a 0 (HashMap Pos Int)) (insert a (manhattan a b) (HashMap Pos Int)) (insert a (manhattan a b) (HashMap Pos Int))
-- 	where
-- 		path' :: Game -> Pos -> Pos -> [Pos] -> [Pos] -> HashMap Pos Pos -> HashMap Pos Int -> HashMap Pos Int -> HashMap Pos Int -> [Pos]
-- 		path' g a b closed open cameFrom gScores hScores fScores
-- 			| not $ null open = -- ...
-- 				where
-- 					x = sortBy ((\k, \v) -> v) $ assoc fScores
-- 					
-- 
-- -- Let each monster respond.
-- respond :: Game -> [Pos] -> IO Game
-- respond g [] = return g
-- respond g (a:as) = do
-- 	let b = rogueLoc g
-- 
-- 	let m = fromJust $ occupant $ getCell g a
-- 
-- 	case path g a b of
-- 		Just ps -> do
-- 			-- Monster is next to rogue.
-- 			if length ps == 1
-- 				then do
-- 					let g' = strike g a b
-- 					respond g' as
-- 				-- Monster is away from rogue.
-- 				else do
-- 					-- Move monster one step along path.
-- 					let p = head ps
-- 					let g' = moveOccupant g a p
-- 
-- 					respond g' as
-- 		-- No path. Monster will sit.
-- 		_ -> respond g as

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
			blotMessages (replicate 3 $ join "" $ replicate (width g) " ") (height g + messageSpace - 1)

			-- Display messages
			blotMessages (reverse $ messages g) (height g + messageSpace - 1)

			k <- getKey

			when (k `notElem` [KeyEscape, KeyQ])
				(do
					g' <- if k `elem` [KeyUp, KeyDown, KeyRight, KeyLeft] then
							move g k
						else
							return g

					-- g'' <- respond g' (monsters g')
					-- 
					-- loop g'')

					loop g')

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
commonZombies g = width g `div` 10

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

	let c = getCell g (x, y)
	let t = tile c
	let o = occupant c
	let o' = case o of
		(Just _) -> True
		_ -> False

	-- If cell is occupied or impassible, reroll.
	if (o' || (terrain t == 1.0))
		then placeMonsters g (m:ms)
		else do
			let g' = placeMonster g (x, y) m
			placeMonsters g' ms

newGame :: IO Game
newGame = do
	w <- getWidth
	h <- getHeight

	-- Reserve space for messages
	let h' = h - messageSpace

	let exitLoc = (w - 1, h' `div` 2)
	let rLoc = exitLoc
	let entranceLoc = (0, h' `div` 2)

	d <- generateDungeon w h'

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