import HsCharm
import Control.Monad (when, replicateM)
import Data.List (find, delete)
import Data.List.Utils (join)
import Random (randomRIO)

pick :: [a] -> IO a
pick xs = (randomRIO (0, length xs - 1)) >>= (return . (xs !!))

data Game = Game {
		width :: Int,
		height :: Int,
		messages :: [String],
		level :: [[Monster]],
		rogue :: Monster,
		monsters :: [Monster]
	}

defaultGame :: Game
defaultGame = Game {
		width = 80,
		height = 24 - messageSpace,
		messages = [],
		level = replicate (24 - messageSpace) (replicate 80 defaultFloor),
		rogue = defaultRogue,
		monsters = []
	}

voicemail :: Game -> [String]
voicemail = take 2 . messages

data Monster = Monster {
		symbol :: String,
		loc :: (Int, Int),
		impassible :: Bool,
		hp :: Int
	} deriving (Eq)

instance Show Monster where
	show = symbol

defaultFloor :: Monster
defaultFloor = Monster {
		symbol = " ",
		loc = (0, 0),
		impassible = False,
		hp = 0
	}

defaultWall :: Monster
defaultWall = Monster {
		symbol = "#",
		loc = (0, 0),
		impassible = True,
		hp = 0
	}

safehouseExit :: Monster
safehouseExit = Monster {
		symbol = "]",
		loc = (0, 0),
		impassible = False,
		hp = 0
	}

safehouseEntrance :: Monster
safehouseEntrance = Monster {
		symbol = "[",
		loc = (0, 0),
		impassible = False,
		hp = 0
	}

defaultRogue :: Monster
defaultRogue = Monster {
		symbol = "@",
		loc = (0, 0),
		impassible = True,
		hp = 10
	}

commonZombie :: Monster
commonZombie = Monster {
		symbol = "z",
		loc = (0, 0),
		impassible = True,
		hp = 1
	}

cellAt :: Game -> (Int, Int) -> Monster
cellAt g (x, y) = ((level g) !! y) !! x

thingAt :: Game -> (Int, Int) -> Monster
thingAt g (x, y) = case find (\e -> (((x, y) ==) . loc) e) (monsters g) of
	Just m -> m
	_ -> cellAt g (x, y)

attack :: Game -> Monster -> IO Game
attack g m = case (symbol m) of
	"#" -> return g { messages = "Immobile wall.":(voicemail g) }
	"z" -> do
		let m' = m { hp = hp m - 1 }
		let ms = delete m (monsters g)
		return g {
				monsters = if hp m' == 0 then ms else m':ms,
				messages = "You hit a zombie.":(voicemail g)
			}
	_ -> return g

move :: Game -> Key -> IO Game
move g KeyUp
	| y == 0 = return $ g { messages = "Edge of the world.":(voicemail g) }
	| impassible c = attack g c
	| otherwise = return $ g {
			rogue = r { loc = (x, y - 1) },
			messages = "You moved up!":(voicemail g)
		}
	where
		r = rogue g
		(x, y) = loc r
		c = thingAt g (x, y - 1)

move g KeyDown
	| y == (height g) - 1 = return $ g { messages = "Edge of the world.":(voicemail g) }
	| impassible c = attack g c
	| otherwise = return $ g {
			rogue = r { loc = (x, y + 1) },
			messages = "You moved down!":(voicemail g)
		}
	where
		r = rogue g
		(x, y) = loc r
		c = thingAt g (x, y + 1)

move g KeyRight
	| x == (width g) - 1 = return $ g { messages = "Edge of the world.":(voicemail g) }
	| impassible c = attack g c
	| otherwise = return $ g {
			rogue = r { loc = (x + 1, y) },
			messages = "You moved right!":(voicemail g)
		}
	where
		r = rogue g
		(x, y) = loc r
		c = thingAt g (x + 1, y)

move g KeyLeft
	| x == 0 = return $ g { messages = "Edge of the world.":(voicemail g) }
	| impassible c = attack g c
	| otherwise = return $ g {
			rogue = r { loc = (x - 1, y) },
			messages = "You moved left!":(voicemail g)
		}
	where
		r = rogue g
		(x, y) = loc r
		c = thingAt g (x - 1, y)

messageSpace :: Int
messageSpace = 3

blotMessages :: [String] -> Int -> IO ()
blotMessages [] _ = return ()
blotMessages (m:ms) row = do
	moveCursor 0 row
	hCenterString m
	blotMessages ms (row - 1)

blotLevel :: [[Monster]] -> IO ()
blotLevel lev = do
	moveCursor 0 0
	(blotString . join "\n" . (map (join "" . (map show)))) lev

blotMonster :: Monster -> IO ()
blotMonster m = do
	let (x, y) = loc m
	moveCursor x y
	blotString $ show m

win :: Game -> Bool
win g = (("[" ==) . symbol . cellAt g . loc . rogue) g

lose :: Game -> Bool
lose = (0 ==) . hp . rogue

blotRecap :: Game -> String -> IO ()
blotRecap g s = do
	clearScreen
	vCenterString $ s ++ " Play again (y/n)?"

	k <- getKey

	case k of
		KeyY -> restart
		KeyN -> return ()
		_ -> blotRecap g s

loop :: Game -> IO ()
loop g = do
	case (win g, lose g) of
		(True, _) -> blotRecap g "You're safe for now."
		(_, True) -> blotRecap g "You were overwhelmed."
		_ -> do
			blotLevel $ level g

			mapM blotMonster (monsters g)

			blotMonster $ rogue g

			-- Clear messages
			blotMessages (replicate 3 $ join "" $ replicate (width g) " ") (height g + messageSpace - 1)

			blotMessages (reverse $ messages g) (height g + messageSpace - 1)

			k <- getKey

			when (k `notElem` [KeyEscape, KeyQ])
				(do
					g' <- if k `elem` [KeyUp, KeyDown, KeyRight, KeyLeft] then
							move g k
						else
							return g

					loop g')

generateRow :: Int -> IO [Monster]
generateRow w = replicateM w (pick (defaultWall:(replicate 10 defaultFloor)))

generateSafehouseRow :: Int -> IO [Monster]
generateSafehouseRow w = do
	cells <- replicateM (w - 2) (pick (defaultWall:(replicate 10 defaultFloor)))

	return $ safehouseEntrance:(cells ++ [safehouseExit])

generateLevel :: Int -> Int -> IO [[Monster]]
generateLevel w h = do
	as <- replicateM ((h - 1) `div` 2) (generateRow w)
	b <- generateSafehouseRow w
	cs <- replicateM ((h - 1) `div` 2) (generateRow w)

	return $ as ++ (b:cs)

commonZombies :: Game -> Int
commonZombies g = width g `div` 10

generateMonsters :: Game -> [Monster] -> IO Game
generateMonsters g [] = return g
generateMonsters g (m:ms) = do
	let r = (loc . rogue) g

	x <- pick [0 .. (width g - 1)]
	y <- pick [0 .. (height g - 1)]

	if r == (x, y) then do
		generateMonsters g (m:ms)
	else do
		let c = cellAt g (x, y)

		case symbol c of
			" " -> do
				let placedMonsters = monsters g
				let locs = map loc placedMonsters

				if (x, y) `elem` locs then do
					generateMonsters g (m:ms)
				else do
					let m' = m { loc = (x, y) }
					let g' = g { monsters = m':placedMonsters }
					generateMonsters g' ms
			_ -> generateMonsters g (m:ms)

restart :: IO ()
restart = do
	w <- getWidth
	h <- getHeight

	-- Reserve space for messages
	let h' = h - messageSpace

	lev <- generateLevel w h'
	let g = defaultGame {
			width = w,
			height = h',
			level = lev,
			rogue = defaultRogue {
					loc = (w - 1, h' `div` 2)
				}
		}

	g' <- generateMonsters g (replicate (commonZombies g) commonZombie)

	loop g'

main :: IO ()
main = do
	startCharm

	restart

	endCharm