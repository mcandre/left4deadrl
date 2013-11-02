{-# LANGUAGE ForeignFunctionInterface #-}

module HsCharm (
  charmVersion,

  getWidth,
  getHeight,

  cursorOff,
  cursorOn,

  echoOff,
  echoOn,

  rawOn,
  rawOff,

  getCursor,
  moveCursor,
  blotChar,
  blotString,
  hCenterString,
  vCenterString,

  clearScreen,

  handleSignal,

  startCharm,
  endCharm,

  getKey,
  Key(
    KeyBackspace,
    KeyTab,
    KeyNewline,

    KeySpace,
    KeyExclamation,
    KeyDoubleQuote,
    KeyHash,
    KeyDollar,
    KeyPercent,
    KeyAmpersand,
    KeySingleQuote,
    KeyLeftParen,
    KeyRightParen,
    KeyAsterisk,
    KeyPlus,
    KeyComma,
    KeyMinus,
    KeyPeriod,
    KeySlash,

    KeyZero,
    KeyOne,
    KeyTwo,
    KeyThree,
    KeyFour,
    KeyFive,
    KeySix,
    KeySeven,
    KeyEight,
    KeyNine,

    KeyColon,
    KeySemicolon,
    KeyLessThan,
    KeyEquals,
    KeyGreaterThan,
    KeyQuestion,
    KeyAt,

    KeyCapitalA,
    KeyCapitalB,
    KeyCapitalC,
    KeyCapitalD,
    KeyCapitalE,
    KeyCapitalF,
    KeyCapitalG,
    KeyCapitalH,
    KeyCapitalI,
    KeyCapitalJ,
    KeyCapitalK,
    KeyCapitalL,
    KeyCapitalM,
    KeyCapitalN,
    KeyCapitalO,
    KeyCapitalP,
    KeyCapitalQ,
    KeyCapitalR,
    KeyCapitalS,
    KeyCapitalT,
    KeyCapitalU,
    KeyCapitalV,
    KeyCapitalW,
    KeyCapitalX,
    KeyCapitalY,
    KeyCapitalZ,

    KeyLeftBracket,
    KeyBackslash,
    KeyRightBracket,
    KeyCaret,
    KeyUnderscore,
    KeyBacktick,

    KeyA,
    KeyB,
    KeyC,
    KeyD,
    KeyE,
    KeyF,
    KeyG,
    KeyH,
    KeyI,
    KeyJ,
    KeyK,
    KeyL,
    KeyM,
    KeyN,
    KeyO,
    KeyP,
    KeyQ,
    KeyR,
    KeyS,
    KeyT,
    KeyU,
    KeyV,
    KeyW,
    KeyX,
    KeyY,
    KeyZ,

    KeyLeftBrace,
    KeyPipe,
    KeyRightBrace,
    KeyTilde,

    KeyUp,
    KeyDown,
    KeyRight,
    KeyLeft,

    KeyEscape,
    KeyUnknown
    )
  ) where

import Foreign.C

charmVersion :: String
charmVersion = "0.0.1"

foreign import ccall "charm.h get_width" getWidth :: IO Int
foreign import ccall "charm.h get_height" getHeight :: IO Int

foreign import ccall "charm.h cursor_off" cursorOff :: IO ()
foreign import ccall "charm.h cursor_on" cursorOn :: IO ()

foreign import ccall "charm.h echo_off" echoOff :: IO ()
foreign import ccall "charm.h echo_on" echoOn :: IO ()

foreign import ccall "charm.h raw_on" rawOn :: IO ()
foreign import ccall "charm.h raw_off" rawOff :: IO ()

foreign import ccall "charm.h get_x" getX :: IO Int
foreign import ccall "charm.h get_y" getY :: IO Int

getCursor :: IO (Int, Int)
getCursor = do
  x <- getX
  y <- getY

  return (x, y)

foreign import ccall "charm.h move_cursor" moveCursor :: Int -> Int -> IO ()
foreign import ccall "charm.h blot_char" blotChar :: Char -> IO ()

foreign import ccall "charm.h blot_string" blotString' :: CString -> IO ()

blotString :: String -> IO ()
blotString s = do
  s' <- newCString s
  blotString' s'

foreign import ccall "charm.h hcenter_string" hCenterString' :: CString -> IO ()

hCenterString :: String -> IO ()
hCenterString s = do
  s' <- newCString s
  hCenterString' s'

foreign import ccall "charm.h vcenter_string" vCenterString' :: CString -> IO ()

vCenterString :: String -> IO ()
vCenterString s = do
  s' <- newCString s
  vCenterString' s'

foreign import ccall "charm.h clear_screen" clearScreen :: IO ()

foreign import ccall "charm.h handle_signal" handleSignal :: Int -> IO ()

foreign import ccall "charm.h start_charm" startCharm :: IO ()
foreign import ccall "charm.h end_charm" endCharm :: IO ()

foreign import ccall "charm.h get_key" getKey' :: IO Int

data Key
  = KeyBackspace
  | KeyTab
  | KeyNewline

  | KeySpace
  | KeyExclamation
  | KeyDoubleQuote
  | KeyHash
  | KeyDollar
  | KeyPercent
  | KeyAmpersand
  | KeySingleQuote
  | KeyLeftParen
  | KeyRightParen
  | KeyAsterisk
  | KeyPlus
  | KeyComma
  | KeyMinus
  | KeyPeriod
  | KeySlash

  | KeyZero
  | KeyOne
  | KeyTwo
  | KeyThree
  | KeyFour
  | KeyFive
  | KeySix
  | KeySeven
  | KeyEight
  | KeyNine

  | KeyColon
  | KeySemicolon
  | KeyLessThan
  | KeyEquals
  | KeyGreaterThan
  | KeyQuestion
  | KeyAt

  | KeyCapitalA
  | KeyCapitalB
  | KeyCapitalC
  | KeyCapitalD
  | KeyCapitalE
  | KeyCapitalF
  | KeyCapitalG
  | KeyCapitalH
  | KeyCapitalI
  | KeyCapitalJ
  | KeyCapitalK
  | KeyCapitalL
  | KeyCapitalM
  | KeyCapitalN
  | KeyCapitalO
  | KeyCapitalP
  | KeyCapitalQ
  | KeyCapitalR
  | KeyCapitalS
  | KeyCapitalT
  | KeyCapitalU
  | KeyCapitalV
  | KeyCapitalW
  | KeyCapitalX
  | KeyCapitalY
  | KeyCapitalZ

  | KeyLeftBracket
  | KeyBackslash
  | KeyRightBracket
  | KeyCaret
  | KeyUnderscore
  | KeyBacktick

  | KeyA
  | KeyB
  | KeyC
  | KeyD
  | KeyE
  | KeyF
  | KeyG
  | KeyH
  | KeyI
  | KeyJ
  | KeyK
  | KeyL
  | KeyM
  | KeyN
  | KeyO
  | KeyP
  | KeyQ
  | KeyR
  | KeyS
  | KeyT
  | KeyU
  | KeyV
  | KeyW
  | KeyX
  | KeyY
  | KeyZ

  | KeyLeftBrace
  | KeyPipe
  | KeyRightBrace
  | KeyTilde
  | KeyUp
  | KeyDown
  | KeyRight
  | KeyLeft

  | KeyEscape
  | KeyUnknown

  deriving (Eq, Ord, Enum, Show)

getKey :: IO Key
getKey = do
  k <- getKey'
  return (toEnum k :: Key)
