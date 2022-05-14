module Common.Styling (
  StyleBlock,

  reset,
  bold,
  dim,
  italic,
  underline,
  inverse,
  hidden,
  strikethrough,

  black,
  red,
  green,
  yellow,
  blue,
  magenta,
  cyan,
  white,
  gray,

  bgBlack,
  bgRed,
  bgGreen,
  bgYellow,
  bgBlue,
  bgMagenta,
  bgCyan,
  bgWhite,

  styleString
) where

-- | Specifies a style of ANSI escape sequences for terminal formatting.
data StyleBlock = StyleBlock {
  open :: String,
  close :: String
} deriving (Show, Eq)

-- | Initializes an ANSI escape sequence.
initStyle :: Int -> Int -> StyleBlock
initStyle open close = StyleBlock {
  open = "\x1b[" ++ show open ++ "m",
  close = "\x1b[" ++ show close ++ "m"
}

reset :: StyleBlock
reset = initStyle 0 0

bold :: StyleBlock
bold = initStyle 1 22

dim :: StyleBlock
dim = initStyle 2 22

italic :: StyleBlock
italic = initStyle 3 23

underline :: StyleBlock
underline = initStyle 4 24

inverse :: StyleBlock
inverse = initStyle 7 27

hidden :: StyleBlock
hidden = initStyle 8 28

strikethrough :: StyleBlock
strikethrough = initStyle 9 29

black :: StyleBlock
black = initStyle 30 39

red :: StyleBlock
red = initStyle 31 39

green :: StyleBlock
green = initStyle 32 39

yellow :: StyleBlock
yellow = initStyle 33 39

blue :: StyleBlock
blue = initStyle 34 39

magenta :: StyleBlock
magenta = initStyle 35 39

cyan :: StyleBlock
cyan = initStyle 36 39

white :: StyleBlock
white = initStyle 37 39

gray :: StyleBlock
gray = initStyle 90 39

bgBlack :: StyleBlock
bgBlack = initStyle 40 49

bgRed :: StyleBlock
bgRed = initStyle 41 49

bgGreen :: StyleBlock
bgGreen = initStyle 42 49

bgYellow :: StyleBlock
bgYellow = initStyle 43 49

bgBlue :: StyleBlock
bgBlue = initStyle 44 49

bgMagenta :: StyleBlock
bgMagenta = initStyle 45 49

bgCyan :: StyleBlock
bgCyan = initStyle 46 49

bgWhite :: StyleBlock
bgWhite = initStyle 47 49

-- | Applies a set of styles to a given strings.
styleString :: String -> [StyleBlock] -> String
styleString str [] = str
styleString str (s:ss) = open s ++ styleString str ss ++ close s
