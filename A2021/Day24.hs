{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- |
-- Module      : Day24
-- Description : Solution to AOC 2021 Day 24: Arithmetic Logic Unit
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2021/day/24>
module Day24 where

import Algorithm.Search qualified as Search
import Data.Char (toUpper)
import Data.Foldable (foldl')
import Data.Function (on, (&))
import Data.List (find)
import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))

-- | Solution to Day 24.
main24 :: FilePath -> IO ()
main24 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input

parse :: String -> [Op]
parse = map parseOp . lines
 where
  parseOp o = case words o of
    ["inp", [v]] -> Inp v
    [sb, [v], sevi] -> BinOp (read $ capitalise sb) v (parseVarOrInt sevi)
    ws -> error $ "Could not parse op: " <> show ws
  capitalise [] = []
  capitalise (h : t) = toUpper h : t

-- | Parse variable or integer.
--
-- >>> parseVarOrInt "z"
-- Left 'z'
-- >>> parseVarOrInt "-12"
-- Right (-12)
parseVarOrInt :: String -> Either Var Int
parseVarOrInt = \case
  [c] | 'a' <= c && c <= 'z' -> Left c
  w -> Right $ read w

-- | Variable names from 'a' to 'z'.
type Var = Char

data BinOp
  = -- | add a b - Add the value of a to the value of b, then store the result in variable a.
    Add
  | -- | mul a b - Multiply the value of a by the value of b, then store the result in variable a.
    Mul
  | -- | div a b - Divide the value of a by the value of b, truncate the result to an integer, then store the result in variable a.
    --
    -- Here, "truncate" means to round the value toward zero.
    Div
  | -- | mod a b - Divide the value of a by the value of b, then store the remainder in variable a.
    --
    -- This is also called the modulo operation.
    Mod
  | -- | eql a b - If the value of a and b are equal, then store the value 1 in variable a. Otherwise, store the value 0 in variable a.
    Eql
  deriving (Eq, Ord, Show, Read)

data Op
  = -- | inp a - Read an input value and write it to variable a.
    Inp Var
  | -- | <binary_operation> a b - Evaluate the operation on a and b, then store the result in variable a.
    BinOp BinOp Var (Either Var Int)
  deriving (Eq, Ord, Show, Read)

data ALUState = ALUState
  { vars :: Map Var Int
  , input :: [Int]
  }
  deriving (Eq, Show, Read)

emptyState :: ALUState
emptyState = ALUState Map.empty []

evalOp :: BinOp -> (Int -> Int -> Int)
evalOp = \case
  Add -> (+)
  Mul -> (*)
  Div -> div
  Mod -> mod
  Eql -> \i j -> fromEnum $ i == j

eval :: ALUState -> Op -> ALUState
eval s = \case
  Inp v -> case input s of
    [] -> error "inp: No more input specified!"
    (i : is) ->
      ALUState
        { vars = Map.insert v i s.vars
        , input = is
        }
  BinOp b v evi -> s{vars = Map.insert v res s.vars}
   where
    res = evalOp b iv iu
    iv = fromMaybe 0 $ Map.lookup v s.vars
    iu = case evi of
      Right x -> x
      Left u -> fromMaybe 0 $ Map.lookup u s.vars

evals :: ALUState -> [Op] -> ALUState
evals = foldl' eval

-- ----------------------------------------------------------------------------
-- PART 1
-- ----------------------------------------------------------------------------

data Block = B {
      z26 :: Bool
    , addX :: Int
    , addY :: Int
  } deriving (Eq, Ord, Show)

-- >>> parseBlock inputPart
-- B {z26 = True, addX = -11, addY = 15}
parseBlock :: [Op] -> Block
parseBlock = \case
  [ Inp 'w'
    , BinOp Mul 'x' (Right 0)
    , BinOp Add 'x' (Left 'z')
    , BinOp Mod 'x' (Right 26)
    , BinOp Div 'z' (Right 26)
    , BinOp Add 'x' (Right addX)
    , BinOp Eql 'x' (Left 'w')
    , BinOp Eql 'x' (Right 0)
    , BinOp Mul 'y' (Right 0)
    , BinOp Add 'y' (Right 25)
    , BinOp Mul 'y' (Left 'x')
    , BinOp Add 'y' (Right 1)
    , BinOp Mul 'z' (Left 'y')
    , BinOp Mul 'y' (Right 0)
    , BinOp Add 'y' (Left 'w')
    , BinOp Add 'y' (Right addY)
    , BinOp Mul 'y' (Left 'x')
    , BinOp Add 'z' (Left 'y')
    ] -> B True addX addY
  [ Inp 'w'
    , BinOp Mul 'x' (Right 0)
    , BinOp Add 'x' (Left 'z')
    , BinOp Mod 'x' (Right 26)
    , BinOp Div 'z' (Right 1)
    , BinOp Add 'x' (Right addX)
    , BinOp Eql 'x' (Left 'w')
    , BinOp Eql 'x' (Right 0)
    , BinOp Mul 'y' (Right 0)
    , BinOp Add 'y' (Right 25)
    , BinOp Mul 'y' (Left 'x')
    , BinOp Add 'y' (Right 1)
    , BinOp Mul 'z' (Left 'y')
    , BinOp Mul 'y' (Right 0)
    , BinOp Add 'y' (Left 'w')
    , BinOp Add 'y' (Right addY)
    , BinOp Mul 'y' (Left 'x')
    , BinOp Add 'z' (Left 'y')
    ] -> B False addX addY
  b -> error $ "The block structure does not match:\n" <> show b

-- >>> zBlock (parseBlock inputPart) (9,20)
-- 0
zBlock :: Block -> (Int, Int) -> Int
zBlock (B zMod addX addY) (w, z0) = z3
  where
    x1 = z0 `mod` 26
    z1 = if zMod then z0 `div` 26 else z0 
    x2 = fromEnum $ (x1 + addX) /= w
    y1 = 25 * x2 + 1
    z2 = z1 * y1
    y2 = (w + addY) * x2
    z3 = z2 + y2

data MinMax = Min | Max deriving (Eq, Ord, Show, Enum)

-- | Execute blocks of code that have each exactly one input.
zOnDigits :: MinMax -> Int -> [Block] -> [([Int], Int)]
zOnDigits mm z = \case
  [] -> [([], z)]
  (ops : opss) ->
    [ (d : n, res)
    | d <- case mm of { Max -> [9,8..1]; Min -> [1..9]}
    , let ns = zBlock ops (d, z)
    , (n, res) <- zOnDigits mm ns opss
    ]

solve1 :: [Op] -> Int
solve1 = solveMinMaxForStart Max 0

-- >>> solveMinMaxForStart Max 20 inputPart
-- 9
solveMinMaxForStart :: MinMax -> Int -> [Op] -> Int
solveMinMaxForStart mm zStart ops = maybe 0 (toI . fst) $ find ((==0) . snd) res
 where
  res = zOnDigits mm zStart (map parseBlock $ splitOnInput ops)
  toI = foldl' (\b a -> b * 10 + a) 0

checkZ :: Int -> ALUState -> Bool
checkZ z s = Map.lookup 'z' s.vars == Just z

data S = S
  { rIndex :: Int
  , zStart :: Int
  , digits :: [Int]
  }
  deriving (Eq, Show)

instance Ord S where
  compare :: S -> S -> Ordering
  compare = compare `on` (\s -> (s.rIndex, Down s.digits))

-- >>> reverseSearch example3x
-- Just [S {rIndex = 0, zStart = 3, digits = [9]},S {rIndex = 1, zStart = 100, digits = [3,9]}]
-- >>> reverseSearch inputPart
-- Just [S {rIndex = 0, zStart = 20, digits = [9]}]
reverseSearch :: [Op] -> Maybe [S]
reverseSearch ops = Search.dfs search ((== inputLen) . length . digits) (S (-1) 0 [])
 where
  opss = splitOnInput ops
  inputLen = length opss
  oi i = reverse opss !! i
  -- given the input numbers, z is likely 0-50 or close
  zRange = [-100..100]
  searchZ :: Int -> Int -> Int -> [S]
  searchZ i endZ startZ =
    evalsZ emptyState [oi i] startZ
    & filter (checkZ endZ . snd)
    & map (S i startZ . fst)
  search :: S -> [S]
  search = \case
    S (-1) finalZ [] -> concatMap (searchZ 0 finalZ) zRange
    S i zStartNext dgs -> [s{digits = s.digits <> dgs}
      | z <- if i + 1 == inputLen then [0] else zRange
      , s <- searchZ (i + 1) zStartNext z]

-- >>> splitOnInput example3x
-- [[Inp 'z'],[Inp 'x',BinOp Mul 'z' (Right 3),BinOp Eql 'z' (Left 'x')]]
splitOnInput :: [Op] -> [[Op]]
splitOnInput ops = (input <> beforeInp) : if null rest then [] else splitOnInput rest
 where
  isInp = \case
    Inp _v -> True
    _ -> False
  (input, ops') = case ops of
    (Inp v : os) -> ([Inp v], os)
    _os -> ([], ops)
  (beforeInp, rest) = break isInp ops'

-- | Valid MONAD digits (9 to 1).
ds :: [Int]
ds = [9, 8 .. 1]

-- | Execute blocks of code that have each exactly one input.
evalsOnDigits :: ALUState -> [[Op]] -> [([Int], ALUState)]
evalsOnDigits s = \case
  [] -> [([], s)]
  (ops : opss) ->
    [ (d : n, res)
    | d <- ds
    , let ns = evals (s{input = [d]}) ops
    , (n, res) <- evalsOnDigits ns opss
    ]

-- ----------------------------------------------------------------------------
-- PART 2
-- ----------------------------------------------------------------------------

solve2 :: [Op] -> Int
solve2 = solveMinMaxForStart Min 0
    
-- ----------------------------------------------------------------------------
-- EXAMPLES
-- ----------------------------------------------------------------------------

-- | Execute blocks of code that have each exactly one input, BUT SET Z.
evalsZ :: ALUState -> [[Op]] -> Int -> [([Int], ALUState)]
evalsZ s ops z = evalsOnDigits (s{vars = Map.insert 'z' z s.vars}) ops

-- >>> evals (startingState 9) exampleNeg
-- ALUState {vars = fromList [('x',-9)], input = []}
exampleNeg :: [Op]
exampleNeg =
  parse . unlines $
    [ "inp x"
    , "mul x -1"
    ]

-- >>> evals (startingState 39) example3x
-- ALUState {vars = fromList [('x',9),('z',1)], input = []}
example3x :: [Op]
example3x =
  parse . unlines $
    [ "inp z"
    , "inp x"
    , "mul z 3"
    , "eql z x"
    , "eql z 0"
    ]

{-
inp w
---------------------------
z0 = z `div` 26
x0 = (z `mod` 26) - 11 != w
---------------------------
y0 = 25 * x0 + 1
z1 = z0 * y0
---------------------------
y1 = (w + 15) * x0
---------------------------
z2 = z1 + y1

z2 = (z `div` 26) * (25 * ((z `mod` 26) - 11 != w) + 1) + (w + 15) * ((z `mod` 26) - 11 != w)

>>> evals (startingState 1) inputPart
ALUState {vars = fromList [('w',1),('x',1),('y',16),('z',16)], input = []}
-}
inputPart :: [Op]
inputPart =
  parse . unlines $
    [ "inp w"
    , "mul x 0"
    , "add x z"
    , "mod x 26"
    , "div z 26"
    , "add x -11"
    , "eql x w"
    , "eql x 0"
    , "mul y 0"
    , "add y 25"
    , "mul y x"
    , "add y 1"
    , "mul z y"
    , "mul y 0"
    , "add y w"
    , "add y 15"
    , "mul y x"
    , "add z y"
    ]

-- >>> toDigits 123
-- [1,2,3]
toDigits :: Int -> [Int]
toDigits i =
  let (d, m) = i `divMod` 10
   in if d == 0 then [m] else toDigits d ++ [m]

startingState :: Int -> ALUState
startingState i = ALUState Map.empty (toDigits i)
