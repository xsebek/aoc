-- |
-- Module      : Day16
-- Description : Solution to AOC 2021 Day 16: Packet Decoder
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2021/day/16>
module Day16 where

import Data.Bifunctor (first)
import Data.Char (digitToInt, isHexDigit)
import Data.Foldable (fold)
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe)
import Data.Monoid (Sum (Sum, getSum))
import Numeric.Optics
import Optics

-- | Solution to Day 16.
main16 :: FilePath -> IO ()
main16 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input

type Hex = String
type Binary = String

parse :: Hex -> Packet
parse = noLeftover . packetRest . toBinary . filter isHexDigit
 where
  noLeftover (p, r) = if all (== '0') r then p else error $ "Leftover binary: " <> r

toBinary :: Hex -> Binary
toBinary = concatMap (lpad 4 . review binary . digitToInt)
 where
  lpad m xs = replicate (m - length xs) '0' ++ xs

type Version = Int
type TypeID = Int
type Header = (Version, TypeID)

data Packet
  = Literal {header :: Header, value :: Integer}
  | Operator {header :: Header, packets :: [Packet]}
  deriving (Eq, Ord, Show)

data Length = BitLength Int | PackLength Int
  deriving (Eq, Ord, Show)

b2i :: Integral a => Binary -> a
b2i = fromMaybe (error "Not binary") . preview binary

-- >>> headerBody example2021
-- ((6,4),"101111111000101000")
headerBody :: Binary -> (Header, Binary)
headerBody s = (over both b2i (v, t), b)
 where
  (v, (t, b)) = splitAt 3 <$> splitAt 3 s

-- >>> literalRest <$> headerBody example2021
-- ((6,4),(2021,"000"))
literalRest :: Binary -> (Integer, Binary)
literalRest = go 0
 where
  go :: Integer -> Binary -> (Integer, Binary)
  go i = \case
    [] -> (i, "")
    c : s ->
      let (d, r) = splitAt 4 s
          i4 = i * 2 ^ (4 :: Int)
          n = i4 + b2i d
       in case c of
            '0' -> (n, r)
            '1' -> go n r
            _ -> error $ "Not a bit: " <> show c

-- | Get the length of operator packet list along with rest of binary string.
--
-- >>> lengthBody $ drop 6 example6__10_20
-- (BitLength 27,"1101000101001010010001001000000000")
-- >>> lengthBody $ drop 6 example3__1_2_3
-- (PackLength 3,"01010000001100100000100011000001100000")
lengthBody :: Binary -> (Length, Binary)
lengthBody bs = case listToMaybe bs of
  Just '0' -> first (BitLength . b2i) . splitAt 15 $ tail bs
  Just '1' -> first (PackLength . b2i) . splitAt 11 $ tail bs
  mc -> error $ "Not a bit: " <> show mc

-- | Get packet along with rest of binary string.
--
-- >>> packetRest example2021
-- (Literal {header = (6,4), value = 2021},"000")
-- >>> packetRest example6__10_20
-- (Operator {header = (1,6), packets = [Literal {header = (6,4), value = 10},Literal {header = (2,4), value = 20}]},"0000000")
-- >>> packetRest example3__1_2_3
-- (Operator {header = (7,3), packets = [Literal {header = (2,4), value = 1},Literal {header = (4,4), value = 2},Literal {header = (1,4), value = 3}]},"00000")
packetRest :: Binary -> (Packet, Binary)
packetRest bs = case t of
  4 -> first (Literal h) $ literalRest b
  _ ->
    let (l, r) = lengthBody b
     in first (Operator h) $ case l of
          BitLength bl -> first packetList' $ splitAt bl r
          PackLength pl -> packetList pl r
 where
  (h@(_v, t), b) = headerBody bs

packetList' :: Binary -> [Packet]
packetList' = fst . packetList maxBound

packetList :: Int -> Binary -> ([Packet], Binary)
packetList n = keepLast . take n . takeWhileJust . tail . iterate nextMPacket . Just . (undefined,)
 where
  nextMPacket :: Maybe (a, Binary) -> Maybe (Packet, Binary)
  nextMPacket mab = mab >>= fmap packetRest . (\bi -> if all (== '0') bi then Nothing else Just bi) . snd

foldPacketHeaders :: Monoid m => (Header -> m) -> Packet -> m
foldPacketHeaders f = \case
  Literal h _ -> f h
  Operator h pas -> f h <> fold (foldPacketHeaders f <$> pas)

-- >>> solve1 . parse <$> examples
-- [6,9,14,16,12,23,31]
solve1 :: Packet -> Int
solve1 = getSum . foldPacketHeaders (\(v, _t) -> Sum v)

foldPackets :: (TypeID -> ([Integer] -> Integer)) -> Packet -> Integer
foldPackets f = \case
  Literal _ v -> v
  Operator (_v, t) pas -> f t (foldPackets f <$> pas)

-- >>> solve2 . parse <$> examples
-- [2021,1,3,15,46,46,54]
solve2 :: Packet -> Integer
solve2 = foldPackets $ \case
  0 -> sum
  1 -> product
  2 -> minimum
  3 -> maximum
  5 -> binOp (>)
  6 -> binOp (<)
  7 -> binOp (==)
  i -> error $ "Weird TypeID: " <> show i
 where
  binOp o [x, y] = fromIntegral . fromEnum $ x `o` y
  binOp _ xs = error $ "Weird args for binary operator: " <> show xs

-- | Literal 2021.
--
-- > 110100101111111000101000
-- > VVVTTTAAAAABBBBBCCCCC
example2021 :: Binary
example2021 = toBinary $ head examples

-- | Operator 6 with literals 10 and 20.
--
-- > 00111000000000000110111101000101001010010001001000000000
-- > VVVTTTILLLLLLLLLLLLLLLAAAAAAAAAAABBBBBBBBBBBBBBBB
example6__10_20 :: Binary
example6__10_20 = toBinary $ examples !! 1

-- | Operator 3 with literals 1,2,3.
--
-- > 11101110000000001101010000001100100000100011000001100000
-- > VVVTTTILLLLLLLLLLLAAAAAAAAAAABBBBBBBBBBBCCCCCCCCCCC
example3__1_2_3 :: Binary
example3__1_2_3 = toBinary $ examples !! 2

examples :: [Hex]
examples =
  [ "D2FE28" -- 2021
  , "38006F45291200" -- 10,20
  , "EE00D40C823060" -- 1,2,3
  , "8A004A801A8002F478"
  , "620080001611562C8802118E34"
  , "C0015000016115A2E0802F182340"
  , "A0016C880162017C3686B18A3D4780"
  ]

keepLast :: [(a, b)] -> ([a], b)
keepLast = fmap last . unzip

takeWhileJust :: [Maybe a] -> [a]
takeWhileJust = catMaybes . takeWhile isJust

data PacketT
  = PL Int Integer
  | Psum Int [PacketT]
  | Pprod Int [PacketT]
  | Pmin Int [PacketT]
  | Pmax Int [PacketT]
  | Pge Int [PacketT]
  | Ple Int [PacketT]
  | Peq Int [PacketT]
  deriving (Eq, Ord, Show)

prettyPackets :: Packet -> PacketT
prettyPackets = \case
  Literal (v, _t) val -> PL v val
  Operator (v, t) pas -> f t v (prettyPackets <$> pas)
 where
  f = \case
    0 -> Psum
    1 -> Pprod
    2 -> Pmin
    3 -> Pmax
    5 -> Pge
    6 -> Ple
    7 -> Peq
    i -> error $ "Weird TypeID: " <> show i
