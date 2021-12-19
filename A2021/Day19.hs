{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Day19
-- Description : Solution to AOC 2021 Day 19: Beacon Scanner
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2021/day/19>
module Day19 where

import Data.Foldable (find)
import Data.IntMap (IntMap, (!))
import Data.IntMap qualified as Map
import Data.List (intercalate)
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import GHC.Exts (IsList (Item, fromList, toList))
import Linear
import Parser

-- | Solution to Day 19.
main19 :: FilePath -> IO ()
main19 f = do
  input <- parseFile parser f
  print $ solve1 input
  print $ solve2 input

type P3 = V3 Int
type M3 = M33 Int
type Beacons = Set P3

parser :: Parser (IntMap Beacons)
parser = Map.fromList <$> parseScanner `sepEndBy` eol
 where
  parseScanner :: Parser (Int, Beacons)
  parseScanner = (,) <$> (string "--- scanner " *> decimal <* string " ---" <* eol) <*> parseBeacons
  parseBeacons :: Parser Beacons
  parseBeacons = Set.fromList <$> ((V3 <$> num' <*> num' <*> num) `sepEndBy` eol)
  num = signed mempty decimal
  num' = num <* char ','

-- | Find overlap between scanned beacons of size at least N.
findOverlapOf :: Int -> Beacons -> Beacons -> Maybe ((M3, P3), Beacons)
findOverlapOf i l r = find ((i <=) . Set.size . snd) overlaps
 where
  overlaps :: [((M3, P3), Beacons)]
  overlaps =
    [ ((m, v), Set.intersection l mr)
    | m <- Set.toList rotations3D
    , let rr = Set.map (m !*) r
    , v <- (-) <$> Set.toList l <*> Set.toList rr
    , let mr = Set.map (v +) rr
    ]

findAll0 :: Int -> IntMap Beacons -> IntMap (P3, Beacons)
findAll0 i scanners = go (Map.delete 0 scanners) (Map.singleton 0 (v0, scanners ! 0))
 where
  v0 :: P3
  v0 = V3 0 0 0
  go :: IntMap Beacons -> IntMap (P3, Beacons) -> IntMap (P3, Beacons)
  go leftScans known =
    let newKnown = step leftScans known
        newLeft = leftScans Map.\\ known
     in if null leftScans || Map.size known == Map.size newKnown
          then newKnown
          else go newLeft newKnown
  step :: IntMap Beacons -> IntMap (P3, Beacons) -> IntMap (P3, Beacons)
  step leftScans known =
    Map.union known . Map.fromList . catMaybes $
      [ moveB <$> findOverlapOf i kb b
      | (_, kb) <- Map.elems known
      , (l, b) <- Map.toList leftScans
      , let moveB (m,_int) = (l, (move m v0,) $ Set.map (move m) b)
      ]

move :: (M3, P3) -> P3 -> P3
move (m,v) p = v + (m !* p)

-- >>> solve1 <$> example
-- 79
solve1 :: IntMap Beacons -> Int
solve1 scanners = length . Set.unions . map snd $ Map.elems all12
 where
  all12 = findAll0 12 scanners

-- >>> solve2 example
-- 3621
solve2 :: IntMap Beacons -> Int
solve2 scanners = maximum dists
 where
  dist a b = sum . fmap abs $ a - b
  dists = dist <$> allS <*> allS
  allS = map fst $ Map.elems all12
  all12 = findAll0 12 scanners

instance IsList (V3 a) where
  type Item (V3 a) = a
  fromList [x, y, z] = V3 x y z
  fromList _ = error "Bad length of V3 list"
  toList (V3 x y z) = [x, y, z]

-- | Example with same beacons rotated - last one has nice [1,1,1] vector, so it's 0.
--
-- >>> let allMove0 = all ((== V3 0 0 0) . snd . fst . fromJust)
-- >>> (\e -> allMove0 $ findOverlapOf 6 (e Map.! 0) <$> Map.delete 0 e) <$> exampleR
-- True
-- >>> let printM = putStrLn . pretty . fst . fst . fromJust
-- >>> (\e -> mapM_ printM $ findOverlapOf 6 (e Map.! 0) <$> Map.delete 0 e) =<< exampleR
-- [ [-1,0,0]
-- , [0,0,-1]
-- , [0,-1,0]
-- ]
-- [ [0,0,1]
-- , [0,1,0]
-- , [-1,0,0]
-- ]
-- [ [0,0,1]
-- , [0,-1,0]
-- , [1,0,0]
-- ]
-- [ [0,0,-1]
-- , [-1,0,0]
-- , [0,1,0]
-- ]
exampleR :: IO (IntMap Beacons)
exampleR =
  parseExample parser . T.unlines $
    [ "--- scanner 1 ---"
    , "-1,-1,1"
    , "-2,-2,2"
    , "-3,-3,3"
    , "-2,-3,1"
    , "5,6,-4"
    , "8,0,7"
    , ""
    , "--- scanner 2 ---"
    , "1,-1,1"
    , "2,-2,2"
    , "3,-3,3"
    , "2,-1,3"
    , "-5,4,-6"
    , "-8,-7,0"
    , ""
    , "--- scanner 3 ---"
    , "-1,-1,-1"
    , "-2,-2,-2"
    , "-3,-3,-3"
    , "-1,-3,-2"
    , "4,6,5"
    , "-7,0,8"
    , ""
    , "--- scanner 4 ---"
    , "1,1,-1"
    , "2,2,-2"
    , "3,3,-3"
    , "1,3,-2"
    , "-4,-6,5"
    , "7,0,8"
    , ""
    , "--- scanner 0 ---"
    , "1,1,1"
    , "2,2,2"
    , "3,3,3"
    , "3,1,2"
    , "-6,-4,-5"
    , "0,7,-8"
    ]

example :: IO (IntMap Beacons)
example =
  parseExample parser . T.unlines $
    [ "--- scanner 0 ---"
    , "404,-588,-901"
    , "528,-643,409"
    , "-838,591,734"
    , "390,-675,-793"
    , "-537,-823,-458"
    , "-485,-357,347"
    , "-345,-311,381"
    , "-661,-816,-575"
    , "-876,649,763"
    , "-618,-824,-621"
    , "553,345,-567"
    , "474,580,667"
    , "-447,-329,318"
    , "-584,868,-557"
    , "544,-627,-890"
    , "564,392,-477"
    , "455,729,728"
    , "-892,524,684"
    , "-689,845,-530"
    , "423,-701,434"
    , "7,-33,-71"
    , "630,319,-379"
    , "443,580,662"
    , "-789,900,-551"
    , "459,-707,401"
    , ""
    , "--- scanner 1 ---"
    , "686,422,578"
    , "605,423,415"
    , "515,917,-361"
    , "-336,658,858"
    , "95,138,22"
    , "-476,619,847"
    , "-340,-569,-846"
    , "567,-361,727"
    , "-460,603,-452"
    , "669,-402,600"
    , "729,430,532"
    , "-500,-761,534"
    , "-322,571,750"
    , "-466,-666,-811"
    , "-429,-592,574"
    , "-355,545,-477"
    , "703,-491,-529"
    , "-328,-685,520"
    , "413,935,-424"
    , "-391,539,-444"
    , "586,-435,557"
    , "-364,-763,-893"
    , "807,-499,-711"
    , "755,-354,-619"
    , "553,889,-390"
    , ""
    , "--- scanner 2 ---"
    , "649,640,665"
    , "682,-795,504"
    , "-784,533,-524"
    , "-644,584,-595"
    , "-588,-843,648"
    , "-30,6,44"
    , "-674,560,763"
    , "500,723,-460"
    , "609,671,-379"
    , "-555,-800,653"
    , "-675,-892,-343"
    , "697,-426,-610"
    , "578,704,681"
    , "493,664,-388"
    , "-671,-858,530"
    , "-667,343,800"
    , "571,-461,-707"
    , "-138,-166,112"
    , "-889,563,-600"
    , "646,-828,498"
    , "640,759,510"
    , "-630,509,768"
    , "-681,-892,-333"
    , "673,-379,-804"
    , "-742,-814,-386"
    , "577,-820,562"
    , ""
    , "--- scanner 3 ---"
    , "-589,542,597"
    , "605,-692,669"
    , "-500,565,-823"
    , "-660,373,557"
    , "-458,-679,-417"
    , "-488,449,543"
    , "-626,468,-788"
    , "338,-750,-386"
    , "528,-832,-391"
    , "562,-778,733"
    , "-938,-730,414"
    , "543,643,-506"
    , "-524,371,-870"
    , "407,773,750"
    , "-104,29,83"
    , "378,-903,-323"
    , "-778,-728,485"
    , "426,699,580"
    , "-438,-605,-362"
    , "-469,-447,-387"
    , "509,732,623"
    , "647,635,-688"
    , "-868,-804,481"
    , "614,-800,639"
    , "595,780,-596"
    , ""
    , "--- scanner 4 ---"
    , "727,592,562"
    , "-293,-554,779"
    , "441,611,-461"
    , "-714,465,-776"
    , "-743,427,-804"
    , "-660,-479,-426"
    , "832,-632,460"
    , "927,-485,-438"
    , "408,393,-506"
    , "466,436,-512"
    , "110,16,151"
    , "-258,-428,682"
    , "-393,719,612"
    , "-211,-452,876"
    , "808,-476,-593"
    , "-575,615,604"
    , "-485,667,467"
    , "-680,325,-822"
    , "-627,-443,-432"
    , "872,-547,-609"
    , "833,512,582"
    , "807,604,487"
    , "839,-516,451"
    , "891,-625,532"
    , "-652,-548,-490"
    , "30,-46,-14"
    ]

-- MATH SECTION

rotateX :: M3
rotateX =
  [ [1, 0, 0]
  , [0, 0, -1]
  , [0, 1, 0]
  ]

rotateY :: M3
rotateY =
  [ [0, 0, 1]
  , [0, 1, 0]
  , [-1, 0, 0]
  ]

rotateZ :: M3
rotateZ =
  [ [0, -1, 0]
  , [1, 0, 0]
  , [0, 0, 1]
  ]

identity3D :: M3
identity3D = identity

rotations3D :: Set M3
rotations3D = Set.fromList $ (\x y z -> x !*! y !*! z) <$> rxs <*> rys <*> rzs
 where
  rotations m = take 4 $ iterate (!*! m) identity3D
  [rxs, rys, rzs] = map rotations [rotateX, rotateY, rotateZ]

pretty3 :: P3 -> String
pretty3 = show . GHC.Exts.toList

pretty33 :: M3 -> String
pretty33 = (<> "\n]") . ("[ " <>) . intercalate "\n, " . map (show . GHC.Exts.toList) . GHC.Exts.toList
