{-# LANGUAGE OverloadedRecordDot #-}

-- |
-- Module      : Day07
-- Description : Solution to AOC 2022 Day 07: ?????
-- Maintainer  : <xsebek@fi.muni.cz>
--
-- <https://adventofcode.com/2022/day/07>
module Day07 where

import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Tree (Tree)
import Data.Tree qualified as Tree
import qualified Data.List as List
import Data.Function ((&))

-- | Solution to Day 07.
main07 :: FilePath -> IO ()
main07 f = do
  input <- parse <$> readFile f
  print $ solve1 input
  print $ solve2 input

parse :: String -> [Line]
parse = map (readLine . words) . lines
 where
  readLine = \case
    ["$", "ls"] -> LS
    ["$", "cd", dir] -> CD dir
    ["dir", dir] -> Directory dir
    [size, file] -> File (read size) file
    other -> error $ "Could not parse line: " <> show other

data Line
  = CD String
  | LS
  | Directory String
  | File Int String
  deriving (Eq, Show)

-- >>> solve1 example
-- 95437
solve1 :: [Line] -> Int
solve1 = sumTree . root . system . List.foldl' evalLine emptyS

sumTree :: SystemEntry -> Int
sumTree se = se
  & recomputeDirSizes
  & Tree.flatten
  & filter (\(siz, e) -> isDir e && siz <= 100_000)
  & map fst
  & sum

data Entry = F {unE :: String} | D {unE :: String} deriving (Eq, Show)

isDir :: Entry -> Bool
isDir = \case {D _e -> True; _-> False}

type FileSystem = [SystemEntry]
type SystemEntry = Tree (Int, Entry)
type Path = Seq String

root :: FileSystem -> SystemEntry
root = Tree.Node (0, D "/")

data S = S {ls :: Bool, path :: Path, system :: FileSystem}
  deriving (Eq, Show)

emptyS :: S
emptyS = S False Seq.empty []

evalLine :: S -> Line -> S
evalLine s = \case
  CD ".." -> s{ls = False, path = seqInit s.path}
  CD "/" -> s{ls = False, path = Seq.empty}
  CD dir -> s{ls = False, path = s.path Seq.|> dir}
  LS -> s{ls = True}
  Directory dir ->
    if not s.ls
      then error "unexpected dir line"
      else s{system = insertPath False (s.path Seq.|> dir) 0 s.system}
  File size file ->
    if not s.ls
      then error "unexpected file line"
      else s{system = insertPath True (s.path Seq.|> file) size s.system}

seqInit :: Seq a -> Seq a
seqInit p = case Seq.viewr p of (np Seq.:> _r) -> np; Seq.EmptyR -> p

insertPath :: Bool -> Path -> Int -> FileSystem -> FileSystem
insertPath isFile p s fs =
 case Seq.viewl p of
  Seq.EmptyL -> fs
  (e Seq.:< r) -> insertF e r fs
 where
  insertF :: String -> Path -> FileSystem -> FileSystem
  insertF e r = \case
    [] -> [mkPath isFile e r s]
    (x : xs) -> case e `compare` unE (snd $ Tree.rootLabel x) of
      LT -> mkPath isFile e r s : x : xs
      EQ -> x {Tree.subForest = insertPath isFile r s $ Tree.subForest x} : xs
      GT -> x : insertF e r xs

mkPath :: Bool -> String -> Path -> Int -> SystemEntry
mkPath isFile e r s = case Seq.viewl r of
  Seq.EmptyL -> Tree.Node (s, entry) []
  ne Seq.:< nr -> Tree.Node (0, entry) [mkPath isFile ne nr s]
 where
  entry = (if isFile then F else D) e

recomputeDirSizes :: SystemEntry -> SystemEntry
recomputeDirSizes se@(Tree.Node (_s, e) sub) = case e of
  F _f -> se
  D _d -> Tree.Node (ns, e) resub
 where
  resub = map recomputeDirSizes sub
  ns = sum $ map (fst . Tree.rootLabel) resub

-- >>> solve2 example
-- 24933642
solve2 :: [Line] -> Int
solve2 = fst . head . findCandidates . root . system . List.foldl' evalLine emptyS

total :: Int
total = 70000000

required :: Int
required = 30000000

-- >>> findCandidates . root . system $ List.foldl' evalLine emptyS example
-- [(24933642,D {unE = "d"}),(48381165,D {unE = "/"})]
findCandidates :: SystemEntry -> [(Int, Entry)]
findCandidates se = dropWhile ((<reqFree) . fst) ds
 where
  re = recomputeDirSizes se
  reqFree = fst (Tree.rootLabel re) + required - total
  ds = List.sortOn fst . filter (isDir . snd) $ Tree.flatten re

printEntry :: SystemEntry -> IO ()
printEntry = putStrLn . Tree.drawTree . fmap show

printSystem :: FileSystem -> IO ()
printSystem = printEntry . root

example :: [Line]
example =
  parse . unlines $
    [ "$ cd /"
    , "$ ls"
    , "dir a"
    , "14848514 b.txt"
    , "8504156 c.dat"
    , "dir d"
    , "$ cd a"
    , "$ ls"
    , "dir e"
    , "29116 f"
    , "2557 g"
    , "62596 h.lst"
    , "$ cd e"
    , "$ ls"
    , "584 i"
    , "$ cd .."
    , "$ cd .."
    , "$ cd d"
    , "$ ls"
    , "4060174 j"
    , "8033020 d.log"
    , "5626152 d.ext"
    , "7214296 k"
    ]
