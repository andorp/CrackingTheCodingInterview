module Heap where

data Heap e
  = Empty
  | Node Int e (Heap e) (Heap e)
  deriving (Eq, Show)

rank :: Heap e -> Int
rank Empty          = 0
rank (Node r _ _ _) = r

makeNode :: e -> Heap e -> Heap e -> Heap e
makeNode x a b =
  let ra = rank a in
  let rb = rank b in
  if ra >= rb
    then Node (rb + 1) x a b
    else Node (ra + 1) x b a

empty :: Heap e
empty = Empty

isEmpty :: Heap e -> Bool
isEmpty Empty = True
isEmpty _     = False

merge :: (Ord e) => Heap e -> Heap e -> Heap e
merge h Empty = h
merge Empty h = h
merge h1@(Node _ x a1 b1) h2@(Node _ y a2 b2)
  | x < y     = makeNode x a1 (merge b1 h2)
  | otherwise = makeNode y a2 (merge h1 b2)

insert :: (Ord e) => e -> Heap e -> Heap e
insert x h1 = merge (makeNode x empty empty) h1

findMin :: Heap e -> Maybe e
findMin Empty          = Nothing
findMin (Node _ x _ _) = Just $ x

deleteMin :: (Ord e) => Heap e -> Maybe (Heap e)
deleteMin Empty          = Nothing
deleteMin (Node _ _ a b) = Just $ merge a b
