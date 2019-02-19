module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

data BST a = Leaf | Node a (BST a) (BST a) deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft Leaf                = Nothing
bstLeft (Node _ leftTree _) = Just leftTree

bstRight :: BST a -> Maybe (BST a)
bstRight Leaf                 = Nothing
bstRight (Node _ _ rightTree) = Just rightTree

bstValue :: BST a -> Maybe a
bstValue Leaf         = Nothing
bstValue (Node x _ _) = Just x

empty :: BST a
empty = Leaf

fromList :: Ord a => [a] -> BST a
fromList = foldl (flip insert) Leaf

insert :: Ord a => a -> BST a -> BST a
insert x Leaf                        = Node x Leaf Leaf
insert x (Node y leftTree rightTree)
    | x <= y = Node y (insert x leftTree) rightTree
    | otherwise = Node y leftTree (insert x rightTree)

singleton :: a -> BST a
singleton x = Node x Leaf Leaf

toList :: BST a -> [a]
toList Leaf = []
toList (Node x leftTree rightTree) = toList leftTree ++ [x] ++ toList rightTree
