--new dataType
data BinaryTree a = Nil | Node' (BinaryTree a) a (BinaryTree a)  -- (Ord a, Eq a) => apperantly datatype context is deprecated
--------------------------------------------------------------------------------
size' :: BinaryTree a -> int
size' Nil = 0
size' (Node' leftChild _ rightChild) = size' leftChild + size' rightChild + 1
--------------------------------------------------------------------------------
insert :: (Eq a , Ord a) => BinaryTree a -> a -> BinaryTree a
insert Nil val = Node' Nil val Nil
insert (Node' leftChild val rightChild) toInsert
    |toInsert == val = Node' leftChild val rightChild
    |toInsert > val = Node' leftChild val  (insert rightChild toInsert)
    |otherwise = Node' (insert leftChild toInsert) val rightChild
--------------------------------------------------------------------------------
remove ::(Eq a , Ord a) => BinaryTree a -> a -> BinaryTree a

remove Nil _ = Nil
remove (Node' leftChild val rightChild) toRemove
    |val == toRemove = removeNode (Node' leftChild val rightChild)
    |val > toRemove = remove rightChild toRemove
    |otherwise = remove leftChild toRemove
    where
    removeNode :: (Eq a , Ord a) => BinaryTree a -> BinaryTree a
    removeNode (Node' Nil _ rightChild) = rightChild
    removeNode (Node' leftChild _ Nil) =  leftChild
    removeNode (Node' Nil _ Nil) = Nil
    removeNode (Node' leftChild _ rightChild) = Node' leftChild suc (remove rightChild suc)
        where
            suc = findSmallest rightChild
                where
                    findSmallest ::(Eq a , Ord a) => BinaryTree a -> a
                    findSmallest (Node' Nil val _) = val
                    findSmallest (Node' leftChild _ _) = findSmallest leftChild

--------------------------------------------------------------------------------
contains :: (Eq a , Ord a) => BinaryTree a -> a -> Bool

contains Nil _ = False

contains (Node' leftChild val rightChild) query
    | val == query = True
    | val > query = contains rightChild query
    | otherwise = contains leftChild query

--------------------------------------------------------------------------------
fromList :: (Ord a, Eq a) => [a] -> BinaryTree a
--this operation is asymptoticly bound by nlogn  - so the naive implementation is enough
fromList [] = Nil
fromList (h:t) = insert (fromList t) h
--------------------------------------------------------------------------------




-- aaaahhhhhhh i accidentally made a binary tree instead of a general tree


-- new dataType 
data Tree a =  Node a [Tree a] -- (Ord a, Eq a) =>  deprecated -- Nil| unneeded?...
--------------------------------------------------------------------------------
size :: (Num b) => (Tree a) -> b

-- size Nil = 0
size(Node _ []) = 1
size (Node _ children) = 1 + (sum (map size children))
--------------------------------------------------------------------------------
treeMap :: (Ord b, Eq b) => (a -> b) -> Tree a -> Tree b
-- treeMap _ Nil = Nil
treeMap func (Node val children) = Node (func val) (map composite children)
    where composite = treeMap func 
--------------------------------------------------------------------------------
-- going left to right on the children --> and as a result on the tree itself
treeFoldl :: (b -> a -> b) -> b -> Tree a -> b
-- treeFoldl _ acc Nil = acc -- is this needed?
treeFoldl func acc (Node val []) = func acc val
treeFoldl func acc (Node val children) = foldl composite (func acc val) children
    where composite = treeFoldl func

-- going right to left on the children --> and as a result on the tree itself
treeFoldr :: (a -> b -> b) -> Tree a -> b -> b
-- treeFoldr _ Nil acc  = acc
treeFoldr func (Node val []) acc  = func val acc
treeFoldr func (Node val children) acc  = foldr composite (func val acc) children
    where composite = treeFoldr func

--Up, meaning children first 
treeFoldu :: (b -> a -> b) -> b -> Tree a -> b
-- treeFoldu _ acc Nil = acc
treeFoldu func acc (Node val []) = func acc val
treeFoldu func acc (Node val children) = func (foldl composite acc children) val
    where composite = treeFoldl func

--------------------------------------------------------------------------------
-- treeScanl :: (b -> a -> b) -> b -> Tree a -> [b]
-- --not so sure about this.....................................................
-- -- treeScanl  _ acc Nil = [acc]
-- treeScanl func acc (Node val []) = [func val acc]
-- treeScanl func acc (Node val children) = scanl func (func acc val) children
--     where composite = treeScanl func
--------------------------------------------------------------------------------
treeShow :: (Show a) =>  String -> Tree a -> String
-- treeShow acc Nil = show acc
treeShow acc (Node val [])  = show (acc ++ (show val))
treeShow acc (Node val children) = foldl treeShow (acc ++ (show val)) children

instance Show a =>Show (Tree a)  where
    show t = treeShow "" t

main = print (show "a")
