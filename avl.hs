-- import System.Random
-- shuffle x = if length x < 2 then return x else do
--     i <- System.Random.randomRIO (0, length(x)-1)
--     r <- shuffle (take i x ++ drop (i+1) x)
--     return (x!!i : r)

-- random :: Integer -> IO [Integer]
-- random n = shuffle [1..n]

r1 :: [Integer]
r1 = [79,23,77,11,100,91,19,88,51,45,86,15,28,5,87,55,94,69,75,68,85,30,82,89,26,38,14,81,29,99,90,3,62,39,8,9,78,17,80,97,1,44,7,66,64,73,41,84,20,98,10,67,70,32,37,50,56,31,34,60,54,12,24,65,21,13,52,96,49,27,43,63,2,33,57,40,46,35,93,4,92,36,22,58,71,95,72,42,59,61,83,25,47,48,18,6,16,53,76,74]

r2 :: [Integer]
r2 = [43,34,4,94,13,67,38,21,5,85,64,100,30,26,77,83,62,90,45,16,17,3,71,57,93,60,48,50,51,1,53,91,29,22,14,97,46,35,63,6,56,95,79,40,59,10,8,41,49,25,80,65,66,37,68,20,69,52,98,9,24,82,84,23,72,31,39,12,42,11,75,73,7,87,99,88,27,61,47,15,2,44,81,32,33,74,76,92,96,89,55,54,78,18,86,36,70,58,19,28]

-- r3 :: [Integer]
-- r3 = [46,95,79,3,23,61,39,81,44,78,22,14,85,100,25,54,9,34,68,84,87,30,56,55,8,72,13,10,42,31,37,73,62,74,36,20,82,16,4,75,29,45,43,99,35,33,51,67,93,52,69,94,6,15,60,86,24,48,32,89,65,2,50,26,27,77,38,7,92,28,64,41,71,76,17,18,21,40,88,66,90,70,63,98,57,5,91,97,11,59,49,80,96,19,1,83,47,53,12,58]

-- r4 :: [Integer]
-- r4 = [78,16,75,48,32,74,36,2,83,26,10,57,70,53,99,39,22,72,25,64,46,30,45,90,54,29,47,97,87,85,49,56,13,68,9,84,82,69,38,77,71,60,20,17,96,6,41,34,62,76,88,12,8,89,1,19,35,63,24,18,81,15,3,31,5,94,11,66,44,58,27,67,14,21,23,33,86,52,55,7,98,4,73,61,50,59,79,95,28,40,42,37,93,65,43,100,91,80,51,92]

-- r5 :: [Integer]
-- r5 = [76,21,19,17,32,85,57,74,28,61,23,68,49,36,42,13,77,92,90,30,46,99,43,53,6,20,100,29,16,22,41,35,84,3,11,15,50,55,25,44,38,88,56,83,47,97,1,63,10,82,71,72,2,4,45,93,14,37,12,73,52,65,27,86,81,40,9,89,7,34,80,79,62,98,51,18,24,54,69,95,94,48,39,66,75,64,8,58,87,33,78,67,70,5,59,60,91,26,31,96]

-- (a1 -> a2 -> a1) -> a1 -> [a2] -> [a1]
foldl'' :: (a->b->a)->a->[b]->[a]
foldl'' _ _ []     = []
foldl'' f z (x:xs) = fzx `seq` fzx : foldl'' f fzx xs
    where fzx = (f z x)

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- STARTS HERE -- STARTS HERE -- STARTS HERE -- STARTS HERE -- STARTS HERE --
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

data AVL a = Leaf | Node a Integer (AVL a) (AVL a) deriving (Show,Read)

key :: AVL t -> t
key Leaf = undefined
key (Node k _ _ _) = k

empty :: AVL t -> Bool
empty Leaf = True
empty  _  = False

find :: (Ord t) => t -> AVL t -> AVL t
find _ Leaf = Leaf  -- this will happen if couldnt find y in the tree
find y curr 
    | x == y = curr
    | x > y = find y leftTree
    | otherwise = find y rightTree
        where
        (Node x _ leftTree rightTree) = curr

height :: AVL t -> Integer
height Leaf = 0
height (Node _ h _ _) = h

computeHeight :: AVL t -> Integer
computeHeight Leaf = 0
computeHeight (Node _ _ left right) = 1 + max (computeHeight left) (computeHeight right) 

rightRotate :: AVL t -> AVL t
rightRotate Leaf = Leaf
rightRotate (Node gp _ (Node p1 _ c1 c2) p2) = Node p1 (1 + max newHeightgp (height c1)) c1 (Node gp newHeightgp c2 p2)
    where
        newHeightgp = 1 + max (height c2) (height p2)
--                gp                 p1            
--               / \               /   \           
--             p1   p2     ==>   c1    gp         
--            / \                      / \            
--          c1  c2                    c2 p2       
rightRotate tree = tree 
--                  gp                       
--                 /  \ 
--               /     \      DO NOTHING
--           Leaf      p2           

leftRotate ::  AVL t -> AVL t
leftRotate Leaf = Leaf
leftRotate (Node gp _ p1 (Node p2 _ c1 c2)) = Node p2 (1 + max (height c2) newHeightgp) (Node gp newHeightgp p1 c1) c2
    where
        newHeightgp = 1 + max (height c1) (height p1)
--                gp                 p2           
--               / \               /   \           
--             p1   p2     ==>   gp    c2         
--                 / \          /  \                  
--               c1  c2        p1  c1             
leftRotate tree = tree 
--                gp                       
--               / \      DO NOTHING
--             p1   Leaf           


leftRightRotate :: AVL t -> AVL t
leftRightRotate Leaf = Leaf
leftRightRotate (Node gp _ p1 p2) = rotatedp1 `seq` rightRotate (Node gp (1 + height rotatedp1) rotatedp1 p2)
    where
        rotatedp1 = leftRotate p1

--                  gp                                          
--                 /  \                                         
--                /    p2                                       
--               /                                              
--       _     p1      _                                                                 
--      |     / \      |                                        
--      |   c1  c2     | = np1                                  
--      |      / \     |                                        
--      |_   cc1  cc2 _|                                        

rightLeftRotate ::  AVL t -> AVL t
rightLeftRotate Leaf = Leaf
rightLeftRotate (Node gp _ p1 p2) = rotatedp2 `seq` leftRotate (Node gp (1 + height rotatedp2) p1 (rightRotate p2))
    where
        rotatedp2 = rightRotate p2

balanceFactor :: AVL t -> Integer
balanceFactor Leaf = 0
balanceFactor (Node _ _ c1 c2) = height c1 - height c2

-- | bf <= 1 && -1 <= bf = node
balance :: AVL t -> AVL t
balance Leaf = Leaf
balance node
    | bf > 1 && bfc1 >= 0 = rightRotate node     
    | bf > 1 && bfc1 < 0 = leftRightRotate node
    | bf < -1 && bfc2 > 0 =  rightLeftRotate node
    | bf < -1 && bfc2 <= 0 =  leftRotate node
    | otherwise = node
    where
        bf = balanceFactor node
        bfc1 = balanceFactor c1
        bfc2 = balanceFactor c2
        (Node _ _ c1 c2) = node

insert :: (Ord t) => t -> AVL t -> AVL t
insert y Leaf = Node y 1 Leaf Leaf 
insert y node
    | x > y = newLeft `seq` balance (Node x (1 + max (height newLeft) (height right)) newLeft right) 
    | x < y = newRight `seq` balance (Node x (1 + max (height left) (height newRight)) left newRight) 
    | otherwise = node
    where
        (Node x _ left right) = node
        newRight = (insert y right)
        newLeft = (insert y left)

lowest :: (Ord t) => AVL t -> t
lowest Leaf = undefined
lowest (Node x _ Leaf _) = x -- return x if cant go any more left
lowest (Node _ _ left _) = lowest left

-- successor :: (Ord t) => AVL t -> t
-- successor Leaf = undefined
-- successor (Node _ _ Leaf Leaf) = undefined -- first "right" parent. first parent on the path from x to root that is right of x. need root to find it. The second case only needed for the delete function
-- successor (Node _ _ _ right) = lowest right -- lowest values of the right subtree, if it has one

-- First find x
delete :: (Ord t) => t -> AVL t -> AVL t
delete _ Leaf = Leaf
delete x node
    | x == y = balance (deleteRoot node)
    | x < y = newLeft `seq` balance (Node y newHeightLeft newLeft right)
    | x > y = newRight `seq` balance (Node y newHeightRight left newRight)
    where 
        (Node y _ left right) = node
        newLeft = (delete x left)
        newHeightLeft = (1 + max (height newLeft) (height right))
        newRight = (delete x right)
        newHeightRight = (1 + max (height left) (height newRight))
delete _ (Node _ _ _ _) = undefined

-- Delete the x found by delete
deleteRoot :: (Ord t) => AVL t -> AVL t
deleteRoot Leaf = Leaf
deleteRoot (Node _ _ Leaf Leaf) = Leaf
deleteRoot (Node _ _ left Leaf) = left
deleteRoot (Node _ _ Leaf right) = right
deleteRoot node = newRight `seq` balance (Node next newHeight left newRight)
    where
        (Node _ _ left right) = node
        next = lowest right
        newRight = delete next right -- this will be balanced
        newHeight = 1 + max (height newRight) (height right)

inOrderToList :: AVL t -> [t]
inOrderToList Leaf = []
inOrderToList tree = inOrderToList left ++ [x] ++ inOrderToList right
    where (Node x _ left right) = tree

checkBST :: (Ord t) => AVL t -> Bool
checkBST Leaf = True
checkBST tree = checkBST left && checkBST right && all (\y->y<x) (inOrderToList left) && all (\y->y>x) (inOrderToList right)
    where (Node x _ left right) = tree

checkBalanceFactor :: AVL a -> Bool
checkBalanceFactor tree = -1 <= bf && bf <= 1
    where
        bf = balanceFactor tree

checkAVL :: (Ord t) => AVL t -> Bool
checkAVL Leaf = True
checkAVL tree = checkAVL left && checkAVL right && checkBalanceFactor tree
    where (Node _ _ left right) = tree

check :: (Ord t) => AVL t -> Bool
check tree = checkBST tree && checkAVL tree

leftOrderShow :: (Show t) => AVL t -> String
leftOrderShow Leaf = "Leaf"
leftOrderShow (Node x ht leftTree rightTree) = (show x) ++ "[" ++ (show ht) ++ "]" ++ " " ++ leftOrderShow leftTree ++ " " ++ leftOrderShow rightTree

leftOrderAVLShow :: (Show t) => AVL t -> String
leftOrderAVLShow Leaf = "Leaf"
leftOrderAVLShow node = (show x) ++ "[" ++ (show $ balanceFactor node) ++ "]" ++ " " ++ leftOrderAVLShow leftTree ++ " " ++ leftOrderAVLShow rightTree
    where
        (Node x _ leftTree rightTree) = node

n :: Integer
n = 10

-- t :: AVL Integer
-- t = foldl (\tree x-> insert x tree) Leaf [1..n]

ts :: [AVL Integer]
ts = foldl'' (\tree x-> insert x tree) Leaf $ filter (<n) r1

tds :: [AVL Integer]
tds = foldl'' (\tree x-> delete x tree) (last ts) $ filter (<n) r2

main :: IO ()
main = do
    print $ all check ts
    print $ all checkAVL ts
    print $ all check tds
    print $ all checkAVL tds

    -- print $ map check ts
    -- print $ map checkAVL ts
    -- print $ map check tds
    -- print $ map checkAVL tds
