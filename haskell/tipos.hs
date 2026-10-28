type Pos = (Int, Int);

origem :: Pos
origem = (0,0)

left :: Pos -> Pos
left (x,y) = (x-1, y)

rigth :: Pos -> Pos
rigth (x,y) = (x+1,y)

-- EX 2
type Pair a = (a,a)

copy = a -> Pair a
copy x = (x,x   )

--ex 3
data move = North | South | East | West deriving(Show)

move:: Move -> Pos -> Pos
move North (x,y) = (x,y+1)
move South (x,y) = (x,y-1)
move East (x,y) = (x+1,y)
move West (x,y) = (x-1,y)

moves :: [Move] -> Pos -> Pos
moves[] p = p
moves(m:ms) p = moves ms (moves m p)

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y x) = x == y || occurs x l || occurs x r 

-- fazer percurco de arvore pre central e pos 
