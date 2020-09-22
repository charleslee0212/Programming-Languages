module W1 where
-- Charles Lee
-- Data type for the TriTree
data TriTree a = EmptyNode | TriNode a (TriTree a) (TriTree a) (TriTree a)
            deriving (Show)

--  Typeclass extension
instance (Eq a) => Eq (TriTree a) where
  EmptyNode           == EmptyNode = True
  TriNode a la ma ra  == TriNode b lb mb rb = (a == b) &&
                                              (la == lb) &&
                                              (ma == mb) &&
                                              (ra == rb)
  _                   == _ = False

-- The function gives the value of the current node
-- The parameter is the TriTree
-- Returns the value a
nodeValue :: TriTree a -> a
nodeValue EmptyNode = error "The Node is Empty"
nodeValue (TriNode a la ma ra) = a

-- The function gives the left child of the current node
-- The parameter is the TriTree
-- Returns the value la
leftChild :: TriTree a -> TriTree a
leftChild EmptyNode = error "The Node is Empty"
leftChild (TriNode a la ma ra) = la

-- The function gives the middle child of the current node
-- The parameter is the TriTree
-- Returns the value ma
middleChild :: TriTree a -> TriTree a
middleChild EmptyNode = error "The Node is Empty"
middleChild (TriNode a la ma ra) = ma

-- The function gives the right child of the current node
-- The parameter is the TriTree
-- Returns the value ra
rightChild :: TriTree a -> TriTree a
rightChild EmptyNode = error "The Node is Empty"
rightChild (TriNode a la ma ra) = ra

-- The function checks if the given value is in the tree
-- The parameters are a value and the TriTree
-- Returns a boolean
inTree :: Eq a => a -> TriTree a -> Bool
inTree x EmptyNode = False
inTree x (TriNode a la ma ra) = if x == a
                                then True
                                else (inTree x la) || (inTree x ma) || (inTree x ra)

-- The function gives all the leaf nodes of the tree
-- The parameter is the TriTree
-- Returns a list of leaf nodes
leafList :: TriTree a -> [a]
leafList EmptyNode = []
leafList (TriNode a EmptyNode EmptyNode EmptyNode) = [a]
leafList (TriNode a la ma ra) = leafList la ++ leafList ma ++ leafList ra

-- The function allows map functions on the tree applying the function to each element
-- The parameters are a function and the TriTree
-- Returns a TriTree
inOrderMap :: (a -> b) -> TriTree a -> TriTree b
inOrderMap f EmptyNode = EmptyNode
inOrderMap f (TriNode a la ma ra) = TriNode (f a) (inOrderMap f la) (inOrderMap f ma) (inOrderMap f ra)

-- The function allows fold on the tree applying the function to each element and passing the result to the next call
-- The parameters are a function, the accumulator value, and the TriTree
-- Returns a value c 

preOrderFold :: (b -> a -> b) -> b -> TriTree a -> b
preOrderFold f c EmptyNode = c
preOrderFold f c (TriNode a la ma ra) = (preOrderFold f (f c a) la)
