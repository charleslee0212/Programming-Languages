module A3 where

-- removeAllExcept
-- This function removes all the elements except for the one specified
-- The parameters are an element and a list
-- The return type is a list
removeAllExcept a [] = []
removeAllExcept a (x:xs) = if x == a
                            then x : removeAllExcept a xs
                            else removeAllExcept a xs

-- removeAll
-- This function removes all elemtents that equals the one specified
-- The parameters are an element and a list
-- The return type is a list
removeAll a [] = []
removeAll a (x:xs) = if x == a
                      then removeAll a xs
                      else x : removeAll a xs

-- substitute
-- This function substitute all the specified elements in the list with a new value
-- The parameters are two elements and a list
-- The return type is a list
substitute a b [] = []
substitute a b (x:xs) = if x == a
                         then b : substitute a b xs
                         else x : substitute a b xs

--mergeSorted3
-- This function merges three ordered list of numbers into one ordered list
-- The parameters are three lists
-- The return type is a list
mergeSorted3 [] [] [] = []
mergeSorted3 [] [] (z:zs) = z : zs
mergeSorted3 [] (y:ys) [] = y : ys
mergeSorted3 (x:xs) [] [] = x : xs
mergeSorted3 [] (y:ys) (z:zs) = if y < z
                                  then y : mergeSorted3 [] (ys) (z:zs)
                                  else z : mergeSorted3 [] (y:ys) (zs)
mergeSorted3 (x:xs) (y:ys) [] = if x < y
                                  then x : mergeSorted3 (xs) (y:ys) []
                                  else y : mergeSorted3 (x:xs) (ys) []
mergeSorted3 (x:xs) [] (z:zs) = if x < z
                                  then x : mergeSorted3 (xs) [] (z:zs)
                                  else z : mergeSorted3 (x:xs) [] (zs)
mergeSorted3 (x:xs) (y:ys) (z:zs) =
  let
    min = findmin [x,y,z]
  in
    if min == x
      then x : mergeSorted3 (xs) (y:ys) (z:zs)
      else if min == y
        then y : mergeSorted3 (x:xs) (ys) (z:zs)
        else z : mergeSorted3 (x:xs) (y:ys) (zs)

-- findmin
-- This function is a helper function for mergeSorted3 which finds the min
-- The parameter is a list
-- The return type is a number
findmin [] = error "Empty List"
findmin [x] = x
findmin (x:y:xs) = findmin ((if x < y then x else y):xs)
