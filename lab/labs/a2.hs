module A2 where

-- The function finds the min of a given list
-- The parameter is a list of numbers
-- It will return a number
findmin [] = error "Empty List"
findmin [x] = x
findmin (x:y:xs) = findmin ((if x < y then x else y):xs)

-- The function gives the dot product of two list of numbers with the same length
-- The parameters are two lists
-- It will return a number
tupleDotProduct list1 list2 = if list1 == [] && list2 == []
                                then 0
                                else (head list1 * head list2) + tupleDotProduct (tail list1) (tail list2)

-- The function couples the nth element of list 1 and list 2 into a reversed list
-- The parameters are two lists
-- It will return a list of tuples
revZip2Lists list1 list2 = if list1 == [] && list2 == []
                              then []
                              else revZip2Lists (tail list1) (tail list2) ++ [(head list1, head list2)]

-- The function gives out a list of every 3rd element in the given list
-- The parameter is a list
-- It will return a list
everyThird list = if list == []
                    then []
                    else if mod (length list) 3 == 0
                            then [list!!2] ++ everyThird (tail list)
                            else everyThird (tail list)
