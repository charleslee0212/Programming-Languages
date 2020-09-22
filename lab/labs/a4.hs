module A4 where

import Data.List
import Data.Char

-- onlyLowerCase
-- This function triverse the list to find only the strings beginning with lower case
-- The parameter is a list
-- Returns a list of lower case words
onlyLowerCase = filter (\strings -> isLower (head strings))

-- longestString
-- This function triverse the list from the left and finds the longest string
-- The parameter is a list
-- Returns a string
longestString [] = ""
longestString (x:xs) = foldl (\z y -> if length z >= length y then z else y) x xs

-- multpairs
-- This function goes to the list and multiplies the couple of integers
-- The parameter is a list
-- Returns a list
multpairs [] = []
multpairs (x:xs) = fst x * snd x : multpairs xs

-- sqsum
-- This function squares each element in the list and then adds them
-- The parameter is a list
-- Returns a number
sqsum [] = 0
sqsum (x:xs) = x**2 + sqsum xs

-- duplist
-- This function doubles the frequency in each element in the list
-- The parameter is a list
-- Returns a list
duplist [] = []
duplist (x:xs) = x:x:duplist xs
