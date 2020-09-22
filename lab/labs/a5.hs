module A5 where

import Data.List
import Data.Char

-- longestString'
-- The function finds the longest string in a given list. If tied return the second
-- The parameter is a list of strings
-- Returns a string of the longest length
longestString' [] = ""
longestString' (x:xs) = foldl (\z y -> if length z > length y then z else y) x xs

-- longestStringHelper
-- The function is a helper function for longestString3 and longestString4 *acts like a map
-- The parameters are a function and a list of strings
-- Returns a value either z or y depending on function parameter
longestStringHelper f (x:xs) = foldl (\z y -> if f (length z) (length y) then z else y) x xs

-- longestString3
-- The function finds the longest string ina  given list. If tied return the first
-- The parameter is a list of strings
-- Returns the longest string
longestString3 [] = ""
longestString3 strings = longestStringHelper (>=) (strings)

-- longestString4
-- The function is the same as longestString' but uses a helper function
-- The parameter is a list of strings
-- Returns the longest string.
longestString4 [] = ""
longestString4 strings = longestStringHelper (>) (strings)
