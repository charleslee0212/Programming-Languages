module W1 where

-- The data types
data Pattern = WildcardPat | VariablePat (String) | UnitPat | ConstantPat (Int) | ConstructorPat (String, Pattern) | TuplePat ([Pattern]) deriving (Eq, Show)
data Value = Constant (Int) | Unit | Constructor (String, Value) | Tuple [Value] deriving (Eq, Show)

-- checkPat
-- This function check to see if the pattern it was given has any repeats.
-- The parameter is a Pattern type
-- Returns a Boolean depeding on existance of repeats.
checkPat pattern = case pattern of
                      WildcardPat -> True
                      ConstantPat c -> True
                      UnitPat -> True
                      VariablePat string -> helperCheckPat (helperListOfPat (VariablePat string))
                      ConstructorPat (string,pattern) -> helperCheckPat (helperListOfPat (ConstructorPat (string, pattern)))
                      TuplePat list -> helperCheckPat (helperListOfPat (TuplePat list))

-- helperListOfPat
-- Helper function for checkPat
-- The parameters is a Pattern type
-- Returns [Patterns]
helperListOfPat (VariablePat string) = [string]
helperListOfPat (ConstructorPat (string, pattern)) = helperListOfPat pattern
helperListOfPat (TuplePat list) = foldl (++) [] (map helperListOfPat list)

-- helperCheckPat
-- Helper function for checkPat
-- The parameter is []
-- Returns a Boolean
helperCheckPat [] = True
helperCheckPat (x:xs) = case (elem x xs) of
                          True -> False
                          False -> True && (helperCheckPat (xs))

-- match
-- This function matches a Pattern and a Value type depending on specified conditions
-- The parameter is (Value,Pattern)
-- Returns Maybe []
match (x, y) = case y of
                  WildcardPat -> Just[]
                  VariablePat string -> Just[(string, x)]
                  UnitPat -> case x of
                                Unit -> Just[]
                                _ -> Nothing
                  ConstantPat int -> case x of
                                        Constant int_ -> if int == int_
                                                          then Just[]
                                                          else Nothing
                                        _ -> Nothing
                  ConstructorPat (string, pattern) -> case x of
                                                        Constructor (string_, value) -> if string == string_
                                                                                          then match (value, pattern)
                                                                                          else Nothing
                                                        _ -> Nothing
                  TuplePat list -> case x of
                                      Tuple list_ -> if length list == length list_
                                                      then allAnswers match (zip list_ list)
                                                      else Nothing
                                      _ -> Nothing

-- firstMatch
-- This function finds the first match and returns the match
-- The parameters are a Value and [Patterns]
-- Returns Maybe []
firstMatch value [] = Nothing
firstMatch value list = firstAnswer match [(x, y) | x <- [value], y <- list]

-- allAnswers
-- Helper function for match
-- The parameters are a function f and []
-- Returns Maybe []
allAnswers :: (a -> Maybe [b]) -> [a] -> Maybe [b]
allAnswers f [] = Nothing
allAnswers f (x:xs) = allAnswersHelper f xs []

allAnswersHelper f [] acc = Just acc
allAnswersHelper f (x:xs) acc = case (f x) of
                                  Just x -> allAnswersHelper f xs (acc ++ x)
                                  Nothing -> Nothing

-- firstAnswer
-- Helper function for firstMatch
-- The parameters are a function f and []
-- Returns Maybe 
firstAnswer :: ( a -> Maybe b ) -> [a] -> Maybe b
firstAnswer f [] = Nothing
firstAnswer f (x:xs) = case (f x) of
                          Nothing -> firstAnswer f xs
                          x -> x
