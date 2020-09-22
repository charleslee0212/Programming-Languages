module A6 where
-- firstAnswer
-- A function is passed and mapped to each element in the list returning nothing or the first "answer"
-- The parameter is a function and a list
-- Returns a Maybe value
firstAnswer :: ( a -> Maybe b ) -> [a] -> Maybe b
firstAnswer f [] = Nothing
firstAnswer f (x:xs) = case (f x) of
                          Nothing -> firstAnswer f xs
                          x -> x

-- allAnswers
-- A function is passed and mapped to each element in the list returning nothing or all the "answers"
-- The parameter is a function and a list
-- Returns a list of Maybe values
allAnswers :: (a -> Maybe [b]) -> [a] -> Maybe [b]
allAnswers f [] = Nothing
allAnswers f (x:xs) = allAnswersHelper f xs []

allAnswersHelper f [] acc = Just acc
allAnswersHelper f (x:xs) acc = case (f x) of
                                  Nothing -> Nothing
                                  Just x -> allAnswersHelper f xs (acc ++ x)
