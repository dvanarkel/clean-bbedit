implementation module GoToModule2

funcMultipleResults :: Bool
funcMultipleResults = True

(<#$$) infixl 4 :: !Bool !Bool -> Bool
(<#$$) _ _ = True

:: RecordTestTwo = {a :: !Bool}