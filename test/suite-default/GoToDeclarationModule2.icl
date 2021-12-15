implementation module GoToDeclarationModule2

funcMultipleResults :: Bool
funcMultipleResults = True

(<#$$) infixl 4 :: !Bool !Bool -> Bool
(<#$$) _ _ = True