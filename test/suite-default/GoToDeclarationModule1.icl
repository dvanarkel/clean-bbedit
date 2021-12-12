implementation module GoToDeclarationModule1

funcSingleResult :: Bool
funcSingleResult = True

funcMultipleResults :: Bool
funcMultipleResults = (#$$) True (True #$$ True)

FunctionThatStartsWithCapitalLetter :: Bool
FunctionThatStartsWithCapitalLetter = (#$) True (True #$ True)

(#$) :: !Bool !Bool -> Bool
(#$) _ _ = True

(#$$) :: !Bool !Bool -> Bool
(#$$) _ _ = True