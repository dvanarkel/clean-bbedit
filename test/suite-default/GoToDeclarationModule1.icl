implementation module GoToDeclarationModule1

funcSingleResult :: Bool
funcSingleResult = (True <#$> True) #$> True

funcMultipleResults :: Bool
funcMultipleResults = (<#$$) True (True <#$$ True)

FunctionThatStartsWithCapitalLetter :: Bool
FunctionThatStartsWithCapitalLetter = (<#$) True (True <#$ True)

(<#$) infixl 4 :: !Bool !Bool -> Bool
(<#$) _ _ = True

(<#$$) infixl 4 :: !Bool !Bool -> Bool
(<#$$) _ _ = True

(#$>) infixr 4 :: !Bool !Bool -> Bool
(#$>) _ _ = True

(<#$>) infix 4 :: !Bool !Bool -> Bool
(<#$>) _ _ = True

import StdEnv

stdEnvFunc :: Bool
stdEnvFunc = not False