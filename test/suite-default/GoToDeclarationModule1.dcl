definition module GoToDeclarationModule1

import StdGeneric

:: TypeMultipleResults = Foo

:: TypeSingleResult = Bar

funcSingleResult :: Bool

funcMultipleResults :: Bool

FunctionThatStartsWithCapitalLetter :: Bool

generic gFuncSingleResult a :: !a -> String

generic gFuncMultipleResult a :: !a -> String

:: RecordFieldsSingleResult =
	{ foo :: !Bool
	, bar :: !Bool
	}

:: RecordFieldsMultipleResults =
	{ fooMultiple :: !Bool
	, barMultiple :: !Bool
	}

class classSingleResult a where
	fooSingleResultFunc :: !a -> Bool

class classMultipleResults a where
	fooMultipleResultsFunc :: !a -> Bool

//* Used single.
(<#$) infixl 4 :: !Bool !Bool -> !Bool
//* Used multiple
(<#$$) infixl 4 :: !Bool !Bool -> !Bool

(#$>) infixr 4 :: !Bool !Bool -> !Bool

(<#$>) infix 4 :: !Bool !Bool -> !Bool

