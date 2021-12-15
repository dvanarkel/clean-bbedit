definition module GoToDeclarationModule2

import StdGeneric

:: TypeMultipleResults = Foo

funcMultipleResults :: Bool

generic gFuncMultipleResult a :: !a -> String

:: RecordFieldsMultipleResults =
	{ fooMultiple :: !Bool
	, barMultiple :: !Bool
	}

class classMultipleResults a where
	fooMultipleResultsFunc :: !a -> Bool

//* Used multiple
(<#$$) infixl 4 :: !Bool !Bool -> Bool