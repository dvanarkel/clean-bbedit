definition module GoToModule1

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
(<#$) infixl 4 :: !Bool !Bool -> Bool
//* Used multiple
(<#$$) infixl 4 :: !Bool !Bool -> Bool

(#$>) infixr 4 :: !Bool !Bool -> Bool

(<#$>) infix 4 :: !Bool !Bool -> Bool

:: ConstructorTestOne arg1 arg2 = FooOne | FooTwo |
	FooThree |
	FooFour
	| FooFive
	| FooSix arg1 arg2 | FooSeven arg2 arg1

:: ConstructorTestTwo
	= FooEight

:: ConstructorTestThree =
	FooNine

import StdGeneric
from StdBool import qualified &&, not
from StdStrictLists import class UList, class UTSList

generic gEqTest a  :: !a !a -> Bool
// base cases
gEqTest{|OBJECT|} f (OBJECT x) (OBJECT y) 		= f x y
gEqTest{|CONS|} f (CONS x) (CONS y) 			= f x y
gEqTest{|RECORD|} f (RECORD x) (RECORD y) 		= f x y
gEqTest{|FIELD|} f (FIELD x) (FIELD y) 			= f x y
gEqTest{|EITHER|} fl fr (LEFT x) (LEFT y) 		= fl x y
gEqTest{|EITHER|} fl fr (LEFT _) (RIGHT _) 		= False
gEqTest{|EITHER|} fl fr (RIGHT x) (RIGHT y) 	= fr x y
gEqTest{|EITHER|} fl fr (RIGHT _) (LEFT _)		= False
gEqTest{|UNIT|} UNIT UNIT 						= True
gEqTest{|PAIR|} fx fy (PAIR x1 y1) (PAIR x2 y2) = fx x1 x2 'StdBool'. && fy y1 y2
derive gEqTest Int, Char, Bool, Real, String, {}, {!}
// standard types
derive gEqTest [], [!], [ !], [!!], [#], [#!], (), (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)
//* @type a a -> Bool | gEqTest{|*|} a
(===) infix 4
(===) x y :== gEqTest{|*|} x y
//* @type a a -> Bool | gEqTest{|*|} a
(=!=) infix 4
(=!=) x y :== 'StdBool'.not (x === y)

macroOne arg1 arg2 arg3 :== True

macroTwo :== True

stdEnvFunc :: Bool

:: TypeSynonym :== Bool
:: !*AbstractTypeSynonym (:== Bool)

:: NewType =: NewType Bool
:: !*AbstractNewType (=: AbstractNewType Bool)

:: User = FooUser

:: UserList =: UserList Int