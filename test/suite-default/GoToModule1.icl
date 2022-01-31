implementation module GoToModule1

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

from Data.Map import :: Map

genericFuncMonoKind :: Bool
genericFuncMonoKind = gEqTest{|*|} True True

// The parentheses around (*) are redundant but are used to show that using () within kind specifications is supported.
// (this is an equality check which only compares the values of the map for equality using gEqTest)
genericFuncHigherKindedTypes :: !(Map k v) !(Map k v) -> Bool | gEqTest{|*|} v
genericFuncHigherKindedTypes m1 m2 = gEqTest{|(*)->*->*|} (\k1 k2 -> True) (gEqTest {|*|}) m1 m2

import StdGeneric, StdEnv
generic gEqTest a  :: !a !a -> Bool
derive gEqTest Map
gEqTest{|Int|} 	x y 							= x == y
gEqTest{|Char|} x y 							= x == y
gEqTest{|Bool|} x y 							= x == y
gEqTest{|Real|} x y 							= x == y
gEqTest{|String|} x y 							= x == y
gEqTest{|UNIT|} UNIT UNIT 						= True
gEqTest{|PAIR|} fx fy (PAIR x1 y1) (PAIR x2 y2) = fx x1 x2 && fy y1 y2
gEqTest{|EITHER|} fl fr (LEFT x) (LEFT y) 		= fl x y
gEqTest{|EITHER|} fl fr (LEFT _) (RIGHT _) 		= False
gEqTest{|EITHER|} fl fr (RIGHT x) (RIGHT y) 	= fr x y
gEqTest{|EITHER|} fl fr (RIGHT _) (LEFT _)		= False
gEqTest{|CONS|} f (CONS x) (CONS y) 			= f x y
gEqTest{|RECORD|} f (RECORD x) (RECORD y) 		= f x y
gEqTest{|FIELD|} f (FIELD x) (FIELD y) 			= f x y
gEqTest{|OBJECT|} f (OBJECT x) (OBJECT y) 		= f x y
gEqTest{|{}|} f xs ys 							= eqArray f xs ys
gEqTest{|{!}|} f xs ys 							= eqArray f xs ys
derive gEqTest [], [!], [ !], [!!], [#], [#!], (), (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)
eqArray f xs ys = size xs == size ys && eq 0 (size xs) xs ys
where
	eq i n xs ys
	| i == n    = True
	| i < n     = f xs.[i] ys.[i] && eq (inc i) n xs ys
	| otherwise = abort "error in eqArray\n"

import StdEnv

stdEnvFunc :: Bool
stdEnvFunc = not False

:: *AbstractTypeSynonym :== Bool
:: *AbstractNewType =: AbstractNewType Bool

:: RecordTestOne a =
	{
		testOneFieldOne :: !(a a -> Bool),
		testOneFieldTwo :: !Bool
		,testOneFieldFour :: !Bool
	}

:: RecordTestTwo = {testTwoFieldOne :: !Bool, testTwoFieldTwo :: !Bool}

zipTwo::![.a] [.b] -> [(.a,.b)]
zipTwo [a:as] [b:bs]	= [(a,b):zipTwo as bs]
zipTwo as bs			= []

:: NewTypeIcl =: NewTypeIcl Int

MACRO_ICL :== 5

:: TypeSynonymIcl :== Bool

class classIcl a where
	foo :: !a -> Bool

class classIcl2 a :: !a -> Bool

:: ConstructorIclOne arg1 arg2 = FooIclOne | FooIclTwo |
	FooIclThree |
	FooIclFour
	| FooIclFive
	| FooIclSix arg1 arg2 | FooIclSeven arg2 arg1

:: ConstructorIclTwo
	= FooIclEight

:: ConstructorIclThree =
	FooIclNine

goToRecordFieldTest :: RecordFieldsSingleResult -> Bool
goToRecordFieldTest rf = rf.RecordFieldsSingleResult.foo

funcWithoutTypeAnnotationTest1 = True

funcWithoutTypeAnnotationTest2
	= True

funcWithoutTypeAnnotationTest3 a b c d e f
	# a = True
	= a

funcWithoutTypeAnnotationTest4
	| True = True
	= False

:: *UniqueType = { fooBool :: !Bool }

(<#|) infixl 4 :: !Bool !Bool -> Bool
