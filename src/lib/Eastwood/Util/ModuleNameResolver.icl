implementation module Eastwood.Util.ModuleNameResolver

import StdEnv
import StdOverloadedList

import Control.Monad
import Data.Error
import Data.Func
import Data.Tuple
import System.File
import System.FilePath
from Text import class Text(split), instance Text String, concat3, concat4

resolveModuleName :: !FilePath ![FilePath] !*World -> (!MaybeError String String, !*World)
resolveModuleName file searchPaths w
	=	appFst (check file searchPaths <=< scanHeader <=< mapError toString) $
		readFile file w

// Perform a final check that the module name in the file matches the file name.
check :: !FilePath ![FilePath] !String -> MaybeError String String
check file searchPaths foundModuleName =
	check` (dropExtension file) (reverse $ split "." foundModuleName) >>= \path ->
	if (isMember path searchPaths)
		(Ok foundModuleName)
		(Error "incomplete module name or missing search path")
where
	check` path [] = Ok path
	check` path [modulePart:rest]
		| file <> modulePart
			= Error $ concat4 "unexpected module name '" foundModuleName "' in " file
		| otherwise
			= check` dir rest
	where
		(dir, file) = splitFileName path

scanHeader :: !String -> MaybeError String String
scanHeader s = scan 0
where
	scan i
		| i >= size s
			= Error "read until end of file without encountering a module header"
		| startsWithAt i "implementation" s && i+14 < size s && not (isIdentChar s.[i+14])
			= scanModuleKeyword (i+14)
		| startsWithAt i "definition" s && i+10 < size s && not (isIdentChar s.[i+10])
			= scanModuleKeyword (i+10)
		| startsWithAt i "module" s && i+6 < size s && not (isIdentChar s.[i+6])
			= scanModuleName (i+6)
		| startsWithAt i "system" s && i+6 < size s && not (isIdentChar s.[i+6])
			= scanModuleKeyword (i+6)
		| startsWithAt i "//" s
			= scan (skipToEndOfLine (i+2) s)
		| startsWithAt i "/*" s
			= scan =<< scanMultiLineComment (i+2)
		| isSpace s.[i]
			= scan (i+1)
		| otherwise
			= Error (concat3 "unexpected character '" {s.[i]} "'")

	//* Scans the `module` keyword after `implementation` & friends
	scanModuleKeyword i
		| startsWithAt i "module" s && i+6 < size s && not (isIdentChar s.[i+6])
			= scanModuleName (i+6)
		| startsWithAt i "//" s
			= scanModuleKeyword (skipToEndOfLine (i+2) s)
		| startsWithAt i "/*" s
			= scanModuleKeyword =<< scanMultiLineComment (i+2)
		| i < size s && isSpace s.[i]
			= scanModuleKeyword (i+1)
		| otherwise
			= Error (concat3 "unexpected character '" {s.[i]} "'")

	//* Scans the module name after the start of the header (e.g. `module `)
	scanModuleName = scanModuleName` [!!]
	where
		scanModuleName` read i
			| i < size s && (isIdentChar s.[i] || s.[i]=='.')
				= scanModuleName` [|s.[i]:read] (i+1)
			| not (IsEmpty read)
				= pure {#c \\ c <|- Reverse read}
			| startsWithAt i "//" s
				= scanModuleName` read (skipToEndOfLine (i+2) s)
			| startsWithAt i "/*" s
				= scanModuleName` read =<< scanMultiLineComment (i+2)
			| i < size s && isSpace s.[i]
				= scanModuleName` read (i+1)
			| otherwise
				= Error (concat3 "unexpected character '" {s.[i]} "'")

	//* Scans over a multi-line comment (/* .. */)
	scanMultiLineComment i
		| i >= size s
			= Error "end of file while scanning a multi-line comment"
		| s.[i]=='*' && i+1 < size s && s.[i+1]=='/'
			= pure $ i+2
		| s.[i]=='/'
			| i+1 >= size s
				= scanMultiLineComment (i+1)
			| s.[i+1]=='*' // nested multi-line comments
				= scanMultiLineComment =<< scanMultiLineComment (i+2)
			| s.[i+1]=='/' // */ after // does not close a multi-line comment
				= scanMultiLineComment (skipToEndOfLine (i+2) s)
			| otherwise
				= scanMultiLineComment (i+1)
		| otherwise
			= scanMultiLineComment (i+1)

//* Checks whether a character is part of a normal identifier.
isIdentChar c = isAlphanum c || c=='_' || c=='`'

//* Gives the index of the first following non-whitespace character.
skipWhitespace i s
	| i < size s && isSpace s.[i]
		= skipWhitespace (i+1) s
	| otherwise
		= i

//* Gives the index of the first character on the next line.
skipToEndOfLine i s
	| i >= size s
		= i
	| s.[i]=='\n'
		= if (i+1 < size s && s.[i+1]=='\r') (i+2) (i+1)
	| s.[i]=='\r'
		= if (i+1 < size s && s.[i+1]=='\n') (i+2) (i+1)
	| otherwise
		= skipToEndOfLine (i+1) s

/**
 * `startsWithAt i s1 s2` iff `s2` taken from the `i`th character starts with
 * `s1`.
 */
startsWithAt :== startsWithAt` 0
where
	startsWithAt` i j start s =
		i >= size start ||
		j < size s && start.[i] == s.[j] && startsWithAt` (i+1) (j+1) start s
