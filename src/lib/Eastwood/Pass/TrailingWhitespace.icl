implementation module Eastwood.Pass.TrailingWhitespace

import StdEnv

from Data.Array import foldlArr
from Data.Maybe import fromMaybe, class Maybe

import Eastwood.Configuration
import Eastwood.Diagnostic

WHITESPACE_CODE :== 0

runPass :: !TrailingWhitespaceConfiguration ![String] -> [Diagnostic]
runPass configuration contents = runPass` contents 1
where
	runPass` :: ![String] !Int -> [Diagnostic]
	runPass` [] _ = []
	runPass` [l:ls] lineNumber
		| finalCharacterIsWhitespace l =
			[newDiagnostic l lineNumber : runPass` ls (lineNumber + 1)]
		| otherwise = runPass` ls (lineNumber + 1)

	finalCharacterIsWhitespace :: !String -> Bool
	finalCharacterIsWhitespace "" = False
	finalCharacterIsWhitespace line = isSpace line.[size line - 1]

	newDiagnostic :: !String !Int -> Diagnostic
	newDiagnostic line lineNumber =
		{ Diagnostic
		| range = findCharacterRange line lineNumber
		, severity = fromMaybe Warning configuration.TrailingWhitespaceConfiguration.severity
		, dCode = WHITESPACE_CODE
		, source = TrailingWhitespacePass
		, message = "Found trailing whitespace"
		}

	findCharacterRange :: !String !Int -> CharacterRange
	findCharacterRange line lineNumber
		#! numberOfWhitespaceCharacters = foldlArr (\counter char -> if (isSpace char) (counter + 1) 0) 0 line
		#! endColumn = size line
		#! startColumn = endColumn - numberOfWhitespaceCharacters
		= newSingleLineRange lineNumber startColumn endColumn
