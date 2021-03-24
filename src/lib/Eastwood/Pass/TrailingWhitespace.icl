implementation module Eastwood.Pass.TrailingWhitespace

import StdEnv

from Data.Array import foldlArr
from Data.Maybe import fromMaybe, class Maybe

import Eastwood.Configuration
import Eastwood.Diagnostic

runPass :: !TrailingWhitespaceConfiguration ![String] -> [Diagnostic]
runPass configuration contents = runPass` contents 1 0
where
	runPass` :: ![String] !Int !Int -> [Diagnostic]
	runPass` [] _ _ = []
	runPass` lx=:[l:ls] lineNumber diagnosticCounter
		| finalCharacterIsWhitespace l =
			[newDiagnostic l lineNumber diagnosticCounter : runPass` ls (lineNumber + 1) (diagnosticCounter + 1)]
		| otherwise = runPass` ls (lineNumber + 1) diagnosticCounter

	finalCharacterIsWhitespace :: !String -> Bool
	finalCharacterIsWhitespace "" = False
	finalCharacterIsWhitespace line = isSpace line.[size line - 1]

	newDiagnostic :: !String !Int !Int -> Diagnostic
	newDiagnostic line lineNumber diagnosticCounter =
		{ Diagnostic
		| range = findCharacterRange line lineNumber
		, severity = fromMaybe Warning configuration.TrailingWhitespaceConfiguration.severity
		, dCode = diagnosticCounter
		, source = "whitespace"
		, message = "Found trailing whitespace"
		}

	findCharacterRange :: !String !Int -> CharacterRange
	findCharacterRange line lineNumber
		#! numberOfWhitespaceCharacters = foldlArr (\counter char -> if (isSpace char) (counter + 1) 0) 0 line
		#! endColumn = size line
		#! startColumn = endColumn - numberOfWhitespaceCharacters
		= newRange startColumn endColumn lineNumber

	newRange :: !Int !Int !Int -> CharacterRange
	newRange startColumn endColumn lineNumber =
		{ Range
		| start =
			{ Position
			| line = lineNumber
			, character = startColumn
			}
		, end =
			{ Position
			| line = lineNumber
			, character = endColumn
			}
		}
