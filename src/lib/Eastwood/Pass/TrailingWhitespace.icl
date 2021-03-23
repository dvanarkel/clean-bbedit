implementation module Eastwood.Pass.TrailingWhitespace

import StdEnv

from Data.Array import foldlArr
from Data.Maybe import fromMaybe, class Maybe

import Eastwood.Configuration
import Eastwood.Diagnostic

runPass :: ![LineRange] !TrailingWhitespaceConfiguration ![String] -> [Diagnostic]
runPass lineRanges configuration contents = runPass` lineRanges contents 1 0
where
	runPass` :: ![LineRange] ![String] !Int !Int -> [Diagnostic]
	runPass` _ [] _ _ = []
	runPass` [] _ _ _ = []
	runPass` lrx=:[lr:lrs] lx=:[l:ls] lineNumber diagnosticCounter
		// If we are in a line range and it is a whitespace, produce error continue with next line
		| inLineRange lr lineNumber && finalCharacterIsWhitespace l =
			[newDiagnostic l lineNumber diagnosticCounter : runPass` lrx ls (lineNumber + 1) (diagnosticCounter + 1)]
		// If we are past the current line range, go to the next one, stay on current line
		| afterLineRange lr lineNumber = runPass` lrs lx lineNumber diagnosticCounter
		// Otherwise, go to next line
		| otherwise = runPass` lrx ls (lineNumber + 1) diagnosticCounter

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
