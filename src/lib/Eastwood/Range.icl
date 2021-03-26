implementation module Eastwood.Range

import StdEnv

newSingleLineRange :: !Int !Int !Int -> CharacterRange
newSingleLineRange lineNumber startColumn endColumn =
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

inLineRange :: !LineRange !CharacterRange -> Bool
inLineRange { start = ?None, end = ?None } _ = True
inLineRange { start = ?Just start, end = ?None } cr = cr.start.line >= start
inLineRange { start = ?None, end = ?Just end } cr = cr.end.line <= end
inLineRange { start = ?Just start, end = ?Just end } cr = cr.start.line >= start && cr.end.line <= end

afterLineRange :: !LineRange !CharacterRange -> Bool
afterLineRange { end = ?None } _ = False
afterLineRange { end = ?Just end } cr = cr.start.line > end
