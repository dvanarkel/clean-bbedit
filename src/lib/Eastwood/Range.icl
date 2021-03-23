implementation module Eastwood.Range

import StdEnv

inLineRange :: !LineRange !Int -> Bool
inLineRange { start = ?None, end = ?None } _ = True
inLineRange { start = ?Just start, end = ?None } lineNumber = lineNumber >= start
inLineRange { start = ?None, end = ?Just end } lineNumber = lineNumber <= end
inLineRange { start = ?Just start, end = ?Just end } lineNumber = lineNumber >= start && lineNumber <= end

afterLineRange :: !LineRange !Int -> Bool
afterLineRange { end = ?None } _ = False
afterLineRange { end = ?Just end } lineNumber = lineNumber > end
