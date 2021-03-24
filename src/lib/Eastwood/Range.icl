implementation module Eastwood.Range

import StdEnv

inLineRange :: !LineRange !CharacterRange -> Bool
inLineRange { start = ?None, end = ?None } _ = True
inLineRange { start = ?Just start, end = ?None } cr = cr.start.line >= start
inLineRange { start = ?None, end = ?Just end } cr = cr.end.line <= end
inLineRange { start = ?Just start, end = ?Just end } cr = cr.start.line >= start && cr.end.line <= end

afterLineRange :: !LineRange !CharacterRange -> Bool
afterLineRange { end = ?None } _ = False
afterLineRange { end = ?Just end } cr = cr.start.line > end
