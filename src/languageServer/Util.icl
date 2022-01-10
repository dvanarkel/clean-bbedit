implementation module Util

import StdEnv

import Text
import Text.YAML

from LSP.BasicTypes import :: UInt, uint
import qualified LSP.Position
from LSP.Position import qualified :: Position {..}
import LSP.ResponseMessage
import qualified LSP.Range
from LSP.Range import qualified :: Range {..}

import qualified Eastwood.Range
from Eastwood.Range import :: Range {..}, :: CharacterRange
from Eastwood.Range import qualified :: Position {..}

rangeCorrespondingTo :: !('Eastwood.Range'.Range 'Eastwood.Range'.Position) -> 'LSP.Range'.Range
rangeCorrespondingTo range =
	{'LSP.Range'.Range
		| 'LSP.Range'.start = positionCorrespondingTo range.'Eastwood.Range'.Range.start
		, 'LSP.Range'.end = positionCorrespondingTo range.'Eastwood.Range'.Range.end
	}
where
	positionCorrespondingTo :: !'Eastwood.Range'.Position -> 'LSP.Position'.Position
	positionCorrespondingTo {'Eastwood.Range'.Position| 'Eastwood.Range'.line, 'Eastwood.Range'.character} =
		{'LSP.Position'.Position
			| 'LSP.Position'.line = uint line
			, 'LSP.Position'.character = uint character
		}

instance toString YAMLErrorWithLocations where
	toString {error, locations} =
		concat5
			"Error occurred while constructing YAML: "
			(toString error)
			"."
			(if (isEmpty locations) ("") ("The following hints were provided for solving the error: "))
			(join ". " (map (\l -> "Error occurred " +++ toString l) locations))

instance toString ErrorLocation where
	toString (ADT a) = concat3 "while parsing ADT \"" a "\""
	toString (Constructor c) = concat3 "while parsing constructor \"" c "\""
	toString (Record r) = concat3 "while parsing record \"" r "\""
	toString (Field f) = concat3 "while parsing field \"" f "\""
	toString (SequenceIndex i) = "at sequence index " +++ (toString i)

errorResponse :: !RequestId !ErrorCode !String -> ResponseMessage
errorResponse id err msg =
	{ ResponseMessage
		| id = ?Just id
	, result = ?None
	, error = ?Just
		{ ResponseError
		| errorCode = err
		, message = msg
		, data = ?None
		}
	}
