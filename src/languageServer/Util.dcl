definition module Util

import StdEnv
from Eastwood.Range import qualified :: Range, :: Position
from LSP.Range import qualified :: Range
from LSP.RequestId import :: RequestId
from LSP.ResponseMessage import :: ResponseMessage, :: ErrorCode
from Text.YAML import :: YAMLErrorWithLocations

/**
 * Converts an Eastwood range to an LSP range.
 * @param The Eastwood range.
 * @result The LSP range.
 */
rangeCorrespondingTo :: !('Eastwood.Range'.Range 'Eastwood.Range'.Position) -> 'LSP.Range'.Range

instance toString YAMLErrorWithLocations

/**
 * A standard error response.
 * @param The request ID.
 * @param The error code.
 * @param The error message.
 * @result The error response.
 */
errorResponse :: !RequestId !ErrorCode !String -> ResponseMessage
