definition module Util

import StdEnv
from Eastwood.Range import qualified :: Range, :: Position
from LSP.Range import qualified :: Range
from LSP.RequestId import :: RequestId
from LSP.ResponseMessage import :: ResponseMessage, :: ErrorCode
from Text.YAML import :: YAMLErrorWithLocations

rangeCorrespondingTo :: !('Eastwood.Range'.Range 'Eastwood.Range'.Position) -> 'LSP.Range'.Range

instance toString YAMLErrorWithLocations

errorResponse :: !RequestId !ErrorCode !String -> ResponseMessage
