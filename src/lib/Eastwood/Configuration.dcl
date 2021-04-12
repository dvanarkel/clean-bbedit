definition module Eastwood.Configuration

from Eastwood.Pass.BasicValueCAFs import :: BasicValueCAFsConfiguration
from Eastwood.Pass.TrailingWhitespace import :: TrailingWhitespaceConfiguration
import Eastwood.Range

/**
 * The configuration of all passes. The order of the passes field is respected by the library.
 */
:: Configuration =
	{ lineRanges :: ![LineRange]
	//* What lines of the given file should be considered. LineRanges are assumed to be ordered and non-overlapping.
	, passes :: ![PassConfiguration]
	//* What passes should be performed, and what should their configuration be
	}

/**
 * Should be extended for every additional pass. Configuration of the passes should be defined in their specific module.
 */
:: PassConfiguration
	= BasicValueCAFsConfiguration BasicValueCAFsConfiguration
	| TrailingWhitespaceConfiguration TrailingWhitespaceConfiguration
