definition module Eastwood.Range

/**
 * A parameterized range with a lower an upperbound. Can be instantiated, e.g. for the LSP protocol, this range is
 * instantiated with Position, for the configuration with the line numbers.
 */
:: Range t =
	{ start :: t
	, end :: t
	}
