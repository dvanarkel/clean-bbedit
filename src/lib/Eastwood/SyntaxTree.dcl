definition module Eastwood.SyntaxTree

/**
 * This modules provides general functions to inspect the parsed syntax tree received from the compiler.
 */

from syntax import :: Module, :: ParsedDefinition, :: ParsedExpr, :: ParsedModule

/**
 * Retrieve all `ParsedDefinition`s found in the syntax tree. This includes
 * `ParsedDefinition`s that are part of another `ParsedDefinition`.
 */
class allDefinitions a :: !a -> [ParsedDefinition]
instance allDefinitions ParsedModule
