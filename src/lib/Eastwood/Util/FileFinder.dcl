definition module Eastwood.Util.FileFinder

//* This module provides functionality to find the search path a file is in.

from _SystemStrictLists import class List
from System.FilePath import :: FilePath

/**
 * Find the search path containing a particular file.
 *
 * File system errors are ignored.
 *
 * NB: the context has `{#Char}` instead of `FilePath` due to
 * https://gitlab.science.ru.nl/clean-compiler-and-rts/compiler/-/issues/98.
 *
 * @param The relative file path to look for.
 * @param The list of search paths.
 * @result The first search path containing the file, if it could be found.
 */
findSearchPath :: !String !(l FilePath) !*World -> (!?FilePath, !*World) | List l {#Char}
	special l=[]; l=[!]
