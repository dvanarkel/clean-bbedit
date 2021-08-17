implementation module Eastwood.Util.FileFinder

import StdEnv

import System.File
import System.FilePath

findSearchPath :: !String !(l FilePath) !*World -> (!?FilePath, !*World) | List l {#Char}
findSearchPath _ [|] w = (?None, w)
findSearchPath fileName [|path:paths] w
	# (exi, w) = fileExists (path </> fileName) w
	= if exi (?Just path, w) (findSearchPath fileName paths w)
