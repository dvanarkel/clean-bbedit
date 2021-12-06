module errors

/**
 * This is a test program which should result on various compiler errors and warnings.
 */

// type error and ignored ! warning for the same line
Start :: !Int
Start = ()

// type error
f :: Int
f = ()

// type errors including "["
// (important to test as the compiler uses "["/"]" in it's output around module name and line number
g :: Int
g = [1]

h :: Int
h = [[1]]
