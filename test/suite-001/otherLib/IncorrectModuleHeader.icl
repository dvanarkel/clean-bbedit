module IncorrectModuleHeader

// This module header is fine *if* we would include otherLib as a search path.
// Since otherLib is not a search path, the expected module name would be
// otherLib.IncorrectModuleHeader. Eastwood should recognize this case and
// generate a diagnostic for this file complaining about the module name.
