module MainModule

// This file is in testPrograms/someLib, but it is nevertheless called
// `MainModule`.  Importing a module from testPrograms with errors, it will
// then test that the generated diagnostics have the right path for these files
// (see #38).

import DclErrors
