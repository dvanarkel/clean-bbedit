# Design
## Linter Library
The linter library will be designed as a library that can be used in a small wrapper to produce errors/warnings to
stdout or be used to produce a language server.

### Library Interface
In general, the interface of the library we will be designed to be as compatible with the LSP protocol as possible
within a reasonable limit.

Given a `String` containing the uri of a file and `Configuration`, the library will output a `[Diagnostic]` with:
```Clean
:: Diagnostic =
	{ range :: Range
	, severity :: ?DiagnosticSeverity
	, message :: String
	}

:: Range =
	{ start :: Position
	, end :: Position
	}

:: Position =
	{ line :: Int
	, character :: Int
	}

:: DiagnosticSeverity = Error | Warning | Information | Hint

:: Configuration =
	{ lineRange :: ?LineRange
	, indentation :: ?IdentationConfiguration
	, trailingWhitespace :: ?TrailingWhitespaceConfiguration
	...
	}

:: LineRange =
	{ start :: ?Int
	, end :: ?Int
	}

:: IndentationConfiguration =
	{ severity :: ?DiagnosticSeverity
	, type :: IndentationType
	}

:: IndentationType = Tabs | Spaces Int

:: TrailingWhitespaceConfiguration =
	{ severity :: ?DiagnosticSeverity
	}

```
For more information see the [language server protocol specification][lsp-specification]. As the need arises, further
fields defined by the LSP specification can be added. While there is technically no need for the library to be
compatible with the interface specified by the LSP protocol, it reduces the number of types that might have to be
introduced in the future and the conversions between them.

The `Configuration` type of the library contains an optional range, this range bounds the part of the program that
is to be analyzed. A `?None` indicates that this side of the range is unbounded. The `Configuration` type also
contains the parameters for all passes. It is up to the passes to determine if they should return any diagnostics based
on their parameters and what their defaults are. If a field in the `Configuration` type is `?None`, that pass is
considered to be disabled.

### Library internals
The library takes the uri and produces several representations. For now there shall be one, `[String]`, where lines are
separated into individual strings. In the future there might be a another representation detailing the Clean AST after
parsing and one containing the AST after lexing.

Assuming only the first representation, passes shall have the form:
```Clean
... :: !LineRange !{Their configuration type} ![String] -> [Diagnostic]
```

A pass shall be defined in their own module, including their configuration.

## Linter Binary
The linter binary shall wrap the library. It will be responsible for parsing the configuration file and print the
diagnostics. The configuration of the linter will be done in a JSON file. The JSON file can be overridden by specifying
command line arguments, although not all arguments can be overridden. The JSON file shall be such that it can be parsed
by the generic JSON parser to the `Configuration` type. The command line arguments shall be as follows:
```
usage :: ... [OPTION]
OPTION:
	-start=Int
		Specifies the starting line for the linter, only diagnostics found after this line are output. Defaults
		to 0 if not specified.
	-end=Int
		Specifies the end line for the linter, only diagnostics found before this line are output. Defaults to
		the end of the file if not spcified.
	-c FILENAME
		Load the configuration file. Defaults to .clean-lint.json if not specified.
	-Werror
		Lifts all warning diagnostics to errors.
RETURN:
	The linter returns 0 if no errors were found, 1 otherwise.
```

[lsp-specification]: https://microsoft.github.io/language-server-protocol/specifications/specification-current
