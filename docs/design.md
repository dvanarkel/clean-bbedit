# Design
## Linter Library
The linter library will be designed as a library that can be used in a small wrapper to produce errors/warnings to
stdout or be used to produce a language server.

### Library Interface
In general, the interface of the library we will be designed to be as compatible with the LSP protocol as possible
within a reasonable limit. The language server wrapper around the library is responsible for making it 100% compatible.
This is done because we do not want to be limited by the design of the LSP for features we want to have in the linter
wrapper.

Given a `String` containing the uri of a file and `Configuration`, the library will output a `[Diagnostic]` with:
```Clean
:: Diagnostic =
	{ range :: CharacterRange
	, severity :: DiagnosticSeverity
	// An incremental number that forms a unique identifier in combination with the source
	, code :: Int
	// A lowercase human readable name for the pass that created the diagnosic
	, source :: String
	// A human readable string describing the issue
	, message :: String
	}

:: Range t =
	{ start :: t
	, end :: t
	}

:: CharacterRange :== Range Position

:: Position =
	{ line :: Int
	, character :: Int
	}

:: DiagnosticSeverity = Error | Warning | Information | Hint

:: Configuration =
	{ lineRange :: [LineRange]
	, passes :: [PassConfiguration]
	}

:: PassConfiguration
	= IdentationConfiguration IdentationConfiguration
	| TrailingWhitespaceConfiguration TrailingWhitespaceConfiguration
	...

:: LineRange :== Range ?Int

:: IndentationConfiguration =
	{ severity :: ?DiagnosticSeverity
	, type :: IndentationType
	, tabstop :: ?Int // Argument only used if the type is Spaces
	}

:: IndentationType = Tabs | Spaces

:: TrailingWhitespaceConfiguration =
	{ severity :: ?DiagnosticSeverity
	}

```
For more information see the [language server protocol specification][lsp-specification]. As the need arises, further
fields defined by the LSP specification can be added. While there is technically no need for the library to be
compatible with the interface specified by the LSP protocol, it reduces the number of types that might have to be
introduced in the future and the conversions between them.

The `Configuration` type of the library contains an optional list of line range, these ranges bounds the part of the
program that is to be analyzed. A `?None` indicates that this side of the range is unbounded. The `Configuration` type
also contains the parameters for all passes. It is up to the passes to determine if they should return any diagnostics
based on their parameters and what their defaults are. If a pass is not present in the list, that pass is considered to
be disabled.

### Library internals
The library takes the uri and produces several representations. For now there shall be one, `[String]`, where lines are
separated into individual strings. In the future there might be a another representation detailing the Clean AST after
parsing and one containing the AST after lexing.

Assuming only the first representation, passes shall have the form:
```Clean
... :: ![LineRange] !{Their configuration type} ![String] -> [Diagnostic]
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
	-lines
		A comma seperated list of ranges in the form 0,1-5,99-. Overlapping ranges are undefined. Leaving out the
		beginning or end of the range (-10,100-) considers all starting from the beginning or end of the file
		respectively.
	-c FILENAME
		Load the configuration file. Defaults to .clean-lint.json if not specified.
	-Werror
		Lifts all warning diagnostics to errors.
	--output-format=FORMAT
		Changes the output format to the specified standard. Defaults to lsp is not specified.
FORMAT:
	code-climate
		Output error formats compatible with the Code Climate specification.
	lsp
		Output error formats compatible with the language server protocol.

RETURN:
	The linter returns 0 if no errors were found, 1 otherwise.
```
For the `--output-format=code-climate` flag, the binary wrapper shall be responsible for mapping the lsp severity to
the code-climate format.

## Language Server
The language server shall wrap the library to produce diagnostics. The language
server itself shall be written in a language for which a LSP SDK is available.
Communication with the library shall happen through a foreign function
interface[^1] or through some process of IPC.

[^1]: Calling Clean functions from a binary not started by the Clean RTS
  cannot currently be done. This is to be considered when the final design for
  the LSP is realized.

[lsp-specification]: https://microsoft.github.io/language-server-protocol/specifications/specification-current
