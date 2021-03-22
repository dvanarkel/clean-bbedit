# Roadmap
## Phase one
Setup basic infrastructure that allows checking a number of basic properties (see #8). When this is completed, a
wrapper should be created that allows using the program in CI. This includes creating the code-climate output format.

## Phase two
Add more specialized passes. At this point we will link with the compiler to parse Clean and provide context aware
diagnositcs. These could include checking if every argument of a function has documentation.

## Phase three
Create a language server that propagates these issues to a language client. Initial support would be VSCode. All
existing passes should be ported.

## Phase four
Clean compiler integration. On a save or during linting, we could call the clean compiler and parse all errors and
warning it produces via the LSP or as diagnositcs.

## Phase five
Provide more complex features to the LSP (e.g. code actions, refactorings, type derivations for untyped functions).
