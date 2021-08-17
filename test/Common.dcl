definition module Common

from System.Process import :: ProcessHandle, :: ProcessIO, :: WritePipe, :: ReadPipe

/**
 * Starts the language server process.
 * @result The process handle, and the process IO.
 */
startLanguageServer :: !*World -> ((ProcessHandle, ProcessIO), *World)

/**
 * Shuts down the language server process.
 * @param The process handle.
 * @param The process pipes.
 * @result Any remaining data in the stdout pipe.
 */
shutdownLanguageServer :: !ProcessHandle !ProcessIO !*World -> (?String, *World)

/**
 * Generates a message in the LSP format.
 * @param The message body.
 * @result A well-formed LSP message.
 */
generateMessage :: !String -> String

/**
 * Writes a message to a pipe.
 * @param The message.
 * @param The write pipe.
 */
writeMessage :: !String !WritePipe !*World -> *World

/**
 * Reads a message from a pipe.
 * @param The read pipe.
 * @result A complete LSP message.
 */
readMessage :: !ReadPipe !*World -> (String, *World)
