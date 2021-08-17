implementation module Common

import StdEnv
import Text
import Data.Func
from System.FilePath import :: FilePath
import System.OSError
from System.Process import :: ProcessHandle, :: ProcessIO {..}, :: ReadPipe, :: WritePipe, runProcessIO,
    terminateProcess, writePipe, readPipeBlocking, readPipeNonBlocking

LANGUAGE_SERVER_REL_PATH = "../src/languageServer/eastwood-cls"
CONTENT_LENGTH_FIELD = "Content-Length"
NEWLINE :== "\r\n"

startLanguageServer :: !*World -> ((ProcessHandle, ProcessIO), *World)
startLanguageServer world
# (mbProc, world) = runProcessIO LANGUAGE_SERVER_REL_PATH [] ?None world
| isError mbProc = abort "Unable to start language server"
= (fromOk mbProc, world)

shutdownLanguageServer :: !ProcessHandle !ProcessIO !*World -> *World
shutdownLanguageServer handle io world
# (mbStdErr, world) = readPipeNonBlocking io.stdErr world // pass through stderr of server in this process, for debugging
# (mbError, world) = terminateProcess handle world //TODO: implement shutdown/exit in LSP to avoid ugly process killing
| isError mbError = abort "Unable to terminate language server"
| isError mbStdErr
	= world
	# (_, world) = fclose (stderr <<< fromOk mbStdErr) world
	= world

generateMessage :: !String -> String
generateMessage content = concat
    [ CONTENT_LENGTH_FIELD
    , ":"
    , toString $ size content
    , NEWLINE
    , NEWLINE
    , content
    ]

writeMessage :: !String !WritePipe !*World -> *World
writeMessage message pipe world
# (mbError, world) = writePipe message pipe world
| isError mbError = abort $ "Unable to write message: " +++ message
= world

readMessage :: !ReadPipe !*World -> (String, *World)
readMessage pipe world = readMessage` pipe "" world
where
    readMessage` :: !ReadPipe !String !*World -> (String, *World)
    readMessage` pipe acc world
    # (mbString, world) = readPipeBlocking pipe world
    | isError mbString = abort "Error reading language server response"
    # messageSoFar = acc +++ fromOk mbString
    | isCompleteMessage messageSoFar = (messageSoFar, world)
    = readMessage` pipe messageSoFar world

    isCompleteMessage :: !String -> Bool
    // TODO: it can happen that the message read from the pipe is not complete, because the buffer was flushed in
    // between. We can detect incomplete messages by verifying the content length. This has already been done in LSP
    // itself, and we can borrow from there.
    isCompleteMessage message = True
