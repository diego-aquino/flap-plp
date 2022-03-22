:- module(terminal,[getTerminalWidth/1,getTerminalHeight/1,startPlayerInputThread/0,fetchFromThread/1]).

getTerminalWidth(Width):- tty_size(_, Width).
getTerminalHeight(Height):- tty_size(Height, _).

startPlayerInputThread:- 
thread_create(getPlayerInput, Id).

getPlayerInput:- 
  get_key(NewInput),
  handlePlayerInput(NewInput),
  getPlayerInput.

get_key(X):-                                         
  ttyflush, 
  get_single_char(X).

handlePlayerInput(Input):- thread_send_message(main,Input).

fetchFromThread(Resp):- thread_get_message(main,Resp,[timeout(0)]).
fetchFromThread(Resp):- thread_get_message(main,Resp,[timeout(0)]); Resp = "Nothing".

/*


interruptSignal :: Char
interruptSignal = chr 0 -- NULL character in the ASCII table

newtype Terminal = Terminal
  {inputChar :: MVar Char}

createTerminal :: IO Terminal
createTerminal = do
  configureTerminalProperties

  inputChar <- newEmptyMVar

  installHandler sigINT (\signal -> onInterrupt inputChar)
  installHandler sigTERM (\signal -> onInterrupt inputChar)

  startPlayerInputThread inputChar

  let terminal = Terminal inputChar

  return terminal

configureTerminalProperties :: IO ()
configureTerminalProperties = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  ANSI.hideCursor

onInterrupt :: MVar Char -> IO ()
onInterrupt inputChar = do
  ANSI.showCursor
  putMVar inputChar interruptSignal

startPlayerInputThread :: MVar Char -> IO ThreadId
startPlayerInputThread inputChar = forkIO $ getPlayerInput inputChar

getPlayerInput :: MVar Char -> IO ()
getPlayerInput inputChar = do
  newChar <- getChar
  putMVar inputChar newChar

  let shouldStop = newChar == interruptSignal
  if shouldStop
    then return ()
    else do
      getPlayerInput inputChar

takeLastReceivedCharacter :: MVar Char -> IO (Maybe Char)
takeLastReceivedCharacter inputChar = do tryTakeMVar inputChar

getTerminalWidth :: IO Int
getTerminalWidth = do
  terminalDimensions <- Size.size
  return (Size.width (fromJust terminalDimensions))

getTerminalHeight :: IO Int
getTerminalHeight = do
  terminalDimensions <- Size.size
  return (Size.height (fromJust terminalDimensions))

resetStylesAndCursor :: IO ()
resetStylesAndCursor = do
  ANSI.setSGR [ANSI.Reset]
  ANSI.setCursorPosition 0 0*/

