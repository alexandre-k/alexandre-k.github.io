---
title: Basics about functors
---

# Managing a global state


---
## Abstract

We will build a basic TCP server accepting connections to better
understand how to manage a global state in a pure functional language.

---

# Getting data from the command line


If we look at the [Haskell Wiki: State
Monad](https://wiki.haskell.org/State_Monad), we can get an overview
of how the State monad is implemented and how it works. Yet better,
[the talk of Phil Freeman on Mutable State in
Haskell](https://www.youtube.com/watch?v=thyO6p2bMAc), which explains
it very wel. So basically under the hood, the state is passed as an
argument to a function and is returned at the end to keep track of any
update we did with it. Also a good post on
[StackOverflow](https://stackoverflow.com/questions/13536761/what-other-ways-can-state-be-handled-in-a-pure-functional-language-besides-with)
explains it.

The idea itself being quite simple, I didn't use the monad and
implemented a simple data type myself like this:

```
data State = State { connectedUsers :: Int
                   , sockets :: [SockAddr]
                   }
```

I will keep track of all users who connected to our TCP server, and
all sockets as well. I will create also a function to initialize our
state like this:

```
initState :: State
initState = State 0 []
```

Then, inside my main loop where the server runs (see the
[Wiki](https://wiki.haskell.org/Implement_a_chat_server) for more
details about the implementation of a basic TCP server), I will update
my global state by passing it as an argument. As we can see in the
function signature, we pass a Socket, a State, and it returns an IO
operation.

We print as well our state to give us an idea. So we print in the
console any socket as a IP address and a port newly connected in a raw
format as a list, and how many users have been connecting on our
server.

```
mainLoop :: Socket -> State -> IO ()
mainLoop sock state = do
  conn <- accept sock

  let ip = snd conn
  putStrLn $ "[+] New connection: " ++ show ip
  let updatedState = State { connectedUsers = currentUsers + 1, sockets = currentSockets ++ [ip]}
  putStrLn $ "[+] State updated:\nUsers connected: " ++ show (connectedUsers updatedState) ++ "\nSockets connected: "++ show (sockets updatedState)
  forkIO (runConn conn)
  mainLoop sock updatedState
  where
    currentUsers = connectedUsers state
    currentSockets = sockets state
```

For the runConn function, just see below the whole source code.

# Fun with applicatives and global state

Once it is done, just for fun we make an ip and a port configurable
from the command line (TODO: I still have to figure out how to convert
from a String to a Word32).

```
data WebServerOptions = WebServerOptions
  { ip :: String
  , port :: PortNumber
  }


serverOptions :: Parser WebServerOptions
serverOptions = WebServerOptions
    <$> strOption
        ( long "ip"
        <> short 'i'
        <> help "IPv4 address of your server"
        <> showDefault
        <> value "127.0.0.1"
        <> metavar "STRING")
    <*> option auto
        ( long "port"
        <> short 'p'
        <> help "A port where the server will listen to"
        <> showDefault
        <> value 4000
        <> metavar "INT")


```

Then in the main function, we get our arguments passed on the command
line (so an ip address and/or a port), and just use the default
provided if nothing is passed.

We initialize there our global state, we pass it to the mainLoop, thats's all.

```
main :: IO ()
main = do
  config <- execParser opts
  let userPort = port config
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet userPort iNADDR_ANY)
  listen sock 2
  putStrLn $ "Listening for connections on port " ++ show userPort ++ "..."
  mainLoop sock initState

  where
  opts = info (serverOptions <**> helper)
      ( fullDesc
      <> progDesc "Run a web server"
      <> header "A command line application to run a TCP server on a given port." )
```

When we run it from the command line, we get an output like this:

```
$ stack exec ghc app/Main.hs && app/Main -p 5000
[1 of 1] Compiling Main             ( app/Main.hs, app/Main.o )
Linking app/Main ...
Listening for connections on port 5000...

```

As you can see I passed the port `5000` as a command line
argument. Then I connect to my server with Telnet

```
telnet localhost 5000
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
Hello!
Connection closed by foreign host.
```

And then, from the server I get the output below:

```
[+] New connection: 127.0.0.1:44102
[+] State updated:
Users connected: 1
Sockets connected: [127.0.0.1:44102]
[+] New connection: 127.0.0.1:44104
[+] State updated:
Users connected: 2
Sockets connected: [127.0.0.1:44102,127.0.0.1:44104]
[+] New connection: 127.0.0.1:44106
[+] State updated:
Users connected: 3
Sockets connected: [127.0.0.1:44102,127.0.0.1:44104,127.0.0.1:44106]
```

Basically the state gets updated, so any new socket keeps being added
to the list in the state, and our number gets incremented as
well. Quite and trivial example, still it reflects the basic idea of
state management in Haskell.


# Whole source code

```
module Main where

import Control.Concurrent
import Data.Semigroup ((<>))
import Data.Tuple (snd)
import Network.Socket
import Options.Applicative
import System.IO hiding (putStrLn)


data State = State { connectedUsers :: Int
                   , sockets :: [SockAddr]
                   }


data WebServerOptions = WebServerOptions
  { ip :: String
  , port :: PortNumber
  }


serverOptions :: Parser WebServerOptions
serverOptions = WebServerOptions
    <$> strOption
        ( long "ip"
        <> short 'i'
        <> help "IPv4 address of your server"
        <> showDefault
        <> value "127.0.0.1"
        <> metavar "STRING")
    <*> option auto
        ( long "port"
        <> short 'p'
        <> help "A port where the server will listen to"
        <> showDefault
        <> value 4000
        <> metavar "INT")


initState :: State
initState = State 0 []


mainLoop :: Socket -> State -> IO ()
mainLoop sock state = do
  conn <- accept sock

  let ip = snd conn
  putStrLn $ "[+] New connection: " ++ show ip
  let updatedState = State { connectedUsers = currentUsers + 1, sockets = currentSockets ++ [ip]}
  putStrLn $ "[+] State updated:\nUsers connected: " ++ show (connectedUsers updatedState) ++ "\nSockets connected: "++ show (sockets updatedState)
  forkIO (runConn conn)
  mainLoop sock updatedState
  where
    currentUsers = connectedUsers state
    currentSockets = sockets state



runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering
  hPutStrLn hdl "Hello!"
  hClose hdl


main :: IO ()
main = do
  config <- execParser opts
  let userPort = port config
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet userPort iNADDR_ANY)
  listen sock 2
  putStrLn $ "Listening for connections on port " ++ show userPort ++ "..."
  mainLoop sock initState

  where
  opts = info (serverOptions <**> helper)
      ( fullDesc
      <> progDesc "Run a web server"
      <> header "A command line application to run a TCP server on a given port." )
```
