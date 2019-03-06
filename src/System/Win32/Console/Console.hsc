{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Win32.Console.Console where

import System.Win32.Console.Types

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import System.Win32

import Data.Bits
import Foreign.Storable
import System.Win32
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV


#include <cool_windows/consoleapi.h>

#undef hsc_enum
#define hsc_enum(t, f, print_name, x)           \
    hsc_printf ("pattern ");                            \
    print_name;                                         \
    hsc_printf (":: %s\n", #t);                        \
    hsc_printf ("pattern ");                            \
    print_name;                                         \
    hsc_printf ("= %s ", #f);                          \
    if ((x) < 0)                                        \
        hsc_printf ("(%lld)\n", (long long)(x));        \
    else                                                \
        hsc_printf ("%llu\n", (unsigned long long)(x));

pattern ATTACH_PARENT_PROCESS :: DWORD
pattern ATTACH_PARENT_PROCESS = #const ATTACH_PARENT_PROCESS

{-
WINBASEAPI
BOOL
WINAPI
AllocConsole(
    VOID
    );
 -}
foreign import ccall "windows.h AllocConsole" c_allocConsole :: IO BOOL

allocConsole :: IO ()
allocConsole = failIfFalse_ "AllocConsole" c_allocConsole

{- 
WINBASEAPI
BOOL
WINAPI
FreeConsole(
    VOID
    );
 -}
foreign import ccall "windows.h FreeConsole" c_freeConsole :: IO BOOL

freeConsole :: IO ()
freeConsole = failIfFalse_ "FreeConsole" c_freeConsole

{- 
WINBASEAPI
BOOL
WINAPI
AttachConsole(
    _In_ DWORD dwProcessId
    );
 -}
foreign import ccall "windows.h AttachConsole" c_attachConsole :: DWORD -> IO BOOL

attachConsole :: DWORD -> IO ()
attachConsole x = failIfFalse_ "AttachConsole" $ c_attachConsole x

{- 
WINBASEAPI
UINT
WINAPI
GetConsoleCP(
    VOID
    );
 -}
foreign import ccall "windows.h GetConsoleCP" c_GetConsoleCP :: IO UINT

getConsoleCP :: IO UINT
getConsoleCP = failIfZero "GetConsoleCP" c_GetConsoleCP

{- 
WINBASEAPI
UINT
WINAPI
GetConsoleOutputCP(
    VOID
    );
 -}
foreign import ccall "windows.h GetConsoleOutputCP" c_GetConsoleOutputCP :: IO UINT

getConsoleOutputCP :: IO UINT
getConsoleOutputCP = failIfZero "GetConsoleOutputCP" c_GetConsoleOutputCP

{- 
WINBASEAPI
BOOL
WINAPI
GetConsoleMode(
    _In_ HANDLE hConsoleHandle,
    _Out_ LPDWORD lpMode
    );
 -}
newtype ConsoleModeFlags = ConsoleModeFlags { unConsoleModeFlags :: DWORD }
                    deriving (Show, Eq, Ord, Storable, Bits)

#{enum ConsoleModeFlags, ConsoleModeFlags
  , ENABLE_PROCESSED_INPUT              = ENABLE_PROCESSED_INPUT
  , ENABLE_LINE_INPUT                   = ENABLE_LINE_INPUT
  , ENABLE_ECHO_INPUT                   = ENABLE_ECHO_INPUT
  , ENABLE_WINDOW_INPUT                 = ENABLE_WINDOW_INPUT
  , ENABLE_MOUSE_INPUT                  = ENABLE_MOUSE_INPUT
  , ENABLE_INSERT_MODE                  = ENABLE_INSERT_MODE
  , ENABLE_QUICK_EDIT_MODE              = ENABLE_QUICK_EDIT_MODE
  , ENABLE_EXTENDED_FLAGS               = ENABLE_EXTENDED_FLAGS
  , ENABLE_AUTO_POSITION                = ENABLE_AUTO_POSITION
  , ENABLE_VIRTUAL_TERMINAL_INPUT       = ENABLE_VIRTUAL_TERMINAL_INPUT
  }

#{enum ConsoleModeFlags, ConsoleModeFlags
  , ENABLE_PROCESSED_OUTPUT             = ENABLE_PROCESSED_OUTPUT
  , ENABLE_WRAP_AT_EOL_OUTPUT           = ENABLE_WRAP_AT_EOL_OUTPUT
  , ENABLE_VIRTUAL_TERMINAL_PROCESSING  = ENABLE_VIRTUAL_TERMINAL_PROCESSING
  , DISABLE_NEWLINE_AUTO_RETURN         = DISABLE_NEWLINE_AUTO_RETURN
  , ENABLE_LVB_GRID_WORLDWIDE           = ENABLE_LVB_GRID_WORLDWIDE
  }

foreign import ccall "windows.h GetConsoleMode" c_GetConsoleMode :: HANDLE -> Ptr ConsoleModeFlags -> IO BOOL

getConsoleMode :: HANDLE -> IO ConsoleModeFlags
getConsoleMode h = alloca $ \pcmf -> do
    failIfFalse_ "GetConsoleMode" $ c_GetConsoleMode h pcmf
    peek pcmf


{- 
WINBASEAPI
BOOL
WINAPI
SetConsoleMode(
    _In_ HANDLE hConsoleHandle,
    _In_ DWORD dwMode
    );
 -}
foreign import ccall "windows.h SetConsoleMode" c_SetConsoleMode :: HANDLE -> ConsoleModeFlags -> IO BOOL

setConsoleMode :: HANDLE -> ConsoleModeFlags -> IO ()
setConsoleMode h f = alloca $ \pcmf -> do
    poke pcmf f
    failIfFalse_ "SetConsoleMode" $ c_SetConsoleMode h f

{- 
WINBASEAPI
BOOL
WINAPI
GetNumberOfConsoleInputEvents(
    _In_ HANDLE hConsoleInput,
    _Out_ LPDWORD lpNumberOfEvents
    );
 -}
foreign import ccall "windows.h GetNumberOfConsoleInputEvents" c_GetNumberOfConsoleInputEvents :: HANDLE -> LPDWORD -> IO BOOL

getNumberOfConsoleInputEvents :: HANDLE -> IO DWORD
getNumberOfConsoleInputEvents h = alloca $ \pd -> do
    failIfFalse_ "GetNumberOfConsoleInputEvents" $ c_GetNumberOfConsoleInputEvents h pd
    peek pd

{- 
WINBASEAPI
_Success_(return != FALSE)
BOOL
WINAPI
ReadConsoleInputW(
    _In_ HANDLE hConsoleInput,
    _Out_writes_to_(nLength,*lpNumberOfEventsRead) PINPUT_RECORD lpBuffer,
    _In_ DWORD nLength,
    _Out_ _Deref_out_range_(<=,nLength) LPDWORD lpNumberOfEventsRead
    );
 -}
foreign import ccall "windows.h ReadConsoleInputW" c_ReadConsoleInputW :: HANDLE -> PINPUT_RECORD -> DWORD -> LPDWORD -> IO BOOL

readConsoleInput :: HANDLE -> DWORD -> IO [INPUT_RECORD]
readConsoleInput h l = allocaArray (fromIntegral l) $ \pira ->
                       alloca $ \pd -> do
                       failIfFalse_ "ReadConsoleInputW" (c_ReadConsoleInputW h pira l pd)
                       len <- peek pd
                       peekArray (fromIntegral len) pira

{- 
WINBASEAPI
BOOL
WINAPI
PeekConsoleInputW(
    _In_ HANDLE hConsoleInput,
    _Out_writes_(nLength) PINPUT_RECORD lpBuffer,
    _In_ DWORD nLength,
    _Out_ LPDWORD lpNumberOfEventsRead
    );
 -}
foreign import ccall "windows.h PeekConsoleInputW" c_PeekConsoleInputW :: HANDLE -> PINPUT_RECORD -> DWORD -> LPDWORD -> IO BOOL

peekConsoleInput :: HANDLE -> DWORD -> IO [INPUT_RECORD]
peekConsoleInput h l = allocaArray (fromIntegral l) $ \pira ->
                       alloca $ \pd -> do
                       failIfFalse_ "PeekConsoleInputW" (c_PeekConsoleInputW h pira l pd)
                       len <- peek pd
                       peekArray (fromIntegral len) pira
data CONSOLE_READCONSOLE_CONTROL = CONSOLE_READCONSOLE_CONTROL { crccInitialChars :: ULONG
                                                               , crccCtrlWakeupMask :: ULONG
                                                               , crccControlKeyState :: ULONG
                                                               }
                       deriving (Show, Eq, Ord)
type PCONSOLE_READCONSOLE_CONTROL = Ptr CONSOLE_READCONSOLE_CONTROL

instance Storable CONSOLE_READCONSOLE_CONTROL where
  sizeOf _ = #{size CONSOLE_READCONSOLE_CONTROL}
  alignment _ = #{alignment CONSOLE_READCONSOLE_CONTROL}

  peek p = CONSOLE_READCONSOLE_CONTROL <$> #{peek CONSOLE_READCONSOLE_CONTROL, nInitialChars} p
                                       <*> #{peek CONSOLE_READCONSOLE_CONTROL, dwCtrlWakeupMask} p
                                       <*> #{peek CONSOLE_READCONSOLE_CONTROL, dwControlKeyState} p

  poke p x = #{poke CONSOLE_READCONSOLE_CONTROL, nLength} p (sizeOf x)
          *> #{poke CONSOLE_READCONSOLE_CONTROL, nInitialChars} p (crccInitialChars x) 
          *> #{poke CONSOLE_READCONSOLE_CONTROL, dwCtrlWakeupMask} p (crccCtrlWakeupMask x) 
          *> #{poke CONSOLE_READCONSOLE_CONTROL, dwControlKeyState} p (crccControlKeyState x) 

{- 
WINBASEAPI
_Success_(return != FALSE)
BOOL
WINAPI
ReadConsoleW(
    _In_ HANDLE hConsoleInput,
    _Out_writes_bytes_to_(nNumberOfCharsToRead * sizeof(WCHAR),*lpNumberOfCharsRead * sizeof(WCHAR)) LPVOID lpBuffer,
    _In_ DWORD nNumberOfCharsToRead,
    _Out_ _Deref_out_range_(<=,nNumberOfCharsToRead) LPDWORD lpNumberOfCharsRead,
    _In_opt_ PCONSOLE_READCONSOLE_CONTROL pInputControl
    );
 -}
foreign import ccall "windows.h ReadConsoleW" c_ReadConsoleW :: HANDLE -> LPVOID -> DWORD -> LPDWORD -> PCONSOLE_READCONSOLE_CONTROL -> IO BOOL

readConsole :: HANDLE -> DWORD -> Maybe CONSOLE_READCONSOLE_CONTROL -> IO WinString 
readConsole h numChars control =
  alloca $ \pReadChars -> do
  v <- newVectorWith (fromIntegral numChars) $ \p -> do
    failIfFalse_ "ReadConsoleW" $
            case control of
              Nothing -> c_ReadConsoleW h (castPtr p) numChars pReadChars nullPtr
              Just control' ->
                alloca $ \pControl ->
                poke pControl control' *>
                c_ReadConsoleW h (castPtr p) numChars pReadChars pControl 
  readChars <- peek pReadChars
  pure (V.take (fromIntegral readChars) v)

{- 
WINBASEAPI
BOOL
WINAPI
WriteConsoleW(
    _In_ HANDLE hConsoleOutput,
    _In_reads_(nNumberOfCharsToWrite) CONST VOID* lpBuffer,
    _In_ DWORD nNumberOfCharsToWrite,
    _Out_opt_ LPDWORD lpNumberOfCharsWritten,
    _Reserved_ LPVOID lpReserved
    );
 -}
foreign import ccall "windows.h WriteConsoleW" c_WriteConsoleW :: HANDLE -> LPVOID -> DWORD -> LPDWORD -> LPDWORD -> IO BOOL

writeConsole :: HANDLE -> WinString -> IO DWORD
writeConsole h buf =
  alloca $ \pCharsWritten -> do
  withVectorPtr buf $ \ptr len ->
    failIfFalse_ "WriteConsoleW" $ c_WriteConsoleW h (castPtr ptr) (fromIntegral len) pCharsWritten nullPtr
  peek pCharsWritten

{-
WINBASEAPI
BOOL
WINAPI
SetConsoleCtrlHandler(
    _In_opt_ PHANDLER_ROUTINE HandlerRoutine,
    _In_ BOOL Add
    );
 -}
type HANDLER_ROUTINE = CtrlEventFlags -> IO BOOL
type PHANDLER_ROUTINE = FunPtr HANDLER_ROUTINE

newtype CtrlEventFlags = CtrlEventFlags { unCtrlEventFlags :: DWORD }
                         deriving (Show, Eq, Ord, Storable, Bits)
#{enum CtrlEventFlags, CtrlEventFlags
  , CTRL_C_EVENT        = CTRL_C_EVENT
  , CTRL_BREAK_EVENT    = CTRL_BREAK_EVENT
  , CTRL_CLOSE_EVENT    = CTRL_CLOSE_EVENT
  , CTRL_LOGOFF_EVENT   = CTRL_LOGOFF_EVENT
  , CTRL_SHUTDOWN_EVENT = CTRL_SHUTDOWN_EVENT
  }
foreign import ccall "wrapper" makeHandlerRoutinePtr :: HANDLER_ROUTINE -> IO PHANDLER_ROUTINE
foreign import ccall "windows.h SetConsoleCtrlHandler" c_SetConsoleCtrlHandler :: PHANDLER_ROUTINE -> BOOL -> IO BOOL

setConsoleCtrlHandler :: HANDLER_ROUTINE -> BOOL -> IO ()
setConsoleCtrlHandler hr add = do
  phr <- makeHandlerRoutinePtr hr
  failIfFalse_ "SetConsoleCtrlHandler" $ c_SetConsoleCtrlHandler phr add