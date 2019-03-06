{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Win32.Console.Console3 where

import System.Win32.Console.Types

import Foreign.C
import Foreign.Ptr
import Foreign.C.String
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import System.Win32
import Graphics.Win32.GDI.Types (HWND)
import Data.Maybe

import Data.Bits
import Foreign.Storable

#include <cool_windows/consoleapi3.h>

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


{- 
WINBASEAPI
BOOL
WINAPI
GetNumberOfConsoleMouseButtons(
    _Out_ LPDWORD lpNumberOfMouseButtons
    );
 -}
foreign import ccall "windows.h GetNumberOfConsoleMosueButtons" c_getNumberOfConsoleMouseButtons :: LPDWORD
                                                                                                 -> IO BOOL

-- #if (_WIN32_WINNT >= 0x0500)
{- 
WINBASEAPI
COORD
WINAPI
GetConsoleFontSize(
    _In_ HANDLE hConsoleOutput,
    _In_ DWORD nFont
    );
 -}
foreign import ccall "windows.h GetConsoleFontSize" c_getConsoleFontSize :: HANDLE
                                                                          -> DWORD
                                                                          -> IO {- COORD -} DWORD

{- 
WINBASEAPI
BOOL
WINAPI
GetCurrentConsoleFont(
    _In_ HANDLE hConsoleOutput,
    _In_ BOOL bMaximumWindow,
    _Out_ PCONSOLE_FONT_INFO lpConsoleCurrentFont
    );
 -}

foreign import ccall "windows.h GetCurrentConsoleFont" c_getCurrentConsoleFont :: HANDLE
                                                                               -> BOOL
                                                                               -> PCONSOLE_FONT_INFO
                                                                               -> IO BOOL

-- #ifndef NOGDI

data CONSOLE_FONT_INFOEX = CONSOLE_FONT_INFOEX { cfiexFont :: DWORD
                                               , cfiexFontSize :: COORD
                                               , cfiexFontFamily :: UINT
                                               , cfiexFontWeight :: UINT
                                               , cfiexFaceName :: [WCHAR]
                                               }
                       deriving (Show, Eq, Ord)
type PCONSOLE_FONT_INFOEX = Ptr CONSOLE_FONT_INFOEX

instance Storable CONSOLE_FONT_INFOEX where
  sizeOf _ = #{size CONSOLE_FONT_INFOEX}
  alignment _ = #{alignment CONSOLE_FONT_INFOEX}

  peek p = CONSOLE_FONT_INFOEX <$> #{peek CONSOLE_FONT_INFOEX, nFont} p
                               <*> #{peek CONSOLE_FONT_INFOEX, dwFontSize} p
                               <*> #{peek CONSOLE_FONT_INFOEX, FontFamily} p
                               <*> #{peek CONSOLE_FONT_INFOEX, FontWeight} p
                               <*> peekArray (#const LF_FACESIZE) (#{ptr CONSOLE_FONT_INFOEX, FaceName} p)

  poke p x = #{poke CONSOLE_FONT_INFOEX, cbSize} p (sizeOf x)
          *> #{poke CONSOLE_FONT_INFOEX, nFont} p (cfiexFont x)
          *> #{poke CONSOLE_FONT_INFOEX, dwFontSize} p (cfiexFontSize x) 
          *> #{poke CONSOLE_FONT_INFOEX, FontFamily} p (cfiexFontFamily x) 
          *> #{poke CONSOLE_FONT_INFOEX, FontWeight} p (cfiexFontWeight x) 
          *> pokeArray (#{ptr CONSOLE_FONT_INFOEX, FaceName} p) (cfiexFaceName x) 

{- 
WINBASEAPI
BOOL
WINAPI
GetCurrentConsoleFontEx(
    _In_ HANDLE hConsoleOutput,
    _In_ BOOL bMaximumWindow,
    _Out_ PCONSOLE_FONT_INFOEX lpConsoleCurrentFontEx
    );
 -}
foreign import ccall "windows.h GetCurrentConsoleFontEx" c_getCurrentConsoleFontEx :: HANDLE
                                                                                   -> BOOL
                                                                                   -> PCONSOLE_FONT_INFOEX
                                                                                   -> IO BOOL

{- 
WINBASEAPI
BOOL
WINAPI
SetCurrentConsoleFontEx(
    _In_ HANDLE hConsoleOutput,
    _In_ BOOL bMaximumWindow,
    _In_ PCONSOLE_FONT_INFOEX lpConsoleCurrentFontEx
    );
 -}
foreign import ccall "windows.h SetCurrentConsoleFontEx" c_setCurrentConsoleFontEx :: HANDLE
                                                                                   -> BOOL
                                                                                   -> PCONSOLE_FONT_INFOEX
                                                                                   -> IO BOOL

-- #endif

--
-- Selection flags
--

newtype SelectionFlags = SelectionFlags { unSelectionFlags :: DWORD }
                         deriving (Show, Eq, Ord, Storable, Bits)
#{enum SelectionFlags, SelectionFlags
  , CONSOLE_NO_SELECTION            = CONSOLE_NO_SELECTION
  , CONSOLE_SELECTION_IN_PROGRESS   = CONSOLE_SELECTION_IN_PROGRESS
  , CONSOLE_SELECTION_NOT_EMPTY     = CONSOLE_SELECTION_NOT_EMPTY
  , CONSOLE_MOUSE_SELECTION         = CONSOLE_MOUSE_SELECTION
  , CONSOLE_MOUSE_DOWN              = CONSOLE_MOUSE_DOWN
  }


data CONSOLE_SELECTION_INFO = CONSOLE_SELECTION_INFO { csiFlags :: SelectionFlags
                                                     , csiSelectionAnchor :: COORD
                                                     , csiSelection :: SMALL_RECT
                                                     }
                       deriving (Show, Eq, Ord)
type PCONSOLE_SELECTION_INFO = Ptr CONSOLE_SELECTION_INFO

instance Storable CONSOLE_SELECTION_INFO where
  sizeOf _ = #{size CONSOLE_SELECTION_INFO}
  alignment _ = #{alignment CONSOLE_SELECTION_INFO}

  peek p = CONSOLE_SELECTION_INFO <$> #{peek CONSOLE_SELECTION_INFO, dwFlags} p
                                  <*> #{peek CONSOLE_SELECTION_INFO, dwSelectionAnchor} p
                                  <*> #{peek CONSOLE_SELECTION_INFO, srSelection} p

  poke p x = #{poke CONSOLE_SELECTION_INFO, dwFlags} p (csiFlags x)
          *> #{poke CONSOLE_SELECTION_INFO, dwSelectionAnchor} p (csiSelectionAnchor x)
          *> #{poke CONSOLE_SELECTION_INFO, srSelection} p (csiSelection x) 

{- 
WINBASEAPI
BOOL
WINAPI
GetConsoleSelectionInfo(
    _Out_ PCONSOLE_SELECTION_INFO lpConsoleSelectionInfo
    );
 -}
foreign import ccall "windows.h GetConsoleSelectionInfo" c_getConsoleSelectionInfo :: PCONSOLE_SELECTION_INFO
                                                                                   -> IO BOOL

--
-- History flags
--

newtype HistoryFlags = HistoryFlags { unHistoryFlags :: DWORD }
                         deriving (Show, Eq, Ord, Storable, Bits)
#{enum HistoryFlags, HistoryFlags
  , HISTORY_NO_DUP_FLAG = 0x1
  }

data CONSOLE_HISTORY_INFO = CONSOLE_HISTORY_INFO { chiHistoryBufferSize :: UINT
                                                 , chiNumberOfHistoryBuffers :: UINT
                                                 , chiFlags :: HistoryFlags
                                                 }
                       deriving (Show, Eq, Ord)
type PCONSOLE_HISTORY_INFO = Ptr CONSOLE_HISTORY_INFO

instance Storable CONSOLE_HISTORY_INFO where
  sizeOf _ = #{size CONSOLE_HISTORY_INFO}
  alignment _ = #{alignment CONSOLE_HISTORY_INFO}

  peek p = CONSOLE_HISTORY_INFO <$> #{peek CONSOLE_HISTORY_INFO, HistoryBufferSize} p
                                <*> #{peek CONSOLE_HISTORY_INFO, NumberOfHistoryBuffers} p
                                <*> #{peek CONSOLE_HISTORY_INFO, dwFlags} p

  poke p x = #{poke CONSOLE_HISTORY_INFO, cbSize} p (sizeOf x)
          *> #{poke CONSOLE_HISTORY_INFO, HistoryBufferSize} p (chiHistoryBufferSize x)
          *> #{poke CONSOLE_HISTORY_INFO, NumberOfHistoryBuffers} p (chiNumberOfHistoryBuffers x) 
          *> #{poke CONSOLE_HISTORY_INFO, dwFlags} p (chiFlags x) 

{- 
WINBASEAPI
BOOL
WINAPI
GetConsoleHistoryInfo(
    _Out_ PCONSOLE_HISTORY_INFO lpConsoleHistoryInfo
    );
 -}
foreign import ccall "windows.h GetConsoleHistoryInfo" c_getConsoleHistoryInfo :: PCONSOLE_HISTORY_INFO
                                                                               -> IO BOOL

{- 
WINBASEAPI
BOOL
WINAPI
SetConsoleHistoryInfo(
    _In_ PCONSOLE_HISTORY_INFO lpConsoleHistoryInfo
    );
 -}
foreign import ccall "windows.h SetConsoleHistoryInfo" c_setConsoleHistoryInfo :: PCONSOLE_HISTORY_INFO
                                                                               -> IO BOOL


newtype GetDisplayMode = GetDisplayMode { unGetDisplayMode :: DWORD }
                         deriving (Show, Eq, Ord, Storable, Bits)
#{enum GetDisplayMode, GetDisplayMode
  , CONSOLE_FULLSCREEN = CONSOLE_FULLSCREEN
  , CONSOLE_FULLSCREEN_HARDWARE = CONSOLE_FULLSCREEN_HARDWARE
  }

{- 
WINBASEAPI
BOOL
APIENTRY
GetConsoleDisplayMode(
    _Out_ LPDWORD lpModeFlags
    );
 -}
foreign import ccall "windows.h GetConsoleDisplayMode" c_getConsoleDisplayMode :: Ptr GetDisplayMode
                                                                               -> IO BOOL



{- 
WINBASEAPI
BOOL
APIENTRY
SetConsoleDisplayMode(
    _In_ HANDLE hConsoleOutput,
    _In_ DWORD dwFlags,
    _Out_opt_ PCOORD lpNewScreenBufferDimensions
    );
 -}
newtype SetDisplayMode = SetDisplayMode { unSetDisplayMode :: DWORD }
                         deriving (Show, Eq, Ord, Storable, Bits)
#{enum SetDisplayMode, SetDisplayMode
  , CONSOLE_FULLSCREEN_MODE = CONSOLE_FULLSCREEN_MODE
  , CONSOLE_WINDOWED_MODE = CONSOLE_WINDOWED_MODE
  }

foreign import ccall "windows.h SetConsoleDisplayMode" c_SetConsoleDisplayMode :: HANDLE
                                                                               -> SetDisplayMode
                                                                               -> PCOORD
                                                                               -> IO BOOL


{- 
WINBASEAPI
HWND
APIENTRY
GetConsoleWindow(
    VOID
    );
 -}

foreign import ccall "windows.h GetConsoleWindow" c_getConsoleWindow :: IO HWND

-- #endif {- _WIN32_WINNT >= 0x0500 -}

#if (_WIN32_WINNT >= 0x0501)
{- 
WINBASEAPI
BOOL
APIENTRY
AddConsoleAliasA(
    _In_ LPSTR Source,
    _In_ LPSTR Target,
    _In_ LPSTR ExeName
    );
 -}
foreign import ccall "windows.h AddConsoleAliasA" c_addConsoleAliasA :: LPSTR
                                                                     -> LPSTR
                                                                     -> LPSTR
                                                                     -> IO BOOL

{- 
WINBASEAPI
BOOL
APIENTRY
AddConsoleAliasW(
    _In_ LPWSTR Source,
    _In_ LPWSTR Target,
    _In_ LPWSTR ExeName
    );
 -}

foreign import ccall "windows.h AddConsoleAliasW" c_addConsoleAliasW :: LPWSTR
                                                                     -> LPWSTR
                                                                     -> LPWSTR
                                                                     -> IO BOOL

{- 
#ifdef UNICODE
#define AddConsoleAlias  AddConsoleAliasW
#else
#define AddConsoleAlias  AddConsoleAliasA
#endif -- !UNICODE
 -}

{- 
WINBASEAPI
DWORD
APIENTRY
GetConsoleAliasA(
    _In_ LPSTR Source,
    _Out_writes_(TargetBufferLength) LPSTR TargetBuffer,
    _In_ DWORD TargetBufferLength,
    _In_ LPSTR ExeName
    );
 -}
foreign import ccall "windows.h GetConsoleAliasA" c_getConsoleAliasA :: LPSTR
                                                                     -> LPSTR
                                                                     -> DWORD
                                                                     -> LPSTR
                                                                     -> IO DWORD

{- 
WINBASEAPI
DWORD
APIENTRY
GetConsoleAliasW(
    _In_ LPWSTR Source,
    _Out_writes_(TargetBufferLength) LPWSTR TargetBuffer,
    _In_ DWORD TargetBufferLength,
    _In_ LPWSTR ExeName
    );
 -}
foreign import ccall "windows.h GetConsoleAliasW" c_getConsoleAliasW :: LPWSTR
                                                                     -> LPWSTR
                                                                     -> DWORD
                                                                     -> LPWSTR
                                                                     -> IO DWORD

{- 
#ifdef UNICODE
#define GetConsoleAlias  GetConsoleAliasW
#else
#define GetConsoleAlias  GetConsoleAliasA
#endif -- !UNICODE
 -}

{- 
WINBASEAPI
DWORD
APIENTRY
GetConsoleAliasesLengthA(
    _In_ LPSTR ExeName
    );
 -}
foreign import ccall "windows.h GetConsoleAliasLengthA" c_getConsoleAliasLengthA :: LPSTR
                                                                                 -> IO DWORD

{- 
WINBASEAPI
DWORD
APIENTRY
GetConsoleAliasesLengthW(
    _In_ LPWSTR ExeName
    );
 -}
foreign import ccall "windows.h GetConsoleAliasLengthW" c_getConsoleAliasLengthW :: LPWSTR
                                                                                 -> IO DWORD

{- 
#ifdef UNICODE
#define GetConsoleAliasesLength  GetConsoleAliasesLengthW
#else
#define GetConsoleAliasesLength  GetConsoleAliasesLengthA
#endif -- !UNICODE
 -}

{- 
WINBASEAPI
DWORD
APIENTRY
GetConsoleAliasExesLengthA(
    VOID
    );
 -}
foreign import ccall "windows.h GetConsoleAliasExesLengthA" c_getConsoleAliasExesLengthA :: IO DWORD
{- 
WINBASEAPI
DWORD
APIENTRY
GetConsoleAliasExesLengthW(
    VOID
    );
 -}
foreign import ccall "windows.h GetConsoleAliasExesLengthW" c_getConsoleAliasExesLengthW :: IO DWORD

{- 
#ifdef UNICODE
#define GetConsoleAliasExesLength  GetConsoleAliasExesLengthW
#else
#define GetConsoleAliasExesLength  GetConsoleAliasExesLengthA
#endif -- !UNICODE
 -}

{- 
WINBASEAPI
DWORD
APIENTRY
GetConsoleAliasesA(
    _Out_writes_(AliasBufferLength) LPSTR AliasBuffer,
    _In_ DWORD AliasBufferLength,
    _In_ LPSTR ExeName
    );
 -}
foreign import ccall "windows.h GetConsoleAliasesA" c_getConsoleAliasesA :: LPSTR
                                                                         -> DWORD
                                                                         -> LPSTR
                                                                         -> IO DWORD

{- 
WINBASEAPI
DWORD
APIENTRY
GetConsoleAliasesW(
    _Out_writes_(AliasBufferLength) LPWSTR AliasBuffer,
    _In_ DWORD AliasBufferLength,
    _In_ LPWSTR ExeName
    );
 -}
foreign import ccall "windows.h GetConsoleAliasesW" c_getConsoleAliasesW :: LPWSTR
                                                                         -> DWORD
                                                                         -> LPWSTR
                                                                         -> IO DWORD

{- 
#ifdef UNICODE
#define GetConsoleAliases  GetConsoleAliasesW
#else
#define GetConsoleAliases  GetConsoleAliasesA
#endif -- !UNICODE
 -}

{- 
WINBASEAPI
DWORD
APIENTRY
GetConsoleAliasExesA(
    _Out_writes_(ExeNameBufferLength) LPSTR ExeNameBuffer,
    _In_ DWORD ExeNameBufferLength
    );
 -}
foreign import ccall "windows.h GetConsoleAliasExesA" c_getConsoleAliasExesA :: LPSTR
                                                                             -> DWORD
                                                                             -> IO DWORD

{- 
WINBASEAPI
DWORD
APIENTRY
GetConsoleAliasExesW(
    _Out_writes_(ExeNameBufferLength) LPWSTR ExeNameBuffer,
    _In_ DWORD ExeNameBufferLength
    );
 -}
foreign import ccall "windows.h GetConsoleAliasExesW" c_getConsoleAliasExesW :: LPWSTR
                                                                             -> DWORD
                                                                             -> IO DWORD

{- 
#ifdef UNICODE
#define GetConsoleAliasExes  GetConsoleAliasExesW
#else
#define GetConsoleAliasExes  GetConsoleAliasExesA
#endif -- !UNICODE
 -}
#endif {- _WIN32_WINNT >= 0x0501 -}


{- 
WINBASEAPI
VOID
APIENTRY
ExpungeConsoleCommandHistoryA(
    _In_ LPSTR ExeName
    );
 -}
foreign import ccall "windows.h ExpungeConsoleCOmmandHistoryA" c_expungeConsoleCommandHistoryA :: LPSTR
                                                                                               -> IO ()

{- 
WINBASEAPI
VOID
APIENTRY
ExpungeConsoleCommandHistoryW(
    _In_ LPWSTR ExeName
    );
 -}
foreign import ccall "windows.h ExpungeConsoleCOmmandHistoryW" c_expungeConsoleCommandHistoryW :: LPSTR
                                                                                               -> IO ()
{- 
#ifdef UNICODE
#define ExpungeConsoleCommandHistory  ExpungeConsoleCommandHistoryW
#else
#define ExpungeConsoleCommandHistory  ExpungeConsoleCommandHistoryA
#endif -- !UNICODE
 -}

{- 
WINBASEAPI
BOOL
APIENTRY
SetConsoleNumberOfCommandsA(
    _In_ DWORD Number,
    _In_ LPSTR ExeName
    );
 -}
foreign import ccall "windows.h SetConsoleNumberOfComandsA" c_setConsoleNumberOfCommandsA :: DWORD
                                                                                          -> LPSTR
                                                                                          -> IO BOOL

{- 
WINBASEAPI
BOOL
APIENTRY
SetConsoleNumberOfCommandsW(
    _In_ DWORD Number,
    _In_ LPWSTR ExeName
    );
 -}
foreign import ccall "windows.h SetConsoleNumberOfComandsW" c_setConsoleNumberOfCommandsW :: DWORD
                                                                                          -> LPWSTR
                                                                                          -> IO BOOL

{- 
#ifdef UNICODE
#define SetConsoleNumberOfCommands  SetConsoleNumberOfCommandsW
#else
#define SetConsoleNumberOfCommands  SetConsoleNumberOfCommandsA
#endif -- !UNICODE
 -}

{- 
WINBASEAPI
DWORD
APIENTRY
GetConsoleCommandHistoryLengthA(
    _In_ LPSTR ExeName
    );
 -}
foreign import ccall "windows.h GetConsoleCommandHistoryLengthA" c_getConsoleCommandHistoryLengthA :: LPSTR
                                                                                                   -> IO DWORD

{- 
WINBASEAPI
DWORD
APIENTRY
GetConsoleCommandHistoryLengthW(
    _In_ LPWSTR ExeName
    );
 -}
foreign import ccall "windows.h GetConsoleCommandHistoryLengthW" c_getConsoleCommandHistoryLengthW :: LPWSTR
                                                                                                   -> IO DWORD

{- 
#ifdef UNICODE
#define GetConsoleCommandHistoryLength  GetConsoleCommandHistoryLengthW
#else
#define GetConsoleCommandHistoryLength  GetConsoleCommandHistoryLengthA
#endif -- !UNICODE
 -}

{- 
WINBASEAPI
DWORD
APIENTRY
GetConsoleCommandHistoryA(
    _Out_writes_bytes_(CommandBufferLength) LPSTR Commands,
    _In_ DWORD CommandBufferLength,
    _In_ LPSTR ExeName
    );
 -}
foreign import ccall "windows.h GetConsoleCommandHistoryA" c_getConsoleCommandHistoryA :: LPWSTR
                                                                                       -> DWORD
                                                                                       -> LPSTR
                                                                                       -> IO DWORD

{- 
WINBASEAPI
DWORD
APIENTRY
GetConsoleCommandHistoryW(
    _Out_writes_bytes_(CommandBufferLength) LPWSTR Commands,
    _In_ DWORD CommandBufferLength,
    _In_ LPWSTR ExeName
    );
 -}
foreign import ccall "windows.h GetConsoleCommandHistoryW" c_getConsoleCommandHistoryW :: LPWSTR
                                                                                       -> DWORD
                                                                                       -> LPWSTR
                                                                                       -> IO DWORD

{- 
#ifdef UNICODE
#define GetConsoleCommandHistory  GetConsoleCommandHistoryW
#else
#define GetConsoleCommandHistory  GetConsoleCommandHistoryA
#endif -- !UNICODE
 -}

#if (_WIN32_WINNT >= 0x0501)

{- 
WINBASEAPI
DWORD
APIENTRY
GetConsoleProcessList(
    _Out_writes_(dwProcessCount) LPDWORD lpdwProcessList,
    _In_ DWORD dwProcessCount
    );
 -}
foreign import ccall "windows.h GetConsoleProcessList" c_getConsoleProcessList :: LPDWORD
                                                                               -> DWORD
                                                                               -> IO DWORD

#endif {- _WIN32_WINNT >= 0x0501 -}
