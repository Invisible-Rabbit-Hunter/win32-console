{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Win32.Console.Console2 where

import System.Win32.Console.Types

import Foreign.C
import Foreign.Ptr
import Foreign.C.String
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import System.Win32
import Graphics.Win32.GDI.Types (COLORREF)
import Data.Maybe

import Data.Bits
import Foreign.Storable
import System.Win32
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV

#include <cool_windows/consoleapi2.h>

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

{- Generic access rights -}
newtype GenericAccessRights = GenericAccessRights { unGenericAccessRights :: DWORD }
                              deriving (Show, Eq, Ord, Bits, Storable)
#{enum GenericAccessRights, GenericAccessRights
  , GENERIC_READ = GENERIC_READ
  , GENERIC_WRITE = GENERIC_WRITE
  , GENERIC_EXECUTE = GENERIC_EXECUTE
  , GENERIC_ALL = GENERIC_ALL
  }
  
{- File Share Modes -}
newtype FileShareMode = FileShareMode { unFileShareMode :: DWORD }
                        deriving (Show, Eq, Ord, Bits, Storable)
#{enum FileShareMode, FileShareMode
  , FILE_SHARE_READ = FILE_SHARE_READ
  , FILE_SHARE_WRITE = FILE_SHARE_WRITE
  , FILE_SHARE_DELETE = FILE_SHARE_DELETE
  }

{-
WINBASEAPI
BOOL
WINAPI
FillConsoleOutputCharacterW(
    _In_ HANDLE hConsoleOutput,
    _In_ WCHAR cCharacter,
    _In_ DWORD nLength,
    _In_ COORD dwWriteCoord,
    _Out_ LPDWORD lpNumberOfCharsWritten
    );
 -}
foreign import ccall "windows.h FillConsoleOutputCharacterW" c_FillConsoleOutputCharacterW :: HANDLE -> WCHAR -> DWORD -> {- COORD -} DWORD -> LPDWORD -> IO BOOL

fillConsoleOutputCharacter :: HANDLE -> Char -> DWORD -> COORD -> IO DWORD
fillConsoleOutputCharacter h c l p =
  alloca $ \pCharsWritten -> do
  let c' = fromIntegral (fromEnum c)
  failIfFalse_ "FillCOnsoleOutputCharacterW" $ c_FillConsoleOutputCharacterW h c' l (coordToDWord p) pCharsWritten
  peek pCharsWritten


-- fillConsoleOutputCharacter :: HANDLE -> WCHAR -> DWORD -> COORD -> IO DWORD
-- fillConsoleOutputCharacter h c l p = do
--   let p' = coordToDWord

{-
WINBASEAPI
BOOL
WINAPI
FillConsoleOutputAttribute(
    _In_ HANDLE hConsoleOutput,
    _In_ WORD wAttribute,
    _In_ DWORD nLength,
    _In_ COORD dwWriteCoord,
    _Out_ LPDWORD lpNumberOfAttrsWritten
    );
 -}
foreign import ccall "windows.h FillConsoleOutputAttribute" c_FillConsoleOutputAttribute :: HANDLE -> AttributeFlags -> DWORD -> {- COORD -} DWORD -> LPDWORD -> IO BOOL

fillConsoleOutputAttribute :: HANDLE -> AttributeFlags -> DWORD -> COORD -> IO DWORD
fillConsoleOutputAttribute h a l p =
    alloca $ \pAttrsWritten -> do
    failIfFalse_ "FillCOnsoleOutputAttribute" $ c_FillConsoleOutputAttribute h a l (coordToDWord p) pAttrsWritten
    peek pAttrsWritten

{- 
WINBASEAPI
BOOL
WINAPI
GenerateConsoleCtrlEvent(
    _In_ DWORD dwCtrlEvent,
    _In_ DWORD dwProcessGroupId
    );
 -}
foreign import ccall "windows.h GenerateConsoleCtrlEvent" c_GenerateConsoleCtrlEvent :: CtrlEvent -> DWORD -> IO BOOL


{- WINBASEAPI
HANDLE
WINAPI
CreateConsoleScreenBuffer(
    _In_ DWORD dwDesiredAccess,
    _In_ DWORD dwShareMode,
    _In_opt_ CONST SECURITY_ATTRIBUTES* lpSecurityAttributes,
    _In_ DWORD dwFlags,
    _Reserved_ LPVOID lpScreenBufferData
    );
 -}
foreign import ccall "windows.h CreateConsoleScreenBuffer" c_CreateConsoleScreenBuffer :: GenericAccessRights -> FileShareMode -> LPVOID -> DWORD -> LPVOID -> IO HANDLE

createConsoleScreenBuffer :: GenericAccessRights -> FileShareMode -> IO HANDLE
createConsoleScreenBuffer gar fsm = c_CreateConsoleScreenBuffer gar fsm nullPtr CONSOLE_TEXTMODE_BUFFER nullPtr

{- 
WINBASEAPI
BOOL
WINAPI
SetConsoleActiveScreenBuffer(
    _In_ HANDLE hConsoleOutput
    );
 -}
foreign import ccall "windows.h SetConsoleActiveScreenBuffer" c_SetConsoleActiveScreenBuffer :: HANDLE -> IO BOOL

setConsoleActiveScreenBuffer :: HANDLE -> IO ()
setConsoleActiveScreenBuffer h = failIfFalse_ "SetConsoleActiveScreenBuffer" $ c_SetConsoleActiveScreenBuffer h

{- 
WINBASEAPI
BOOL
WINAPI
FlushConsoleInputBuffer(
    _In_ HANDLE hConsoleInput
    );
 -}
foreign import ccall "windows.h FlushConsoleInputBuffer" c_FlushConsoleInputBuffer :: HANDLE -> IO BOOL

flushConsoleInputBuffer :: HANDLE -> IO ()
flushConsoleInputBuffer h = failIfFalse_ "FlushConsoleInputBuffer" $ c_FlushConsoleInputBuffer h

{- 
WINBASEAPI
BOOL
WINAPI
SetConsoleCP(
    _In_ UINT wCodePageID
    );
 -}
foreign import ccall "windows.h SetConsoleCP" c_SetConsoleCP :: UINT -> IO BOOL

setConsoleCP :: UINT -> IO ()
setConsoleCP cp = failIfFalse_ "SetConsoleCP" $ c_SetConsoleCP cp

{- 
WINBASEAPI
BOOL
WINAPI
SetConsoleOutputCP(
    _In_ UINT wCodePageID
    );
 -}
foreign import ccall "windows.h SetConsoleOutputCP" c_SetConsoleOutputCP :: UINT -> IO BOOL

setConsoleOutputCP :: UINT -> IO ()
setConsoleOutputCP cp = failIfFalse_ "SetConsoleOutputCP" $ c_SetConsoleOutputCP cp

{- 
WINBASEAPI
BOOL
WINAPI
GetConsoleCursorInfo(
    _In_ HANDLE hConsoleOutput,
    _Out_ PCONSOLE_CURSOR_INFO lpConsoleCursorInfo
    );
 -}
data CONSOLE_CURSOR_INFO = CONSOLE_CURSOR_INFO { cciSize :: DWORD
                                               , cciVisible :: BOOL
                                               }
                         deriving (Show, Eq, Ord)
type PCONSOLE_CURSOR_INFO = Ptr CONSOLE_CURSOR_INFO

instance Storable CONSOLE_CURSOR_INFO where
  sizeOf _ = #{size CONSOLE_CURSOR_INFO}
  alignment _ = #{alignment CONSOLE_CURSOR_INFO}

  peek p = CONSOLE_CURSOR_INFO <$> #{peek CONSOLE_CURSOR_INFO, dwSize} p
                               <*> #{peek CONSOLE_CURSOR_INFO, bVisible} p

  poke p x = #{poke CONSOLE_CURSOR_INFO, dwSize} p (cciSize x)
          *> #{poke CONSOLE_CURSOR_INFO, bVisible} p (cciVisible x) 

foreign import ccall "windows.h GetConsoleCursorInfo" c_GetConsoleCursorInfo :: HANDLE -> PCONSOLE_CURSOR_INFO -> IO BOOL

getConsoleCursorInfo :: HANDLE -> IO CONSOLE_CURSOR_INFO
getConsoleCursorInfo h =
    alloca $ \pCursorInfo -> do
    failIfFalse_ "GetConsoleCursorInfo" $ c_GetConsoleCursorInfo h pCursorInfo
    peek pCursorInfo

{- 
WINBASEAPI
BOOL
WINAPI
SetConsoleCursorInfo(
    _In_ HANDLE hConsoleOutput,
    _In_ CONST CONSOLE_CURSOR_INFO* lpConsoleCursorInfo
    );
 -}
foreign import ccall "windows.h SetConsoleCursorInfo" c_SetConsoleCursorInfo :: HANDLE -> PCONSOLE_CURSOR_INFO -> IO BOOL
setConsoleCursorInfo :: HANDLE -> CONSOLE_CURSOR_INFO -> IO ()
setConsoleCursorInfo h cursorInfo =
    alloca $ \pCursorInfo -> do
    poke pCursorInfo cursorInfo
    failIfFalse_ "SetConsoleCursorInfo" $ c_SetConsoleCursorInfo h pCursorInfo

{- 
WINBASEAPI
BOOL
WINAPI
GetConsoleScreenBufferInfo(
    _In_ HANDLE hConsoleOutput,
    _Out_ PCONSOLE_SCREEN_BUFFER_INFO lpConsoleScreenBufferInfo
    );
 -}
data CONSOLE_SCREEN_BUFFER_INFO = CONSOLE_SCREEN_BUFFER_INFO { csbiSize :: COORD
                                                             , csbiCursorPosition :: COORD
                                                             , csbiAttributes :: WORD
                                                             , csbiWindow :: SMALL_RECT
                                                             , csbiMaximumWindowSize :: COORD
                                                             }
                                deriving (Show, Eq, Ord)
type PCONSOLE_SCREEN_BUFFER_INFO = Ptr CONSOLE_SCREEN_BUFFER_INFO

instance Storable CONSOLE_SCREEN_BUFFER_INFO where
  sizeOf _ = #{size CONSOLE_SCREEN_BUFFER_INFO}
  alignment _ = #{alignment CONSOLE_SCREEN_BUFFER_INFO}

  peek p = CONSOLE_SCREEN_BUFFER_INFO <$> #{peek CONSOLE_SCREEN_BUFFER_INFO, dwSize} p
                                      <*> #{peek CONSOLE_SCREEN_BUFFER_INFO, dwCursorPosition} p
                                      <*> #{peek CONSOLE_SCREEN_BUFFER_INFO, wAttributes} p
                                      <*> #{peek CONSOLE_SCREEN_BUFFER_INFO, srWindow} p
                                      <*> #{peek CONSOLE_SCREEN_BUFFER_INFO, dwMaximumWindowSize} p

  poke p x = #{poke CONSOLE_SCREEN_BUFFER_INFO, dwSize} p (csbiSize x)
          *> #{poke CONSOLE_SCREEN_BUFFER_INFO, dwCursorPosition} p (csbiCursorPosition x) 
          *> #{poke CONSOLE_SCREEN_BUFFER_INFO, wAttributes} p (csbiAttributes x) 
          *> #{poke CONSOLE_SCREEN_BUFFER_INFO, srWindow} p (csbiWindow x) 
          *> #{poke CONSOLE_SCREEN_BUFFER_INFO, dwMaximumWindowSize} p (csbiMaximumWindowSize x) 

foreign import ccall "windows.h GetConsoleScreenBufferInfo" c_GetConsoleScreenBufferInfo :: HANDLE -> PCONSOLE_SCREEN_BUFFER_INFO -> IO BOOL

getConsoleScreenBufferInfo :: HANDLE -> IO CONSOLE_SCREEN_BUFFER_INFO
getConsoleScreenBufferInfo h =
    alloca $ \pConsoleScreenBufferInfo -> do
    failIfFalse_ "GetConsoleScreenBufferInfo" $ c_GetConsoleScreenBufferInfo h pConsoleScreenBufferInfo
    peek pConsoleScreenBufferInfo

{- 
WINBASEAPI
BOOL
WINAPI
GetConsoleScreenBufferInfoEx(
    _In_ HANDLE hConsoleOutput,
    _Inout_ PCONSOLE_SCREEN_BUFFER_INFOEX lpConsoleScreenBufferInfoEx
    );
 -}
data CONSOLE_SCREEN_BUFFER_INFOEX = CONSOLE_SCREEN_BUFFER_INFOEX { csbiexSize :: COORD
                                                                 , csbiexCursorPosition :: COORD
                                                                 , csbiexAttributes :: WORD
                                                                 , csbiexWindow :: SMALL_RECT
                                                                 , csbiexMaximumWindowSize :: COORD
                                                                 , csbiexPopupAttributes :: WORD
                                                                 , csbiexFullscreenSupported :: BOOL
                                                                 , csbiexColorTable :: [COLORREF]
                                                                 }
                                  deriving (Show, Eq, Ord)
type PCONSOLE_SCREEN_BUFFER_INFOEX = Ptr CONSOLE_SCREEN_BUFFER_INFOEX

instance Storable CONSOLE_SCREEN_BUFFER_INFOEX where
  sizeOf _ = #{size CONSOLE_SCREEN_BUFFER_INFOEX}
  alignment _ = #{alignment CONSOLE_SCREEN_BUFFER_INFOEX}

  peek p = CONSOLE_SCREEN_BUFFER_INFOEX <$> #{peek CONSOLE_SCREEN_BUFFER_INFOEX, dwSize} p
                                        <*> #{peek CONSOLE_SCREEN_BUFFER_INFOEX, dwCursorPosition} p
                                        <*> #{peek CONSOLE_SCREEN_BUFFER_INFOEX, wAttributes} p
                                        <*> #{peek CONSOLE_SCREEN_BUFFER_INFOEX, srWindow} p
                                        <*> #{peek CONSOLE_SCREEN_BUFFER_INFOEX, dwMaximumWindowSize} p
                                        <*> #{peek CONSOLE_SCREEN_BUFFER_INFOEX, wPopupAttributes} p
                                        <*> #{peek CONSOLE_SCREEN_BUFFER_INFOEX, bFullscreenSupported} p
                                        <*> peekArray 16 (#{ptr CONSOLE_SCREEN_BUFFER_INFOEX, ColorTable} p)

  poke p x = #{poke CONSOLE_SCREEN_BUFFER_INFOEX, cbSize} p (sizeOf x)
          *> #{poke CONSOLE_SCREEN_BUFFER_INFOEX, dwSize} p (csbiexSize x)
          *> #{poke CONSOLE_SCREEN_BUFFER_INFOEX, dwCursorPosition} p (csbiexCursorPosition x) 
          *> #{poke CONSOLE_SCREEN_BUFFER_INFOEX, wAttributes} p (csbiexAttributes x) 
          *> #{poke CONSOLE_SCREEN_BUFFER_INFOEX, srWindow} p (csbiexWindow x) 
          *> #{poke CONSOLE_SCREEN_BUFFER_INFOEX, dwMaximumWindowSize} p (csbiexMaximumWindowSize x) 
          *> #{poke CONSOLE_SCREEN_BUFFER_INFOEX, wPopupAttributes} p (csbiexPopupAttributes x) 
          *> #{poke CONSOLE_SCREEN_BUFFER_INFOEX, bFullscreenSupported} p (csbiexFullscreenSupported x) 
          *> pokeArray (#{ptr CONSOLE_SCREEN_BUFFER_INFOEX, ColorTable} p) (csbiexColorTable x) 

foreign import ccall "windows.h GetConsoleScreenBufferInfoEx" c_GetConsoleScreenBufferInfoEx :: HANDLE -> PCONSOLE_SCREEN_BUFFER_INFOEX -> IO BOOL

getConsoleScreenBufferInfoEx :: HANDLE -> IO CONSOLE_SCREEN_BUFFER_INFOEX
getConsoleScreenBufferInfoEx h =
    alloca $ \pConsoleScreenBufferInfo -> do
    failIfFalse_ "GetConsoleScreenBufferInfoEx" $ c_GetConsoleScreenBufferInfoEx h pConsoleScreenBufferInfo
    peek pConsoleScreenBufferInfo

{- 
WINBASEAPI
BOOL
WINAPI
SetConsoleScreenBufferInfoEx(
    _In_ HANDLE hConsoleOutput,
    _In_ PCONSOLE_SCREEN_BUFFER_INFOEX lpConsoleScreenBufferInfoEx
    );
 -}
foreign import ccall "windows.h SetConsoleScreenBufferInfoEx" c_SetConsoleScreenBufferInfoEx :: HANDLE -> PCONSOLE_SCREEN_BUFFER_INFOEX -> IO BOOL

setConsoleScreenBufferInfoEx :: HANDLE -> CONSOLE_SCREEN_BUFFER_INFOEX -> IO ()
setConsoleScreenBufferInfoEx h consoleScreenBufferInfo =
    alloca $ \pConsoleScreenBufferInfo -> do
    poke pConsoleScreenBufferInfo consoleScreenBufferInfo
    failIfFalse_ "SetConsoleScreenBufferInfoEx" $ c_SetConsoleScreenBufferInfoEx h pConsoleScreenBufferInfo

{- 
WINBASEAPI
BOOL
WINAPI
SetConsoleScreenBufferSize(
    _In_ HANDLE hConsoleOutput,
    _In_ COORD dwSize
    );
 -}
foreign import ccall "windows.h SetConsoleScreenBufferSize" c_SetConsoleScreenBufferSize :: HANDLE -> {- COORD -} DWORD -> IO BOOL

setConsoleScreenBufferBufferSize :: HANDLE -> COORD -> IO ()
setConsoleScreenBufferBufferSize h size =
    failIfFalse_ "SetConsoleScreenBufferSize" $ c_SetConsoleScreenBufferSize h (coordToDWord size)


{- 
WINBASEAPI
BOOL
WINAPI
SetConsoleCursorPosition(
    _In_ HANDLE hConsoleOutput,
    _In_ COORD dwCursorPosition
    );
 -}
foreign import ccall "windows.h SetConsoleCursorPosition" c_SetConsoleCursorPosition :: HANDLE -> {- COORD -} DWORD -> IO BOOL

setConsoleCursorPosition :: HANDLE -> COORD -> IO ()
setConsoleCursorPosition h p = failIfFalse_ "SetConsoleCursorPositionW" $ c_SetConsoleCursorPosition h (coordToDWord p)

{- 
WINBASEAPI
COORD
WINAPI
GetLargestConsoleWindowSize(
    _In_ HANDLE hConsoleOutput
    );
 -}
foreign import ccall "windows.h GetLargestConsoleWindowSize" c_GetLargestConsoleWindowSize :: HANDLE -> IO  {- COORD -} DWORD

getLargestConsoleWindowSize :: HANDLE -> IO COORD
getLargestConsoleWindowSize x = dwordToCoord <$> failIf (== 0) "GetLargestConsoleWindowSize" (c_GetLargestConsoleWindowSize x)

{- 
WINBASEAPI
BOOL
WINAPI
SetConsoleTextAttribute(
    _In_ HANDLE hConsoleOutput,
    _In_ WORD wAttributes
    );
 -}
foreign import ccall "windows.h SetConsoleTextAttribute" c_SetConsoleTextAttribute :: HANDLE -> AttributeFlags -> IO BOOL

setConsoleTextAttribute :: HANDLE -> AttributeFlags -> IO ()
setConsoleTextAttribute h a = failIfFalse_ "SetConsoleTextAttribute" $ c_SetConsoleTextAttribute h a

{- 
WINBASEAPI
BOOL
WINAPI
SetConsoleWindowInfo(
    _In_ HANDLE hConsoleOutput,
    _In_ BOOL bAbsolute,
    _In_ CONST SMALL_RECT* lpConsoleWindow
    );
 -}
foreign import ccall "windows.h SetConsoleWindowInfo" c_SetConsoleWindowInfo :: HANDLE -> BOOL -> Ptr SMALL_RECT -> IO BOOL

setConsoleWindowInfo :: HANDLE -> BOOL -> SMALL_RECT -> IO ()
setConsoleWindowInfo h a sr =
    alloca $ \pSr -> do
    poke pSr sr
    failIfFalse_ "SetConsoleWindowInfo" $ c_SetConsoleWindowInfo h a pSr

{- 
WINBASEAPI
BOOL
WINAPI
WriteConsoleOutputCharacterW(
    _In_ HANDLE hConsoleOutput,
    _In_reads_(nLength) LPCWSTR lpCharacter,
    _In_ DWORD nLength,
    _In_ COORD dwWriteCoord,
    _Out_ LPDWORD lpNumberOfCharsWritten
    );
 -}
foreign import ccall "windows.h WriteConsoleOutputCharacterW" c_WriteConsoleOutputCharacterW :: HANDLE -> LPCWSTR -> DWORD -> {- COORD -} DWORD -> LPDWORD -> IO BOOL

writeConsoleOutputCharacter :: HANDLE -> WinString -> COORD -> IO DWORD
writeConsoleOutputCharacter h chars p =
    alloca $ \pCharsWritten ->
    withVectorPtr chars $ \ptr len -> do
    failIfFalse_ "WriteConsoleOutputCharacterW" $ c_WriteConsoleOutputCharacterW h ptr (fromIntegral len) (coordToDWord p) pCharsWritten
    peek pCharsWritten

{- 
WINBASEAPI
BOOL
WINAPI
WriteConsoleOutputAttribute(
    _In_ HANDLE hConsoleOutput,
    _In_reads_(nLength) CONST WORD* lpAttribute,
    _In_ DWORD nLength,
    _In_ COORD dwWriteCoord,
    _Out_ LPDWORD lpNumberOfAttrsWritten
    );
 -}
foreign import ccall "windows.h WriteConsoleOutputAttribute" c_WriteConsoleOutputAttribute :: HANDLE -> Ptr AttributeFlags -> DWORD -> {- COORD -} DWORD -> LPDWORD -> IO BOOL

writeConsoleOutputAttribute :: HANDLE -> V.Vector AttributeFlags -> COORD -> IO DWORD
writeConsoleOutputAttribute h attrs p =
    alloca $ \pAttrsWritten ->
    withVectorPtr attrs $ \ptr len -> do
    failIfFalse_ "WriteConsoleOutputAttribute" $ c_WriteConsoleOutputAttribute h ptr (fromIntegral len) (coordToDWord p) pAttrsWritten
    peek pAttrsWritten

{- 
WINBASEAPI
BOOL
WINAPI
ReadConsoleOutputCharacterW(
    _In_ HANDLE hConsoleOutput,
    _Out_writes_(nLength) LPWSTR lpCharacter,
    _In_ DWORD nLength,
    _In_ COORD dwReadCoord,
    _Out_ LPDWORD lpNumberOfCharsRead
    );
 -}
foreign import ccall "windows.h ReadConsoleOutputCharacterW" c_ReadConsoleOutputCharacterW :: HANDLE -> LPWSTR -> DWORD -> {- COORD -} DWORD -> LPDWORD -> IO BOOL

readConsoleOutputCharacter :: HANDLE -> DWORD -> COORD -> IO WinString
readConsoleOutputCharacter h len pos =
  alloca $ \pReadChars -> do
  v <- newVectorWith (fromIntegral len) $ \p ->
    failIfFalse_ "ReadConsoleOutputCharacterW" $ c_ReadConsoleOutputCharacterW h p len (coordToDWord pos) pReadChars
  readChars <- peek pReadChars
  pure (V.take (fromIntegral readChars) v)

{- 
WINBASEAPI
BOOL
WINAPI
ReadConsoleOutputAttribute(
    _In_ HANDLE hConsoleOutput,
    _Out_writes_(nLength) LPWORD lpAttribute,
    _In_ DWORD nLength,
    _In_ COORD dwReadCoord,
    _Out_ LPDWORD lpNumberOfAttrsRead
    );
 -}
foreign import ccall "windows.h ReadConsoleOutputAttribute" c_ReadConsoleOutputAttribute :: HANDLE -> Ptr AttributeFlags -> DWORD -> {- COORD -} DWORD -> LPDWORD -> IO BOOL

readConsoleOutputAttribute :: HANDLE -> DWORD -> COORD -> IO (V.Vector AttributeFlags)
readConsoleOutputAttribute h len pos =
  alloca $ \pReadChars -> do
  v <- newVectorWith (fromIntegral len) $ \p ->
    failIfFalse_ "ReadConsoleOutputAttribute" $ c_ReadConsoleOutputAttribute h p len (coordToDWord pos) pReadChars
  readChars <- peek pReadChars
  pure (V.take (fromIntegral readChars) v)

{- 
WINBASEAPI
BOOL
WINAPI
WriteConsoleInputW(
    _In_ HANDLE hConsoleInput,
    _In_reads_(nLength) CONST INPUT_RECORD* lpBuffer,
    _In_ DWORD nLength,
    _Out_ LPDWORD lpNumberOfEventsWritten
    );
 -}
foreign import ccall "windows.h WriteConsoleInputW" c_WriteConsoleInputW :: HANDLE -> Ptr INPUT_RECORD -> DWORD -> LPDWORD -> IO BOOL

writeConsoleInput :: HANDLE -> V.Vector INPUT_RECORD -> IO DWORD
writeConsoleInput h v =
  alloca $ \pEventsWritten ->
  withVectorPtr v $ \p l -> do
  failIfFalse_ "WriteConsoleInputW" $ c_WriteConsoleInputW h p (fromIntegral l) pEventsWritten
  peek pEventsWritten

{- 
WINBASEAPI
BOOL
WINAPI
ScrollConsoleScreenBufferW(
    _In_ HANDLE hConsoleOutput,
    _In_ CONST SMALL_RECT* lpScrollRectangle,
    _In_opt_ CONST SMALL_RECT* lpClipRectangle,
    _In_ COORD dwDestinationOrigin,
    _In_ CONST CHAR_INFO* lpFill
    );
 -}
foreign import ccall "windows.h ScrollConsoleScreenBufferW" c_ScrollConsoleScreenBufferW :: HANDLE
                                                                                         -> Ptr SMALL_RECT
                                                                                         -> Ptr SMALL_RECT
                                                                                         -> {- COORD -} DWORD
                                                                                         -> Ptr CHAR_INFO
                                                                                         -> IO BOOL

scrollConsoleScreenBuffer :: HANDLE -> SMALL_RECT -> Maybe SMALL_RECT -> COORD -> CHAR_INFO -> IO ()
scrollConsoleScreenBuffer h scroll mclip dest fill =
  alloca $ \pScroll -> do
  poke pScroll scroll
  alloca $ \pFill -> do
    poke pFill fill
    case mclip of
     Nothing -> failIfFalse_ "ScrollConsoleScreenBufferW" $ c_ScrollConsoleScreenBufferW h pScroll nullPtr (coordToDWord dest) pFill
     Just clip -> alloca $ \pClip -> do
                  poke pClip clip
                  failIfFalse_ "ScrollConsoleScreenBufferW" $ c_ScrollConsoleScreenBufferW h pScroll pClip (coordToDWord dest) pFill

{- 
WINBASEAPI
BOOL
WINAPI
WriteConsoleOutputW(
    _In_ HANDLE hConsoleOutput,
    _In_reads_(dwBufferSize.X * dwBufferSize.Y) CONST CHAR_INFO* lpBuffer,
    _In_ COORD dwBufferSize,
    _In_ COORD dwBufferCoord,
    _Inout_ PSMALL_RECT lpWriteRegion
    );
 -}
foreign import ccall "windows.h WriteConsoleOutputW" c_WriteConsoleOutputW :: HANDLE
                                                                           -> Ptr CHAR_INFO
                                                                           -> {- COORD -} DWORD
                                                                           -> {- COORD -} DWORD
                                                                           -> PSMALL_RECT
                                                                           -> IO BOOL

-- writeConsoleOutput :: HANDLE -> V.Vector CHAR_INFO -> COORD -> COORD -> IO SMALL_RECT
-- writeConsoleOutput h v size pos =
--   alloca $ \pWriteRegion ->
--   withVectorPtr v $ \p l -> do
--   failIfFalse_ "WriteConsoleOutputW" $ c_WriteConsoleOutputW h p (coordToDWord size) (coordToDWord pos) pWriteRegion
--   peek pWriteRegion

-- {- 
-- WINBASEAPI
-- BOOL
-- WINAPI
-- ReadConsoleOutputW(
--     _In_ HANDLE hConsoleOutput,
--     _Out_writes_(dwBufferSize.X * dwBufferSize.Y) PCHAR_INFO lpBuffer,
--     _In_ COORD dwBufferSize,
--     _In_ COORD dwBufferCoord,
--     _Inout_ PSMALL_RECT lpReadRegion
--     );
--  -}
-- foreign import ccall "windows.h ReadConsoleOutputW" c_ReadConsoleOutputW :: HANDLE
--                                                                          -> PCHAR_INFO
--                                                                          -> {- COORD -} DWORD
--                                                                          -> {- COORD -} DWORD
--                                                                          -> PSMALL_RECT
--                                                                          -> IO BOOL

-- readConsoleOutput :: HANDLE -> COORD -> COORD -> SMALL_RECT -> IO (V.Vector CHAR_INFO, SMALL_RECT) 
-- readConsoleOutput h size pos readRegion =
--   alloca $ \pReadRegion -> do
--   poke pReadRegion readRegion
--   v <- newVectorWith $\pBuffer -> do
--     failWithFalse_ "ReadConsoleOutputW" 
  

{- 
WINBASEAPI
DWORD
WINAPI
GetConsoleTitleW(
    _Out_writes_(nSize) LPWSTR lpConsoleTitle,
    _In_ DWORD nSize
    );
 -}
foreign import ccall "windows.h GetConsoleTitleW" c_GetConsoleTitleW :: HANDLE
                                                                     -> LPWSTR
                                                                     -> DWORD
                                                                     -> IO DWORD

-- getConsoleTitle :: DWORD -> IO WinString
-- getConsoleTitle l = do
--   newVectorWith $ \p -> do
--     p <- failIfZero "GetConsoleTitle"

{- 
WINBASEAPI
DWORD
WINAPI
GetConsoleOriginalTitleW(
    _Out_writes_(nSize) LPWSTR lpConsoleTitle,
    _In_ DWORD nSize
    );
 -}
foreign import ccall "windows.h GetConsoleOriginalTitleW" c_GetConsoleOriginalTitleW :: HANDLE
                                                                                     -> LPWSTR
                                                                                     -> DWORD
                                                                                     -> IO DWORD

{- 
WINBASEAPI
BOOL
WINAPI
SetConsoleTitleA(
    _In_ LPCSTR lpConsoleTitle
    );
 -}
foreign import ccall "windows.h SetConsoleTitleA" c_SetConsoleTitleA :: HANDLE
                                                                     -> LPSTR
                                                                     -> IO BOOL

{- 
WINBASEAPI
BOOL
WINAPI
SetConsoleTitleW(
    _In_ LPCWSTR lpConsoleTitle
    );
 -}
foreign import ccall "windows.h SetConsoleTitleW" c_SetConsoleTitleW :: HANDLE
                                                                     -> LPWSTR
                                                                     -> IO BOOL
