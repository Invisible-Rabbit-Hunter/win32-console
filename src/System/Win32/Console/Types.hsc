{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Win32.Console.Types where

import System.Win32.Console.VirtualKeyCodes

import Data.Bits
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Array
import System.Win32
import System.Win32.String
import Unsafe.Coerce
import System.IO.Unsafe

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV

#include <cool_windows/wincontypes.h>
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

type WCHAR = CWchar
type CHAR = CChar
type LPWORD = Ptr WORD

data COORD = COORD { cX :: SHORT
                   , cY :: SHORT
                   }
           deriving (Show, Eq, Ord)
type PCOORD = Ptr COORD

dwordToCoord :: DWORD -> COORD
dwordToCoord x = COORD (unsafeCoerce (x .&. 0xFF)) (unsafeCoerce ((x .&. 0xFF00) `shiftR` 8))

coordToDWord :: COORD -> DWORD
coordToDWord (COORD x y) = (fromIntegral y) `shiftL` (sizeOf x * 8) .|. (fromIntegral x)

instance Storable COORD where
  sizeOf _ = #{size COORD}
  alignment _ = #{alignment COORD}

  peek p = COORD <$> #{peek COORD, X} p
                 <*> #{peek COORD, Y} p

  poke p x = #{poke COORD, X} p (cX x)
          *> #{poke COORD, Y} p (cY x) 

data SMALL_RECT = SMALL_RECT { srLeft :: SHORT
                             , srTop :: SHORT
                             , srRight :: SHORT
                             , srBottom :: SHORT
                             }
                deriving (Show, Eq, Ord)
type PSMALL_RECT = Ptr SMALL_RECT

instance Storable SMALL_RECT where
  sizeOf _ = #{size SMALL_RECT}
  alignment _ = #{alignment SMALL_RECT}

  peek p = SMALL_RECT <$> #{peek SMALL_RECT, Left} p
                      <*> #{peek SMALL_RECT, Top} p
                      <*> #{peek SMALL_RECT, Right} p
                      <*> #{peek SMALL_RECT, Bottom} p

  poke p x = #{poke SMALL_RECT, Left} p (srLeft x)
          *> #{poke SMALL_RECT, Top} p (srTop x) 
          *> #{poke SMALL_RECT, Right} p (srRight x) 
          *> #{poke SMALL_RECT, Bottom} p (srBottom x) 

data KEY_EVENT_RECORD = KEY_EVENT_RECORD { kerKeyDown :: BOOL
                                         , kerRepeatCount :: WORD
                                         , kerVirtualKeyCode :: VirtualKeyCode
                                         , kerVirtualScanCode :: WORD
                                         , kerChar :: WCHAR
                                         , kerControlKeyState :: ControlKeyState
                                         }
                      deriving (Show, Eq, Ord)
type PKEY_EVENT_RECORD = Ptr KEY_EVENT_RECORD

instance Storable KEY_EVENT_RECORD where
  sizeOf _ = #{size KEY_EVENT_RECORD}
  alignment _ = #{alignment KEY_EVENT_RECORD}

  peek p = KEY_EVENT_RECORD <$> #{peek KEY_EVENT_RECORD, bKeyDown} p
                            <*> #{peek KEY_EVENT_RECORD, wRepeatCount} p
                            <*> #{peek KEY_EVENT_RECORD, wVirtualKeyCode} p
                            <*> #{peek KEY_EVENT_RECORD, wVirtualScanCode} p
                            <*> #{peek KEY_EVENT_RECORD, uChar} p
                            <*> #{peek KEY_EVENT_RECORD, dwControlKeyState} p

  poke p x = #{poke KEY_EVENT_RECORD, bKeyDown} p (kerKeyDown x)
          *> #{poke KEY_EVENT_RECORD, wRepeatCount} p (kerRepeatCount x) 
          *> #{poke KEY_EVENT_RECORD, wVirtualKeyCode} p (kerVirtualKeyCode x) 
          *> #{poke KEY_EVENT_RECORD, wVirtualScanCode} p (kerVirtualScanCode x) 
          *> #{poke KEY_EVENT_RECORD, uChar} p (kerChar x) 
          *> #{poke KEY_EVENT_RECORD, dwControlKeyState} p (kerControlKeyState x) 

newtype ControlKeyState = ControlKeyState { unControlKeyState :: DWORD }
                          deriving (Show, Eq, Ord, Bits, Storable)
#{enum ControlKeyState, ControlKeyState
  , RIGHT_ALT_PRESSED = RIGHT_ALT_PRESSED
  , LEFT_ALT_PRESSED = LEFT_ALT_PRESSED
  , RIGHT_CTRL_PRESSED = RIGHT_CTRL_PRESSED
  , LEFT_CTRL_PRESSED = LEFT_CTRL_PRESSED
  , SHIFT_PRESSED = SHIFT_PRESSED
  , NUMLOCK_ON = NUMLOCK_ON
  , SCROLLLOCK_ON = SCROLLLOCK_ON
  , CAPSLOCK_ON = CAPSLOCK_ON
  , ENHANCED_KEY = ENHANCED_KEY
  , NLS_DBCSCHAR = NLS_DBCSCHAR 
  , NLS_ALPHANUMERIC = NLS_ALPHANUMERIC 
  , NLS_KATAKANA = NLS_KATAKANA 
  , NLS_HIRAGANA = NLS_HIRAGANA 
  , NLS_ROMAN = NLS_ROMAN 
  , NLS_IME_CONVERSION = NLS_IME_CONVERSION 
  , NLS_IME_DISABLE = NLS_IME_DISABLE 
  }

data MOUSE_EVENT_RECORD = MOUSE_EVENT_RECORD { merMousePosition :: COORD
                                             , merButtonState :: MouseButtonState
                                             , merControlKeyState :: ControlKeyState
                                             , merEventFlags :: MouseEventFlags
                                             }
                        deriving (Show, Eq, Ord)
type PMOUSE_EVENT_RECORD = Ptr MOUSE_EVENT_RECORD

instance Storable MOUSE_EVENT_RECORD where
  sizeOf _ = #{size MOUSE_EVENT_RECORD}
  alignment _ = #{alignment MOUSE_EVENT_RECORD}

  peek p = MOUSE_EVENT_RECORD <$> #{peek MOUSE_EVENT_RECORD, dwMousePosition} p
                              <*> #{peek MOUSE_EVENT_RECORD, dwButtonState} p
                              <*> #{peek MOUSE_EVENT_RECORD, dwControlKeyState} p
                              <*> #{peek MOUSE_EVENT_RECORD, dwEventFlags} p

  poke p x = #{poke MOUSE_EVENT_RECORD, dwMousePosition} p (merMousePosition x)
          *> #{poke MOUSE_EVENT_RECORD, dwButtonState} p (merButtonState x) 
          *> #{poke MOUSE_EVENT_RECORD, dwControlKeyState} p (merControlKeyState x) 
          *> #{poke MOUSE_EVENT_RECORD, dwEventFlags} p (merEventFlags x) 

newtype MouseButtonState = MouseButtonState { unMouseButtonState :: DWORD }
                    deriving (Show, Eq, Ord, Storable, Bits)
#{enum MouseButtonState, MouseButtonState
  , FROM_LEFT_1ST_BUTTON_PRESSED = FROM_LEFT_1ST_BUTTON_PRESSED
  , RIGHTMOST_BUTTON_PRESSED     = RIGHTMOST_BUTTON_PRESSED
  , FROM_LEFT_2ND_BUTTON_PRESSED = FROM_LEFT_2ND_BUTTON_PRESSED
  , FROM_LEFT_3RD_BUTTON_PRESSED = FROM_LEFT_3RD_BUTTON_PRESSED
  , FROM_LEFT_4TH_BUTTON_PRESSED = FROM_LEFT_4TH_BUTTON_PRESSED
  }

newtype MouseEventFlags = MouseEventFlags { unEventFlags :: WORD }
                        deriving (Show, Eq, Ord, Storable, Bits)
#{enum MouseEventFlags, MouseEventFlags
  , MOUSE_MOVED = MOUSE_MOVED   
  , DOUBLE_CLICK = DOUBLE_CLICK  
  , MOUSE_WHEELED = MOUSE_WHEELED 
  }

data WINDOW_BUFFER_SIZE_RECORD = WINDOW_BUFFER_SIZE_RECORD { wbsrSize :: COORD
                                                           }
                               deriving (Show, Eq, Ord)
type PWINDOW_BUFFER_SIZE_RECORD = Ptr WINDOW_BUFFER_SIZE_RECORD

instance Storable WINDOW_BUFFER_SIZE_RECORD where
  sizeOf _ = #{size WINDOW_BUFFER_SIZE_RECORD}
  alignment _ = #{alignment WINDOW_BUFFER_SIZE_RECORD}

  peek p = WINDOW_BUFFER_SIZE_RECORD <$> #{peek WINDOW_BUFFER_SIZE_RECORD, dwSize} p

  poke p x = #{poke WINDOW_BUFFER_SIZE_RECORD, dwSize} p (wbsrSize x)

data MENU_EVENT_RECORD = MENU_EVENT_RECORD { merCommandId :: UINT
                                           }
                       deriving (Show, Eq, Ord)
type PMENU_EVENT_RECORD = Ptr MENU_EVENT_RECORD

instance Storable MENU_EVENT_RECORD where
  sizeOf _ = #{size MENU_EVENT_RECORD}
  alignment _ = #{alignment MENU_EVENT_RECORD}

  peek p = MENU_EVENT_RECORD <$> #{peek MENU_EVENT_RECORD, dwCommandId} p

  poke p x = #{poke MENU_EVENT_RECORD, dwCommandId} p (merCommandId x)

data FOCUS_EVENT_RECORD = FOCUS_EVENT_RECORD { ferSetFocus :: BOOL
                                             }
                        deriving (Show, Eq, Ord)
type PFOCUS_EVENT_RECORD = Ptr FOCUS_EVENT_RECORD

instance Storable FOCUS_EVENT_RECORD where
  sizeOf _ = #{size FOCUS_EVENT_RECORD}
  alignment _ = #{alignment FOCUS_EVENT_RECORD}

  peek p = FOCUS_EVENT_RECORD <$> #{peek FOCUS_EVENT_RECORD, bSetFocus} p

  poke p x = #{poke FOCUS_EVENT_RECORD, bSetFocus} p (ferSetFocus x)

data INPUT_RECORD = KeyEvent KEY_EVENT_RECORD
                  | MouseEvent MOUSE_EVENT_RECORD
                  | WindowBufferSizeEvent WINDOW_BUFFER_SIZE_RECORD
                  | MenuEvent MENU_EVENT_RECORD
                  | FocusEvent FOCUS_EVENT_RECORD
                  deriving (Show, Eq, Ord)
type PINPUT_RECORD = Ptr INPUT_RECORD

instance Storable INPUT_RECORD where
  sizeOf _ = #{size INPUT_RECORD}
  alignment _ = #{alignment INPUT_RECORD}

  peek p = do
    eventType <- #{peek INPUT_RECORD, EventType} p
    case eventType of
      KEY_EVENT -> KeyEvent <$> #{peek INPUT_RECORD, Event} p
      MOUSE_EVENT -> MouseEvent <$> #{peek INPUT_RECORD, Event} p
      WINDOW_BUFFER_SIZE_EVENT -> WindowBufferSizeEvent <$> #{peek INPUT_RECORD, Event} p
      MENU_EVENT -> MenuEvent <$> #{peek INPUT_RECORD, Event} p
      FOCUS_EVENT -> FocusEvent <$> #{peek INPUT_RECORD, Event} p

  poke p x =
    case x of
      KeyEvent x' -> #{poke INPUT_RECORD, EventType} p KEY_EVENT *> #{poke INPUT_RECORD, Event} p x' 
      MouseEvent x' -> #{poke INPUT_RECORD, EventType} p MOUSE_EVENT *> #{poke INPUT_RECORD, Event} p x' 
      WindowBufferSizeEvent x' -> #{poke INPUT_RECORD, EventType} p WINDOW_BUFFER_SIZE_EVENT *> #{poke INPUT_RECORD, Event} p x' 
      MenuEvent x' -> #{poke INPUT_RECORD, EventType} p MENU_EVENT *> #{poke INPUT_RECORD, Event} p x' 
      FocusEvent x' -> #{poke INPUT_RECORD, EventType} p FOCUS_EVENT *> #{poke INPUT_RECORD, Event} p x' 

{- EventType flags -}
newtype EventType = EventType { unEventType :: WORD }
                  deriving (Show, Eq, Ord, Storable)
#{enum EventType, EventType
  , KEY_EVENT = KEY_EVENT
  , MOUSE_EVENT = MOUSE_EVENT
  , WINDOW_BUFFER_SIZE_EVENT = WINDOW_BUFFER_SIZE_EVENT
  , MENU_EVENT = MENU_EVENT
  , FOCUS_EVENT = FOCUS_EVENT
  }

data CHAR_INFO = CHAR_INFO { ciChar :: WCHAR
                           , ciAttributes :: AttributeFlags
                           }
               deriving (Show, Eq, Ord)
type PCHAR_INFO = Ptr CHAR_INFO

instance Storable CHAR_INFO where
  sizeOf _ = #{size CHAR_INFO}
  alignment _ = #{alignment CHAR_INFO}

  peek p = CHAR_INFO <$> #{peek CHAR_INFO, Char} p
                     <*> #{peek CHAR_INFO, Attributes} p

  poke p x = #{poke CHAR_INFO, Char} p (ciChar x)
          *> #{poke CHAR_INFO, Attributes} p (ciAttributes x) 


--
-- Attributes flags:
--

newtype AttributeFlags = AttributeFlags { unAttributeFlags :: WORD }
                       deriving (Show, Eq, Ord, Storable, Bits)
#{enum AttributeFlags, AttributeFlags
  , FOREGROUND_BLUE      = FOREGROUND_BLUE
  , FOREGROUND_GREEN     = FOREGROUND_GREEN
  , FOREGROUND_RED       = FOREGROUND_RED
  , FOREGROUND_INTENSITY = FOREGROUND_INTENSITY
  , BACKGROUND_BLUE      = BACKGROUND_BLUE
  , BACKGROUND_GREEN     = BACKGROUND_GREEN
  , BACKGROUND_RED       = BACKGROUND_RED
  , BACKGROUND_INTENSITY = BACKGROUND_INTENSITY
  , COMMON_LVB_LEADING_BYTE    = COMMON_LVB_LEADING_BYTE
  , COMMON_LVB_TRAILING_BYTE   = COMMON_LVB_TRAILING_BYTE
  , COMMON_LVB_GRID_HORIZONTAL = COMMON_LVB_GRID_HORIZONTAL
  , COMMON_LVB_GRID_LVERTICAL  = COMMON_LVB_GRID_LVERTICAL
  , COMMON_LVB_GRID_RVERTICAL  = COMMON_LVB_GRID_RVERTICAL
  , COMMON_LVB_REVERSE_VIDEO   = COMMON_LVB_REVERSE_VIDEO
  , COMMON_LVB_UNDERSCORE      = COMMON_LVB_UNDERSCORE

  , COMMON_LVB_SBCSDBCS        = COMMON_LVB_SBCSDBCS
  }

data CONSOLE_FONT_INFO = CONSOLE_FONT_INFO { cfiFont :: DWORD
                                           , cfiFontSize :: COORD
                                           }
                       deriving (Show, Eq, Ord)
type PCONSOLE_FONT_INFO = Ptr CONSOLE_FONT_INFO

instance Storable CONSOLE_FONT_INFO where
  sizeOf _ = #{size CONSOLE_FONT_INFO}
  alignment _ = #{alignment CONSOLE_FONT_INFO}

  peek p = CONSOLE_FONT_INFO <$> #{peek CONSOLE_FONT_INFO, nFont} p
                             <*> #{peek CONSOLE_FONT_INFO, dwFontSize} p

  poke p x = #{poke CONSOLE_FONT_INFO, nFont} p (cfiFont x)
          *> #{poke CONSOLE_FONT_INFO, dwFontSize} p (cfiFontSize x) 

type WinString = V.Vector WCHAR
type WinMString = MV.MVector WCHAR

fromString :: String ->  WinString
fromString s = unsafePerformIO $
  withTStringLen s $ \(sp, l) -> do
    mv <- MV.new (l - 1)
    MV.unsafeWith mv $ \tp ->
      copyArray tp sp (l - 1)
    V.unsafeFreeze mv

toString :: WinString ->  String
toString s = unsafePerformIO $
  let (fp, o, l) = V.unsafeToForeignPtr s in
  withForeignPtr fp $ \p ->
    peekTStringLen (p `plusPtr` o, l)

withVectorPtr :: Storable a => V.Vector a -> (Ptr a -> Int -> IO b) -> IO b
withVectorPtr str m = do
  let (fptr, off, len) = V.unsafeToForeignPtr str
  withForeignPtr fptr $ \ptr ->
    m (ptr `plusPtr` off) len

newVectorWith :: Storable a => Int -> (Ptr a -> IO ()) -> IO (V.Vector a)
newVectorWith l f = do
  mv <- MV.unsafeNew (fromIntegral l)
  let (fp, o, _) = MV.unsafeToForeignPtr mv
  withForeignPtr fp $ \p -> f (p `plusPtr` o)
  v <- V.unsafeFreeze mv
  pure v
pattern CONSOLE_TEXTMODE_BUFFER :: DWORD
pattern CONSOLE_TEXTMODE_BUFFER = 0x00000001
