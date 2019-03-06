{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Win32.Console.VirtualKeyCodes where

import System.Win32
import Foreign.Storable

#include <windows.h>

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

newtype VirtualKeyCode = VirtualKeyCode { unVirtualKeyCode :: WORD}
                       deriving (Show, Eq, Ord, Storable)

#{enum VirtualKeyCode, VirtualKeyCode


  , VK_LBUTTON        = VK_LBUTTON
  , VK_RBUTTON        = VK_RBUTTON
  , VK_CANCEL         = VK_CANCEL
  , VK_MBUTTON        = VK_MBUTTON    

  , VK_XBUTTON1       = VK_XBUTTON1    
  , VK_XBUTTON2       = VK_XBUTTON2    




  , VK_BACK           = VK_BACK
  , VK_TAB            = VK_TAB



  , VK_CLEAR          = VK_CLEAR
  , VK_RETURN         = VK_RETURN



  , VK_SHIFT          = VK_SHIFT
  , VK_CONTROL        = VK_CONTROL
  , VK_MENU           = VK_MENU
  , VK_PAUSE          = VK_PAUSE
  , VK_CAPITAL        = VK_CAPITAL

  , VK_KANA           = VK_KANA
  , VK_HANGEUL        = VK_HANGEUL  
  , VK_HANGUL         = VK_HANGUL



  , VK_JUNJA          = VK_JUNJA
  , VK_FINAL          = VK_FINAL
  , VK_HANJA          = VK_HANJA
  , VK_KANJI          = VK_KANJI



  , VK_ESCAPE         = VK_ESCAPE

  , VK_CONVERT        = VK_CONVERT
  , VK_NONCONVERT     = VK_NONCONVERT
  , VK_ACCEPT         = VK_ACCEPT
  , VK_MODECHANGE     = VK_MODECHANGE

  , VK_SPACE          = VK_SPACE
  , VK_PRIOR          = VK_PRIOR
  , VK_NEXT           = VK_NEXT
  , VK_END            = VK_END
  , VK_HOME           = VK_HOME
  , VK_LEFT           = VK_LEFT
  , VK_UP             = VK_UP
  , VK_RIGHT          = VK_RIGHT
  , VK_DOWN           = VK_DOWN
  , VK_SELECT         = VK_SELECT
  , VK_PRINT          = VK_PRINT
  , VK_EXECUTE        = VK_EXECUTE
  , VK_SNAPSHOT       = VK_SNAPSHOT
  , VK_INSERT         = VK_INSERT
  , VK_DELETE         = VK_DELETE
  , VK_HELP           = VK_HELP

, VK_0 = '0'
, VK_1 = '1'
, VK_2 = '2'
, VK_3 = '3'
, VK_4 = '4'
, VK_5 = '5'
, VK_6 = '6'
, VK_7 = '7'
, VK_8 = '8'
, VK_9 = '9'

, VK_A = 'A'
, VK_B = 'B'
, VK_C = 'C'
, VK_D = 'D'
, VK_E = 'E'
, VK_F = 'F'
, VK_G = 'G'
, VK_H = 'H'
, VK_I = 'I'
, VK_J = 'J'
, VK_K = 'K'
, VK_L = 'L'
, VK_M = 'M'
, VK_N = 'N'
, VK_O = 'O'
, VK_P = 'P'
, VK_Q = 'Q'
, VK_R = 'R'
, VK_S = 'S'
, VK_T = 'T'
, VK_U = 'U'
, VK_V = 'V'
, VK_W = 'W'
, VK_X = 'X'
, VK_Y = 'Y'
, VK_Z = 'Z'

  , VK_LWIN           = VK_LWIN
  , VK_RWIN           = VK_RWIN
  , VK_APPS           = VK_APPS



  , VK_SLEEP          = VK_SLEEP

  , VK_NUMPAD0        = VK_NUMPAD0
  , VK_NUMPAD1        = VK_NUMPAD1
  , VK_NUMPAD2        = VK_NUMPAD2
  , VK_NUMPAD3        = VK_NUMPAD3
  , VK_NUMPAD4        = VK_NUMPAD4
  , VK_NUMPAD5        = VK_NUMPAD5
  , VK_NUMPAD6        = VK_NUMPAD6
  , VK_NUMPAD7        = VK_NUMPAD7
  , VK_NUMPAD8        = VK_NUMPAD8
  , VK_NUMPAD9        = VK_NUMPAD9
  , VK_MULTIPLY       = VK_MULTIPLY
  , VK_ADD            = VK_ADD
  , VK_SEPARATOR      = VK_SEPARATOR
  , VK_SUBTRACT       = VK_SUBTRACT
  , VK_DECIMAL        = VK_DECIMAL
  , VK_DIVIDE         = VK_DIVIDE
  , VK_F1             = VK_F1
  , VK_F2             = VK_F2
  , VK_F3             = VK_F3
  , VK_F4             = VK_F4
  , VK_F5             = VK_F5
  , VK_F6             = VK_F6
  , VK_F7             = VK_F7
  , VK_F8             = VK_F8
  , VK_F9             = VK_F9
  , VK_F10            = VK_F10
  , VK_F11            = VK_F11
  , VK_F12            = VK_F12
  , VK_F13            = VK_F13
  , VK_F14            = VK_F14
  , VK_F15            = VK_F15
  , VK_F16            = VK_F16
  , VK_F17            = VK_F17
  , VK_F18            = VK_F18
  , VK_F19            = VK_F19
  , VK_F20            = VK_F20
  , VK_F21            = VK_F21
  , VK_F22            = VK_F22
  , VK_F23            = VK_F23
  , VK_F24            = VK_F24

  , VK_NUMLOCK        = VK_NUMLOCK
  , VK_SCROLL         = VK_SCROLL


  , VK_OEM_NEC_EQUAL  = VK_OEM_NEC_EQUAL  


  , VK_OEM_FJ_JISHO   = VK_OEM_FJ_JISHO  
  , VK_OEM_FJ_MASSHOU = VK_OEM_FJ_MASSHOU  
  , VK_OEM_FJ_TOUROKU = VK_OEM_FJ_TOUROKU  
  , VK_OEM_FJ_LOYA    = VK_OEM_FJ_LOYA  
  , VK_OEM_FJ_ROYA    = VK_OEM_FJ_ROYA  



  , VK_LSHIFT         = VK_LSHIFT
  , VK_RSHIFT         = VK_RSHIFT
  , VK_LCONTROL       = VK_LCONTROL
  , VK_RCONTROL       = VK_RCONTROL
  , VK_LMENU          = VK_LMENU
  , VK_RMENU          = VK_RMENU

  , VK_BROWSER_BACK        = VK_BROWSER_BACK
  , VK_BROWSER_FORWARD     = VK_BROWSER_FORWARD
  , VK_BROWSER_REFRESH     = VK_BROWSER_REFRESH
  , VK_BROWSER_STOP        = VK_BROWSER_STOP
  , VK_BROWSER_SEARCH      = VK_BROWSER_SEARCH
  , VK_BROWSER_FAVORITES   = VK_BROWSER_FAVORITES
  , VK_BROWSER_HOME        = VK_BROWSER_HOME

  , VK_VOLUME_MUTE         = VK_VOLUME_MUTE
  , VK_VOLUME_DOWN         = VK_VOLUME_DOWN
  , VK_VOLUME_UP           = VK_VOLUME_UP
  , VK_MEDIA_NEXT_TRACK    = VK_MEDIA_NEXT_TRACK
  , VK_MEDIA_PREV_TRACK    = VK_MEDIA_PREV_TRACK
  , VK_MEDIA_STOP          = VK_MEDIA_STOP
  , VK_MEDIA_PLAY_PAUSE    = VK_MEDIA_PLAY_PAUSE
  , VK_LAUNCH_MAIL         = VK_LAUNCH_MAIL
  , VK_LAUNCH_MEDIA_SELECT = VK_LAUNCH_MEDIA_SELECT
  , VK_LAUNCH_APP1         = VK_LAUNCH_APP1
  , VK_LAUNCH_APP2         = VK_LAUNCH_APP2



  , VK_OEM_1          = VK_OEM_1  
  , VK_OEM_PLUS       = VK_OEM_PLUS  
  , VK_OEM_COMMA      = VK_OEM_COMMA  
  , VK_OEM_MINUS      = VK_OEM_MINUS  
  , VK_OEM_PERIOD     = VK_OEM_PERIOD  
  , VK_OEM_2          = VK_OEM_2  
  , VK_OEM_3          = VK_OEM_3  

  , VK_OEM_4          = VK_OEM_4 
  , VK_OEM_5          = VK_OEM_5 
  , VK_OEM_6          = VK_OEM_6 
  , VK_OEM_7          = VK_OEM_7 
  , VK_OEM_8          = VK_OEM_8




  , VK_OEM_AX         = VK_OEM_AX 
  , VK_OEM_102        = VK_OEM_102 
  , VK_ICO_HELP       = VK_ICO_HELP 
  , VK_ICO_00         = VK_ICO_00 

  , VK_PROCESSKEY     = VK_PROCESSKEY

  , VK_ICO_CLEAR      = VK_ICO_CLEAR


  , VK_PACKET         = VK_PACKET




  , VK_OEM_RESET      = VK_OEM_RESET
  , VK_OEM_JUMP       = VK_OEM_JUMP
  , VK_OEM_PA1        = VK_OEM_PA1
  , VK_OEM_PA2        = VK_OEM_PA2
  , VK_OEM_PA3        = VK_OEM_PA3
  , VK_OEM_WSCTRL     = VK_OEM_WSCTRL
  , VK_OEM_CUSEL      = VK_OEM_CUSEL
  , VK_OEM_ATTN       = VK_OEM_ATTN
  , VK_OEM_FINISH     = VK_OEM_FINISH
  , VK_OEM_COPY       = VK_OEM_COPY
  , VK_OEM_AUTO       = VK_OEM_AUTO
  , VK_OEM_ENLW       = VK_OEM_ENLW
  , VK_OEM_BACKTAB    = VK_OEM_BACKTAB

  , VK_ATTN           = VK_ATTN
  , VK_CRSEL          = VK_CRSEL
  , VK_EXSEL          = VK_EXSEL
  , VK_EREOF          = VK_EREOF
  , VK_PLAY           = VK_PLAY
  , VK_ZOOM           = VK_ZOOM
  , VK_NONAME         = VK_NONAME
  , VK_PA1            = VK_PA1
  , VK_OEM_CLEAR      = VK_OEM_CLEAR
}