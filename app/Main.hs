{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import System.Win32.Console.API
import System.Win32
import Graphics.Win32.Misc
import Data.Bits
import Control.Exception
import Foreign.C
import Foreign.Storable

import Data.Traversable (for)
import Control.Monad (void)
import Control.Concurrent

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV

main :: IO ()
main = do
  stdIn <- getStdHandle sTD_INPUT_HANDLE
  stdOut <- getStdHandle sTD_OUTPUT_HANDLE
  buf <- createConsoleScreenBuffer (GENERIC_READ .|. GENERIC_WRITE) (FILE_SHARE_READ .|. FILE_SHARE_WRITE)

  stdInMode <- getConsoleMode stdIn
  setConsoleMode stdIn (ENABLE_MOUSE_INPUT .|. ENABLE_EXTENDED_FLAGS)

  setConsoleActiveScreenBuffer buf

  flushConsoleInputBuffer stdIn
  let loop = do
        inps <- readConsoleInput stdIn 100
        shouldQuit <- fmap (any id) $
          for inps $ \inp ->
            case inp of
              KeyEvent kev -> 
                case kerVirtualKeyCode kev of
                  VK_ESCAPE -> pure True
                  VK_Q -> pure True
                  _ -> pure False
              MouseEvent mev -> do
                let s = fromString $ "Mouse position: " ++ show (merMousePosition mev) ++ "\n"
                writeConsoleOutputCharacter buf (fromString $ replicate 4 ' ') (COORD (3 + fromIntegral (V.length s)) 3)
                writeConsoleOutputCharacter buf s (COORD 3 3)

                if (zeroBits /= merButtonState mev .&. FROM_LEFT_1ST_BUTTON_PRESSED)
                  then void $ fillConsoleOutputAttribute buf BACKGROUND_BLUE 2 (merMousePosition mev)
                  else pure ()
                  
                if (zeroBits /= merButtonState mev .&. RIGHTMOST_BUTTON_PRESSED)
                  then void $ void $ fillConsoleOutputAttribute buf zeroBits 2 (merMousePosition mev)
                  else pure ()

                pure False
              _ -> pure False

        if shouldQuit then pure ()
                      else loop

  loop

  closeHandle buf

  setConsoleMode stdIn stdInMode

  pure ()