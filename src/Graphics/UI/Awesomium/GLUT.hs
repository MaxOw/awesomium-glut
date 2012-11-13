----------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Awesomium.GLUT
-- Copyright   :  (c) 2012 Maksymilian Owsianny
-- License     :  LGPL-3 (see the file LICENSE)
-- 
-- Maintainer  :  Maksymilian.Owsianny+AwesomiumGLUT@gmail.com
-- Stability   :  Experimental
-- Portability :  Portable?
--
-- This is a helpful little module for using Awesomium with GLUT.
--
-- If you do not care for processing input in your program and want
-- all of it go through @Awesomium@, you can simply write:
--
-- @
--     motionCallback $= Just (injectMouseMotionGLUT wv)
--     passiveMotionCallback $= Just (injectMouseMotionGLUT wv)
--     keyboardMouseCallback $= Just (injectKeyboardMouseGLUT wv)
-- @
--
----------------------------------------------------------------------

module Graphics.UI.Awesomium.GLUT
    ( injectMouseMotionGLUT
    , injectMouseGLUT
    , injectKeyboardGLUT
    , injectKeyboardMouseGLUT
) where

import Data.Bits ((.|.))
import Data.Char
import Graphics.UI.GLUT as GL
import Graphics.UI.Awesomium as Awesomium
import Graphics.UI.Awesomium.Raw as Awesomium

----------------------------------------------------------------------

-- | Injects mouse position to Awesomium.
injectMouseMotionGLUT :: WebView -> Position -> IO ()
injectMouseMotionGLUT wv (Position x y) =
    injectMouseMove wv (fromIntegral x) (fromIntegral y)

----------------------------------------------------------------------

-- | Injects mouse buttons state to Awesomium.
injectMouseGLUT :: WebView -> Key -> KeyState -> IO ()
injectMouseGLUT wv (MouseButton b) Down =
    marshalMouseButton b $ injectMouseDown wv
injectMouseGLUT wv (MouseButton b) Up   =
    marshalMouseButton b $ injectMouseUp wv
injectMouseGLUT wv _ _ = return ()

----------------------------------------------------------------------

-- | Injects keyboard state to Awesomium.
injectKeyboardGLUT :: WebView -> Key -> KeyState -> Modifiers -> IO ()
injectKeyboardGLUT wv key@(Char k) ks m =
    injectKeyboardEvent wv $ defaultKeyboardEvent
        { wkeType = marshalType key ks
        , wkeModifiers = marshalModifiers m
        , wkeText = k
        , wkeVirtualKeyCode = marshalKey key}
injectKeyboardGLUT wv key@(SpecialKey k) ks m =
    injectKeyboardEvent wv $ defaultKeyboardEvent
        { wkeType = marshalType key ks
        , wkeModifiers = marshalModifiers m
        , wkeVirtualKeyCode = marshalKey key}
injectKeyboardGLUT wv _ _ _ = return ()

----------------------------------------------------------------------

-- | Injects both mouse and keyboard state to Awesomium. For
-- simplicity the arguments are the same as required by GLUT
-- 'Graphics.UI.GLUT.Callbacks.Window.keyboarMouseCallback'.
injectKeyboardMouseGLUT
    :: WebView -> Key -> KeyState -> Modifiers -> Position -> IO ()
injectKeyboardMouseGLUT wv mb@(MouseButton _) ks m _ =
    injectMouseGLUT wv mb ks
injectKeyboardMouseGLUT wv kb ks m _ =
    injectKeyboardGLUT wv kb ks m

----------------------------------------------------------------------

defaultKeyboardEvent = WebkeyboardEvent
    { wkeType           = WktKeydown
    , wkeModifiers      = 0
    , wkeVirtualKeyCode = 0
    , wkeNativeKeyCode  = 0
    , wkeText           = '\0'
    , wkeUnmodifiedText = '\0'
    , wkeIsSystemKey    = False }

marshalType (Char k) Down | isPrint k = WktChar
marshalType _ Down = WktKeydown
marshalType _ Up   = WktKeyup

marshalKey (Char k) = fromEnum . toUpper $ k
marshalKey (SpecialKey k) = case k of
    KeyF1        -> 0x70
    KeyF2        -> 0x71
    KeyF3        -> 0x72
    KeyF4        -> 0x73
    KeyF5        -> 0x74
    KeyF6        -> 0x75
    KeyF7        -> 0x76
    KeyF8        -> 0x77
    KeyF9        -> 0x78
    KeyF10       -> 0x79
    KeyF11       -> 0x7A
    KeyF12       -> 0x7B
    KeyLeft      -> 0x25
    KeyUp        -> 0x26
    KeyRight     -> 0x27
    KeyDown      -> 0x28
    KeyPageUp    -> 0x21
    KeyPageDown  -> 0x22
    KeyHome      -> 0x24
    KeyEnd       -> 0x23
    KeyInsert    -> 0x2D
    KeyNumLock   -> 0x90
    KeyDelete    -> 0x2E
    -- KeyUnknown k -> k
marshalKey _ = 0

marshalMouseButton LeftButton   f = f Awesomium.MbLeft
marshalMouseButton RightButton  f = f Awesomium.MbRight
marshalMouseButton MiddleButton f = f Awesomium.MbMiddle
marshalMouseButton _ _ = return ()

marshalModifiers m = foldr1 (.|.)
    [ ifd GL.shift 0x1
    , ifd GL.ctrl  0x2
    , ifd GL.alt   0x4 ]
    where ifd f v = if (f m == Down) then v else 0
