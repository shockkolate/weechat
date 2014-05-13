{-
 - Weechat.hs - haskell API functions
 -
 - Copyright (C) 2014 David Farrell (Shockk) <shokku.ra@gmail.com>
 -
 - This file is part of WeeChat, the extensible chat client.
 -
 - WeeChat is free software; you can redistribute it and/or modify
 - it under the terms of the GNU General Public License as published by
 - the Free Software Foundation; either version 3 of the License, or
 - (at your option) any later version.
 -
 - WeeChat is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU General Public License for more details.
 -
 - You should have received a copy of the GNU General Public License
 - along with WeeChat.  If not, see <http://www.gnu.org/licenses/>.
 -}

{-# LANGUAGE FlexibleInstances #-}

module Weechat
( PtrRep, toPtr, toRep
, RC, ShutdownCB, InputCB, CloseCB, BuildCB
, weechat_rc_ok, weechat_rc_ok_eat, weechat_rc_error
, register
, plugin_get_name
, charset_set
, print
, buffer_new
, bar_item_new
, bar_new
) where

import Prelude hiding (print)
import Foreign.C.Types (CInt(..))
import Foreign.C.String
import Foreign.Ptr
import qualified API

class PtrRep a where
    toPtr :: a -> Ptr ()
    toRep :: Ptr () -> a
instance PtrRep String where
    toPtr "" = nullPtr
    toPtr r = plusPtr nullPtr (read r)
    toRep = show

type RC = API.RC

type ShutdownCB = IO RC
type InputCB = String -> String -> String -> IO RC
type CloseCB = String -> String -> IO RC
type BuildCB = String -> String -> String -> String -> String -> IO String

wrapInputCB :: InputCB -> API.InputCB
wrapInputCB f dat buf s = peekCString s >>= f (toRep dat) (toRep buf)
wrapCloseCB :: CloseCB -> API.CloseCB
wrapCloseCB f dat buf = f (toRep dat) (toRep buf)
wrapBuildCB :: BuildCB -> API.BuildCB
wrapBuildCB f a b c d e = f (toRep a) (toRep b) (toRep c) (toRep d) (toRep e)
    >>= newCString

weechat_rc_ok :: RC
weechat_rc_ok = API.weechat_rc_ok
weechat_rc_ok_eat :: RC
weechat_rc_ok_eat = API.weechat_rc_ok_eat
weechat_rc_error :: RC
weechat_rc_error = API.weechat_rc_error

register :: String -> String -> String -> String -> String -> Maybe ShutdownCB -> String -> IO CInt
register name author version license desc mShutdownCB charset = do
    cName <- newCString name
    cAuthor <- newCString author
    cVersion <- newCString version
    cLicense <- newCString license
    cDesc <- newCString desc
    fpShutdown <- case mShutdownCB of
        Just cb -> API.fromShutdownCB cb
        Nothing -> return nullFunPtr
    cCharset <- newCString charset
    API.plugin_register cName cAuthor cVersion cLicense cDesc fpShutdown cCharset

plugin_get_name :: PtrRep a => a -> IO String
plugin_get_name p = API.plugin_plugin_get_name (toPtr p) >>= peekCString

charset_set :: String -> IO RC
charset_set s = newCString s >>= API.plugin_charset_set

print :: PtrRep a => a -> String -> IO RC
print buf s = withCString s (API.plugin_print (toPtr buf))

buffer_new :: (PtrRep a0, PtrRep a1)
           => String -> Maybe InputCB -> a0 -> Maybe CloseCB -> a1 -> IO String
buffer_new name maybeInputCB inputData maybeCloseCB closeData = do
    cName <- newCString name
    fpInput <- case maybeInputCB of
        Just cb -> API.fromInputCB (wrapInputCB cb)
        Nothing -> return nullFunPtr
    fpClose <- case maybeCloseCB of
        Just cb -> API.fromCloseCB (wrapCloseCB cb)
        Nothing -> return nullFunPtr
    API.plugin_buffer_new cName fpInput (toPtr inputData) fpClose (toPtr closeData)
        >>= return . toRep

bar_item_new :: (PtrRep a) => String -> Maybe BuildCB -> a -> IO String
bar_item_new name maybeBuildCB buildData = do
    cName <- newCString name
    fpBuild <- case maybeBuildCB of
        Just cb -> API.fromBuildCB (wrapBuildCB cb)
        Nothing -> return nullFunPtr
    API.plugin_bar_item_new cName fpBuild (toPtr buildData) >>= return . toRep

bar_new :: String -> String -> String -> String -> String -> String -> String
        -> String -> String -> String -> String -> String -> String -> String
        -> String -> IO String
bar_new x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 = do
    c1 <- newCString x1
    c2 <- newCString x2
    c3 <- newCString x3
    c4 <- newCString x4
    c5 <- newCString x5
    c6 <- newCString x6
    c7 <- newCString x7
    c8 <- newCString x8
    c9 <- newCString x9
    c10 <- newCString x10
    c11 <- newCString x11
    c12 <- newCString x12
    c13 <- newCString x13
    c14 <- newCString x14
    c15 <- newCString x15
    API.plugin_bar_new c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15
        >>= return . toRep
