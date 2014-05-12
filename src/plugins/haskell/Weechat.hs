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

module Weechat
( nullPtr
, RC, DataPtr, GuiBufferPtr
, ShutdownCB, InputCB, CloseCB
, weechat_rc_ok, weechat_rc_ok_eat, weechat_rc_error
, register
, Weechat.print
, buffer_new
) where

import Foreign.C.String
import Foreign.Ptr
import Foreign.StablePtr
import qualified API

type RC = API.RC
type DataPtr = API.DataPtr
type GuiBufferPtr = API.GuiBufferPtr

type ShutdownCB = API.ShutdownCB
type InputCB = Ptr () -> GuiBufferPtr -> String -> IO RC
type CloseCB = API.CloseCB

wrapInputCB :: InputCB -> API.InputCB
wrapInputCB f dat buf s = peekCString s >>= f dat buf

weechat_rc_ok = API.weechat_rc_ok
weechat_rc_ok_eat = API.weechat_rc_ok_eat
weechat_rc_error = API.weechat_rc_error

register :: String -> String -> String -> String -> String -> Maybe ShutdownCB -> String -> IO RC
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

print :: GuiBufferPtr -> String -> IO ()
print buf s = withCString s (API.plugin_print buf)

buffer_new :: String -> Maybe InputCB -> Ptr () -> Maybe CloseCB -> Ptr () -> IO GuiBufferPtr
buffer_new name maybeInputCB inputData maybeCloseCB closeData = do
    cName <- newCString name
    fpInput <- case maybeInputCB of
        Just cb -> API.fromInputCB (wrapInputCB cb)
        Nothing -> return nullFunPtr
    fpClose <- case maybeCloseCB of
        Just cb -> API.fromCloseCB cb
        Nothing -> return nullFunPtr
    API.plugin_buffer_new cName fpInput inputData fpClose closeData
