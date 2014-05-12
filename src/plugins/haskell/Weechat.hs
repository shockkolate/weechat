{-
 - API.hs - haskell foreign API functions
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

module Weechat where

import Foreign.C.Types (CInt(..))
import Foreign.C.String
import Foreign.Ptr
import qualified API

type RC = API.RC
type GuiBufferPtr = API.GuiBufferPtr
type InputCB = Ptr () -> GuiBufferPtr -> String -> IO RC
type CloseCB = Ptr () -> GuiBufferPtr -> IO RC

wrapInputCB :: InputCB -> API.InputCB
wrapInputCB f dat buf s = peekCString s >>= f dat buf

weechat_rc_ok = API.weechat_rc_ok
weechat_rc_ok_eat = API.weechat_rc_ok_eat
weechat_rc_error = API.weechat_rc_error

print :: API.GuiBufferPtr -> String -> IO ()
print buf s = withCString s (API.plugin_print buf)

buffer_new :: String -> Maybe InputCB -> Ptr () -> Maybe CloseCB -> Ptr () -> IO GuiBufferPtr
buffer_new name mInputCB inputData mCloseCB closeData = do
    cName <- newCString name
    fpInput <- case mInputCB of
        Just cb -> API.fromInputCB (wrapInputCB cb)
        Nothing -> return nullFunPtr
    fpClose <- case mCloseCB of
        Just cb -> API.fromCloseCB cb
        Nothing -> return nullFunPtr
    API.plugin_buffer_new cName fpInput inputData fpClose closeData
