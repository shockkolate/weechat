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

{-# LANGUAGE ForeignFunctionInterface #-}

module API where

import Foreign.C.Types (CInt(..))
import Foreign.C.String
import Foreign.Ptr

type GuiBufferPtr = Ptr ()

type InputCB = Ptr () -> GuiBufferPtr -> CString -> IO CInt
type CloseCB = Ptr () -> GuiBufferPtr -> IO CInt
foreign import ccall "wrapper" fromInputCB :: InputCB -> IO (FunPtr InputCB)
foreign import ccall "wrapper" fromCloseCB :: CloseCB -> IO (FunPtr CloseCB)

foreign import ccall "weechat_hs_api_rc_ok" weechat_rc_ok :: CInt
foreign import ccall "weechat_hs_api_rc_ok_eat" weechat_rc_ok_eat :: CInt
foreign import ccall "weechat_hs_api_rc_error" weechat_rc_error :: CInt

foreign import ccall "weechat_hs_api_print"
    weechat_print :: GuiBufferPtr -> CString -> IO ()
foreign import ccall "weechat_hs_api_buffer_new"
    weechat_buffer_new :: CString -> FunPtr InputCB -> Ptr () -> FunPtr CloseCB -> Ptr () -> IO GuiBufferPtr
