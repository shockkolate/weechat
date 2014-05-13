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

type RC = CInt

type ShutdownCB = IO RC
type InputCB = Ptr () -> Ptr () -> CString -> IO RC
type CloseCB = Ptr () -> Ptr () -> IO RC
type BuildCB = Ptr () -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO CString

foreign import ccall "wrapper" fromShutdownCB :: ShutdownCB -> IO (FunPtr ShutdownCB)
foreign import ccall "wrapper" fromInputCB :: InputCB -> IO (FunPtr InputCB)
foreign import ccall "wrapper" fromCloseCB :: CloseCB -> IO (FunPtr CloseCB)
foreign import ccall "wrapper" fromBuildCB :: BuildCB -> IO (FunPtr BuildCB)

foreign import ccall "weechat_hs_api_rc_ok" weechat_rc_ok :: RC
foreign import ccall "weechat_hs_api_rc_ok_eat" weechat_rc_ok_eat :: RC
foreign import ccall "weechat_hs_api_rc_error" weechat_rc_error :: RC

foreign import ccall "weechat_hs_api_register"
    plugin_register :: CString -> CString -> CString -> CString -> CString
                    -> FunPtr ShutdownCB -> CString -> IO CInt
foreign import ccall "weechat_hs_api_plugin_get_name"
    plugin_plugin_get_name :: Ptr () -> IO CString
foreign import ccall "weechat_hs_api_charset_set"
    plugin_charset_set :: CString -> IO RC
foreign import ccall "weechat_hs_api_print"
    plugin_print :: Ptr () -> CString -> IO RC
foreign import ccall "weechat_hs_api_buffer_new"
    plugin_buffer_new :: CString -> FunPtr InputCB -> Ptr () -> FunPtr CloseCB
                      -> Ptr () -> IO (Ptr ())
foreign import ccall "weechat_hs_api_bar_item_new"
    plugin_bar_item_new :: CString -> FunPtr BuildCB -> Ptr () -> IO (Ptr ())
foreign import ccall "weechat_hs_api_bar_new"
    plugin_bar_new :: CString -> CString -> CString -> CString -> CString
                   -> CString -> CString -> CString -> CString -> CString
                   -> CString -> CString -> CString -> CString -> CString
                   -> IO (Ptr ())
