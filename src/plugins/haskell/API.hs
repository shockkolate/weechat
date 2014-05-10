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

module API
( InputCB, CloseCB
, buffer_new
) where

import Foreign.C.Types (CInt)
import Foreign.C.String
import Foreign.Ptr

type GuiBuffer = ()

type InputCB = Ptr () -> Ptr GuiBuffer -> CString -> IO CInt
type CloseCB = Ptr () -> Ptr GuiBuffer -> IO CInt

foreign import ccall "weechat_hs_api_buffer_new" buffer_new
    :: CString -> FunPtr InputCB -> Ptr () -> FunPtr CloseCB -> Ptr () -> IO (Ptr GuiBuffer)
