{-
 - WeechatHaskell.hs - haskell plugin for WeeChat
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

module WeechatHaskell where

import Foreign.C.String (newCString)
import Foreign.Ptr
import API

close :: CloseCB
close dat buffer = return weechat_RC_OK

foreign export ccall test_buffer_new :: IO ()
test_buffer_new :: IO ()
test_buffer_new = do
    cs <- newCString "haskell"
    fpClose <- fromCloseCB close
    buf <- weechat_buffer_new cs nullFunPtr nullPtr fpClose nullPtr
    --weechat_print buf cs
    return ()
