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

import Foreign.C.Types (CInt(..))
import Foreign.C.String
import Language.Haskell.Interpreter
import qualified Weechat

foreign export ccall haskell_load :: CString -> IO Weechat.RC
haskell_load :: CString -> IO Weechat.RC
haskell_load cPath = do
    path <- peekCString cPath
    result <- runInterpreter $ do
        loadModules [path]
        moduleNames <- getLoadedModules
        setTopLevelModules [(head moduleNames)]
        setImports ["Foreign.C.Types"]
        interpret "weechat_init" (as :: IO Weechat.RC)
    case result of
        Left (WontCompile errs) -> do
            mapM_ (Weechat.print "" . errMsg) errs
            return Weechat.weechat_rc_error
        Left e -> do
            Weechat.print "" (show e)
            return Weechat.weechat_rc_error
        Right cmp -> cmp >> return Weechat.weechat_rc_ok
