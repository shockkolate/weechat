#
# Copyright (C) 2014 David Farrell (Shockk) <shokku.ra@gmail.com>
#
# This file is part of WeeChat, the extensible chat client.
#
# WeeChat is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# WeeChat is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with WeeChat.  If not, see <http://www.gnu.org/licenses/>.
#

if(GHC_FOUND)
  add_custom_target(haskell ALL
    COMMAND cd ${CMAKE_CURRENT_SOURCE_DIR} &&
      ghc -Wall -O2 -dynamic -shared -fPIC -o ${CMAKE_CURRENT_BINARY_DIR}/haskell.so
      WeechatHaskell.hs weechat-haskell.c weechat-haskell-api.c
      $<TARGET_LINKER_FILE:weechat_plugins_scripts>
      -lHSrts-ghc7.8.2
    COMMAND cd ${CMAKE_CURRENT_SOURCE_DIR} && cabal install --enable-shared
    DEPENDS WeechatHaskell.hs API.hs weechat-haskell.c weechat-haskell.h
            weechat-haskell-api.c
  )
  install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/haskell.so DESTINATION ${LIBDIR}/plugins)
endif()
