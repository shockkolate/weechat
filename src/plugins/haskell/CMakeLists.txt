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
  add_custom_command(OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/WeechatHaskell.o
                     COMMAND ${GHC_EXECUTABLE} -fPIC ${CMAKE_CURRENT_SOURCE_DIR}/WeechatHaskell.hs
                     MAIN_DEPENDENCY WeechatHaskell.hs
  )
  add_custom_command(OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/weechat-haskell.o
                     COMMAND ${GHC_EXECUTABLE} -fPIC -c ${CMAKE_CURRENT_SOURCE_DIR}/weechat-haskell.c
                                                     -o ${CMAKE_CURRENT_SOURCE_DIR}/weechat-haskell.o
                     MAIN_DEPENDENCY weechat-haskell.c
  )

  add_library(haskell MODULE weechat-haskell.o WeechatHaskell.o
              weechat-haskell.h HsFFI.h)

  add_custom_command(TARGET haskell PRE_BUILD
    COMMAND cd ${CMAKE_CURRENT_SOURCE_DIR} && ghc --make -dynamic -shared -fPIC WeechatHaskell.hs weechat-haskell.c -o haskell.so -optl-Wl,-soname,haskell.so
  )

  set_target_properties(haskell PROPERTIES PREFIX "")
  set_target_properties(haskell PROPERTIES LINKER_LANGUAGE C)

  set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
  set(CMAKE_C_COMPILER ${GHC_EXECUTABLE})
  set(CMAKE_C_FLAGS "--make -Wall -fPIC -shared -dynamic -no-hs-main -lHSrts_thr-ghc7.8.2")
  set(CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS}")
  set(CMAKE_SHARED_LIBRARY_SONAME_C_FLAG "-optl-Wl,-soname,")
  target_link_libraries(haskell weechat_plugins_scripts)

  install(TARGETS haskell LIBRARY DESTINATION ${LIBDIR}/plugins)
endif()
