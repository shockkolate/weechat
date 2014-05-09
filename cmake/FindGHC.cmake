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

# - Find GHC
# This module finds if GHC is installed and determines where the executable and
# libraries are. It also determines what the name of the library is. This code
# sets the following variables:
#
#  GHC_EXECUTABLE = full path to the ghc binary
#  GHC_FOUND = is ghc usable on system?

find_package(PkgConfig)
if(PKG_CONFIG_FOUND)
  pkg_search_module(FFI libffi)
endif()

message(STATUS "checking for GHC executable")
find_program(GHC_EXECUTABLE
  NAMES ghc
  PATHS /usr/bin /usr/local/bin /usr/pkg/bin
)

if(GHC_EXECUTABLE)
  message(STATUS "  found")

  execute_process(
    COMMAND ${GHC_EXECUTABLE} --print-libdir
    OUTPUT_VARIABLE GHC_LIB_DIR
  )

  # remove the new lines from the output by replacing them with empty strings
  string(REPLACE "\n" "" GHC_LIB_DIR "${GHC_LIB_DIR}")
  set(GHC_FOUND TRUE)

  mark_as_advanced(
    GHC_EXECUTABLE
  )
endif()
