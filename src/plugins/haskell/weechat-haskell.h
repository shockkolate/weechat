/*
 * Copyright (C) 2014 David Farrell (Shockk) <shokku.ra@gmail.com>
 *
 * This file is part of WeeChat, the extensible chat client.
 *
 * WeeChat is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * WeeChat is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with WeeChat.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef WEECHAT_HASKELL_H
#define WEECHAT_HASKELL_H 1

#define weechat_plugin weechat_haskell_plugin
#define HASKELL_PLUGIN_NAME "haskell"

extern void __stginit_WeechatHaskell ();

extern struct t_weechat_plugin *weechat_haskell_plugin;
//#define HASKELL_CURRENT_SCRIPT_NAME ((haskell_current_script) ? haskell_current_script->name : "-")

extern int hs_quiet;
extern struct t_plugin_script *hs_scripts;
extern struct t_plugin_script *last_hs_script;
extern struct t_plugin_script *hs_current_script;
extern struct t_plugin_script *hs_registered_script;
extern const char *hs_current_script_filename;

#endif /* WEECHAT_HASKELL_H */
