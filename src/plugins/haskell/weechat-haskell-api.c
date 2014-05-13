/*
 * weechat-haskell-api.c - haskell API functions
 *
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

#undef _

#include <stdlib.h>

#include "../weechat-plugin.h"
#include "../plugin-script.h"
#include "../plugin-script-api.h"
#include "../plugin-script-callback.h"
#include "weechat-haskell.h"

/*
 * Wrappers for functions in haskell API.
 *
 * For more info about these functions, look at their implementation in WeeChat
 * core.
 */

int
weechat_hs_api_rc_ok ()
{
    return WEECHAT_RC_OK;
}

int
weechat_hs_api_rc_ok_eat ()
{
    return WEECHAT_RC_OK_EAT;
}

int
weechat_hs_api_rc_error ()
{
    return WEECHAT_RC_ERROR;
}

int
weechat_hs_api_register (const char *name, const char *author,
                         const char *version, const char *license,
                         const char *desc, const char *shutdown_func,
                         const char *charset)
{
    if (hs_registered_script)
    {
        /* script already registered */
        weechat_printf (NULL,
                        weechat_gettext ("%s%s: script \"%s\" already "
                                         "registered (register ignored)"),
                        weechat_prefix ("error"), HASKELL_PLUGIN_NAME,
                        hs_registered_script->name);
        return 0;
    }
    hs_current_script = NULL;
    hs_registered_script = NULL;

    if (plugin_script_search (weechat_plugin, hs_scripts, name))
    {
        /* another script already exists with same name */
        weechat_printf (NULL,
                        weechat_gettext ("%s%s: unable to register script "
                                         "\"%s\" (another script already "
                                         "exists with this name)"),
                        weechat_prefix ("error"), HASKELL_PLUGIN_NAME, name);
        return 0;
    }

    /* register script */
    hs_current_script = plugin_script_add (weechat_plugin,
                                           &hs_scripts, &last_hs_script,
                                           (hs_current_script_filename) ?
                                           hs_current_script_filename : "",
                                           name, author, version, license,
                                           desc, shutdown_func, charset);

    if (!hs_current_script)
    {
        return 0;
    }

    hs_registered_script = hs_current_script;
    if ((weechat_plugin->debug >= 2) || !hs_quiet)
    {
        weechat_printf (NULL,
                        weechat_gettext ("%s: registered script \"%s\", "
                                         "version %s (%s)"),
                        HASKELL_PLUGIN_NAME, name, version, desc);
    }

    return 1;
}

const char *
weechat_hs_api_plugin_get_name (void *plugin)
{
    return weechat_plugin_get_name (plugin);
}

int
weechat_hs_api_charset_set (const char *charset)
{
    plugin_script_api_charset_set (hs_current_script, charset);
    return WEECHAT_RC_OK;
}

int
weechat_hs_api_print (struct t_gui_buffer *buffer, const char *message)
{
    plugin_script_api_printf (weechat_plugin, hs_current_script,
                              buffer, "%s", message);
    return WEECHAT_RC_OK;
}

struct t_gui_buffer *
weechat_hs_api_buffer_new (const char *name,
                           int (*input_callback)(void *data,
                                                 struct t_gui_buffer *buffer,
                                                 const char *input_data),
                           void *input_callback_data,
                           int (*close_callback)(void *data,
                                                 struct t_gui_buffer *buffer),
                           void *close_callback_data)
{
    return plugin_script_api_buffer_new (weechat_plugin, hs_current_script,
                                         name,
                                         input_callback, "-", input_callback_data,
                                         close_callback, "-", close_callback_data);
}

struct t_gui_bar_item *
weechat_hs_api_bar_item_new (const char *name,
                             char *(*build_callback)(void *data,
                                                     struct t_gui_bar_item *item,
                                                     struct t_gui_window *window,
                                                     struct t_gui_buffer *buffer,
                                                     struct t_hashtable *extra),
                             void *build_callback_data)
{
    return plugin_script_api_bar_item_new (weechat_plugin, hs_current_script,
                                           name, build_callback, "",
                                           build_callback_data);
}

struct t_gui_bar *
weechat_hs_api_bar_new (const char *name, const char *hidden,
                        const char *priority, const char *type,
                        const char *condition, const char *position,
                        const char *filling_top_bottom,
                        const char *filling_left_right, const char *size,
                        const char *size_max, const char *color_fg,
                        const char *color_delim, const char *color_bg,
                        const char *separator, const char *items)
{
    return weechat_bar_new (name, hidden, priority, type, condition, position,
                            filling_top_bottom, filling_left_right, size,
                            size_max, color_fg, color_delim, color_bg, separator,
                            items);
}
