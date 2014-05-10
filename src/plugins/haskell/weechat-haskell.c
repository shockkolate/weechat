/*
 * weechat-haskell.c - haskell plugin for WeeChat
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

#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "../weechat-plugin.h"
#include "../plugin-script.h"
#include "weechat-haskell.h"
#include "WeechatHaskell_stub.h"

WEECHAT_PLUGIN_NAME(HASKELL_PLUGIN_NAME);
WEECHAT_PLUGIN_DESCRIPTION(N_("Support of haskell scripts"));
WEECHAT_PLUGIN_AUTHOR("David Farrell (Shockk) <shokku.ra@gmail.com>");
WEECHAT_PLUGIN_VERSION("0.1");
WEECHAT_PLUGIN_LICENSE("GPL3");

struct t_weechat_plugin *weechat_haskell_plugin = NULL;

int hs_quiet = 0;
struct t_plugin_script *hs_scripts = NULL;
struct t_plugin_script *last_hs_script = NULL;
struct t_plugin_script *hs_current_script = NULL;
struct t_plugin_script *hs_registered_script = NULL;
const char *hs_current_script_filename = NULL;

/*
 * execute a haskell function
 */

void *
weechat_hs_exec (struct t_plugin_script *script,
                 int ret_type, const char *function,
                 const char *format, void **argv)
{
    (void) script;
    (void) ret_type;
    (void) function;
    (void) format;
    (void) argv;

    weechat_printf (NULL, weechat_gettext ("%s: exec not implemented"),
                    HASKELL_PLUGIN_NAME);
    return NULL;
}

/*
 * unload a haskell script
 */

void
weechat_hs_unload (struct t_plugin_script *script)
{
    int *rc;
    char *filename;

    if ((weechat_plugin->debug >= 2) || !hs_quiet)
    {
        weechat_printf (NULL, weechat_gettext ("%s: unloading script \"%s\""),
                        HASKELL_PLUGIN_NAME, script->name);
    }

    if (script->shutdown_func && script->shutdown_func[0])
    {
        rc = (int *)weechat_hs_exec (script, WEECHAT_SCRIPT_EXEC_INT,
                                     script->shutdown_func, NULL, NULL);
        if (rc)
            free (rc);
    }

    filename = strdup (script->filename);

    if (hs_current_script == script)
        hs_current_script = (hs_current_script->prev_script) ?
            hs_current_script->prev_script : hs_current_script->next_script;

    plugin_script_remove (weechat_plugin, &hs_scripts, &last_hs_script, script);

    (void) weechat_hook_signal_send ("hs_script_unloaded",
                                     WEECHAT_HOOK_SIGNAL_STRING, filename);
    if (filename)
        free (filename);
}

/*
 * unload all haskell scripts
 */

void
weechat_hs_unload_all ()
{
    while (hs_scripts)
    {
        weechat_hs_unload (hs_scripts);
    }
}

/*
 * callback for command "/haskell"
 */

int
weechat_hs_command_cb (void *data, struct t_gui_buffer *buffer,
                         int argc, char **argv, char **argv_eol)
{
    (void) data;
    (void) buffer;
    (void) argv;
    (void) argv_eol;

    if (argc == 1)
    {
        plugin_script_display_list (weechat_haskell_plugin, hs_scripts, NULL, 0);
    }

    return WEECHAT_RC_OK;
}

/*
 * initialize haskell plugin
 */

int
weechat_plugin_init (struct t_weechat_plugin *plugin, int argc, char *argv[])
{
    struct t_plugin_script_init init;

    weechat_plugin = plugin;

    hs_init (0, NULL);
    test_buffer_new ();

    init.callback_command = &weechat_hs_command_cb;
/*
    init.callback_completion = &weechat_hs_completion_cb;
    init.callback_hdata = &weechat_hs_hdata_cb;
    init.callback_infolist = &weechat_hs_infolist_cb;
    init.callback_signal_debug_dump = &weechat_hs_signal_debug_dump_cb;
    init.callback_signal_debug_libs = &weechat_hs_signal_debug_libs_cb;
    init.callback_signal_buffer_closed = &weechat_hs_signal_buffer_closed_cb;
    init.callback_signal_script_action = &weechat_hs_signal_script_action_cb;
    init.callback_load_file = &weechat_hs_load_cb;
*/

    hs_quiet = 1;
    plugin_script_init (weechat_plugin, argc, argv, &init);
    hs_quiet = 0;

    plugin_script_display_short_list (weechat_plugin, hs_scripts);

    return WEECHAT_RC_OK;
}

/*
 * end haskell plugin
 */

int
weechat_plugin_end (struct t_weechat_plugin *plugin)
{
    hs_quiet = 1;
    plugin_script_end (plugin, &hs_scripts, &weechat_hs_unload_all);
    hs_quiet = 0;

    hs_exit ();

    return WEECHAT_RC_OK;
}
