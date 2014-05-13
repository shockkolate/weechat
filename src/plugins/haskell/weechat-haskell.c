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
 * string used to execute action "install":
 * when signal "hs_script_install" is received, name of string
 * is added to this string, to be installed later by a timer (when nothing is
 * running in script)
 */
char *hs_action_install_list = NULL;

/*
 * string used to execute action "remove":
 * when signal "hs_script_remove" is received, name of string
 * is added to this string, to be removed later by a timer (when nothing is
 * running in script)
 */
char *hs_action_remove_list = NULL;

/*
 * string used to execute action "autoload":
 * when signal "hs_script_autoload" is received, name of string
 * is added to this string, to autoload or disable autoload later by a timer
 * (when nothing is running in script)
 */
char *hs_action_autoload_list = NULL;

/*
 * execute a haskell function
 */

void *
weechat_hs_exec (struct t_plugin_script *script,
                 int ret_type, const char *function,
                 const char *format, void **argv)
{
    /* make C compiler happy */
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
 * load a haskell script
 *
 * Returns:
 *   1: OK
 *   0: error
 */

int
weechat_hs_load (const char *filename)
{
    int i;
    struct stat buf;

    if (stat (filename, &buf) != 0)
    {
        weechat_printf (NULL,
                        weechat_gettext ("%s%s: script \"%s\" not found"),
                        weechat_prefix ("error"), HASKELL_PLUGIN_NAME, filename);
        return 0;
    }

    if ((weechat_plugin->debug >= 2) || !hs_quiet)
    {
        weechat_printf (NULL,
                        weechat_gettext ("%s: loading script \"%s\""),
                        HASKELL_PLUGIN_NAME, filename);
    }

    hs_current_script = NULL;
    hs_registered_script = NULL;
    hs_current_script_filename = filename;

    haskell_load ((char *) filename);

    if (!hs_registered_script)
    {
        weechat_printf (NULL,
                        weechat_gettext ("%s%s: function \"register\" not "
                                         "found (or failed) in file \"%s\""),
                        weechat_prefix ("error"), HASKELL_PLUGIN_NAME, filename);
        return 0;
    }
    hs_current_script = hs_registered_script;

    /*
     * set input/close callbacks for buffers created by this script
     * (to restore callbacks after upgrade)
     */
/*
    plugin_script_set_buffer_callbacks (weechat_tcl_plugin,
                                        tcl_scripts,
                                        tcl_current_script,
                                        &weechat_tcl_api_buffer_input_data_cb,
                                        &weechat_tcl_api_buffer_close_cb);
*/

    (void) weechat_hook_signal_send ("hs_script_loaded",
                                     WEECHAT_HOOK_SIGNAL_STRING,
                                     hs_current_script->filename);
    return 1;
}

/*
 * callback for load
 */

void
weechat_hs_load_cb (void *data, const char *filename)
{
    /* make C compiler happy */
    (void) data;

    weechat_hs_load (filename);
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
 * unload a haskell script by name
 */

void
weechat_hs_unload_name (const char *name)
{
    struct t_plugin_script *ptr_script;

    ptr_script = plugin_script_search (weechat_plugin, hs_scripts, name);
    if (ptr_script)
    {
        weechat_hs_unload (ptr_script);
        if (!hs_quiet)
        {
            weechat_printf (NULL,
                            weechat_gettext ("%s: script \"%s\" unloaded"),
                            HASKELL_PLUGIN_NAME, name);
        }
    }
    else
    {
        weechat_printf (NULL,
                        weechat_gettext ("%s%s: script \"%s\" not loaded"),
                        weechat_prefix ("error"), HASKELL_PLUGIN_NAME, name);
    }
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
 * reload a haskell script by nam
 */

void
weechat_hs_reload_name (const char *name)
{
    struct t_plugin_script *ptr_script;
    char *filename;

    ptr_script = plugin_script_search (weechat_plugin, hs_scripts, name);
    if (ptr_script)
    {
        filename = strdup (ptr_script->filename);
        if (filename)
        {
            weechat_hs_unload (ptr_script);
            if (!hs_quiet)
            {
                weechat_printf (NULL,
                                weechat_gettext ("%s: script \"%s\" unloaded"),
                                HASKELL_PLUGIN_NAME, name);
            }
            weechat_hs_load (filename);
            free (filename);
        }
    }
    else
    {
        weechat_printf (NULL,
                        weechat_gettext ("%s%s: script \"%s\" not loaded"),
                        weechat_prefix ("error"), HASKELL_PLUGIN_NAME, name);
    }
}

/*
 * callback for command "/haskell"
 */

int
weechat_hs_command_cb (void *data, struct t_gui_buffer *buffer,
                       int argc, char **argv, char **argv_eol)
{
    char *ptr_name, *path_script;

    /* make C compiler happy */
    (void) data;
    (void) buffer;

    if (argc == 1)
    {
        plugin_script_display_list (weechat_haskell_plugin, hs_scripts, NULL, 0);
    }
    else if (argc == 2)
    {
        if (weechat_strcasecmp (argv[1], "list") == 0)
        {
            plugin_script_display_list (weechat_plugin, hs_scripts,
                                        NULL, 0);
        }
        else if (weechat_strcasecmp (argv[1], "listfull") == 0)
        {
            plugin_script_display_list (weechat_plugin, hs_scripts,
                                        NULL, 1);
        }
        else if (weechat_strcasecmp (argv[1], "autoload") == 0)
        {
            plugin_script_auto_load (weechat_plugin, &weechat_hs_load_cb);
        }
        else if (weechat_strcasecmp (argv[1], "reload") == 0)
        {
            weechat_hs_unload_all ();
            plugin_script_auto_load (weechat_plugin, &weechat_hs_load_cb);
        }
        else if (weechat_strcasecmp (argv[1], "unload") == 0)
        {
            weechat_hs_unload_all ();
        }
        else
            return WEECHAT_RC_ERROR;
    }
    else
    {
        if (weechat_strcasecmp (argv[1], "list") == 0)
        {
            plugin_script_display_list (weechat_plugin, hs_scripts,
                                        argv_eol[2], 0);
        }
        else if (weechat_strcasecmp (argv[1], "listfull") == 0)
        {
            plugin_script_display_list (weechat_plugin, hs_scripts,
                                        argv_eol[2], 1);
        }
        else if ((weechat_strcasecmp (argv[1], "load") == 0)
                 || (weechat_strcasecmp (argv[1], "reload") == 0)
                 || (weechat_strcasecmp (argv[1], "unload") == 0))
        {
            ptr_name = argv_eol[2];
            if (strncmp (ptr_name, "-q ", 3) == 0)
            {
                hs_quiet = 1;
                ptr_name += 3;
                while (ptr_name[0] == ' ')
                {
                    ptr_name++;
                }
            }
            if (weechat_strcasecmp (argv[1], "load") == 0)
            {
                /* load hs script */
                path_script = plugin_script_search_path (weechat_plugin,
                                                         ptr_name);
                weechat_hs_load ((path_script) ? path_script : ptr_name);
                if (path_script)
                    free (path_script);
            }
            else if (weechat_strcasecmp (argv[1], "reload") == 0)
            {
                /* reload one hs script */
                weechat_hs_reload_name (ptr_name);
            }
            else if (weechat_strcasecmp (argv[1], "unload") == 0)
            {
                /* unload hs script */
                weechat_hs_unload_name (ptr_name);
            }
            hs_quiet = 0;
        }
        else
            return WEECHAT_RC_ERROR;
    }

    return WEECHAT_RC_OK;
}

/*
 * add haskell scripts to completion list
 */

int
weechat_hs_completion_cb (void *data, const char *completion_item,
                          struct t_gui_buffer *buffer,
                          struct t_gui_completion *completion)
{
    /* make C compiler happy */
    (void) data;
    (void) completion_item;
    (void) buffer;

    plugin_script_completion (weechat_plugin, completion, hs_scripts);

    return WEECHAT_RC_OK;
}

/*
 * return hdata for haskell scripts
 */

struct t_hdata *
weechat_hs_hdata_cb (void *data, const char *hdata_name)
{
    /* make C compiler happy */
    (void) data;

    return plugin_script_hdata_script (weechat_plugin,
                                       &hs_scripts, &last_hs_script,
                                       hdata_name);
}

/*
 * return infolist with haskell scripts
 */

struct t_infolist *
weechat_hs_infolist_cb (void *data, const char *infolist_name,
                        void *pointer, const char *arguments)
{
    /* make C compiler happy */
    (void) data;

    if (!infolist_name || !infolist_name[0])
        return NULL;

    if (weechat_strcasecmp (infolist_name, "hs_script") == 0)
    {
        return plugin_script_infolist_list_scripts (weechat_plugin,
                                                    hs_scripts, pointer,
                                                    arguments);
    }

    return NULL;
}

/*
 * dump haskell plugin data in WeeChat log file
 */

int
weechat_hs_signal_debug_dump_cb (void *data, const char *signal,
                                 const char *type_data, void *signal_data)
{
    /* make C compiler happy */
    (void) data;
    (void) signal;
    (void) type_data;

    if (!signal_data
        || (weechat_strcasecmp ((char *)signal_data, HASKELL_PLUGIN_NAME) == 0))
    {
        plugin_script_print_log (weechat_plugin, hs_scripts);
    }

    return WEECHAT_RC_OK;
}

/*
 * display info about external libraries used
 */

int
weechat_hs_signal_debug_libs_cb (void *data, const char *signal,
                                 const char *type_data, void *signal_data)
{
    /* make C compiler happy */
    (void) data;
    (void) signal;
    (void) type_data;
    (void) signal_data;

#ifdef HASKELL_VERSION
    weechat_printf (NULL, "  %s: %s", HASKELL_PLUGIN_NAME, TCL_VERSION);
#else
    weechat_printf (NULL, "  %s: (?)", HASKELL_PLUGIN_NAME);
#endif

    return WEECHAT_RC_OK;
}

/*
 * callback for buffer closed
 */

int
weechat_hs_signal_buffer_closed_cb (void *data, const char *signal,
                                    const char *type_data, void *signal_data)
{
    /* make C compiler happy */
    (void) data;
    (void) signal;
    (void) type_data;

    if (signal_data)
        plugin_script_remove_buffer_callbacks (hs_scripts, signal_data);

    return WEECHAT_RC_OK;
}

/*
 * timer for executing actions
 */

int
weechat_hs_timer_action_cb (void *data, int remaining_calls)
{
    /* make C compiler happy */
    (void) remaining_calls;

    if (data)
    {
        if (data == &hs_action_install_list)
        {
            plugin_script_action_install (weechat_plugin,
                                          hs_scripts,
                                          &weechat_hs_unload,
                                          &weechat_hs_load,
                                          &hs_quiet,
                                          &hs_action_install_list);
        }
        else if (data == &hs_action_remove_list)
        {
            plugin_script_action_remove (weechat_plugin,
                                         hs_scripts,
                                         &weechat_hs_unload,
                                         &hs_quiet,
                                         &hs_action_remove_list);
        }
        else if (data == &hs_action_autoload_list)
        {
            plugin_script_action_autoload (weechat_plugin,
                                           &hs_quiet,
                                           &hs_action_autoload_list);
        }
    }

    return WEECHAT_RC_OK;
}

/*
 * callback for script actions (install/remove a script)
 */

int
weechat_hs_signal_script_action_cb (void *data, const char *signal,
                                    const char *type_data,
                                    void *signal_data)
{
    /* make C compiler happy */
    (void) data;

    if (strcmp (type_data, WEECHAT_HOOK_SIGNAL_STRING) == 0)
    {
        if (strcmp (signal, "hs_script_install") == 0)
        {
            plugin_script_action_add (&hs_action_install_list,
                                      (const char *)signal_data);
            weechat_hook_timer (1, 0, 1,
                                &weechat_hs_timer_action_cb,
                                &hs_action_install_list);
        }
        else if (strcmp (signal, "hs_script_remove") == 0)
        {
            plugin_script_action_add (&hs_action_remove_list,
                                      (const char *)signal_data);
            weechat_hook_timer (1, 0, 1,
                                &weechat_hs_timer_action_cb,
                                &hs_action_remove_list);
        }
        else if (strcmp (signal, "hs_script_autoload") == 0)
        {
            plugin_script_action_add (&hs_action_autoload_list,
                                      (const char *)signal_data);
            weechat_hook_timer (1, 0, 1,
                                &weechat_hs_timer_action_cb,
                                &hs_action_autoload_list);
        }
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
    hs_add_root (__stginit_WeechatHaskell);

    init.callback_command = &weechat_hs_command_cb;
    init.callback_completion = &weechat_hs_completion_cb;
    init.callback_hdata = &weechat_hs_hdata_cb;
    init.callback_infolist = &weechat_hs_infolist_cb;
    init.callback_signal_debug_dump = &weechat_hs_signal_debug_dump_cb;
    init.callback_signal_debug_libs = &weechat_hs_signal_debug_libs_cb;
    init.callback_signal_buffer_closed = &weechat_hs_signal_buffer_closed_cb;
    init.callback_signal_script_action = &weechat_hs_signal_script_action_cb;
    init.callback_load_file = &weechat_hs_load_cb;

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

    /* free some data */
    if (hs_action_install_list)
        free (hs_action_install_list);
    if (hs_action_remove_list)
        free (hs_action_remove_list);
    if (hs_action_autoload_list)
        free (hs_action_autoload_list);

    hs_exit ();

    return WEECHAT_RC_OK;
}
