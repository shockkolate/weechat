/*
 * Copyright (C) 2003-2012 Sebastien Helleu <flashcode@flashtux.org>
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

/*
 * script-repo.c: download and read repository file (plugins.xml.gz)
 */

#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif

#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <stdio.h>
#include <time.h>
#include <zlib.h>
#include <gcrypt.h>

#include "../weechat-plugin.h"
#include "script.h"
#include "script-repo.h"
#include "script-action.h"
#include "script-buffer.h"
#include "script-config.h"


struct t_repo_script *repo_scripts = NULL;
struct t_repo_script *last_repo_script = NULL;
int script_repo_count = 0;
int script_repo_count_displayed = 0;
struct t_hashtable *script_repo_max_length_field = NULL;
char *script_repo_filter = NULL;


/*
 * script_repo_script_valid: check if a script pointer exists
 *                           return 1 if script exists
 *                                  0 if script is not found
 */

int
script_repo_script_valid (struct t_repo_script *script)
{
    struct t_repo_script *ptr_script;

    if (!script)
        return 0;

    for (ptr_script = repo_scripts; ptr_script;
         ptr_script = ptr_script->next_script)
    {
        if (ptr_script == script)
            return 1;
    }

    /* script not found */
    return 0;
}

/*
 * script_repo_search_displayed_by_number: search a script displayed by number
 *                                         (first script displayed is 0)
 */

struct t_repo_script *
script_repo_search_displayed_by_number (int number)
{
    struct t_repo_script *ptr_script;
    int i;

    if (number < 0)
        return NULL;

    i = 0;
    for (ptr_script = repo_scripts; ptr_script;
         ptr_script = ptr_script->next_script)
    {
        if (ptr_script->displayed)
        {
            if (i == number)
                return ptr_script;
            i++;
        }
    }

    /* script not found */
    return NULL;
}

/*
 * script_repo_search_by_name: search a script by name
 *                             (example: "iset")
 */

struct t_repo_script *
script_repo_search_by_name (const char *name)
{
    struct t_repo_script *ptr_script;

    for (ptr_script = repo_scripts; ptr_script;
         ptr_script = ptr_script->next_script)
    {
        if (strcmp (ptr_script->name, name) == 0)
            return ptr_script;
    }

    /* script not found */
    return NULL;
}

/*
 * script_repo_search_by_name_ext: search a script by name/extension
 *                                 (example: "iset.pl")
 */

struct t_repo_script *
script_repo_search_by_name_ext (const char *name_with_extension)
{
    struct t_repo_script *ptr_script;

    for (ptr_script = repo_scripts; ptr_script;
         ptr_script = ptr_script->next_script)
    {
        if (strcmp (ptr_script->name_with_extension, name_with_extension) == 0)
            return ptr_script;
    }

    /* script not found */
    return NULL;
}

/*
 * script_repo_get_status_for_display: get status for display
 *                                     list is the codes of status to display
 *                                     (exemple: "*iHrN" for all status)
 */

const char *
script_repo_get_status_for_display (struct t_repo_script *script,
                                    const char *list,
                                    int collapse)
{
    static char str_status[128];
    const char *ptr_list;
    char str_space[2];

    str_space[0] = (collapse) ? '\0' : ' ';
    str_space[1] = '\0';

    str_status[0] = '\0';

    for (ptr_list = list; ptr_list[0]; ptr_list++)
    {
        switch (ptr_list[0])
        {
            case '*':
                strcat (str_status, weechat_color (weechat_config_string (script_config_color_status_popular)));
                strcat (str_status, (script && (script->popularity > 0)) ? "*" : str_space);
                break;
            case 'i':
                strcat (str_status, weechat_color (weechat_config_string (script_config_color_status_installed)));
                strcat (str_status, (script && (script->status & SCRIPT_STATUS_INSTALLED)) ? "i" : str_space);
                break;
            case '?':
                strcat (str_status, weechat_color (weechat_config_string (script_config_color_status_unknown)));
                strcat (str_status, (script) ? str_space : "?");
                break;
            case 'H':
                strcat (str_status, weechat_color (weechat_config_string (script_config_color_status_held)));
                strcat (str_status, (script && (script->status & SCRIPT_STATUS_HELD)) ? "H" : str_space);
                break;
            case 'r':
                strcat (str_status, weechat_color (weechat_config_string (script_config_color_status_running)));
                strcat (str_status, (script && (script->status & SCRIPT_STATUS_RUNNING)) ? "r" : str_space);
                break;
            case 'N':
                strcat (str_status, weechat_color (weechat_config_string (script_config_color_status_obsolete)));
                strcat (str_status, (script && (script->status & SCRIPT_STATUS_NEW_VERSION)) ? "N" : str_space);
                break;
        }
    }

    return str_status;
}

/*
 * script_repo_alloc: allocate a script structure
 */

struct t_repo_script *
script_repo_alloc ()
{
    struct t_repo_script *new_script;

    new_script = malloc (sizeof (*new_script));
    if (new_script)
    {
        new_script->name = NULL;
        new_script->name_with_extension = NULL;
        new_script->language = -1;
        new_script->author = NULL;
        new_script->mail = NULL;
        new_script->version = NULL;
        new_script->license = NULL;
        new_script->description = NULL;
        new_script->tags = NULL;
        new_script->requirements = NULL;
        new_script->min_weechat = NULL;
        new_script->max_weechat = NULL;
        new_script->md5sum = NULL;
        new_script->url = NULL;
        new_script->popularity = 0;
        new_script->date_added = 0;
        new_script->date_updated = 0;
        new_script->status = 0;
        new_script->version_loaded = NULL;
        new_script->displayed = 1;
        new_script->install_order = 0;
        new_script->prev_script = NULL;
        new_script->next_script = NULL;
    }

    return new_script;
}

/*
 * script_repo_compare_scripts: compare two scripts using sort key(s)
 *                              (from option script.look.sort)
 */

int
script_repo_compare_scripts (struct t_repo_script *script1,
                             struct t_repo_script *script2)
{
    const char *ptr_sort;
    int cmp, reverse;

    reverse = 1;
    ptr_sort = weechat_config_string (script_config_look_sort);
    while (ptr_sort[0])
    {
        cmp = 0;
        switch (ptr_sort[0])
        {
            case '-': /* reverse order */
                reverse = -1;
                break;
            case 'a': /* author */
                cmp = strcmp (script1->author, script2->author);
                break;
            case 'd': /* date added */
                if (script1->date_added > script2->date_added)
                    cmp = -1;
                else if (script1->date_added < script2->date_added)
                    cmp = 1;
                break;
            case 'e': /* extension */
                cmp = strcmp (script_extension[script1->language],
                              script_extension[script2->language]);
                break;
            case 'i': /* status "installed" */
                if ((script1->status & SCRIPT_STATUS_INSTALLED)
                    && !(script2->status & SCRIPT_STATUS_INSTALLED))
                    cmp = -1;
                else if (!(script1->status & SCRIPT_STATUS_INSTALLED)
                         && (script2->status & SCRIPT_STATUS_INSTALLED))
                    cmp = 1;
                break;
            case 'l': /* language */
                cmp = strcmp (script_language[script1->language],
                              script_language[script2->language]);
                break;
            case 'n': /* name */
                cmp = strcmp (script1->name, script2->name);
                break;
            case 'o': /* status "new version" (script obsolete) */
                if ((script1->status & SCRIPT_STATUS_NEW_VERSION)
                    && !(script2->status & SCRIPT_STATUS_NEW_VERSION))
                    cmp = -1;
                else if (!(script1->status & SCRIPT_STATUS_NEW_VERSION)
                         && (script2->status & SCRIPT_STATUS_NEW_VERSION))
                    cmp = 1;
                break;
            case 'p': /* popularity */
                if (script1->popularity > script2->popularity)
                    cmp = -1;
                else if (script1->popularity < script2->popularity)
                    cmp = 1;
                break;
            case 'r': /* status "running" */
                if ((script1->status & SCRIPT_STATUS_RUNNING)
                    && !(script2->status & SCRIPT_STATUS_RUNNING))
                    cmp = -1;
                else if (!(script1->status & SCRIPT_STATUS_RUNNING)
                         && (script2->status & SCRIPT_STATUS_RUNNING))
                    cmp = 1;
                break;
            case 'u': /* date updated */
                if (script1->date_updated > script2->date_updated)
                    cmp = -1;
                else if (script1->date_updated < script2->date_updated)
                    cmp = 1;
                break;
            default:
                reverse = 1;
                break;
        }
        if (cmp != 0)
            return cmp * reverse;
        ptr_sort++;
    }

    return 0;
}

/*
 * script_repo_find_pos: find position for script in list
 */

struct t_repo_script *
script_repo_find_pos (struct t_repo_script *script)
{
    struct t_repo_script *ptr_script;

    for (ptr_script = repo_scripts; ptr_script;
         ptr_script = ptr_script->next_script)
    {
        if (script_repo_compare_scripts (ptr_script, script) > 0)
            return ptr_script;
    }

    /* position not found, add to the end */
    return NULL;
}

/*
 * script_repo_set_max_length_field: set max length for a field in hashtable
 *                                   "script_repo_max_length_field"
 */

void
script_repo_set_max_length_field (const char *field, int length)
{
    int *value;

    value = weechat_hashtable_get (script_repo_max_length_field, field);
    if (!value || (length > *value))
        weechat_hashtable_set (script_repo_max_length_field, field, &length);
}

/*
 * script_repo_add: add script to list of scripts
 */

void
script_repo_add (struct t_repo_script *script)
{
    struct t_repo_script *ptr_script;

    ptr_script = script_repo_find_pos (script);
    if (ptr_script)
    {
        /* insert script before script found */
        script->prev_script = ptr_script->prev_script;
        script->next_script = ptr_script;
        if (ptr_script->prev_script)
            (ptr_script->prev_script)->next_script = script;
        else
            repo_scripts = script;
        ptr_script->prev_script = script;
    }
    else
    {
        /* add script to the end */
        script->prev_script = last_repo_script;
        script->next_script = NULL;
        if (repo_scripts)
            last_repo_script->next_script = script;
        else
            repo_scripts = script;
        last_repo_script = script;
    }

    /* set max length for fields */
    if (script->name)
        script_repo_set_max_length_field ("N", weechat_utf8_strlen_screen (script->name));
    if (script->name_with_extension)
        script_repo_set_max_length_field ("n", weechat_utf8_strlen_screen (script->name_with_extension));
    if (script->language >= 0)
    {
        script_repo_set_max_length_field ("l", weechat_utf8_strlen_screen (script_language[script->language]));
        script_repo_set_max_length_field ("e", weechat_utf8_strlen_screen (script_extension[script->language]));
    }
    if (script->author)
        script_repo_set_max_length_field ("a", weechat_utf8_strlen_screen (script->author));
    if (script->version)
        script_repo_set_max_length_field ("v", weechat_utf8_strlen_screen (script->version));
    if (script->version_loaded)
        script_repo_set_max_length_field ("V", weechat_utf8_strlen_screen (script->version_loaded));
    if (script->license)
        script_repo_set_max_length_field ("L", weechat_utf8_strlen_screen (script->license));
    if (script->description)
        script_repo_set_max_length_field ("d", weechat_utf8_strlen_screen (script->description));
    if (script->tags)
        script_repo_set_max_length_field ("t", weechat_utf8_strlen_screen (script->tags));
    if (script->requirements)
        script_repo_set_max_length_field ("r", weechat_utf8_strlen_screen (script->requirements));
    if (script->min_weechat)
        script_repo_set_max_length_field ("w", weechat_utf8_strlen_screen (script->min_weechat));
    if (script->max_weechat)
        script_repo_set_max_length_field ("W", weechat_utf8_strlen_screen (script->max_weechat));

    script_repo_count++;
    if (script->displayed)
        script_repo_count_displayed++;
}

/*
 * script_repo_free: free data in script
 */

void
script_repo_free (struct t_repo_script *script)
{
    if (script->name)
        free (script->name);
    if (script->name_with_extension)
        free (script->name_with_extension);
    if (script->author)
        free (script->author);
    if (script->mail)
        free (script->mail);
    if (script->version)
        free (script->version);
    if (script->license)
        free (script->license);
    if (script->description)
        free (script->description);
    if (script->tags)
        free (script->tags);
    if (script->requirements)
        free (script->requirements);
    if (script->min_weechat)
        free (script->min_weechat);
    if (script->max_weechat)
        free (script->max_weechat);
    if (script->md5sum)
        free (script->md5sum);
    if (script->url)
        free (script->url);
    if (script->version_loaded)
        free (script->version_loaded);

    free (script);
 }

/*
 * script_repo_remove: remove a script from list
 */

void
script_repo_remove (struct t_repo_script *script)
{
    struct t_repo_script *new_repo_scripts;

    /* remove script from list */
    if (last_repo_script == script)
        last_repo_script = script->prev_script;
    if (script->prev_script)
    {
        (script->prev_script)->next_script = script->next_script;
        new_repo_scripts = repo_scripts;
    }
    else
        new_repo_scripts = script->next_script;
    if (script->next_script)
        (script->next_script)->prev_script = script->prev_script;

    /* free data */
    if (script->displayed)
        script_repo_count_displayed--;
    script_repo_free (script);

    repo_scripts = new_repo_scripts;

    script_repo_count--;

    if (script_buffer_selected_line >= script_repo_count_displayed)
    {
        script_buffer_selected_line = (script_repo_count_displayed == 0) ?
            0 : script_repo_count_displayed - 1;
    }
}

/*
 * script_repo_remove_all: remove all scripts from list
 */

void
script_repo_remove_all ()
{
    while (repo_scripts)
    {
        script_repo_remove (repo_scripts);
    }
    if (script_repo_max_length_field)
    {
        weechat_hashtable_free (script_repo_max_length_field);
        script_repo_max_length_field = NULL;
    }
}

/*
 * script_repo_script_is_held: return 1 if script is held, 0 otherwise
 */

int
script_repo_script_is_held (struct t_repo_script *script)
{
    const char *hold;
    char *pos;
    int length;

    hold = weechat_config_string (script_config_scripts_hold);
    length = strlen (script->name_with_extension);
    pos = strstr (hold, script->name_with_extension);
    while (pos)
    {
        if (((pos == hold) || (*(pos - 1) == ','))
            && ((pos[length] == ',') || !pos[length]))
        {
            /* script held */
            return 1;
        }
        pos = strstr (pos + 1, script->name_with_extension);
    }

    /* script not held */
    return 0;
}

/*
 * script_repo_md5sum_file: return MD5 checksum for content of a file
 *                          Note: result has to be free() after use
 */

char *
script_repo_md5sum_file (const char *filename)
{
    struct stat st;
    FILE *file;
    char md5sum[512];
    const char *hexa = "0123456789abcdef";
    unsigned char *data, *result;
    gcry_md_hd_t hd;
    int mdlen, i;

    md5sum[0] = '\0';

    if (stat (filename, &st) == -1)
        return NULL;

    data = malloc (st.st_size);
    if (!data)
        return NULL;

    file = fopen (filename, "r");
    if ((int)fread (data, 1, st.st_size, file) < st.st_size)
    {
        free (data);
        return NULL;
    }
    fclose (file);

    gcry_md_open (&hd, GCRY_MD_MD5, 0);
    mdlen = gcry_md_get_algo_dlen (GCRY_MD_MD5);
    gcry_md_write (hd, data, st.st_size);
    result = gcry_md_read (hd, GCRY_MD_MD5);
    for (i = 0; i < mdlen; i++)
    {
        md5sum[i * 2] = hexa[(result[i] & 0xFF) / 16];
        md5sum[(i * 2) + 1] = hexa[(result[i] & 0xFF) % 16];
    }
    md5sum[((mdlen - 1) * 2) + 2] = '\0';
    gcry_md_close (hd);

    free (data);

    return strdup (md5sum);
}

/*
 * script_repo_update_status: update status of a script, which are:
 *                              - script installed?
 *                              - script running?
 *                              - new version available?
 */

void
script_repo_update_status (struct t_repo_script *script)
{
    const char *weechat_home, *version;
    char *filename, *md5sum;
    struct stat st;
    int length;
    struct t_repo_script *ptr_script;

    script->status = 0;
    md5sum = NULL;

    /* check if script is installed (file found on disk) */
    weechat_home = weechat_info_get ("weechat_dir", NULL);
    length = strlen (weechat_home) + strlen (script->name_with_extension) + 64;
    filename = malloc (length);
    if (filename)
    {
        snprintf (filename, length, "%s/%s/autoload/%s",
                  weechat_home,
                  script_language[script->language],
                  script->name_with_extension);
        if (stat (filename, &st) == 0)
        {
            script->status |= SCRIPT_STATUS_INSTALLED;
            md5sum = script_repo_md5sum_file (filename);
        }
        else
        {
            snprintf (filename, length, "%s/%s/%s",
                      weechat_home,
                      script_language[script->language],
                      script->name_with_extension);
            if (stat (filename, &st) == 0)
            {
                script->status |= SCRIPT_STATUS_INSTALLED;
                md5sum = script_repo_md5sum_file (filename);
            }
        }
        free (filename);
    }

    /* check if script is held */
    if (script_repo_script_is_held (script))
        script->status |= SCRIPT_STATUS_HELD;

    /* check if script is running (loaded) */
    version = weechat_hashtable_get (script_loaded, script->name_with_extension);
    if (version)
    {
        script->status |= SCRIPT_STATUS_RUNNING;
        if (script->version_loaded)
            free (script->version_loaded);
        script->version_loaded = strdup (version);
    }
    else
    {
        if (script->version_loaded)
        {
            free (script->version_loaded);
            script->version_loaded = NULL;
        }
    }

    /* check if script has new version (script is obsolete) */
    if (md5sum && script->md5sum && (strcmp (script->md5sum, md5sum) != 0))
        script->status |= SCRIPT_STATUS_NEW_VERSION;

    /* recompute max length for version loaded (for display) */
    if (script_repo_max_length_field)
    {
        length = 0;
        weechat_hashtable_set (script_repo_max_length_field, "V", &length);
        for (ptr_script = repo_scripts; ptr_script;
             ptr_script = ptr_script->next_script)
        {
            if (ptr_script->version_loaded)
                script_repo_set_max_length_field ("V", weechat_utf8_strlen_screen (ptr_script->version_loaded));
        }
    }

    if (md5sum)
        free (md5sum);
}

/*
 * script_repo_update_status_all: update status of all scripts
 */

void
script_repo_update_status_all ()
{
    struct t_repo_script *ptr_script;

    for (ptr_script = repo_scripts; ptr_script;
         ptr_script = ptr_script->next_script)
    {
        script_repo_update_status (ptr_script);
    }
}

/*
 * script_repo_file_exists: return 1 if repository file (plugins.xml.gz) exists
 *                          otherwise 0
 */

int
script_repo_file_exists ()
{
    char *filename;
    int rc;
    struct stat st;

    filename = script_config_get_xml_filename();
    if (!filename)
        return 0;

    rc = 0;
    if (stat (filename, &st) == 0)
        rc = 1;

    free (filename);

    return rc;
}

/*
 * script_repo_file_is_uptodate: return 1 if repository file (plugins.xml.gz)
 *                               is up-to-date (file exists and is not outdated)
 *                               otherwise 0 (file has to be downloaded)
 */

int
script_repo_file_is_uptodate ()
{
    char *filename;
    struct stat st;
    int cache_expire;
    time_t current_time;

    cache_expire = weechat_config_integer (script_config_scripts_cache_expire);

    /* cache always expires? => NOT up-to-date */
    if (cache_expire == 0)
        return 0;

    filename = script_config_get_xml_filename ();

    /* filename not found? => NOT up-to-date */
    if (!filename)
        return 0;

    /* file does not exist? => NOT up-to-date */
    if (stat (filename, &st) == -1)
    {
        free (filename);
        return 0;
    }

    /* cache never expires? => OK, up-to-date! */
    if (cache_expire < 0)
    {
        free (filename);
        return 1;
    }

    current_time = time (NULL);

    /* cache has expired? => NOT up-to-date */
    if (current_time > st.st_mtime + (cache_expire * 60))
    {
        free (filename);
        return 0;
    }

    /* OK, up-to-date! */
    free (filename);
    return 1;
}

/*
 * script_repo_file_read: read scripts in repository file (plugins.xml.gz)
 *                        return 1 if ok, 0 if error
 */

int
script_repo_file_read (int quiet)
{
    char *filename, *ptr_line, line[4096], *pos, *pos2, *pos3;
    char *name, *value1, *value2, *value3, *value, *error;
    const char *version;
    gzFile file;
    struct t_repo_script *script;
    int version_number, version_ok, script_ok, length;
    struct tm tm_script;

    script_get_loaded_scripts ();

    script_repo_remove_all ();

    if (!script_repo_max_length_field)
    {
        script_repo_max_length_field = weechat_hashtable_new (16,
                                                              WEECHAT_HASHTABLE_STRING,
                                                              WEECHAT_HASHTABLE_INTEGER,
                                                              NULL,
                                                              NULL);
    }
    else
        weechat_hashtable_remove_all (script_repo_max_length_field);

    if (script_repo_filter)
    {
        free (script_repo_filter);
        script_repo_filter = NULL;
    }

    version = weechat_info_get ("version", NULL);
    version_number = weechat_util_version_number (version);

    filename = script_config_get_xml_filename ();
    if (!filename)
    {
        weechat_printf (NULL, _("%s%s: error reading list of scripts"),
                        weechat_prefix ("error"),
                        SCRIPT_PLUGIN_NAME);
        return 0;
    }

    script = NULL;
    file = gzopen (filename, "r");
    free (filename);
    if (!file)
    {
        weechat_printf (NULL, _("%s%s: error reading list of scripts"),
                        weechat_prefix ("error"),
                        SCRIPT_PLUGIN_NAME);
        return 0;
    }

    while (!gzeof (file))
    {
        ptr_line = gzgets (file, line, sizeof (line) - 1);
        if (ptr_line)
        {
            if (strstr (ptr_line, "<plugin id="))
            {
                script = script_repo_alloc ();
            }
            else if (strstr (ptr_line, "</plugin>"))
            {
                if (script)
                {
                    script_ok = 0;
                    if (script->name && (script->language >= 0))
                    {
                        version_ok = 1;
                        if (script->min_weechat)
                        {
                            if (weechat_util_version_number (script->min_weechat) > version_number)
                                version_ok = 0;
                        }
                        if (version_ok && script->max_weechat)
                        {
                            if (weechat_util_version_number (script->max_weechat) < version_number)
                                version_ok = 0;
                        }
                        if (version_ok)
                        {
                            length = strlen (script->name) + 1 +
                                strlen (script_extension[script->language]) + 1;
                            script->name_with_extension = malloc (length);
                            if (script->name_with_extension)
                            {
                                snprintf (script->name_with_extension,
                                          length,
                                          "%s.%s",
                                          script->name,
                                          script_extension[script->language]);
                            }
                            script_repo_update_status (script);
                            script_repo_add (script);
                            script_ok = 1;
                        }
                    }
                    if (!script_ok)
                    {
                        script_repo_free (script);
                    }
                    script = NULL;
                }
            }
            else if (script)
            {
                pos = strchr (ptr_line, '<');
                if (pos)
                {
                    pos2 = strchr (pos + 1, '>');
                    if (pos2 && (pos2 > pos + 1))
                    {
                        pos3 = strstr (pos2 + 1, "</");
                        if (pos3 && (pos3 > pos2 + 1))
                        {
                            name = weechat_strndup (pos + 1, pos2 - pos - 1);
                            value1 = weechat_strndup (pos2 + 1, pos3 - pos2 - 1);
                            value2 = weechat_string_replace (value1, "&amp;", "&");
                            value3 = weechat_string_replace (value2, "&gt;", ">");
                            value = weechat_string_replace (value3, "&lt;", "<");
                            if (name && value)
                            {
                                if (strcmp (name, "name") == 0)
                                    script->name = strdup (value);
                                else if (strcmp (name, "language") == 0)
                                    script->language = script_language_search (value);
                                else if (strcmp (name, "author") == 0)
                                    script->author = strdup (value);
                                else if (strcmp (name, "mail") == 0)
                                    script->mail = strdup (value);
                                else if (strcmp (name, "version") == 0)
                                    script->version = strdup (value);
                                else if (strcmp (name, "license") == 0)
                                    script->license = strdup (value);
                                else if (strcmp (name, "desc_en") == 0)
                                    script->description = strdup (value);
                                else if (strcmp (name, "tags") == 0)
                                    script->tags = strdup (value);
                                else if (strcmp (name, "requirements") == 0)
                                    script->requirements = strdup (value);
                                else if (strcmp (name, "min_weechat") == 0)
                                    script->min_weechat = strdup (value);
                                else if (strcmp (name, "max_weechat") == 0)
                                    script->max_weechat = strdup (value);
                                else if (strcmp (name, "md5sum") == 0)
                                    script->md5sum = strdup (value);
                                else if (strcmp (name, "url") == 0)
                                    script->url = strdup (value);
                                else if (strcmp (name, "popularity") == 0)
                                {
                                    error = NULL;
                                    script->popularity = (int)strtol (value,
                                                                      &error,
                                                                      10);
                                    if (!error || error[0])
                                        script->popularity = 0;
                                }
                                else if (strcmp (name, "added") == 0)
                                {
                                    /*
                                     * initialize structure, because strptime
                                     * does not do it
                                     */
                                    memset (&tm_script, 0, sizeof (tm_script));
                                    error = strptime (value,
                                                      "%Y-%m-%d %H:%M:%S",
                                                      &tm_script);
                                    if (error && !error[0])
                                        script->date_added = mktime (&tm_script);
                                }
                                else if (strcmp (name, "updated") == 0)
                                {
                                    /*
                                     * initialize structure, because strptime
                                     * does not do it
                                     */
                                    memset (&tm_script, 0, sizeof (tm_script));
                                    error = strptime (value,
                                                      "%Y-%m-%d %H:%M:%S",
                                                      &tm_script);
                                    if (error && !error[0])
                                        script->date_updated = mktime (&tm_script);
                                }
                            }
                            if (name)
                                free (name);
                            if (value1)
                                free (value1);
                            if (value2)
                                free (value2);
                            if (value3)
                                free (value3);
                            if (value)
                                free (value);
                        }
                    }
                }
            }
        }
    }

    gzclose (file);

    if (repo_scripts && !quiet)
    {
        weechat_printf (NULL,
                        _("%s: %d scripts for WeeChat %s"),
                        SCRIPT_PLUGIN_NAME, script_repo_count,
                        weechat_info_get ("version", NULL));
    }

    if (!repo_scripts)
    {
        weechat_printf (NULL,
                        _("%s%s: list of scripts is empty (repository file "
                          "is broken, or download has failed)"),
                        weechat_prefix ("error"),
                        SCRIPT_PLUGIN_NAME);
    }

    return 1;
}

/*
 * script_repo_file_update_process_cb: callback called when list of scripts is
 *                                     downloaded
 */

int
script_repo_file_update_process_cb (void *data, const char *command,
                                    int return_code, const char *out,
                                    const char *err)
{
    int quiet;

    /* make C compiler happy */
    (void) command;

    quiet = (data == 0) ? 0 : 1;

    if (return_code >= 0)
    {
        if ((err && err[0]) || (out && (strncmp (out, "error:", 6) == 0)))
        {
            weechat_printf (NULL,
                            _("%s%s: error downloading list of scripts: %s"),
                            weechat_prefix ("error"),
                            SCRIPT_PLUGIN_NAME,
                            (err && err[0]) ? err : out + 6);
            return WEECHAT_RC_OK;
        }

        if (script_repo_file_read (quiet) && repo_scripts)
        {
            if (!script_action_run ())
                script_buffer_refresh (1);
        }
        else
            script_buffer_refresh (1);
    }

    return WEECHAT_RC_OK;
}

/*
 * script_repo_file_update: update repository file, and read it
 */

void
script_repo_file_update (int quiet)
{
    char *filename, *url;
    int length;
    struct t_hashtable *options;

    script_repo_remove_all ();

    filename = script_config_get_xml_filename ();
    if (!filename)
        return;

    options = weechat_hashtable_new (8,
                                     WEECHAT_HASHTABLE_STRING,
                                     WEECHAT_HASHTABLE_STRING,
                                     NULL,
                                     NULL);
    if (options)
    {
        length = 4 + strlen (weechat_config_string (script_config_scripts_url)) + 1;
        url = malloc (length);
        if (url)
        {
            if (!quiet)
            {
                weechat_printf (NULL,
                                _("%s: downloading list of scripts..."),
                                SCRIPT_PLUGIN_NAME);
            }

            snprintf (url, length, "url:%s",
                      weechat_config_string (script_config_scripts_url));
            weechat_hashtable_set (options, "file_out", filename);
            weechat_hook_process_hashtable (url, options, 30000,
                                            &script_repo_file_update_process_cb,
                                            (quiet) ? (void *)1 : (void *)0);
            free (url);
        }
        weechat_hashtable_free (options);
    }

    free (filename);
}

/*
 * script_repo_match_search: return 1 if script is matching search string,
 *                           otherwise 0
 */

int
script_repo_match_search (struct t_repo_script *script, const char *search)
{
    char **words, **tags;
    int num_words, num_tags, has_tag, match, i, j;

    if (strcmp (search, "*") == 0)
        return 1;

    words = weechat_string_split (search, " ", 0, 0, &num_words);
    tags = weechat_string_split ((script->tags) ? script->tags : "", ",", 0, 0,
                                 &num_tags);
    if (words)
    {
        for (i = 0; i < num_words; i++)
        {
            has_tag = 0;
            if (tags)
            {
                for (j = 0; j < num_tags; j++)
                {
                    if (weechat_strcasecmp (tags[j], words[i]) == 0)
                    {
                        has_tag = 1;
                        break;
                    }
                }
            }
            if (!has_tag)
            {
                match = 0;
                if (script->name_with_extension
                    && weechat_strcasestr (script->name_with_extension, words[i]))
                    match = 1;

                if (!match && script->description
                    && weechat_strcasestr (script->description, words[i]))
                    match = 1;

                if (!match && script->license
                    && weechat_strcasestr (script->license, words[i]))
                    match = 1;

                if (!match && script->author
                    && weechat_strcasestr (script->author, words[i]))
                    match = 1;

                if (!match)
                {
                    weechat_string_free_split (words);
                    weechat_string_free_split (tags);
                    return 0;
                }
            }
        }
    }

    if (words)
        weechat_string_free_split (words);
    if (tags)
        weechat_string_free_split (tags);

    return 1;
}

/*
 * script_repo_filter_scripts: filter scripts (search string in
 *                             name/description/tags) and mark scripts found as
 *                             "displayed" (0 in displayed for non-matching
 *                             scripts)
 */

void
script_repo_filter_scripts (const char *search)
{
    struct t_repo_script *ptr_script;

    if (script_repo_filter)
        free (script_repo_filter);
    script_repo_filter = strdup (search);

    script_repo_count_displayed = 0;

    for (ptr_script = repo_scripts; ptr_script;
         ptr_script = ptr_script->next_script)
    {
        ptr_script->displayed = (script_repo_match_search (ptr_script, search));
        if (ptr_script->displayed)
            script_repo_count_displayed++;
    }

    script_buffer_refresh (1);
}

/*
 * script_repo_hdata_script_cb: return hdata for script
 */

struct t_hdata *
script_repo_hdata_script_cb (void *data, const char *hdata_name)
{
    struct t_hdata *hdata;

    /* make C compiler happy */
    (void) data;

    hdata = weechat_hdata_new (hdata_name, "prev_script", "next_script");
    if (hdata)
    {
        WEECHAT_HDATA_VAR(struct t_repo_script, name, STRING, NULL, NULL);
        WEECHAT_HDATA_VAR(struct t_repo_script, name_with_extension, STRING, NULL, NULL);
        WEECHAT_HDATA_VAR(struct t_repo_script, language, INTEGER, NULL, NULL);
        WEECHAT_HDATA_VAR(struct t_repo_script, author, STRING, NULL, NULL);
        WEECHAT_HDATA_VAR(struct t_repo_script, mail, STRING, NULL, NULL);
        WEECHAT_HDATA_VAR(struct t_repo_script, version, STRING, NULL, NULL);
        WEECHAT_HDATA_VAR(struct t_repo_script, license, STRING, NULL, NULL);
        WEECHAT_HDATA_VAR(struct t_repo_script, description, STRING, NULL, NULL);
        WEECHAT_HDATA_VAR(struct t_repo_script, tags, STRING, NULL, NULL);
        WEECHAT_HDATA_VAR(struct t_repo_script, requirements, STRING, NULL, NULL);
        WEECHAT_HDATA_VAR(struct t_repo_script, min_weechat, STRING, NULL, NULL);
        WEECHAT_HDATA_VAR(struct t_repo_script, max_weechat, STRING, NULL, NULL);
        WEECHAT_HDATA_VAR(struct t_repo_script, md5sum, STRING, NULL, NULL);
        WEECHAT_HDATA_VAR(struct t_repo_script, url, STRING, NULL, NULL);
        WEECHAT_HDATA_VAR(struct t_repo_script, popularity, INTEGER, NULL, NULL);
        WEECHAT_HDATA_VAR(struct t_repo_script, date_added, TIME, NULL, NULL);
        WEECHAT_HDATA_VAR(struct t_repo_script, date_updated, TIME, NULL, NULL);
        WEECHAT_HDATA_VAR(struct t_repo_script, status, INTEGER, NULL, NULL);
        WEECHAT_HDATA_VAR(struct t_repo_script, version_loaded, STRING, NULL, NULL);
        WEECHAT_HDATA_VAR(struct t_repo_script, displayed, INTEGER, NULL, NULL);
        WEECHAT_HDATA_VAR(struct t_repo_script, install_order, INTEGER, NULL, NULL);
        WEECHAT_HDATA_VAR(struct t_repo_script, prev_script, POINTER, NULL, hdata_name);
        WEECHAT_HDATA_VAR(struct t_repo_script, next_script, POINTER, NULL, hdata_name);
        WEECHAT_HDATA_LIST(repo_scripts);
        WEECHAT_HDATA_LIST(last_repo_script);
    }
    return hdata;
}

/*
 * script_repo_add_to_infolist: add a script in an infolist
 *                              return 1 if ok, 0 if error
 */

int
script_repo_add_to_infolist (struct t_infolist *infolist,
                             struct t_repo_script *script)
{
    struct t_infolist_item *ptr_item;

    if (!infolist || !script)
        return 0;

    ptr_item = weechat_infolist_new_item (infolist);
    if (!ptr_item)
        return 0;

    if (!weechat_infolist_new_var_string (ptr_item, "name", script->name))
        return 0;
    if (!weechat_infolist_new_var_string (ptr_item, "name_with_extension", script->name_with_extension))
        return 0;
    if (!weechat_infolist_new_var_integer (ptr_item, "language", script->language))
        return 0;
    if (!weechat_infolist_new_var_string (ptr_item, "author", script->author))
        return 0;
    if (!weechat_infolist_new_var_string (ptr_item, "mail", script->mail))
        return 0;
    if (!weechat_infolist_new_var_string (ptr_item, "version", script->version))
        return 0;
    if (!weechat_infolist_new_var_string (ptr_item, "license", script->license))
        return 0;
    if (!weechat_infolist_new_var_string (ptr_item, "description", script->description))
        return 0;
    if (!weechat_infolist_new_var_string (ptr_item, "tags", script->tags))
        return 0;
    if (!weechat_infolist_new_var_string (ptr_item, "requirements", script->requirements))
        return 0;
    if (!weechat_infolist_new_var_string (ptr_item, "min_weechat", script->min_weechat))
        return 0;
    if (!weechat_infolist_new_var_string (ptr_item, "max_weechat", script->max_weechat))
        return 0;
    if (!weechat_infolist_new_var_string (ptr_item, "md5sum", script->md5sum))
        return 0;
    if (!weechat_infolist_new_var_string (ptr_item, "url", script->url))
        return 0;
    if (!weechat_infolist_new_var_integer (ptr_item, "popularity", script->popularity))
        return 0;
    if (!weechat_infolist_new_var_time (ptr_item, "date_added", script->date_added))
        return 0;
    if (!weechat_infolist_new_var_time (ptr_item, "date_updated", script->date_updated))
        return 0;
    if (!weechat_infolist_new_var_integer (ptr_item, "status", script->status))
        return 0;
    if (!weechat_infolist_new_var_string (ptr_item, "version_loaded", script->version_loaded))
        return 0;
    if (!weechat_infolist_new_var_integer (ptr_item, "displayed", script->displayed))
        return 0;
    if (!weechat_infolist_new_var_integer (ptr_item, "install_order", script->install_order))
        return 0;

    return 1;
}

/*
 * script_repo_print_log: print script infos in log (usually for crash dump)
 */

void
script_repo_print_log ()
{
    struct t_repo_script *ptr_script;

    for (ptr_script = repo_scripts; ptr_script;
         ptr_script = ptr_script->next_script)
    {
        weechat_log_printf ("");
        weechat_log_printf ("[script (addr:0x%lx)]", ptr_script);
        weechat_log_printf ("  name. . . . . . . . . : '%s'",  ptr_script->name);
        weechat_log_printf ("  name_with_extension . : '%s'",  ptr_script->name_with_extension);
        weechat_log_printf ("  language. . . . . . . : %d",    ptr_script->language);
        weechat_log_printf ("  author. . . . . . . . : '%s'",  ptr_script->author);
        weechat_log_printf ("  mail. . . . . . . . . : '%s'",  ptr_script->mail);
        weechat_log_printf ("  version . . . . . . . : '%s'",  ptr_script->version);
        weechat_log_printf ("  license . . . . . . . : '%s'",  ptr_script->license);
        weechat_log_printf ("  description . . . . . : '%s'",  ptr_script->description);
        weechat_log_printf ("  tags. . . . . . . . . : '%s'",  ptr_script->tags);
        weechat_log_printf ("  requirements. . . . . : '%s'",  ptr_script->requirements);
        weechat_log_printf ("  min_weechat . . . . . : '%s'",  ptr_script->min_weechat);
        weechat_log_printf ("  max_weechat . . . . . : '%s'",  ptr_script->max_weechat);
        weechat_log_printf ("  md5sum. . . . . . . . : '%s'",  ptr_script->md5sum);
        weechat_log_printf ("  url . . . . . . . . . : '%s'",  ptr_script->url);
        weechat_log_printf ("  popularity. . . . . . : %d",    ptr_script->popularity);
        weechat_log_printf ("  date_added. . . . . . : %ld",   ptr_script->date_added);
        weechat_log_printf ("  date_updated. . . . . : %ld",   ptr_script->date_updated);
        weechat_log_printf ("  status. . . . . . . . : %d (%s%s%s%s )",
                            ptr_script->status,
                            (ptr_script->status & SCRIPT_STATUS_INSTALLED) ? " installed": "",
                            (ptr_script->status & SCRIPT_STATUS_HELD) ? " held": "",
                            (ptr_script->status & SCRIPT_STATUS_RUNNING) ? " running": "",
                            (ptr_script->status & SCRIPT_STATUS_NEW_VERSION) ? " new_version": "");
        weechat_log_printf ("  version_loaded. . . . : '%s'",  ptr_script->version_loaded);
        weechat_log_printf ("  displayed . . . . . . : %d",    ptr_script->displayed);
        weechat_log_printf ("  install_order . . . . : %d",    ptr_script->install_order);
        weechat_log_printf ("  prev_script . . . . . : 0x%lx", ptr_script->prev_script);
        weechat_log_printf ("  next_script . . . . . : 0x%lx", ptr_script->next_script);
    }
}