/* $Id$
 * Copyright (C) 1999, Robert Bihlmeyer
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.GPL.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 *
 */

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include <signal.h>
#include <esd.h>
#include "scwm.h"
#include "guile-compat.h"

static int esd;

#define ESD_CONNECTED_P	(esd > 0)

SCM_DEFINE(sound_load, "sound-load", 1, 1, 0,
	  (SCM file, SCM name),
"Load sound from FILE, tagging it with NAME.\n\
If NAME is not given, \"scwm\" is used.\n\
Returns a sound object usable with the other sound functions.")
#define FUNC_NAME s_sound_load
{
  SCM r;

  if (!gh_string_p(file)) {
    SCWM_WRONG_TYPE_ARG(1, file);
  }
  if (!UNSET_SCM(name) && !gh_string_p(name)) {
      SCWM_WRONG_TYPE_ARG(2, name);
  }
  if (ESD_CONNECTED_P) {
    char *path, *tag;
    int sample;

    path = gh_scm2newstr(file, NULL);
    if (UNSET_SCM(name))
      tag = "scwm";
    else
      tag = gh_scm2newstr(name, NULL);
    sample = esd_file_cache(esd, tag, path);
    r = (sample >= 0) ? gh_int2scm(sample) : SCM_BOOL_F;
    if (!UNSET_SCM(name))
      FREE(tag);
    FREE(path); 
  } else {
    r = file;
  }
  return r;
}
#undef FUNC_NAME

SCM_DEFINE(sound_unload, "sound-unload", 1, 0, 0,
          (SCM sound),
"Unload SOUND, freeing any resources it occupies.\n\
SOUND must be an object returned by `sound-load'.")
#define FUNC_NAME s_sound_unload
{
  if (ESD_CONNECTED_P) {
    if (!gh_number_p(sound)) {
      SCWM_WRONG_TYPE_ARG(1, sound);
    }
    esd_sample_free(esd, gh_scm2int(sound));
  } else {
    if (!gh_string_p(sound)) {
      SCWM_WRONG_TYPE_ARG(1, sound);
    }
    /* no resources were allocated, do nothing */
  }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE(sound_play, "sound-play", 1, 0, 0,
	  (SCM sound),
"Play SOUND.\n\
SOUND must be an object returned by `sound-load'.")
#define FUNC_NAME s_sound_play
{
  if (ESD_CONNECTED_P) {
    int sample;

    if (!gh_number_p(sound)) {
      SCWM_WRONG_TYPE_ARG(1, sound);
    }
    sample = gh_scm2int(sound);
    esd_sample_play(esd, sample);
  } else {
    char *path;

    if (!gh_string_p(sound)) {
      SCWM_WRONG_TYPE_ARG(1, sound);
    }
    path = gh_scm2newstr(sound, NULL);
    esd_play_file("scwm", path, 1);
    FREE(path);
  }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE(esd_reconnect, "esd-reconnect", 0, 1, 0,
	  (SCM host),
"Connect to the ESound daemon on the machine named HOST.\n\
If HOST is not set, the $ESPEAKER environmental variable will be used. If this\n\
is unset too, localhost is contacted.\n\
The esdsound module normally connects at startup. This function is useful\n\
if the connection was lost, esd was not running at startup, or its location\n\
unknown.")
#define FUNC_NAME s_esd_reconnect
{
  char *hostname;

  if (UNSET_SCM(host)) {
    hostname = NULL;
  } else {
    if (!gh_string_p(host)) {
      SCWM_WRONG_TYPE_ARG(1, host);
    }
    hostname = gh_scm2newstr(host, NULL);
  }
  if (ESD_CONNECTED_P)
    esd_close(esd);
  esd = esd_open_sound(hostname);
  if (hostname)
    FREE(hostname);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

void catch_pipe()
{
  fprintf(stderr, "-SIGPIPE-");
}

static void
init_sound()
{
#ifndef SCM_MAGIC_SNARFER
#include "sound.x"
#endif
  esd = esd_open_sound(NULL);
  signal(SIGPIPE, catch_pipe);
}

void scm_init_app_scwm_esdsound_module()
{
  scm_register_module_xxx("app scwm esdsound", init_sound);
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
