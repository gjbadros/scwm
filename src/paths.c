
/****************************************************************************
 * This module has been significantly modified by Maciej Stachowiak.
 * It may be used under the terms of the fvwm copyright (see COPYING.FVWM).
 * Changes Copyright 1997, Maciej stachowiak
 ****************************************************************************/

#include <guile/gh.h>
#include <config.h>
#include "scwm.h"
#include "screen.h"
#include "errors.h"
#include "util.h"
#include "paths.h"


char *PixmapPath = SCWM_ICONDIR;

SCM 
set_pixmap_path_x(SCM newpath)
{
  static char *ptemp = NULL;
  char *tmp;
  int dummy;

  SCM_REDEFER_INTS;
  if (!gh_string_p(newpath)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("set-pixmap-path!", 1, newpath);
  }
  if (ptemp == NULL)
    ptemp = PixmapPath;
  if ((PixmapPath != ptemp) && (PixmapPath != NULL))
    free(PixmapPath);

  tmp = gh_scm2newstr(newpath, &dummy);

  PixmapPath = envDupExpand(tmp, 0);
  free(tmp);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}

char *IconPath = SCWM_ICONDIR;

SCM 
set_icon_path_x(SCM newpath)
{
  static char *ptemp = NULL;
  char *tmp;
  int dummy;

  SCM_REDEFER_INTS;

  if (!gh_string_p(newpath)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("set-icon-path!", 1, newpath);
  }
  if (ptemp == NULL)
    ptemp = IconPath;

  if ((IconPath != ptemp) && (IconPath != NULL))
    free(IconPath);
  tmp = gh_scm2newstr(newpath, &dummy);
  IconPath = envDupExpand(tmp, 0);
  free(tmp);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}

