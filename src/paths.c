/* $Id$ */

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

char *szPicturePath = SCWM_ICONDIR;

SCM 
set_picture_path_x(SCM newpath)
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
    ptemp = szPicturePath;
  if ((szPicturePath != ptemp) && (szPicturePath != NULL))
    free(szPicturePath);

  tmp = gh_scm2newstr(newpath, &dummy);

  szPicturePath = tmp;
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}
