/****************************************************************************
 * This module has been significantly modified by Maciej Stachowiak.
 * It may be used under the terms of the fvwm copyright (see COPYING.FVWM).
 * Changes Copyright 1997, Maciej stachowiak
 ****************************************************************************/

#include <guile/gh.h>
#include "../configure.h"
#include "scwm.h"
#include "screen.h"
#include "errors.h"
#include "util.h"
#include "paths.h"


#ifdef XPM
char *PixmapPath = SCWM_ICONDIR;

SCM set_pixmap_path_x(SCM newpath)
{
  static char *ptemp = NULL;
  char *tmp;
  int dummy;

  if (!gh_string_p(newpath)) {
    scm_wrong_type_arg("set-pixmap-path!",1,newpath);
  }

  if(ptemp == NULL)
    ptemp = PixmapPath;
  if((PixmapPath != ptemp)&&(PixmapPath != NULL))
    free(PixmapPath);

  tmp = gh_scm2newstr(newpath,&dummy);

  PixmapPath = envDupExpand(tmp, 0);
  free(tmp);
}
#endif

char *IconPath = SCWM_ICONDIR;

SCM set_icon_path_x(SCM newpath)
{
  static char *ptemp = NULL;
  char *tmp;
  int dummy;

  if (!gh_string_p(newpath)) {
    scm_wrong_type_arg("set-icon-path!",1,newpath);
  }

  if(ptemp == NULL)
    ptemp = IconPath;

  if((IconPath != ptemp)&&(IconPath != NULL))
    free(IconPath);
  tmp = gh_scm2newstr(newpath,&dummy);
  IconPath = envDupExpand(tmp, 0);
  free(tmp);
}

#if 0 /* no module support */
#ifdef SCWM_MODULEDIR
char *ModulePath = SCWM_MODULEDIR;
#else
char *ModulePath = SCWMDIR;
#endif
void setModulePath(XEvent *eventp,Window w,ScwmWindow *tmp_win,
                   unsigned long context, char *action,int* Module)
{
  static char *ptemp = NULL;
  char *tmp;

  if(ptemp == NULL)
    ptemp = ModulePath;

  if((ModulePath != ptemp)&&(ModulePath != NULL))
    free(ModulePath);
  tmp = stripcpy(action);
  ModulePath = envDupExpand(tmp, 0);
  free(tmp);
}
#endif



