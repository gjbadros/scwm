/* $Id$
 * menuitem.c
 * By Greg J. Badros -- Nov. 14, 1997
 *
 */

#define MENUITEM_IMPLEMENTATION

#include <config.h>
#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <X11/keysym.h>
#include <sys/types.h>
#include <sys/time.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include <guile/gh.h>
#include "scwm.h"
#include "system.h"
#include "menuitem.h"


SCM 
mark_menuitem(SCM obj)
{
  Scwm_MenuItem *mi = SCWM_MENUITEM(obj);

  SCM_SETGC8MARK(obj);
  if (mi->scmAction != SCM_UNDEFINED) {
    scm_gc_mark(mi->scmAction);
  }
  if (mi->scmHover != SCM_UNDEFINED) {
    scm_gc_mark(mi->scmHover);
  }
  return SCM_BOOL_F;
}

size_t 
free_menuitem(SCM obj)
{
  Scwm_MenuItem *mi = SCWM_MENUITEM(obj);
  if (mi->szLabel) {
    free(mi->szLabel);
  }
  if (mi->szExtra) {
    free(mi->szExtra);
  }
  if (mi->picAbove) {
    DestroyPicture(dpy,mi->picAbove);
  }
  if (mi->picLeft) {
    DestroyPicture(dpy,mi->picLeft);
  }
  if (mi->pchHotkeyPreferences) {
    free(mi->pchHotkeyPreferences);
  }
  free(mi);
  return(0);
}

int 
print_menuitem(SCM obj, SCM port, scm_print_state * pstate)
{
  scm_puts("#<menuitem ", port);
  if (SCWM_MENUITEM_P(obj)) {
    Scwm_MenuItem *mi = SCWM_MENUITEM(obj);
    scm_write(gh_str02scm(mi->szLabel),port);
  } else {
    scm_puts("(invalid)", port);
  }
  scm_putc('>', port);

  return 1;
}

SCM 
menuitem_p(SCM obj)
{
  return (SCWM_MENUITEM_P(obj) ? SCM_BOOL_T : SCM_BOOL_F);
}

SCM 
make_menuitem(SCM label, SCM action, SCM extra_label, SCM picture_above,
	      SCM picture_left, SCM hover_action,
	      SCM hotkey_prefs)
{
  Scwm_MenuItem *mi = safemalloc(sizeof(Scwm_MenuItem));
  SCM answer;
  int iarg = 1;

  if (!gh_string_p(label)) {
    scm_wrong_type_arg(__FUNCTION__,iarg,label);
  }
  mi->szLabel = gh_scm2newstr(label,&mi->cchLabel);

  iarg++;
  if (!gh_procedure_p(action)) {
    scm_wrong_type_arg(__FUNCTION__,iarg,action);
  }
  mi->scmAction = action;

  iarg++;
  if (extra_label == SCM_UNDEFINED || extra_label == SCM_BOOL_F) {
    mi->szExtra = NULL;
  } else if (!gh_string_p(extra_label)) {
    scm_wrong_type_arg(__FUNCTION__,iarg,extra_label);
  } else {
    mi->szExtra = gh_scm2newstr(extra_label,&mi->cchExtra);
  }

  iarg++;
  if (picture_above == SCM_UNDEFINED || picture_above == SCM_BOOL_F) {
    mi->picAbove = NULL;
  } else if (!PICTURE_P(picture_above)) {
    scm_wrong_type_arg(__FUNCTION__,iarg,picture_above);
  } else {
    mi->picAbove = PICTURE(picture_above)->pic;
  }

  iarg++;
  if (picture_left == SCM_UNDEFINED || picture_left == SCM_BOOL_F) {
    mi->picLeft = NULL;
  } else if (!picture_p(picture_left)) {
    scm_wrong_type_arg(__FUNCTION__,iarg,picture_left);
  } else {
    mi->picLeft = PICTURE(picture_left)->pic;
  }

  iarg++;
  if (hover_action == SCM_UNDEFINED || hover_action == SCM_BOOL_F) {
    mi->scmHover = SCM_BOOL_F;
  } else if (!gh_procedure_p(hover_action)) {
    scm_wrong_type_arg(__FUNCTION__,iarg,hover_action);
  } else {
    mi->scmHover = hover_action;
  }

  iarg++;
  if (hotkey_prefs == SCM_BOOL_F || hotkey_prefs == SCM_UNDEFINED) {
    mi->pchHotkeyPreferences = NULL;
  } else if (!gh_string_p(hotkey_prefs)) {
    scm_wrong_type_arg(__FUNCTION__,iarg,hotkey_prefs);
  } else {
    mi->pchHotkeyPreferences = 
      gh_scm2newstr(hotkey_prefs,&mi->cchHotkeyPreferences);
  }

  SCM_NEWCELL(answer);
  SCM_SETCAR(answer, scm_tc16_scwm_menuitem);
  SCM_SETCDR(answer, (SCM) mi);
  return answer;
}
