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
  GC_MARK_SCM_IF_SET(mi->scmImgAbove);
  GC_MARK_SCM_IF_SET(mi->scmImgLeft);
  GC_MARK_SCM_IF_SET(mi->scmAction);
  GC_MARK_SCM_IF_SET(mi->scmHover);
  GC_MARK_SCM_IF_SET(mi->scmUnhover);

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


SCM_PROC (s_menuitem_properties, "menuitem-properties", 1, 0, 0, menuitem_properties);
SCM
menuitem_properties(SCM menu_item)
{
  Scwm_MenuItem *pmi = SAFE_SCWM_MENUITEM(menu_item);
  if (!pmi) {
    scm_wrong_type_arg(s_menuitem_properties,1,menu_item);
  }
  return gh_list(gh_str02scm(pmi->szLabel),
		 pmi->scmAction,
		 gh_str02scm(pmi->szExtra),
		 pmi->scmImgAbove,
		 pmi->scmImgLeft,
		 pmi->scmHover,
		 pmi->scmUnhover,
		 gh_str02scm(pmi->pchHotkeyPreferences),
		 SCM_UNDEFINED);
}

SCM 
make_menuitem(SCM label, SCM action, SCM extra_label, SCM picture_above,
	      SCM picture_left, SCM hover_action, SCM unhover_action,
	      SCM hotkey_prefs)
{
  Scwm_MenuItem *pmi = safemalloc(sizeof(Scwm_MenuItem));
  SCM answer;
  int iarg = 1;

  if (!gh_string_p(label)) {
    scm_wrong_type_arg(__FUNCTION__,iarg,label);
  }
  pmi->szLabel = gh_scm2newstr(label,&pmi->cchLabel);

  iarg++;
  if (UNSET_SCM(action)) {
    action = SCM_BOOL_F;
  } else if (!gh_procedure_p(action)) {
    scm_wrong_type_arg(__FUNCTION__,iarg,action);
  }
  pmi->scmAction = action;

  iarg++;
  if (UNSET_SCM(extra_label)) {
    pmi->szExtra = NULL;
    pmi->cchExtra = 0;
  } else if (!gh_string_p(extra_label)) {
    scm_wrong_type_arg(__FUNCTION__,iarg,extra_label);
  } else {
    pmi->szExtra = gh_scm2newstr(extra_label,&pmi->cchExtra);
  }

  iarg++;
  if (UNSET_SCM(picture_above)) {
    picture_above = SCM_BOOL_F;
  } else if (!IMAGE_P(picture_above)) {
    scm_wrong_type_arg(__FUNCTION__,iarg,picture_above);
  }
  pmi->scmImgAbove = picture_above;

  iarg++;
  if (UNSET_SCM(picture_left)) {
    picture_left = SCM_BOOL_F;
  } else if (!IMAGE_P(picture_left)) {
    scm_wrong_type_arg(__FUNCTION__,iarg,picture_left);
  } 
  pmi->scmImgLeft = picture_left;

  iarg++;
  if (UNSET_SCM(hover_action)) {
    pmi->scmHover = SCM_BOOL_F;
  } else if (!gh_procedure_p(hover_action)) {
    scm_wrong_type_arg(__FUNCTION__,iarg,hover_action);
  }
  pmi->scmHover = hover_action;

  iarg++;
  if (UNSET_SCM(unhover_action)) {
    pmi->scmUnhover = SCM_BOOL_F;
  } else if (!gh_procedure_p(unhover_action)) {
    scm_wrong_type_arg(__FUNCTION__,iarg,unhover_action);
  }
  pmi->scmUnhover = unhover_action;

  iarg++;
  if (UNSET_SCM(hotkey_prefs)) {
    pmi->pchHotkeyPreferences = NULL;
  } else if (!gh_string_p(hotkey_prefs)) {
    scm_wrong_type_arg(__FUNCTION__,iarg,hotkey_prefs);
  } else {
    pmi->pchHotkeyPreferences = 
      gh_scm2newstr(hotkey_prefs,&pmi->cchHotkeyPreferences);
  }

  SCM_NEWCELL(answer);
  SCM_SETCAR(answer, scm_tc16_scwm_menuitem);
  SCM_SETCDR(answer, (SCM) pmi);
  return answer;
}


void
init_menuitem()
{
# include "menuitem.x"
}
