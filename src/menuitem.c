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
#include "scwmmenu.h"
#include "menuitem.h"


SCM 
mark_menuitem(SCM obj)
{
  MenuItem *pmi;
  if (SCM_GC8MARKP (obj)) {
    return SCM_BOOL_F;
  }
  pmi = MENUITEM(obj);

  SCM_SETGC8MARK(obj);
  GC_MARK_SCM_IF_SET(pmi->scmImgAbove);
  GC_MARK_SCM_IF_SET(pmi->scmImgLeft);
  GC_MARK_SCM_IF_SET(pmi->scmAction);
  GC_MARK_SCM_IF_SET(pmi->scmHover);
  GC_MARK_SCM_IF_SET(pmi->scmUnhover);

  return SCM_BOOL_F;
}

size_t 
free_menuitem(SCM obj)
{
  MenuItem *mi = MENUITEM(obj);
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
  if (MENUITEM_P(obj)) {
    MenuItem *mi = MENUITEM(obj);
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
  return (MENUITEM_P(obj) ? SCM_BOOL_T : SCM_BOOL_F);
}


SCM_PROC(s_menuitem_properties, "menuitem-properties", 1, 0, 0, menuitem_properties);
SCM
menuitem_properties(SCM menu_item)
{
  MenuItem *pmi = SAFE_MENUITEM(menu_item);
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


SCM_PROC(s_make_menuitem, "make-menuitem", 2,6,0, make_menuitem);

SCM 
make_menuitem(SCM label, SCM action, SCM extra_label, SCM picture_above,
	      SCM picture_left, SCM hover_action, SCM unhover_action,
	      SCM hotkey_prefs)
{
  MenuItem *pmi = safemalloc(sizeof(MenuItem));
  SCM answer;
  int iarg = 1;

  if (!gh_string_p(label)) {
    scm_wrong_type_arg(s_make_menuitem,iarg,label);
  }
  pmi->szLabel = gh_scm2newstr(label,&pmi->cchLabel);

  iarg++;
  if (UNSET_SCM(action)) {
    action = SCM_BOOL_F;
  } else if (!gh_symbol_p(action) && !gh_procedure_p(action) && !MENU_P(action)) {
    scm_wrong_type_arg(s_make_menuitem,iarg,action);
  }
  pmi->scmAction = action;

  iarg++;
  if (UNSET_SCM(extra_label)) {
    pmi->szExtra = NULL;
    pmi->cchExtra = 0;
  } else if (!gh_string_p(extra_label)) {
    scm_wrong_type_arg(s_make_menuitem,iarg,extra_label);
  } else {
    pmi->szExtra = gh_scm2newstr(extra_label,&pmi->cchExtra);
  }

  iarg++;
  if (UNSET_SCM(picture_above)) {
    picture_above = SCM_BOOL_F;
  } else if (!IMAGE_P(picture_above)) {
    scm_wrong_type_arg(s_make_menuitem,iarg,picture_above);
  }
  pmi->scmImgAbove = picture_above;

  iarg++;
  if (UNSET_SCM(picture_left)) {
    picture_left = SCM_BOOL_F;
  } else if (!IMAGE_P(picture_left)) {
    scm_wrong_type_arg(s_make_menuitem,iarg,picture_left);
  } 
  pmi->scmImgLeft = picture_left;

  iarg++;
  if (UNSET_SCM(hover_action)) {
    pmi->scmHover = SCM_BOOL_F;
  } else if (!PROCEDURE_OR_SYMBOL_P(hover_action)) {
    scm_wrong_type_arg(s_make_menuitem,iarg,hover_action);
  }
  pmi->scmHover = hover_action;

  iarg++;
  if (UNSET_SCM(unhover_action)) {
    pmi->scmUnhover = SCM_BOOL_F;
  } else if (!PROCEDURE_OR_SYMBOL_P(unhover_action)) {
    scm_wrong_type_arg(s_make_menuitem,iarg,unhover_action);
  }
  pmi->scmUnhover = unhover_action;

  iarg++;
  if (UNSET_SCM(hotkey_prefs)) {
    pmi->pchHotkeyPreferences = NULL;
    pmi->cchHotkeyPreferences = 0;
  } else if (!gh_string_p(hotkey_prefs)) {
    scm_wrong_type_arg(s_make_menuitem,iarg,hotkey_prefs);
  } else {
    pmi->pchHotkeyPreferences = 
      gh_scm2newstr(hotkey_prefs,&pmi->cchHotkeyPreferences);
  }

  if (action == SCM_BOOL_F && pmi->cchLabel == 0 && pmi->cchExtra == 0 &&
      picture_left == SCM_BOOL_F && picture_above == SCM_BOOL_F) {
    pmi->fIsSeparator = True;
  } else {
    pmi->fIsSeparator = False;
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
