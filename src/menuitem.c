/* $Id$
 * menuitem.c
 * By Greg J. Badros -- Nov. 14, 1997
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/time.h>
#include <X11/keysym.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include <guile/gh.h>

#define MENUITEM_IMPLEMENTATION
#include "menu.h"

#include "scwm.h"
#include "menuitem.h"
#include "guile-compat.h"

#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif

SCM 
mark_menuitem(SCM obj)
{
  MenuItem *pmi;
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
    FREE(mi->szLabel);
  }
  if (mi->szExtra) {
    FREE(mi->szExtra);
  }
  if (mi->pchHotkeyPreferences) {
    FREE(mi->pchHotkeyPreferences);
  }
  FREE(mi);
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
  return SCM_BOOL_FromBool(MENUITEM_P(obj));
}


SCWM_PROC(menuitem_properties, "menuitem-properties", 1, 0, 0,
          (SCM menu_item))
     /** Return a list of the properties of the given MENU-ITEM.
MENU-ITEM is a menuitem object, created by `make-menuitem'.  The
returned list contains the following, in this order:
'(label action extra-label picture-above picture-left hover-action
unhover-action hotkey-preferences)
Note that this is the same as the arguments to the `make-menuitem'
primitive. */
#define FUNC_NAME s_menuitem_properties
{
  MenuItem *pmi = SAFE_MENUITEM(menu_item);
  if (!pmi) {
    scm_wrong_type_arg(FUNC_NAME,1,menu_item);
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
#undef FUNC_NAME


SCWM_PROC(make_menuitem, "make-menuitem", 2,6,0,
          (SCM label, SCM action, SCM extra_label, SCM picture_above,
           SCM picture_left, SCM hover_action, SCM unhover_action,
           SCM hotkey_prefs))
     /** Return a newly created menuitem object using the given arguments.
LABEL is a string giving the main text label of the menu item;
ACTION is a procedure or menu object -- if it is a procedure, it gets
invoked when the menuitem is selected, if it is a menu object, that
menu is attached as a submenu from the enclosing menu that the created 
menuitem is put in.
EXTRA-LABEL is extra text describing the menu item -- often this
contains a shortcut key description, or some other descriptive text.
PICTURE-ABOVE and PICTURE-LEFT are picture objects which correspond to 
images to display within the bounding region of the menuitem.
HOVER-ACTION and UNHOVER-ACTION are procedures to be invoked when the
mouse pointer hovers over the item and is moved away after hovering
over the item, respectively.
HOTKEY-PREFS is a string listing preferred alphanumeric shortcut-keys
for the given menu-item; the menu creation routine uses these as hints 
for assigning shortcut keys to the various menuitems.
For a higher-level interface to this function, see `menuitem'. */
#define FUNC_NAME s_make_menuitem
{
  MenuItem *pmi = NEW(MenuItem);
  SCM answer;
  if (!gh_string_p(label)) {
    scm_wrong_type_arg(FUNC_NAME,1,label);
  }
  pmi->szLabel = gh_scm2newstr(label,&pmi->cchLabel);

  if (UNSET_SCM(action)) {
    action = SCM_BOOL_F;
  } else if (!gh_symbol_p(action) && !gh_procedure_p(action) && !MENU_P(action)) {
    scm_wrong_type_arg(FUNC_NAME,2,action);
  }
  pmi->scmAction = action;

  if (UNSET_SCM(extra_label)) {
    pmi->szExtra = NULL;
    pmi->cchExtra = 0;
  } else if (!gh_string_p(extra_label)) {
    scm_wrong_type_arg(FUNC_NAME,3,extra_label);
  } else {
    pmi->szExtra = gh_scm2newstr(extra_label,&pmi->cchExtra);
  }

  if (UNSET_SCM(picture_above)) {
    picture_above = SCM_BOOL_F;
  } else if (!IMAGE_P(picture_above)) {
    scm_wrong_type_arg(FUNC_NAME,4,picture_above);
  }
  pmi->scmImgAbove = picture_above;

  if (UNSET_SCM(picture_left)) {
    picture_left = SCM_BOOL_F;
  } else if (!IMAGE_P(picture_left)) {
    scm_wrong_type_arg(FUNC_NAME,5,picture_left);
  } 
  pmi->scmImgLeft = picture_left;

  if (UNSET_SCM(hover_action)) {
    pmi->scmHover = SCM_BOOL_F;
  } else if (!PROCEDURE_OR_SYMBOL_P(hover_action)) {
    scm_wrong_type_arg(FUNC_NAME,6,hover_action);
  }
  pmi->scmHover = hover_action;

  if (UNSET_SCM(unhover_action)) {
    pmi->scmUnhover = SCM_BOOL_F;
  } else if (!PROCEDURE_OR_SYMBOL_P(unhover_action)) {
    scm_wrong_type_arg(FUNC_NAME,7,unhover_action);
  }
  pmi->scmUnhover = unhover_action;

  if (UNSET_SCM(hotkey_prefs)) {
    pmi->pchHotkeyPreferences = NULL;
    pmi->cchHotkeyPreferences = 0;
  } else if (!gh_string_p(hotkey_prefs)) {
    scm_wrong_type_arg(FUNC_NAME,8,hotkey_prefs);
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

  SCWM_NEWCELL_SMOB(answer,scm_tc16_scwm_menuitem,pmi);
  return answer;
}
#undef FUNC_NAME


MAKE_SMOBFUNS(menuitem);

void
init_menuitem()
{
  REGISTER_SCWMSMOBFUNS(menuitem);

#ifndef SCM_MAGIC_SNARFER
# include "menuitem.x"
#endif
}

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
