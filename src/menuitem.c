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
print_menuitem(SCM obj, SCM port, scm_print_state *ARG_IGNORE(pstate))
{
  scm_puts("#<menuitem ", port);
  if (MENUITEM_P(obj)) {
    MenuItem *pmi = MENUITEM(obj);
    scm_puts("\"",port);
    scm_puts(pmi->szLabel,port);
    scm_puts("\"",port);
    if (pmi->fIsForcedSubmenu) {
      scm_puts(" (forced-submenu)",port);
    }
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
  MenuItem *pmi;
  VALIDATE_ARG_MENUITEM_COPY(1,menu_item,pmi);
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


SCWM_PROC(make_menuitem, "make-menuitem", 2,7,0,
          (SCM label, SCM action, SCM extra_label, SCM picture_above,
           SCM picture_left, SCM hover_action, SCM unhover_action,
           SCM hotkey_prefs, SCM submenu_p))
     /** Return a newly created menuitem object using the given arguments.
LABEL is a string giving the main text label of the menu item;
ACTION is a procedure or menu object -- if it is a procedure, it gets
invoked when the menuitem is selected, if it is a menu object, that
menu is attached as a submenu from the enclosing menu that the created 
menuitem is put in.  You can also force ACTION to be treated as a
submenu by setting SUBMENU? to #t.
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
  VALIDATE_ARG_STR_NEWCOPY_LEN(1,label,pmi->szLabel,pmi->cchLabel);

  if (UNSET_SCM(action)) {
    action = SCM_BOOL_F;
  } else if (!gh_symbol_p(action) && !gh_procedure_p(action) && !MENU_P(action)) {
    SCWM_WRONG_TYPE_ARG(2,action);
  }
  pmi->scmAction = action;

  if (UNSET_SCM(extra_label)) {
    pmi->szExtra = NULL;
    pmi->cchExtra = 0;
  } else {
    VALIDATE_ARG_STR_NEWCOPY_LEN(3,extra_label,pmi->szExtra,pmi->cchExtra);
  }

  VALIDATE_ARG_IMAGE_USE_F(4,picture_above);
  pmi->scmImgAbove = picture_above;

  VALIDATE_ARG_IMAGE_USE_F(5,picture_left);
  pmi->scmImgLeft = picture_left;

  VALIDATE_ARG_PROC_OR_SYM_USE_F(6,hover_action);
  pmi->scmHover = hover_action;

  VALIDATE_ARG_PROC_OR_SYM_USE_F(7,unhover_action);
  pmi->scmHover = unhover_action;

  if (UNSET_SCM(hotkey_prefs)) {
    pmi->pchHotkeyPreferences = NULL;
    pmi->cchHotkeyPreferences = 0;
  } else {
    VALIDATE_ARG_STR_NEWCOPY_LEN(8,hotkey_prefs,pmi->pchHotkeyPreferences,pmi->cchHotkeyPreferences);
  }

  VALIDATE_ARG_BOOL_COPY_USE_F(9,submenu_p,pmi->fIsForcedSubmenu);

  pmi->fIsSeparator =
    (action == SCM_BOOL_F && pmi->cchLabel == 0 && pmi->cchExtra == 0 &&
     picture_left == SCM_BOOL_F && picture_above == SCM_BOOL_F);

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
