/* $Id$
 * menuitem.h
 * (C) 1998 Greg J. Badros, 11/14/97
 *
 */

#ifndef MENUITEM_H
#define MENUITEM_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <guile/gh.h>

#include "image.h"

#undef EXTERN
#undef EXTERN_SET
#ifdef MENUITEM_IMPLEMENTATION
#define EXTERN
#define EXTERN_SET(x,y) x = y
#else
#define EXTERN extern
#define EXTERN_SET(x,y) extern x
#endif

EXTERN long scm_tc16_scwm_menuitem;

/* menu item states are associated with items in a menu's list of
   items, not with menu items in particular (since the same item
   might appear in multiple lists) */
typedef enum menu_item_state {
  MIS_Hidden, MIS_Grayed, MIS_Enabled, MIS_Selected
} menu_item_state;

/* If you add an SCM object to the below, you need to be sure
   to modify mark_menuitem
 */
typedef struct MenuItem_tag
{
  char *szLabel;		/* main label of the item */
  int cchLabel;
  char *szExtra;		/* extra information to display */
  int cchExtra;
  SCM scmImgAbove;		/* Pixmap to show  above label*/
  SCM scmImgLeft;		/* Pixmap to show to left of label */
  SCM scmAction;		/* action to perform */
  SCM scmHover;			/* hover hook */
  SCM scmUnhover;		/* un-hover hook */
  char *pchHotkeyPreferences;	/* ordered list of hotkeys */
  int cchHotkeyPreferences;	/* number of hotkeys selected */
  Bool fIsSeparator;		/* Is this a separator? */
				/* This gets set true in make_menuitem,
				   iff everything is empty strings
				   or unset (SCM_BOOL_F or SCM_UNDEFINED) */
  Bool fIsForcedSubmenu;        /* True iff we require that this be a submenu */
} MenuItem;

struct DynamicMenu_tag;

typedef struct MenuItemDrawingInfo_tag MenuItemDrawingInfo;

typedef struct MenuItemInMenu_tag
{
  MenuItem *pmi;		/* pointer to the menu item this is for */
  MenuItemDrawingInfo * pmidi;	/* extra info needed by drawing/hit detection */
  struct DynamicMenu_tag *pmd;	/* the dynamic menu it is in */
  int ipmiim;			/* the item number in the dynamic menu */
  menu_item_state mis;		/* current state of item */
  Bool fShowPopupArrow;		/* should we show a popup arrow */
  char chShortcut;		/* GJB:FIXME:: make this a key event */
  int ichShortcutOffset;	/* For drawing */
} MenuItemInMenu;

#define MENUITEM_P(X) (SCM_NIMP(X) && gh_car(X) == (SCM)scm_tc16_scwm_menuitem)
#define MENUITEM(X)  ((MenuItem *)gh_cdr(X))
#define SAFE_MENUITEM(X)  (MENUITEM_P((X))? MENUITEM((X)) : NULL)

#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
