/* $Id$
 * menuitem.h
 * By Greg J. Badros, 11/14/97
 *
 */

#ifndef MENUITEM_H
#define MENUITEM_H

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
} MenuItem;

struct DynamicMenu_tag;

typedef struct MenuItemInMenu_tag
{
  MenuItem *pmi;		/* pointer to the menu item this is for */
  struct DynamicMenu_tag *pmd;	/* the dynamic menu it is in */
  int imiim;			/* the item number in the dynamic menu */
  int cpixOffsetY;		/* top y offset of the item */
  int cpixItemHeight;		/* height for item */
  menu_item_state mis;		/* current state of item */
  Bool fOnTopEdge;		/* is this item on the top edge? */
  Bool fOnBottomEdge;		/* is this item on the bottom edge?  */
  Bool fShowPopupArrow;		/* should we show a popup arrow */
  char chShortcut;		/* FIXGJB: make this a key event */
  int ichShortcutOffset;	/* For drawing */
} MenuItemInMenu;


#define MENUITEM_P(X) (SCM_CAR(X) == (SCM)scm_tc16_scwm_menuitem)
#define MENUITEM(X)  ((MenuItem *)SCM_CDR(X))
#define SAFE_MENUITEM(X)  (MENUITEM_P((X))? MENUITEM((X)) : NULL)

SCM mark_menuitem(SCM obj);
size_t free_menuitem(SCM obj);
int print_menuitem(SCM obj, SCM port, scm_print_state * pstate);
SCM menuitem_p(SCM obj);

SCM make_menuitem(SCM label, SCM action, SCM extra_label, SCM picture_above,
		  SCM picture_left, SCM hover_action, SCM unhover_action,
		  SCM hotkey_prefs);

void init_menuitem();

#endif
