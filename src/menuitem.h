/* $Id$
 * menuitem.h
 * By Greg J. Badros, 11/14/97
 *
 */

#ifndef MENUITEM_H
#define MENUITEM_H

#include <guile/gh.h>
#include "Picture.h"

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

typedef struct Scwm_MenuItem_tag
{
  char *szLabel;		/* main label of the item */
  int cchLabel;
  char *szExtra;		/* extra information to display */
  int cchExtra;
  Picture *picAbove;            /* Pixmap to show  above label*/
  Picture *picLeft;		/* Pixmap to show to left of label */
  SCM scmAction;		/* action to perform */
  SCM scmHover;			/* hover hook */
  char *pchHotkeyPreferences;	/* ordered list of hotkeys */
  int cchHotkeyPreferences;
} Scwm_MenuItem;

typedef struct MenuItemInMenu_tag
{
  Scwm_MenuItem *pmi;		/* pointer to the menu item this is for */
  int cpixLabelX;		/* left x offset of label */
  int cpixOffsetY;		/* left y offset of the item */
  int cpixHeight;		/* height for item */
  int cpixExtraX;		/* left x offset of extra info */
  menu_item_state mis;		/* current state of item */
  Bool fOnTopEdge;		/* is this item on the top edge? */
  Bool fOnBottomEdge;		/* is this item on the bottom edge?  */
  Bool fShowPopupArrow;		/* should we show a popup arrow */
} MenuItemInMenu;


#define SCWM_MENUITEM_P(X) (SCM_CAR(X) == (SCM)scm_tc16_scwm_menuitem)
#define SCWM_MENUITEM(X)  ((Scwm_MenuItem *)SCM_CDR(X))

SCM mark_menuitem(SCM obj);
size_t free_menuitem(SCM obj);
int print_menuitem(SCM obj, SCM port, scm_print_state * pstate);
SCM menuitem_p(SCM obj);

SCM make_menuitem(SCM label, SCM action, SCM extra_label, SCM picture_above,
		  SCM picture_left, SCM hover_action,
		  SCM hotkey_prefs);

#endif
