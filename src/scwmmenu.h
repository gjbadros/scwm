/* $Id$
 * scwmmenu.h
 * By Greg J. Badros, 11/14/97
 *
 */

#ifndef SCWMMENU_H
#define SCWMMENU_H

#include <guile/gh.h>
#include "Picture.h"
#include "menuitem.h"

#undef EXTERN
#undef EXTERN_SET
#ifdef SCWMMENU_IMPLEMENTATION
#define EXTERN
#define EXTERN_SET(x,y) x = y
#else
#define EXTERN extern
#define EXTERN_SET(x,y) extern x
#endif


EXTERN long scm_tc16_scwm_scwmmenu;
EXTERN XContext ScwmMenuContext; /* context for new scwm menus --11/22/97 gjb*/

typedef struct MenuDrawingInfo_tag
{
  Window w;			/* the X window of the drawn menu */
  int x;			/* x offset from root origin */
  int y;			/* y offset from root origin */
  int cpixHeight;		/* the height of the window */
  int cpixWidth;		/* the width of the window */
  int ccol;			/* the number of columns in the menu */
  int cpixItemOffset;		/* how far from the left edge are items */
  int cpixItemWidth;		/* how wide are items */
  /* cpixItemOffset + ccol * cpixItemWidth == cpixWidth */
  Pixel BGColor;		/* the background color */
  Pixel SideBGColor;		/* the side image bg color */
  Pixel TextColor;		/* the text color */
} MenuDrawingInfo;

typedef struct Scwm_Menu_tag
{
  SCM scmMenuItems;		/* list of menu items */
  Picture *picSide;		/* side image */
  SCM scmSideBGColor;		/* side image background color */
  SCM scmBGColor;		/* background color */
  SCM scmTextColor;		/* text color */
  Picture *picBackground;	/* background pixmap */
  SCM scmFont;			/* font for labels */
  char *pchUsedShortcutKeys;	/* list of characters that are shortcut keys */
} Scwm_Menu;

typedef struct DynamicMenu_tag
{
  Scwm_Menu *pmenu;		/* this menu */
  MenuItemInMenu **rgpmiim;	/* the menu item dynamic information */
  int cmiim;			/* size of above array */
  int imiimSelected;		/* the index of the selected item */
  struct DynamicMenu_tag *pmdNext; /* the next-popped up menu */
  struct DynamicMenu_tag *pmdPrior; /* the menu that popped this up */
  MenuDrawingInfo *pmdi;	/* extra info needed by the drawing/hit detection code */
  Bool fPinned;			/* is it not a popup? */
} DynamicMenu;


#define SCWM_MENU_P(X) (SCM_CAR(X) == (SCM)scm_tc16_scwm_scwmmenu)
#define SCWM_SCWMMENU(X)  ((Scwm_Menu *)SCM_CDR(X))

SCM mark_scwmmenu(SCM obj);

size_t free_scwmmenu(SCM obj);

int print_scwmmenu(SCM obj, SCM port, scm_print_state * pstate);

SCM scwmmenu_p(SCM obj);

SCM make_scwmmenu(SCM list_of_menuitems,
		  SCM picture_side, SCM side_bg_color,
		  SCM bg_color, SCM text_color,
		  SCM picture_bg, SCM font);

void init_scwm_menu();

SCM popup_menu(SCM menu);

#endif
