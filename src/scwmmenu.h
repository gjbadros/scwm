/* $Id$
 * scwmmenu.h
 * By Greg J. Badros, 11/14/97
 *
 */

#ifndef SCWMMENU_H
#define SCWMMENU_H

#include <guile/gh.h>
#include "image.h"
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

EXTERN long scm_tc16_scwm_menu;
EXTERN XContext MenuContext; /* context for new scwm menus --11/22/97 gjb*/

typedef struct MenuDrawingInfo_tag
{
  Window w;			/* the X window of the drawn menu */
  int x;			/* x offset from root origin */
  int y;			/* y offset from root origin */
  int cpixHeight;		/* the height of the window */
  int cpixWidth;		/* the width of the window */
  int ccol;			/* the number of columns in the menu */
  int cpixItemOffset;		/* how far from the left edge are items */
  int cpixLeftPicWidth;		/* how wide are the left images */
  int cpixTextWidth;		/* how wide are the text items */
  int cpixExtraTextWidth;	/* how wide are the text items */
  /* cpixItemOffset + ccol * cpixItemWidth == cpixWidth */
  Pixel BGColor;		/* the background color */
  Pixel SideBGColor;		/* the side image bg color */
  Pixel TextColor;		/* the text color */
} MenuDrawingInfo;


/* If you add an SCM object to the below, you need to be sure
   to modify mark_menu */
typedef struct Menu_tag
{
  SCM scmMenuItems;		/* list of menu items */
  SCM scmImgSide;		/* side image */
  SCM scmSideBGColor;		/* side image background color */
  SCM scmBGColor;		/* background color */
  SCM scmTextColor;		/* text color */
  SCM scmImgBackground;		/* background image */
  SCM scmFont;			/* font for labels */
  char *pchUsedShortcutKeys;	/* list of characters that are shortcut keys */
} Menu;

typedef struct DynamicMenu_tag
{
  Menu *pmenu;		/* this menu */
  MenuItemInMenu **rgpmiim;	/* the menu item dynamic information */
  int cmiim;			/* size of above array */
  int imiimSelected;		/* the index of the selected item */
  struct DynamicMenu_tag *pmdNext; /* the next-popped up menu */
  struct DynamicMenu_tag *pmdPrior; /* the menu that popped this up */
  MenuDrawingInfo *pmdi;	/* extra info needed by the drawing/hit detection code */
  Bool fPinned;			/* is it not a popup? */
} DynamicMenu;


#define MENU_P(X) (SCM_CAR(X) == (SCM)scm_tc16_scwm_menu)
#define MENU(X)  ((Menu *)SCM_CDR(X))
#define SAFE_MENU(X)  (MENU_P((X))? MENU((X)): NULL)

SCM mark_menu(SCM obj);

size_t free_menu(SCM obj);

int print_menu(SCM obj, SCM port, scm_print_state * pstate);

SCM menu_p(SCM obj);

SCM make_menu(SCM list_of_menuitems,
	      SCM picture_side, SCM side_bg_color,
	      SCM bg_color, SCM text_color,
	      SCM picture_bg, SCM font);

void init_scwm_menu();
void menu_init_gcs();

SCM popup_menu(SCM menu);

#endif
