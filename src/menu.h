/* $Id$
 * menu.h
 * By Greg J. Badros, 11/14/97
 *
 */

#ifndef MENU_H
#define MENU_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <guile/gh.h>
#include "image.h"
#include "menuitem.h"
#include "font.h"
#include <X11/Intrinsic.h>

#undef EXTERN
#undef EXTERN_SET
#ifdef MENU_IMPLEMENTATION
#define EXTERN
#define EXTERN_SET(x,y) x = y
#else
#define EXTERN extern
#define EXTERN_SET(x,y) extern x
#endif

EXTERN long scm_tc16_scwm_menu;

extern XContext MenuContext;

typedef struct DynamicMenu_tag DynamicMenu;

typedef void (*PfnConstructDynamicMenu)(DynamicMenu *pmd);
typedef void (*PfnPaintMenuItem)(Window w, DynamicMenu *pmd, MenuItemInMenu *pmiim);
typedef void (*PfnPaintDynamicMenu)(DynamicMenu *pmd, XEvent *pxe);


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
  int cpixSideImage;		/* how wide is the side image */
  /* cpixItemOffset + ccol * cpixItemWidth == cpixWidth */
  Pixel BGColor;		/* the background color */
  Pixel SideBGColor;		/* the side image bg color */
  Pixel TextColor;		/* the text color */
  scwm_font *scfont;		/* To use scwm_font instead of XFont */
  void *p;			/* extra information needed by the client drawing code */
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
  SCM scmExtraOptions;		/* extra list of options for the drawing code */
  char *pchUsedShortcutKeys;	/* list of characters that are shortcut keys */
} Menu;

struct DynamicMenu_tag
{
  Menu *pmenu;		/* this menu */
  MenuItemInMenu **rgpmiim;	/* the menu item dynamic information */
  int cmiim;			/* size of above array */
  int ipmiimSelected;		/* the index of the selected item */
  struct DynamicMenu_tag *pmdNext; /* the next-popped up menu */
  struct DynamicMenu_tag *pmdPrior; /* the menu that popped this up */
  MenuDrawingInfo *pmdi;	/* extra info needed by the drawing/hit detection code */
  Bool fPinned;			/* is it not a popup? */
  Bool fHoverActionInvoked;	/* have we done the hover action */
  PfnPaintDynamicMenu fnPaintDynamicMenu; /* the function to paint the whole menu */
  PfnPaintMenuItem fnPaintMenuItem; /* the function to paint a single menu item */
};


#define MENU_P(X) (SCM_NIMP((X)) && (SCM_CAR((X)) == (SCM)scm_tc16_scwm_menu))
#define MENU(X)  ((Menu *)SCM_CDR((X)))

#define MENU_OR_SYMBOL_P(X) (MENU_P((X)) || gh_symbol_p((X)))

#define SAFE_MENU(X)  (MENU_P((X))? MENU((X)): NULL)

#define DYNAMIC_MENU_P(X)  (gh_symbol_p((X))? \
			    MENU_P(scm_symbol_binding(SCM_BOOL_F,(X))) : \
			    MENU_P((X)))
#define DYNAMIC_SAFE_MENU(X)  (gh_symbol_p((X))? \
			       SAFE_MENU(scm_symbol_binding(SCM_BOOL_F,(X))) : \
			       SAFE_MENU((X)))

void menu_init_gcs();

SCM popup_menu(SCM menu, SCM warp_to_first);


#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
