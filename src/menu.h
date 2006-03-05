/* $Id$
 * menu.h
 * By Greg J. Badros, 11/14/97
 *
 */

#ifndef MENU_H
#define MENU_H

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include <X11/Intrinsic.h>

#include "image.h"
#include "menuitem.h"
#include "font.h"

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
typedef struct MenuDrawingInfo_tag MenuDrawingInfo;
typedef struct MenuDrawingVtable_tag MenuDrawingVtable;


/* If you add an SCM object to the below, you need to be sure
   to modify mark_menu */
typedef struct Menu_tag
{
  SCM self;                     /* pointer back to the scheme object */
  SCM scmMenuTitle;		/* Menu title */
  SCM scmMenuItems;		/* list of menu items */
  SCM scmImgSide;		/* side image */
  SCM scmSideAlign;		/* side image alignment */
  SCM scmSideBGColor;		/* side image background color */
  SCM scmBGColor;		/* background color */
  SCM scmTextColor;		/* text color */
  SCM scmHLBGColor;		/* highlight background color */
  SCM scmHLTextColor;		/* highlight text color */
  SCM scmStippleColor;		/* stipple color */
  SCM scmImgBackground;		/* background image */
  SCM scmFont;			/* font for labels */
  SCM scmExtraOptions;		/* extra list of options for the drawing code */
  SCM scmMenuLook;		/* menu look */
  char *pchUsedShortcutKeys;	/* list of characters that are shortcut keys */
  int cmsPopupDelay;            /* delay in ms before submenu popup */
  int cmsHoverDelay;            /* delay in ms before hover action is invoked */
  Bool fHighlightRelief;        /* should we draw a relief when we highlight the item */
} Menu;

struct DynamicMenu_tag
{
  Menu *pmenu;			/* this menu */
  MenuItemInMenu * pmiimTitle;	/* the menu title */
  MenuItemInMenu **rgpmiim;	/* the menu item dynamic information */
  int cmiim;			/* size of above array */
  int ipmiimSelected;		/* the index of the selected item */
  struct DynamicMenu_tag *pmdNext; /* the next-popped up menu */
  struct DynamicMenu_tag *pmdPrior; /* the menu that popped this up */
  MenuDrawingInfo *pmdi;	/* extra info needed by the drawing/hit detection code */
  Bool fPinned;			/* is it not a popup? */
  Bool fHoverActionInvoked;	/* have we done the hover action */
  MenuDrawingVtable * pmdv;	/* functions to implement drawing */
  Window w;			/* The X window of the drawn menu */
  int x;			/* x offset from root origin */
  int y;			/* y offset from root origin */
  int cpixHeight;		/* the height of the window */
  int cpixWidth;		/* the width of the window */
};

#define MENU_P(X) (SCM_SMOB_PREDICATE(scm_tc16_scwm_menu, X))
#define MENU(X)  ((Menu *)SCM_SMOB_DATA(X))

#define MENU_OR_SYMBOL_P(X) (MENU_P(X) || scm_is_symbol(X))

#define SAFE_MENU(X)  (MENU_P((X))? MENU((X)): NULL)

#define DYNAMIC_MENU_P(X) \
  (scm_is_symbol(X)? (\
    scm_symbol_bound_p(SCM_BOOL_F,(X)) == SCM_BOOL_T? \
      MENU_P(scm_variable_ref(scm_lookup(X))) : \
      False ) : \
    MENU_P(X))

#define DYNAMIC_SAFE_MENU(X) \
  (scm_is_symbol(X)? (\
    scm_symbol_bound_p(SCM_BOOL_F,(X)) == SCM_BOOL_T? \
      SAFE_MENU(scm_variable_ref(scm_lookup(X))) : \
      NULL ) : \
    SAFE_MENU(X))

#define VALIDATE_ARG_MENU(pos,scm) \
  do { \
  if (!MENU_P(scm)) scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  } while (0)

#define VALIDATE_ARG_MENU_COPY(pos,scm,cvar) \
  do { \
  if (!MENU_P(scm)) scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  cvar = MENU(scm); \
  } while (0)


#define VALIDATE_ARG_MENU_OR_SYM(pos,scm) \
  do { \
  if (!MENU_P(scm)) scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  } while (0)


#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

