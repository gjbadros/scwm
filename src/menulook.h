/* $Id$ 
 * menulook.h
 * Copyright (C) 1998, 1999, 2000  Greg J. Badros and Todd Larason
 */

#ifndef MENULOOK_H
#define MENULOOK_H

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include "menu.h"

#undef EXTERN
#undef EXTERN_SET
#ifdef MENULOOK_IMPLEMENTATION
#define EXTERN
#define EXTERN_SET(x,y) x = y
#else
#define EXTERN extern
#define EXTERN_SET(x,y) extern x
#endif

struct MenuDrawingVtable_tag
{
  /* Construct the menu */
  void (*fnConstructDynamicMenu)(DynamicMenu *);

  /* paint the whole menu */
  void (*fnPaintDynamicMenu)(DynamicMenu *, XEvent *);

  /* paint a single menu item */
  void (*fnPaintMenuItem)(Window, DynamicMenu *, MenuItemInMenu *);

  /* set a popup position for a menu cascaded (flyright) from this one */
  void (*fnSetPopupMenuPositionFromMenuItem)(DynamicMenu *, MenuItemInMenu *);
  
  /* return position for a menu popped (clicked) up from this one */
  void (*fnGetChildPopupPosition)(DynamicMenu *, int *, int *);

  /* return preferred position for this menu to popup */
  void (*fnGetPreferredPopupPosition)(DynamicMenu *, int *, int *);

  /* warp pointer to a menu item */
  void (*fnWarpPointerToPmiim)(MenuItemInMenu *);

  /* return the menu item the pointer is in */
  MenuItemInMenu * (*fnPmiimFromPmdXY)(DynamicMenu *, int, int);

  /* determine if the pointer is in an area that could cause a popup */
  int  (*fnInPopupZone)(MenuItemInMenu *, int, int);

  /* free a mdi struct */
  void (*fnFreePmdi)(MenuDrawingInfo *);

  /* free a midi struct */
  void (*fnFreePmidi)(MenuItemDrawingInfo *);
};

typedef struct {
    SCM name;
    SCM extra;
    MenuDrawingVtable * mdvt;
} scwm_menulook;

#define MENULOOK_P(X) (SCM_NIMP(X) && gh_car(X) == (SCM)scm_tc16_scwm_menulook)
#define MENULOOK(X) ((scwm_menulook *)gh_cdr(X))
#define SAFE_MENULOOK(X) (MENULOOK_P(X)? MENULOOK(X) : NULL)

#define MENULOOK_OR_SYMBOL_P(X) (MENULOOK_P(X) || gh_symbol_p(X))
#define DYNAMIC_MENULOOK_P(X) (gh_symbol_p(X) ? \
                              MENULOOK_P(scm_symbol_binding(SCM_BOOL_F,(X))) :\
                              MENULOOK_P(X))
#define DYNAMIC_SAFE_MENULOOK(X) (gh_symbol_p(X) ? \
				  SAFE_MENULOOK(scm_symbol_binding(SCM_BOOL_F,(X))) : \
				  SAFE_MENULOOK(X))

EXTERN long scm_tc16_scwm_menulook;

SCM make_menulook(char * szName, SCM extra, MenuDrawingVtable * pmdvt);

#define VALIDATE_ARG_MENULOOK_OR_SYM(pos,scm) \
  do { \
  if (!MENULOOK_OR_SYMBOL_P(scm)) scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  } while (0)

#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

