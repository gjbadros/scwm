/* $Id$ */

#ifndef MENU_H
#define MENU_H

#undef EXTERN
#ifdef MENU_IMPLEMENTATION
#define EXTERN
#else
#define EXTERN extern
#endif

#include <libguile.h>

#include "scwm.h"
#include "menus.h"

typedef struct {
  MenuRoot *mr;
} scwm_menu;

EXTERN long scm_tc16_scwm_menu;

#define MENUP(X) (SCM_CAR(X) == (SCM)scm_tc16_scwm_menu)
#define SCWMMENU(X)  ((scwm_menu *)SCM_CDR(X))
#define MENUROOT(X) (((scwm_menu *)SCM_CDR(X))->mr)

size_t free_menu(SCM obj);
int print_menu(SCM obj, SCM port, scm_print_state * pstate);
SCM mark_menu(SCM obj);

SCM make_menu(SCM title, SCM args);
SCM popup(SCM menu, SCM sticks);
SCM menu_p(SCM obj);

void init_menu();

#endif /* MENU_H */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
