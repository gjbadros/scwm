/* $Id$
 * menulook.c
 *
 * This module is all original code by Todd Larason, from a framework by
 * Greg Badros.
 *
 * It may be used or distributed under either the FVWM license (see
 * COPYING.fvwm) or the GNU General Public License (see COPYING.GPL)
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <assert.h>

#include <guile/gh.h>

#define MENULOOK_IMPLEMENTATION
#include "menulook.h"

#include "scwm.h"
#include "screen.h"

SCM
mark_menulook(SCM scm)
{
  SCM_SETGC8MARK(scm);
  GC_MARK_SCM_IF_SET(MENULOOK(scm)->name);
  GC_MARK_SCM_IF_SET(MENULOOK(scm)->extra);
  return SCM_BOOL_F;
}

size_t
free_menulook(SCM scm)
{
  scwm_menulook * pml = MENULOOK(scm);
  FREE(pml->mdvt);
  return 0;
}

int
print_menulook(SCM scm, SCM port, scm_print_state *pstate)
{
  scm_puts("#<menulook ", port);
  scm_write(MENULOOK(scm)->name, port);
  scm_putc('>', port);
  return 1;
}


SCWM_PROC (menulook_p, "menulook?", 1, 0, 0,
           (SCM scm))
#define FUNC_NAME s_menulook_p
{
  return SCM_BOOL_FromBool(MENULOOK_P(scm));
}
#undef FUNC_NAME

SCM
make_menulook(char * szName, SCM extra, MenuDrawingVtable * mdvt)
{
  SCM result;
  scwm_menulook * pml;

  pml = NEW(scwm_menulook);
  pml->name = gh_str02scm(szName);
  pml->extra = extra;
  pml->mdvt = mdvt;

  gh_defer_ints();
  SCWM_NEWCELL_SMOB(result, scm_tc16_scwm_menulook, pml);
  gh_allow_ints();

  return result;
}

SCWM_PROC(set_menu_look_x, "set-menu-look!", 1, 0, 0,
           (SCM menulook))
/** Set the default menu look used for drawing menus to MENULOOK. */
#define FUNC_NAME s_set_menu_look_x
{
  if (!MENULOOK_P(menulook)) {
    scm_wrong_type_arg(FUNC_NAME, 1, menulook);
  }

  Scr.menu_look = menulook;

  return menulook;
}
#undef FUNC_NAME

MAKE_SMOBFUNS(menulook);

void
init_menulook()
{
  REGISTER_SCWMSMOBFUNS(menulook);

#ifndef SCM_MAGIC_SNARFER
#include "menulook.x"
#endif
}

#ifdef __cplusplus
} // extern "C"
#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
