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
           (SCM obj))
     /** Return #t if OBJ is a menulook object, #f otherwise. */
#define FUNC_NAME s_menulook_p
{
  return SCM_BOOL_FromBool(MENULOOK_P(obj));
}
#undef FUNC_NAME

static SCM
make_menulook_internal(SCM name, SCM extra, MenuDrawingVtable * mdvt)
{
  SCM result;
  scwm_menulook * pml;

  pml = NEW(scwm_menulook);
  pml->name = name;
  pml->extra = extra;
  pml->mdvt = mdvt;

  gh_defer_ints();
  SCWM_NEWCELL_SMOB(result, scm_tc16_scwm_menulook, pml);
  gh_allow_ints();

  return result;
}

SCM
make_menulook(char * szName, SCM extra, MenuDrawingVtable * mdvt)
{
  return make_menulook_internal(gh_str02scm(szName), extra, mdvt);
}

SCWM_PROC(copy_menu_look, "copy-menu-look", 2, 1, 0,
	  (SCM original_menulook, SCM name, SCM extra))
/** FIXJTL: document */
#define FUNC_NAME s_copy_menu_look
{
  int iarg = 0;
  scwm_menulook * pmlOrig;
  
  iarg++;
  if (!DYNAMIC_MENULOOK_P(original_menulook)) {
    scm_wrong_type_arg(FUNC_NAME,iarg,original_menulook);
  }
  pmlOrig = DYNAMIC_SAFE_MENULOOK(original_menulook);

  iarg++;
  if (!gh_string_p(name)) {
    scm_wrong_type_arg(FUNC_NAME,iarg,name);
  }
  
  iarg++;
  if (UNSET_SCM(extra)) {
    extra = pmlOrig->extra;
  }

  return make_menulook_internal(name, extra, pmlOrig->mdvt);
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
