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
#include "guile-compat.h"

#define MENULOOK_IMPLEMENTATION
#include "menulook.h"

#include "scwm.h"
#include "screen.h"

SCM
mark_menulook(SCM scm)
{
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
print_menulook(SCM scm, SCM port, scm_print_state *ARG_IGNORE(pstate))
{
  scm_puts("#<menulook ", port);
  scm_write(MENULOOK(scm)->name, port);
  scm_putc('>', port);
  return 1;
}


SCWM_PROC (menu_look_p, "menu-look?", 1, 0, 0,
           (SCM obj),
"Return #t if OBJ is a menulook object, #f otherwise.")
#define FUNC_NAME s_menu_look_p
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

  SCWM_NEWCELL_SMOB(result, scm_tc16_scwm_menulook, pml);

  return result;
}

/**CONCEPT: Menu Looks

  Menus have an associated menu look, which determines how the menus
are drawn.  Menu look objects are created by dynamically-loaded
C modules.  For example, the xpm-menus module creates a variable
`xpm-shaped-menu-look' that specifies that the menu should be drawn
using that code.  `copy-menu-look' can be used to copy a menu
look and change some of its properties.
*/

SCM
make_menulook(char * szName, SCM extra, MenuDrawingVtable * mdvt)
{
  return make_menulook_internal(gh_str02scm(szName), extra, mdvt);
}

SCWM_PROC(copy_menu_look, "copy-menu-look", 2, 1, 0,
	  (SCM original_menu_look, SCM name, SCM extra),
"Copy menu look ORIGINAL-MENU-LOOK with a new NAME and optional EXTRA.\n\
If EXTRA is not given, the EXTRA information from the original menu is\n\
used. The form and purpose of the EXTRA information varies with the\n\
menu look, and is documented with the original menu looks; currently,\n\
only the Xpm menu look uses the EXTRA information.")
#define FUNC_NAME s_copy_menu_look
{
  scwm_menulook * pmlOrig;
  
  if (!DYNAMIC_MENULOOK_P(original_menu_look)) {
    SCWM_WRONG_TYPE_ARG(1,original_menu_look);
  }
  pmlOrig = DYNAMIC_SAFE_MENULOOK(original_menu_look);

  VALIDATE_ARG_STR(2,name);
  
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
/* vim:ts=8:sw=2:sta 
 */

