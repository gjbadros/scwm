/* $Id$
 * (C) 1998 Maciej Stachowiak and Greg J. Badros
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <assert.h>

#include <guile/gh.h>

#define SCREEN_IMPLEMENTATION
#include "screen.h"

#include "scwm.h"
#include "window.h"
#include "decor.h"
#include "color.h"
#include "font.h"
#include "face.h"
#include "borders.h"
#include "font.h"
#include "guile-compat.h"

#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif

size_t 
free_screen(SCM obj)
{
  return 0;
};

int 
print_screen(SCM obj, SCM port, scm_print_state * pstate)
{
  ScreenInfo *psi = SCREEN(obj);

  scm_puts("#<screen ", port);
  scm_write(gh_long2scm(psi->screen), port);
  scm_putc('>', port);

  return 1;
};


SCM 
mark_screen(SCM obj)
{
  ScreenInfo *psi = SCREEN(obj);
  assert(psi);

  /* Mark the screen object */
  SCM_SETGC8MARK(obj);

  /* Mark the hilight colors and relief colors */
  GC_MARK_SCM_IF_SET(psi->icon_font);
  GC_MARK_SCM_IF_SET(psi->msg_window_font);
  GC_MARK_SCM_IF_SET(psi->msg_window_fg);
  GC_MARK_SCM_IF_SET(psi->msg_window_bg);

  return SCM_BOOL_F;
}

SCM
ScmFromPScreenInfo(ScreenInfo *psi)
{
  SCM answer;

  gh_defer_ints();
  SCWM_NEWCELL_SMOB(answer,scm_tc16_scwm_screen,psi);
  gh_allow_ints();
  return answer;
}


MAKE_SMOBFUNS(screen);

void
init_screen()
{
  REGISTER_SCWMSMOBFUNS(screen);
#ifndef SCM_MAGIC_SNARFER
#include "screen.x"
#endif
}



/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
