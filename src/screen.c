/* $Id$
 * Copyright (C) 1998, 1999, 2000  Greg J. Badros and Maciej Stachowiak 
 */

#include <libguile.h>

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include <assert.h>

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

SCM_GLOBAL_SMOB(scm_tc16_scwm_screen, "scwm-screen", 0);

SCM_SMOB_FREE(scm_tc16_scwm_screen, free_screen, obj)
{
  return 0;
};

SCM_SMOB_PRINT(scm_tc16_scwm_screen, print_screen, obj, port, pstate)
{
  ScreenInfo *psi = SCREEN(obj);

  scm_puts("#<screen ", port);
  scm_write(scwm_ptr2scm(psi->screen), port);
  scm_putc('>', port);

  return 1;
};

SCM_SMOB_MARK(scm_tc16_scwm_screen, mark_screen, obj)
{
  ScreenInfo *psi = SCREEN(obj);
  assert(psi);

  /* Mark the highlight colors and relief colors */
  GC_MARK_SCM_IF_SET(psi->icon_font);
  GC_MARK_SCM_IF_SET(psi->msg_window_font);
  GC_MARK_SCM_IF_SET(psi->msg_window_fg);
  GC_MARK_SCM_IF_SET(psi->msg_window_bg);
  GC_MARK_SCM_IF_SET(psi->msg_window_highlight);
  GC_MARK_SCM_IF_SET(psi->msg_window_shadow);
  GC_MARK_SCM_IF_SET(psi->nonant_highlight_color);

  return SCM_BOOL_F;
}

SCM
ScmFromPScreenInfo(ScreenInfo *psi)
{
  SCM_RETURN_NEWSMOB(scm_tc16_scwm_screen, psi);
}


void
init_screen()
{
#include "screen.x"
}



/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

