/* $Id$
 * Copyright (C) 1998, 1999, 2000  Greg J. Badros and Maciej Stachowiak 
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
free_screen(SCM ARG_IGNORE(obj))
{
  return 0;
};

int 
print_screen(SCM obj, SCM port, scm_print_state *ARG_IGNORE(pstate))
{
  ScreenInfo *psi = SCREEN(obj);

  scm_puts("#<screen ", port);
  scm_write(scwm_ptr2scm(psi->screen), port);
  scm_putc('>', port);

  return 1;
};


SCM 
mark_screen(SCM obj)
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
  SCM answer;
  SCWM_NEWCELL_SMOB(answer,scm_tc16_scwm_screen,psi);
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
/* vim:ts=8:sw=2:sta 
 */

