/* $Id$
 * (C) 1998 Maciej Stachowiak and Greg J. Badros
 *
 * This module has been significantly modified from fvwm2
 * It may be used under the terms indicated by the copyright below.
 *
 * This module is derived from code by Rob Nation 
 * Copyright 1993, Robert Nation
 *     You may use this code for any purpose, as long as the original
 *     copyright remains in the source code and all documentation
 */


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <guile/gh.h>

#include "util.h"

#include "scwm.h"
#include "syscompat.h"
#include "screen.h"
#include "borders.h"
#include "window.h"

void 
redraw_titlebars(ScwmDecor *fl, int extra_height)
{
  ScwmWindow *psw = Scr.ScwmRoot.next;

  for ( ; psw; psw = psw->next) {
    if (!psw->fTitle || psw->fl != fl) {
      continue;
    }

    set_window_internal_title_height(psw, psw->title_height + extra_height);

    SetTitleBar(psw, False, True);
  }
  SetTitleBar(Scr.Hilite, True, True);
}


void
redraw_borders(ScwmDecor *fl) 
{
  ScwmWindow *psw = Scr.ScwmRoot.next;
  ScwmWindow *pswHilite = Scr.Hilite;

  for ( ; psw; psw = psw->next) {
    if (psw->fl != fl) {
      continue;
    }
    SetBorderX(psw, psw==pswHilite, True, True, None, True);
  }
}

void 
refresh_common(Window win_or_root)
{
  XSetWindowAttributes attributes;
  unsigned long valuemask;
  Window w;

  valuemask = CWOverrideRedirect | CWBackingStore | CWSaveUnder | CWBackPixmap;
  attributes.override_redirect = True;
  attributes.save_under = False;
  attributes.background_pixmap = None;
  attributes.backing_store = NotUseful;
  w = XCreateWindow(dpy,
		    win_or_root,
		    0, 0,
		    (unsigned int) Scr.DisplayWidth,
		    (unsigned int) Scr.DisplayHeight,
		    (unsigned int) 0,
		    CopyFromParent, (unsigned int) CopyFromParent,
		    (Visual *) CopyFromParent, valuemask,
		    &attributes);
  XMapWindow(dpy, w);
  XDestroyWindow(dpy, w);
  XFlush(dpy);

}

#if 0 /* FIXGJB: removed when menus reworked to use call*_hooks */
SCM 
call_thunk_with_message_handler(SCM thunk)
{
  struct scm_body_thunk_data thunk_data;

  DEREF_IF_SYMBOL(thunk);
  thunk_data.tag = SCM_BOOL_T;
  thunk_data.body_proc = thunk;
  return scm_internal_catch(SCM_BOOL_T, scm_body_thunk, &thunk_data,
			    scm_handle_by_message_noexit, "scwm");
}
#endif

void
ms_sleep(unsigned long ms)
{
  /* usleep is not guaranteed to work for us > 1,000,000 */
  if (ms > 1000) {
    sleep(ms/1000);
    ms %= 1000;
  }
  usleep(ms*1000);
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
