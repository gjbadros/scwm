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
#include "scwm.h"
#include "screen.h"
#include "borders.h"

void 
redraw_titlebars(ScwmDecor *fl, int extra_height)
{
  int x, w, y, h;
  ScwmWindow *psw = Scr.ScwmRoot.next;

  for ( ; psw != NULL; psw = psw->next) {
    if (!psw->fTitle || psw->fl != fl) {
      continue;
    }
    x = FRAME_X(psw);
    y = FRAME_Y(psw);
    w = FRAME_WIDTH(psw);
    h = FRAME_HEIGHT(psw) - extra_height;
    /* FIXGJB: switch to ResizeTo? --07/26/98 gjb */
    SetupFrame(psw, x, y, w, h, True, WAS_MOVED, WAS_RESIZED);
    /* FIXGJB: why two calls in a row? --07/14/98 gjb */
#if 0
    SetTitleBar(psw, True, True);
#endif
    SetTitleBar(psw, False, True);
    psw = psw->next;
  }
  SetTitleBar(Scr.Hilite, True, True);
}


void
redraw_borders(ScwmDecor *fl) 
{
  ScwmWindow *psw = Scr.ScwmRoot.next;
  ScwmWindow *pswHilite = Scr.Hilite;

  while (psw != NULL) {
    if (psw->fl != fl) {
      psw = psw->next;
      continue;
    }
    SetBorderX(psw, psw==pswHilite, True, True, None, True);
    psw = psw->next;
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


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
