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


#include <guile/gh.h>
#include <config.h>
#include "scwm.h"
#include "screen.h"
#include "borders.h"

void 
redraw_titlebars(ScwmDecor * fl, int extra_height)
{
  int x, w, y, h;
  ScwmWindow *tmp, *hi;

  tmp = Scr.ScwmRoot.next;
  hi = Scr.Hilite;
  while (tmp != NULL) {
    if (!tmp->fTitle || tmp->fl != fl) {
      tmp = tmp->next;
      continue;
    }
    x = tmp->frame_x;
    y = tmp->frame_y;
    w = tmp->frame_width;
    h = tmp->frame_height - extra_height;
    tmp->frame_x = 0;
    tmp->frame_y = 0;
    tmp->frame_height = 0;
    tmp->frame_width = 0;
    SetupFrame(tmp, x, y, w, h, True);
    SetTitleBar(tmp, True, True);
    SetTitleBar(tmp, False, True);
    tmp = tmp->next;
  }
  SetTitleBar(hi, True, True);
}


void
redraw_borders(ScwmDecor *fl) 
{
  ScwmWindow *tmp, *hi;

  tmp = Scr.ScwmRoot.next;
  hi = Scr.Hilite;
  while (tmp != NULL) {
    if (tmp->fl != fl) {
      tmp = tmp->next;
      continue;
    }
    SetBorderX(tmp, tmp==hi, True, True, None, True);
    tmp = tmp->next;
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
		    (unsigned int) Scr.MyDisplayWidth,
		    (unsigned int) Scr.MyDisplayHeight,
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
