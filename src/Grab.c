/* $Id */
#include <stdio.h>
#include <X11/Xlib.h>

static int xgrabcount = 0;

void 
XGrabServer_withSemaphore(Display * disp)
{
  if (xgrabcount == 0) {
    XGrabServer(disp);
  }
  ++xgrabcount;
}

void 
XUngrabServer_withSemaphore(Display * disp)
{
  if (--xgrabcount < 0) {	/* should never happen */
    fprintf(stderr,"%s: too many ungrabs!\n",__FUNCTION__);
    xgrabcount = 0;
  }
  if (xgrabcount == 0) {
    XUngrabServer(disp);
  }
}
