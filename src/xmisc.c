/* $Id$
 * xmisc.c
 * Miscellaneous abstractions of X11 routines
 * for scwm
 */

/*
 * Example Usage: 
 * XGetPointerWindowOffsets(Scr.Root,&pixRootXOffset,&pixRootYOffset)
 */
#include <X11/X.h>
#include <X11/Xlib.h>
#include "scwm.h"
#include "xmisc.h"

void
XGetPointerWindowOffsets(Window w, int *pxReturn, int *pyReturn)
{
  XQueryPointer(dpy,w,&JunkRoot,&JunkChild,
		pxReturn,pyReturn,&JunkX,&JunkY,&JunkMask);
}
