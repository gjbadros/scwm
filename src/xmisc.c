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
#include "screen.h"
#include "Picture.h"

void
XGetPointerWindowOffsets(Window w, int *pxReturn, int *pyReturn)
{
  XQueryPointer(dpy,w,&JunkRoot,&JunkChild,
		pxReturn,pyReturn,&JunkX,&JunkY,&JunkMask);
}

/* Note: gc is only used for bitmaps! */
void
DrawImage(Window w, Picture *pic, int cpixXoffset, int cpixYoffset, GC gc)
{
  if (pic->depth > 0) {
    GC gcPixmap = Scr.ScratchGC1;
    /* a full pixmap (as opposed to just a bitmap) */
    Globalgcv.clip_mask = pic->mask;
    Globalgcv.clip_x_origin = cpixXoffset;
    Globalgcv.clip_y_origin = cpixYoffset;
    
    XChangeGC(dpy,gcPixmap,(GCClipMask | GCClipXOrigin | GCClipYOrigin),&Globalgcv);
    XCopyArea(dpy, pic->picture, w, gcPixmap, 0, 0, pic->width, pic->height,
	      Globalgcv.clip_x_origin, Globalgcv.clip_y_origin);

    Globalgcv.clip_mask = None;
    XChangeGC(dpy,gcPixmap,(GCClipMask),&Globalgcv);
  } else {
    /* bitmap */
    XCopyPlane(dpy, pic->picture, w, gc, 0, 0, pic->width, pic->height,
	       cpixXoffset, cpixYoffset, 1 /* plane */);
  }
}    

