/* $Id$
 * xmisc.c
 *
 * (C) 1998 Maciej Stachowiak and Greg J. Badros
 *
 * Miscellaneous abstractions of X11 routines for scwm
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <X11/X.h>
#include <X11/Xlib.h>
#include "scwm.h"
#include "xmisc.h"
#include "screen.h"
#include "image.h"


XGCValues Globalgcv;
unsigned long Globalgcm;

/*
 * Example Usage: 
 * FXGetPointerWindowOffsets(Scr.Root,&pixRootXOffset,&pixRootYOffset)
 */
Bool
FXGetPointerWindowOffsets(Window w, int *pxReturn, int *pyReturn)
{
  return XQueryPointer(dpy,w,&JunkRoot,&JunkChild,&JunkX,&JunkY,
                       pxReturn,pyReturn,&JunkMask);
}

Bool
FXGetWindowTopLeft(Window w, int *pxReturn, int *pyReturn)
{
  return XGetGeometry(dpy,w,&JunkRoot,pxReturn,pyReturn,
                      &JunkWidth,&JunkHeight,&JunkBW,&JunkDepth);
}


Bool
FXWindowAccessible(Display *dpy, Window w)
{
  /* XGetGeometry returns true if the call was successful */
  return XGetGeometry(dpy, w, &JunkRoot, &JunkX, &JunkY,
		      &JunkWidth, &JunkHeight, &JunkBW, &JunkDepth);
}

/* For some reason, when adding a window, we need to ask the X server
   for a window geometry w/o caring about the results
   Note that this happens to be identical to FXWindowAccessible,
   but their names encode their separate uses (and making these
   inline functions would avoid the extra space overhead)
   --03/29/98 gjb */
Bool
XGetGeometryCacheIt(Display *dpy, Window w)
{
  return XGetGeometry(dpy, w, &JunkRoot, &JunkX, &JunkY,
                      &JunkWidth, &JunkHeight, &JunkBW, &JunkDepth);
}


/* Note: gc is only used for bitmaps! */
void
DrawImage(Window w, scwm_image *psimg, int cpixXoffset, int cpixYoffset, GC gc)
{
  if (psimg->depth > 0) {
    GC gcPixmap = Scr.ScratchGC1;
    /* a full pixmap (as opposed to just a bitmap) */
    Globalgcv.clip_mask = psimg->mask;
    Globalgcv.clip_x_origin = cpixXoffset;
    Globalgcv.clip_y_origin = cpixYoffset;
    
    XChangeGC(dpy,gcPixmap,(GCClipMask | GCClipXOrigin | GCClipYOrigin),&Globalgcv);
    XCopyArea(dpy, psimg->image, w, gcPixmap, 0, 0, psimg->width, psimg->height,
	      Globalgcv.clip_x_origin, Globalgcv.clip_y_origin);

    Globalgcv.clip_mask = None;
    XChangeGC(dpy,gcPixmap,(GCClipMask),&Globalgcv);
  } else {
    /* bitmap */
    XCopyPlane(dpy, psimg->image, w, gc, 0, 0, psimg->width, psimg->height,
	       cpixXoffset, cpixYoffset, 1 /* plane */);
  }
}    

XTextProperty *
PNewXTextPropertyFromSz(const char *sz)
{
  XTextProperty *ptextprop = NEW(XTextProperty);
  ptextprop->value = (unsigned char *) sz;
  ptextprop->encoding = XA_STRING;
  ptextprop->format = 8;
  ptextprop->nitems = strlen(sz);
  return ptextprop;
}

/*
 * Removes expose events for a specific window from the queue 
 */
int 
flush_expose(Window w)
{
  XEvent dummy;
  int i = 0;

  while (XCheckTypedWindowEvent(dpy, w, Expose, &dummy))
    i++;
  return i;
}

/*
 *  Procedure:
 *	RestoreWithdrawnLocation
 * 
 *  Puts windows back where they were before Scwm took over 
 */
void 
RestoreWithdrawnLocation(ScwmWindow *psw, Bool fRestart)
{
  int a, b, w2, h2;
  unsigned int mask;
  XWindowChanges xwc;

  if (!psw)
    return;

  if (FXGetWindowTopLeft(psw->w, &xwc.x, &xwc.y )) {
    XTranslateCoordinates(dpy, psw->frame, Scr.Root, xwc.x, xwc.y,
			  &a, &b, &JunkChild);
    xwc.x = a + psw->xdiff;
    xwc.y = b + psw->ydiff;
    xwc.border_width = psw->old_bw;
    mask = (CWX | CWY | CWBorderWidth);

    /* We can not assume that the window is currently on the screen.
     * Although this is normally the case, it is not always true.  The
     * most common example is when the user does something in an
     * application which will, after some amount of computational delay,
     * cause the window to be unmapped, but then switches screens before
     * this happens.  The XTranslateCoordinates call above will set the
     * window coordinates to either be larger than the screen, or negative.
     * This will result in the window being placed in odd, or even
     * unviewable locations when the window is remapped.  The followin code
     * forces the "relative" location to be within the bounds of the display.
     *
     * gpw -- 11/11/93
     *
     * Unfortunately, this does horrendous things during re-starts, 
     * hence the "if(!fRestart)" clause (RN) 
     *
     * Also, fixed so that it only does this stuff if a window is more than
     * half off the screen. (RN)
     */

    if (!fRestart) {
      /* Don't mess with it if its partially on the screen now */
      if ((FRAME_X(psw) < 0) || (FRAME_Y(psw) < 0) ||
	  (FRAME_X(psw) >= Scr.MyDisplayWidth) ||
	  (FRAME_Y(psw) >= Scr.MyDisplayHeight)) {
	w2 = (FRAME_WIDTH(psw) >> 1);
	h2 = (FRAME_HEIGHT(psw) >> 1);
	if ((xwc.x < -w2) || (xwc.x > (Scr.MyDisplayWidth - w2))) {
	  xwc.x = xwc.x % Scr.MyDisplayWidth;
	  if (xwc.x < -w2)
	    xwc.x += Scr.MyDisplayWidth;
	}
	if ((xwc.y < -h2) || (xwc.y > (Scr.MyDisplayHeight - h2))) {
	  xwc.y = xwc.y % Scr.MyDisplayHeight;
	  if (xwc.y < -h2)
	    xwc.y += Scr.MyDisplayHeight;
	}
      }
    }
    XReparentWindow(dpy, psw->w, Scr.Root, xwc.x, xwc.y);

    if (psw->fIconified && !(psw->fSuppressIcon)) {
      if (psw->icon_w)
	XUnmapWindow(dpy, psw->icon_w);
      if (psw->icon_pixmap_w)
	XUnmapWindow(dpy, psw->icon_pixmap_w);
    }
    XConfigureWindow(dpy, psw->w, mask, &xwc);
    if (!fRestart)
      XSync(dpy, 0);
  }
}
