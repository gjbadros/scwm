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

#include <assert.h>
#include <X11/X.h>
#include <X11/Xlib.h>
#include "scwm.h"
#include "xmisc.h"
#include "window.h"
#include "screen.h"
#include "image.h"


Window JunkChild, JunkRoot;
unsigned int JunkWidth, JunkHeight, JunkBW, JunkDepth, JunkMask;

XGCValues Globalgcv;
unsigned long Globalgcm;

/*
 * Example Usage: 
 * child = WXGetPointerWindowOffsets(Scr.Root,&pixRootXOffset,&pixRootYOffset)
 */

Window
WXGetPointerWindowOffsets(Window w, int *pxReturn, int *pyReturn)
{
  Window child;
  if (XQueryPointer(dpy,w,&JunkRoot,&child,&JunkX,&JunkY,
                    pxReturn,pyReturn,&JunkMask))
    return child;
  else
    return None;
}


Window
WXGetPointerOffsets(Window w, int *pxRoot, int *pyRoot, int *pxReturn, int *pyReturn)
{
  Window child;
  if (XQueryPointer(dpy,w,&JunkRoot,&child,pxRoot,pyRoot,
                    pxReturn,pyReturn,&JunkMask))
    return child;
  else
    return None;
}

Window 
WXGetPointerChild(Window wParent)
{
  Window child;
  if (XQueryPointer(dpy,wParent,&JunkRoot,&child,&JunkX,&JunkY,
                    &JunkWidth,&JunkHeight,&JunkMask))
    return child;
  else
    return None;
}

Bool
FXGetWindowTopLeft(Window w, int *pxReturn, int *pyReturn)
{
  return XGetGeometry(dpy,w,&JunkRoot,pxReturn,pyReturn,
                      &JunkWidth,&JunkHeight,&JunkBW,&JunkDepth);
}


Bool
FXGetWindowSize(Window w, int *pwidthReturn, int *pheightReturn)
{
  return XGetGeometry(dpy,w,&JunkRoot,&JunkX,&JunkY,
                      pwidthReturn,pheightReturn,&JunkBW,&JunkDepth);
}


Bool
FXWindowAccessible(Display *dpy, Window w)
{
  /* XGetGeometry returns true if the call was successful */
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
 *  Puts window back where it was before Scwm took over 
 */
void 
RestoreWithdrawnLocation(ScwmWindow *psw, Bool fRestart)
{
  XWindowChanges xwc;

  if (!psw)
    return;

  if (FXGetWindowTopLeft(psw->w, &xwc.x, &xwc.y )) {
    unsigned int mask;
    /* Undo gravity adjustments. */
    xwc.x = psw->frame_x - GRAV_X_ADJUSTMENT(psw);
    xwc.y = psw->frame_y - GRAV_Y_ADJUSTMENT(psw);

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
	  (FRAME_X(psw) >= Scr.DisplayWidth) ||
	  (FRAME_Y(psw) >= Scr.DisplayHeight)) {
	int w2 = (FRAME_WIDTH(psw) >> 1);
	int h2 = (FRAME_HEIGHT(psw) >> 1);
	if ((xwc.x < -w2) || (xwc.x > (Scr.DisplayWidth - w2))) {
	  xwc.x = xwc.x % Scr.DisplayWidth;
	  if (xwc.x < -w2)
	    xwc.x += Scr.DisplayWidth;
	}
	if ((xwc.y < -h2) || (xwc.y > (Scr.DisplayHeight - h2))) {
	  xwc.y = xwc.y % Scr.DisplayHeight;
	  if (xwc.y < -h2)
	    xwc.y += Scr.DisplayHeight;
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

void 
SetGCFg(GC gc, Pixel pix)
{
  Globalgcv.foreground = pix;
  Globalgcm = GCForeground;
  XChangeGC(dpy,gc,Globalgcm,&Globalgcv);
}

void 
SetGCBg(GC gc, Pixel pix)
{
  Globalgcv.background = pix;
  Globalgcm = GCBackground;
  XChangeGC(dpy,gc,Globalgcm,&Globalgcv);
}

void 
SetGCColors(GC gc, Pixel pixFG, Pixel pixBG)
{
  Globalgcv.foreground = pixFG;
  Globalgcv.background = pixBG;
  Globalgcm = GCForeground | GCBackground;
  XChangeGC(dpy,gc,Globalgcm,&Globalgcv);
}

/*
 * RelieveRectangle - add relief lines to a rectangular window
 */
void
RelieveRectangle(Window win,int x,int y,int w, int h,GC Hilite,GC Shadow)
{
  XDrawLine(dpy, win, Hilite, x, y, w+x-1, y);
  XDrawLine(dpy, win, Hilite, x, y, x, h+y-1);
  XDrawLine(dpy, win, Shadow, x, h+y-1, w+x-1, h+y-1);
  XDrawLine(dpy, win, Shadow, w+x-1, y, w+x-1, h+y-1);
}


char *
SzExtractTextPropValue(const XTextProperty *pxtp)
{
  assert(pxtp);
#ifndef I18N
  return (char *) pxtp->value;
#else
  pxtp->nitems = strlen(pxtp->value);
  if (pxtp->encoding == XA_STRING)
    return (char *) pxtp->value;
  else {
    char **list;
    int num;
    if (XmbTextPropertyToTextList(dpy,pxtp,&list,&num) >= Success
        && num > 0 && *list)
      return *list;
    else
      return (char *) pxtp->value;
  }
#endif
}

/* Return the width of the string sz in pixels
   Takes the font as an XFontStruct or an XFontSet depending
   on i18n support; the arg passed is XFONT(scmFont) */
int
ComputeXTextWidth(XFONT_TYPE pxfs, const char *sz, int cch)
{
  if (cch < 0)
    cch = strlen(sz);
#ifdef I18N
  { /* scope */
  XRectangle dummy,log_ret;
  XmbTextExtents(pxfs, sz, cch, &dummy, &log_ret);
  return log_ret.width;
  }
#else
  return XTextWidth(pxfs, sz, cch);
#endif
}
