/* $Id$
 * xmisc.c
 *
 * Copyright (C) 1998, 1999, 2000  Greg J. Badros and Maciej Stachowiak 
 *
 * Miscellaneous abstractions of X11 routines for scwm
 */

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include <assert.h>
#include <X11/X.h>
#include <X11/Xlib.h>

#include "xmisc.h"

#include "scwm.h"
#include "window.h"
#include "screen.h"
#include "image.h"
#include "xproperty.h"

/* GJB:FIXME:: move the Junk vars from scwm.c into here --
   be sure they aren't used anywhere else, and make them static in 
   this file */
unsigned int JunkWidth, JunkHeight, JunkBW, JunkDepth, JunkMask;

XGCValues Globalgcv;
unsigned long Globalgcm;

void
SetXWindowGravity(Window w, int win_gravity)
{
  XSetWindowAttributes attrib;
  attrib.win_gravity = win_gravity;
  XChangeWindowAttributes(dpy,w,CWWinGravity,&attrib);
}

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

Window
WXGetWindowParent(Window wChild)
{
  Window parent;
  unsigned int nchildren;
  Window *children;
  if (XQueryTree(dpy,wChild,&JunkRoot,&parent,&children, &nchildren)) {
    /* success */
    return parent;
  } else {
    return None;
  }
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

Bool
FXIsWindowMapped(Display *dpy, Window w)
{
  XWindowAttributes xwa;
  if (XGetWindowAttributes(dpy,w,&xwa)) {
    /* success */
    return xwa.map_state != IsUnmapped;
  }
  return False;
}

int
NFromXPropertyCardinal(Window w, Atom a, Bool fDel, int def)
{
  char *prop_data;
  int aformat;
  unsigned long nitems;
  Atom atype;
  prop_data = GetXProperty(w, a, fDel, &atype, &aformat, &nitems);
  if (XA_CARDINAL == atype) def = *((int *)prop_data);
  XFree(prop_data);
  return def;
}
  


/* Note: gc is only used for bitmaps! */
void
DrawImage(Window w, scwm_image *psimg, int cpixXoffset, int cpixYoffset, GC gc)
{
  DrawSubImage(w, psimg, cpixXoffset, cpixYoffset, 0, 0, psimg->width, psimg->height, gc);
}    

/* Note: gc is only used for bitmaps! */
void
DrawSubImage(Window w, scwm_image *psimg,
	     int cpixDstXoffset, int cpixDstYoffset,
	     int cpixSrcXoffset, int cpixSrcYoffset,
	     int cpixWidth, int cpixHeight,
	     GC gc)
{
  if (psimg->depth > 0) {
    GC gcPixmap = Scr.ScratchGC1;
    /* a full pixmap (as opposed to just a bitmap) */
    Globalgcv.clip_mask = psimg->mask;
    Globalgcv.clip_x_origin = cpixDstXoffset - cpixSrcXoffset;
    Globalgcv.clip_y_origin = cpixDstYoffset - cpixSrcYoffset;
    
    XChangeGC(dpy,gcPixmap,(GCClipMask | GCClipXOrigin | GCClipYOrigin),&Globalgcv);
    XCopyArea(dpy, psimg->image, w, gcPixmap,
	      cpixSrcXoffset, cpixSrcYoffset,
	      cpixWidth, cpixHeight,
	      cpixDstXoffset, cpixDstYoffset);

    Globalgcv.clip_mask = None;
    XChangeGC(dpy,gcPixmap,(GCClipMask),&Globalgcv);
  } else {
    /* bitmap */
    XCopyPlane(dpy, psimg->image, w, gc,
	       cpixSrcXoffset, cpixSrcYoffset,
	       cpixWidth, cpixHeight,
	       cpixDstXoffset, cpixDstYoffset,
	       1 /* plane */);
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

XColor
XColorFromPixel(Pixel p)
{
  XColor xc;
  xc.pixel = p;
  XQueryColor(dpy, Scr.ScwmRoot.attr.colormap, &xc);
  return xc;
}

Pixmap
Pixmap1DeepFromPixmap(Pixmap p, Pixel fg, Pixel bg)
{
  int width, height;
  Pixmap answer;
  GC gc;

  FXGetWindowSize(p, &width, &height);
  answer = XCreatePixmap(dpy,Scr.Root,width,height,1);
  gc = XCreateGC(dpy, answer, 0, NULL);
  SetGCColors(gc,bg,bg);
  XFillRectangle(dpy,answer,gc,0,0,width,height);
  SetGCColors(gc,fg,bg);
  XCopyPlane(dpy,p,answer,gc,0,0,width,height,0,0,1);
  return answer;
}



/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */
