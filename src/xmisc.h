/* $Id$
 * xmisc.h
 * Copyright (C) 1997, 1998, 1999, 2000 Greg J. Badros and Maciej Stachowiak
 */

#ifndef XMISC_H__
#define XMISC_H__

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include <X11/X.h>
#include <X11/Xlib.h>

#include "image.h"
#include "window_fwd.h"

extern XGCValues Globalgcv;
extern unsigned long Globalgcm;
extern XEvent Event;

void SetXWindowGravity(Window w, int win_gravity);
Window WXGetPointerWindowOffsets(Window w, int *pxReturn, int *pyReturn);
Window WXGetPointerOffsets(Window w, int *pxRoot, int *pyRoot, int *pxReturn, int *pyReturn);
Window WXGetPointerChild(Window wParent);
Window WXGetWindowParent(Window wChild);
Bool FXGetWindowTopLeft(Window w, int *pxReturn, int *pyReturn);
Bool FXGetWindowSize(Window w, int *pwidthReturn, int *pheightReturn);
Bool FXWindowAccessible(Display *dpy, Window w);
Bool FXIsWindowMapped(Display *dpy, Window w);

void DrawImage(Window w, scwm_image *psimg, int cpixXoffset, int cpixYoffset, GC gc);
void DrawSubImage(Window w, scwm_image *psimg,
		  int cpixDstXoffset, int cpixDstYoffset,
		  int cpixSrcXoffset, int cpixSrcYOffset,
		  int cpixWidth, int cpixHeight,
		  GC gc);
XTextProperty *PNewXTextPropertyFromSz(const char *sz);
int flush_expose(Window w);
void SetGCColors(GC gc, Pixel pix, Pixel pixBG);
void SetGCFg(GC gc, Pixel pix);
void SetGCBg(GC gc, Pixel pix);
void RelieveRectangle(Window win,int x,int y,int w, int h,GC Hilite,GC Shadow);
char *SzExtractTextPropValue(const XTextProperty *pxtp);

#ifdef I18N
#define XFONT_TYPE XFontSet
#define XFONT_FONTTYPE(X) ((X)->fontset)
#else
#define XFONT_TYPE XFontStruct *
#define XFONT_FONTTYPE(X) ((X)->xfs)
#endif

int ComputeXTextWidth(XFONT_TYPE pxfs, const char *sz, int cch);
XColor XColorFromPixel(Pixel p);
Pixmap Pixmap1DeepFromPixmap(Pixmap p, Pixel fg, Pixel bg);
int NFromXPropertyCardinal(Window w, Atom a, Bool fDel, int def);


#endif /* XMISC_H__ */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */
