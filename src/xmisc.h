/* $Id$
 * xmisc.h
 * (C) 1997, 1998 Maciej Stachowiak and Greg J. Badros
 */

#ifndef XMISC_H
#define XMISC_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "image.h"
#include "window.h"

extern XGCValues Globalgcv;
extern unsigned long Globalgcm;
extern XEvent Event;

Bool FXGetPointerWindowOffsets(Window w, int *pxReturn, int *pyReturn);
Bool FXGetWindowTopLeft(Window w, int *pxReturn, int *pyReturn);
Bool FXWindowAccessible(Display *dpy, Window w);
Bool XGetGeometryCacheIt(Display *dpy, Window w);
void DrawImage(Window w, scwm_image *psimg, int cpixXoffset, int cpixYoffset, GC gc);
XTextProperty *PNewXTextPropertyFromSz(const char *sz);
int flush_expose(Window w);
void RestoreWithdrawnLocation(ScwmWindow *, Bool);

#endif
