/* $Id$
 * xmisc.h
 */

#ifndef XMISC_H
#define XMISC_H

#include "image.h"

void XGetPointerWindowOffsets(Window w, int *pxReturn, int *pyReturn);
void XGetWindowTopLeft(Window w, int *pxReturn, int *pyReturn);
void DrawImage(Window w, scwm_image *psimg, int cpixXoffset, int cpixYoffset, GC gc);

#endif
