/* $Id$
 * xmisc.h
 */

#ifndef XMISC_H
#define XMISC_H

void XGetPointerWindowOffsets(Window w, int *pxReturn, int *pyReturn);
void DrawImage(Window w, Picture *pic, int cpixXoffset, int cpixYoffset, GC gc);

#endif
