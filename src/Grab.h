/* $Id$ */

#ifndef _GRAB_H
#define _GRAB_H

void XGrabServer_withSemaphore(Display * disp);

void XUngrabServer_withSemaphore(Display * disp);

#endif
