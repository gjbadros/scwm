/* $Id$
 * move.h
 * (C) 1997, 1998 Maciej Stachowiak and Greg J. Badros
 */

#ifndef MOVE_H
#define MOVE_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <X11/X.h>
#include <X11/Xlib.h>

#include "window_fwd.h"

void AnimatedMoveWindow(Window w,int startX,int startY,int endX, int endY,
			Bool fWarpPointerToo, int cmsDelay, float *ppctMovement);

void AnimatedShadeWindow(ScwmWindow *psw, Bool fRollUp, 
			 int cmsDelay, float *ppctMovement);

void moveLoop(ScwmWindow *psw,
              int XOffset, int YOffset, int Width,
	      int Height, int *FinalX, int *FinalY, Bool fOpaque);


void Keyboard_shortcuts(XEvent * Event, int ReturnEvent, 
                        const ScwmWindow *psw, Bool fResize);

Window WFrameOrIcon(ScwmWindow *psw);
Bool InteractiveMove(ScwmWindow *psw, Bool fOpaque, int *FinalX, int *FinalY);

void DisplayMessage(const char *sz, Bool fRelief);
void MapMessageWindow();
void UnmapMessageWindow();

#endif MOVE_H

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
