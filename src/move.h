/* $Id$
 * move.h
 * (C) 1997, 1998 Maciej Stachowiak and Greg J. Badros
 */

#ifndef MOVE_H
#define MOVE_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "window.h"

void AnimatedMoveWindow(Window w,int startX,int startY,int endX, int endY,
			Bool fWarpPointerToo, int cmsDelay, float *ppctMovement);

void AnimatedShadeWindow(ScwmWindow *psw, Bool fRollUp, 
			 int cmsDelay, float *ppctMovement);

void moveLoop(ScwmWindow *psw, int XOffset, int YOffset, int Width,
	      int Height, int *FinalX, int *FinalY, Bool opaque_move);

void DisplayPosition(ScwmWindow *psw, int x, int y, int Init);

void Keyboard_shortcuts(XEvent * Event, int ReturnEvent);

void InteractiveMove(Window w, ScwmWindow *psw, 
		     int *FinalX, int *FinalY, XEvent *eventp);

void MapSizePositionWindow();

#endif MOVE_H

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
