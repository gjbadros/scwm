/* $Id$
 * move.h
 */

#ifndef MOVE_H
#define MOVE_H

void AnimatedMoveWindow(Window w,int startX,int startY,int endX, int endY,
			Bool fWarpPointerToo, int cmsDelay, float *ppctMovement);

void AnimatedShadeWindow(ScwmWindow *sw, Bool fRollUp, 
			 int cmsDelay, float *ppctMovement);

void moveLoop(ScwmWindow * tmp_win, int XOffset, int YOffset, int Width,
	      int Height, int *FinalX, int *FinalY, Bool opaque_move,
	      Bool fAddWindow);

void DisplayPosition(ScwmWindow * tmp_win, int x, int y, int Init);

void Keyboard_shortcuts(XEvent * Event, int ReturnEvent);

void InteractiveMove(Window * win, ScwmWindow * tmp_win, 
		     int *FinalX, int *FinalY, XEvent * eventp);

#endif MOVE_H
