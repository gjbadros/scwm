/* $Id$
 * borders.h
 */

#ifndef BORDERS_H
#define BORDERS_H

void SetBorderX(ScwmWindow * t, Bool onoroff, Bool force, Bool Mapped,
		 Window expose_win, Bool really_force);

void SetBorder(ScwmWindow * t, Bool onoroff, Bool force, Bool Mapped,
	       Window expose_win);

void SetTitleBar(ScwmWindow * t, Bool onoroff, Bool NewTitle);

void RelieveWindow(ScwmWindow * t, Window win,
		   int x, int y, int w, int h,
		   GC ReliefGC, GC ShadowGC, int hilite);

void SetShape(ScwmWindow *psw, int w);

void SetTitleBar(ScwmWindow *psw, Bool onoroff, Bool NewTitle);

void SetupFrame(ScwmWindow *psw, int x, int y, int w, int h, Bool sendEvent);

#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
