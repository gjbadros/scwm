/* $Id$
 * borders.h
 */

#ifndef BORDERS_H
#define BORDERS_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

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

void SetupFrame(ScwmWindow *psw, int x, int y, int w, int h, Bool sendEvent, 
                Bool fMoved, Bool fResized);

/* A bit dangerous -- since we have to get the order right, but
   still lots more readable */
#define WAS_MOVED 5  /* for checking -- just True, really */
#define NOT_MOVED False
#define WAS_RESIZED 9 /* for checking -- just True, really */
#define NOT_RESIZED False


/* some fancy font handling stuff */
/*
#ifdef I18N
#define NewFontAndColor(newfont,color,backcolor) {\
   Globalgcv.foreground = color;\
   Globalgcv.background = backcolor;\
   Globalgcm = GCFont | GCForeground | GCBackground; \
   XChangeGC(dpy,Scr.ScratchGC3,Globalgcm,&Globalgcv); \
}
#else
*/
#define NewFontAndColor(newfont,color,backcolor) {\
   Globalgcv.font = newfont;\
   Globalgcv.foreground = color;\
   Globalgcv.background = backcolor;\
   Globalgcm = GCFont | GCForeground | GCBackground; \
   XChangeGC(dpy,Scr.ScratchGC3,Globalgcm,&Globalgcv); \
}
/*
#endif
*/

enum border_hilite_flags {
  NO_HILITE = 0,
  TOP_HILITE = 1 << 0,
  RIGHT_HILITE = 1 << 1,
  BOTTOM_HILITE = 1 << 2,
  LEFT_HILITE = 1 << 3,
  FULL_HILITE = 1 << 4,
  HH_HILITE = 1 << 5
};

#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
