/* $Id$
 * borders.h
 */

#ifndef BORDERS_H
#define BORDERS_H

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include "window_fwd.h"

void SetBorderX(ScwmWindow * t, Bool onoroff, Bool force, Bool Mapped,
		 Window expose_win, Bool really_force);

void SetBorder(ScwmWindow * t, Bool onoroff, Bool force, Bool Mapped,
	       Window expose_win);

void RelieveWindow(ScwmWindow * t, Window win,
		   int x, int y, int w, int h,
		   GC ReliefGC, GC ShadowGC, int highlight);

void SetShape(ScwmWindow *psw, int w);
void SetShapedTitlebar(ScwmWindow *psw, int w);

void SetTitleBar(ScwmWindow *psw, Bool onoroff, Bool NewTitle);

void SetupFrame(ScwmWindow *psw, int x, int y, int w, int h, 
                Bool fMoved, Bool fResized);


/* A bit dangerous -- since we have to get the order right, but
   still lots more readable */
#define WAS_MOVED 5  /* for checking -- just True, really */
#define NOT_MOVED False
#define WAS_RESIZED 9 /* for checking -- just True, really */
#define NOT_RESIZED False


/* some fancy font handling stuff */
#define NewFontAndColor(GC,newfont,color,backcolor) do {\
   Globalgcv.font = newfont;\
   Globalgcv.foreground = color;\
   Globalgcv.background = backcolor;\
   Globalgcm = GCFont | GCForeground | GCBackground; \
   XChangeGC(dpy,GC,Globalgcm,&Globalgcv); \
} while(0)

enum border_highlight_flags {
  NO_HILITE = 0,
  TOP_HILITE = 1 << 0,
  RIGHT_HILITE = 1 << 1,
  BOTTOM_HILITE = 1 << 2,
  LEFT_HILITE = 1 << 3,
  FULL_HILITE = (TOP_HILITE | RIGHT_HILITE | BOTTOM_HILITE | LEFT_HILITE),
  HH_HILITE = 1 << 4
};

int CLeftButtons(const ScwmWindow *psw);
int CRightButtons(const ScwmWindow *psw);


#define NO_SIDE_DECORATIONS_P(psw) \
  (!scm_is_false(scm_object_property(SCM_FROM_PSW(psw), sym_no_side_decorations)))

#define NO_TOP_BORDER_DECORATION_P(psw) \
  (!scm_is_false(scm_object_property(SCM_FROM_PSW(psw), sym_no_top_border_decoration)))

#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

