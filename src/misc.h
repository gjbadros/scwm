/* $Id$ */

#ifndef _MISC_
#define _MISC_

#include <ctype.h>
#include "system.h"
#include "window.h"

/************************************************************************
 * ReapChildren - wait() for all dead child processes
 ************************************************************************/
#include <sys/wait.h>
#ifdef HAVE_WAITPID
#define ReapChildren()  while ((waitpid(-1, NULL, WNOHANG)) > 0);
#else
#define ReapChildren()  while ((wait3(NULL, WNOHANG, NULL)) > 0);
#endif


/* Cursor types */
enum cursor {
 CURSOR_POSITION,		/* upper Left corner cursor */
 CURSOR_TITLE,			/* title-bar cursor */
 CURSOR_DEFAULT,		/* cursor for apps to inherit */
 CURSOR_SYS,			/* sys-menu and iconify boxes cursor */
 CURSOR_MOVE,			/* resize cursor */
 CURSOR_WAIT,			/* wait a while cursor */
 CURSOR_MENU,			/* menu cursor */
 CURSOR_SELECT,			/* dot cursor for f.move, etc. from menus */
 CURSOR_DESTROY,		/* skull and cross bones, f.destroy */
 /* Order dependency on these cursors! */
 CURSOR_TOP,			/*  */
 CURSOR_RIGHT,			/*  */
 CURSOR_BOTTOM,			/*  */
 CURSOR_LEFT,			/*  */
 CURSOR_TOP_LEFT,		/*  */
 CURSOR_TOP_RIGHT,		/*  */
 CURSOR_BOTTOM_LEFT,		/*  */
 CURSOR_BOTTOM_RIGHT,		/*  */
 CURSOR_MAX_CURSORS		/*  */
};

/* some fancy font handling stuff */
#define NewFontAndColor(newfont,color,backcolor) {\
   Globalgcv.font = newfont;\
   Globalgcv.foreground = color;\
   Globalgcv.background = backcolor;\
   Globalgcm = GCFont | GCForeground | GCBackground; \
   XChangeGC(dpy,Scr.ScratchGC3,Globalgcm,&Globalgcv); \
}

#define ICON_HEIGHT (FONTHEIGHT(Scr.icon_font)+6)

#define NO_HILITE     0x0000
#define TOP_HILITE    0x0001
#define RIGHT_HILITE  0x0002
#define BOTTOM_HILITE 0x0004
#define LEFT_HILITE   0x0008
#define FULL_HILITE   0x000F
#define HH_HILITE     0x0010

extern XGCValues Globalgcv;
extern unsigned long Globalgcm;
extern Time lastTimestamp;
extern XEvent Event;

extern char NoName[];
extern char NoClass[];
extern char NoResource[];


/*
   ** message levels for scwm_msg:
 */
typedef enum scwm_msg_levels_tag { DBG = -1, INFO, WARN, ERR } scwm_msg_levels;
void scwm_msg(scwm_msg_levels type, char *id, char *msg,...);

void free_window_names(ScwmWindow * tmp, Bool nukename, Bool nukeicon);
int flush_expose(Window w);
void CoerceEnterNotifyOnCurrentWindow();
void RestoreWithdrawnLocation(ScwmWindow *, Bool);
Bool StashEventTime(XEvent * ev);
Bool GrabEm(enum cursor);
void UngrabEm(void);
void KeepOnTop(void);
Bool FXWindowAccessible(Display *dpy, Window w);
Bool XGetGeometryPositionOnly(Display *dpy, Window w, int *x_ret, int *y_ret);
Bool XGetGeometryCacheIt(Display *dpy, Window w);


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
