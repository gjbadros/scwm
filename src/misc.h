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

/* FIXGJB: these should use bit fields! */
/* values for name_list flags */
/* The first 13 items are mapped directly into the ScwmWindow structures
 * flag value, so they MUST correspond to the first 13 entries in scwm.h */
#define START_ICONIC_FLAG    (1<<0)
#define STAYSONTOP_FLAG      (1<<1)
#define STICKY_FLAG          (1<<2)
#define LISTSKIP_FLAG        (1<<3)
#define SUPPRESSICON_FLAG    (1<<4)
#define NOICON_TITLE_FLAG    (1<<5)
#define LENIENCE_FLAG        (1<<6)
#define STICKY_ICON_FLAG     (1<<7)
#define CIRCULATE_SKIP_ICON_FLAG  (1<<8)
#define CIRCULATESKIP_FLAG   (1<<9)
#define CLICK_FOCUS_FLAG     (1<<10)
#define SLOPPY_FOCUS_FLAG    (1<<11)
#define SHOW_MAPPING         (1<<12)

#define NOTITLE_FLAG         (1<<13)
#define NOBORDER_FLAG        (1<<14)
#define ICON_FLAG            (1<<15)
#define STARTSONDESK_FLAG    (1<<16)
#define BW_FLAG              (1<<17)
#define NOBW_FLAG            (1<<18)
#define FORE_COLOR_FLAG      (1<<19)
#define BACK_COLOR_FLAG      (1<<20)
#define RANDOM_PLACE_FLAG    (1<<21)
#define SMART_PLACE_FLAG     (1<<22)
#define MWM_BUTTON_FLAG      (1<<23)
#define MWM_DECOR_FLAG       (1<<24)
#define MWM_FUNCTIONS_FLAG   (1<<25)
#define MWM_OVERRIDE_FLAG    (1<<26)
#define MWM_BORDER_FLAG      (1<<27)
#define DECORATE_TRANSIENT_FLAG (1<<28)
#define NO_PPOSITION_FLAG    (1<<29)
#define OL_DECOR_FLAG        (1<<30)

#define MINIICON_FLAG        (1<<31)

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

/* FIXGJB: BELOW ARE NOT DEFINED IN misc.h */

void FetchWmProtocols(ScwmWindow *);
void InitEventHandlerJumpTable(void);
void DispatchEvent(void);
void HandleEvents(void);
void HandleExpose(void);
void HandleFocusIn(void);
void HandleFocusOut(void);
void HandleDestroyNotify(void);
void HandleMapRequest(void);
void HandleMapRequestKeepRaised(Window keepraised);
void HandleMapNotify(void);
void HandleUnmapNotify(void);
void HandleMotionNotify(void);
void HandleButtonRelease(void);
void HandleButtonPress(void);
void HandleEnterNotify(void);
void HandleLeaveNotify(void);
void HandleConfigureRequest(void);
void HandleClientMessage(void);
void HandlePropertyNotify(void);
void HandleKeyPress(void);
void HandleVisibilityNotify(void);
void GetGravityOffsets(ScwmWindow *, int *, int *);
void MoveViewport(int newx, int newy, Bool);
ScwmWindow *AddWindow(Window w);
int MappedNotOverride(Window w);
void GrabKeys(ScwmWindow *);
void SwitchPages(Bool, Bool);
void NextPage(void);
void PrevPage(void);

void CaptureAllWindows(void);
void SetTimer(int);
void RaiseThisWindow(int);
int GetContext(ScwmWindow *, XEvent *, Window * dummy);
void HandlePaging(int, int, int *, int *, int *, int *, Bool);
void SetFocus(Window, ScwmWindow *, Bool FocusByMouse);
void CheckAndSetFocus(void);
void initModules(void);
void HandleModuleInput(Window w, int channel);
void no_popup(char *ptr);
void KillModule(int channel, int place);
void ClosePipes(void);
void SmartPlacement(ScwmWindow * t, int width, int height, int *x, int *y);
void usage(void);

void GetMwmHints(ScwmWindow * t);
void SelectDecor(ScwmWindow *, unsigned long, int, int);
void SetStickyProp(ScwmWindow *, int, int, int);
void SetClientProp(ScwmWindow *);
void show_panner(void);
void WaitForButtonsUp(void);
void FocusOn(ScwmWindow * t, int DeIconifyOnly);
void WarpOn(ScwmWindow * t, int warp_x, int x_unit, int warp_y, int y_unit);
Bool PlaceWindow(ScwmWindow * tmp_win, unsigned long flags, int Desk);

void MapIt(ScwmWindow * t);
void do_save(void);
void initPanFrames(void);
int XNextEvent_orTimeout(Display * dpy, XEvent * event);

void changeDesks(int val1, int val2);

#ifdef BROKEN_SUN_HEADERS
#include "sun_headers.h"
#endif
#ifdef NEEDS_ALPHA_HEADER
#include "alpha_header.h"
#endif /* NEEDS_ALPHA_HEADER */
#endif /* _MISC_ */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
