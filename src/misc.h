/* $Id$ */

#ifndef _MISC_
#define _MISC_

#include <ctype.h>
#include "menus.h"
#include "Picture.h"
#include "system.h"

/************************************************************************
 * ReapChildren - wait() for all dead child processes
 ************************************************************************/
#include <sys/wait.h>
#ifdef HAVE_WAITPID
#define ReapChildren()  while ((waitpid(-1, NULL, WNOHANG)) > 0);
#else
#define ReapChildren()  while ((wait3(NULL, WNOHANG, NULL)) > 0);
#endif


typedef struct name_list_struct {
  struct name_list_struct *next;	/* pointer to the next name */
  char *name;			/* the name of the window */
  char *value;			/* icon name */
  char *mini_value;		/* mini icon name */
  char *Decor;
  int Desk;			/* Desktop number */
  unsigned long on_flags;
  unsigned long off_flags;
  int border_width;
  int resize_width;
  char *ForeColor;
  char *BackColor;
  int IconBox[4];
  int BoxFillMethod;
  unsigned long on_buttons;
  unsigned long off_buttons;
} name_list;

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

#define ICON_HEIGHT (Scr.IconFont.height+6)

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
void Destroy(ScwmWindow *);
int flush_expose(Window w);
void CoerceEnterNotifyOnCurrentWindow();
void RestoreWithdrawnLocation(ScwmWindow *, Bool);
Bool StashEventTime(XEvent * ev);
Bool GrabEm(int);
void UngrabEm(void);
void KeepOnTop(void);
void UnmapIt(ScwmWindow * t);
void RaiseWindow(ScwmWindow * t);
void LowerWindow(ScwmWindow * t);
void HandleHardFocus(ScwmWindow * t);

/* FIXGJB: BELOW ARE NOT DEFINED IN misc.h */

void FetchWmProtocols(ScwmWindow *);
void PaintEntry(MenuRoot *, MenuItem *);
void PaintMenu(MenuRoot *, XEvent *);
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

MenuRoot *NewMenuRoot(char *name, int function_or_popup);
void AddToMenu(MenuRoot *, char *, char *);
void MakeMenu(MenuRoot *);
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
Bool PopUpMenu(MenuRoot *, int, int);
int DeferExecution(XEvent *, Window *, ScwmWindow **, unsigned long *, int, int);

void SetStickyProp(ScwmWindow *, int, int, int);
void SetClientProp(ScwmWindow *);
void PopDownMenu(void);
void show_panner(void);
void WaitForButtonsUp(void);
void FocusOn(ScwmWindow * t, int DeIconifyOnly);
void WarpOn(ScwmWindow * t, int warp_x, int x_unit, int warp_y, int y_unit);
Bool PlaceWindow(ScwmWindow * tmp_win, unsigned long flags, int Desk);

int do_menu(MenuRoot * menu, int style);
int check_allowed_function(MenuItem * mi);
int check_allowed_function2(int function, ScwmWindow * t);
void MapIt(ScwmWindow * t);
void do_save(void);
void checkPanFrames(void);
void raisePanFrames(void);
void initPanFrames(void);
int XNextEvent_orTimeout(Display * dpy, XEvent * event);

void changeDesks(int val1, int val2);

void MakeMenus(void);

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
