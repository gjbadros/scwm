/* $Id$
 * (C) 1997, 1998 Maciej Stachowiak and Greg J. Badros
 */

#ifndef WINDOW_H__
#define WINDOW_H__

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <libguile.h>

#include "scwm.h"
#include "scwm-constraints.h"

#undef EXTERN
#undef EXTERN_SET
#ifdef WINDOW_IMPLEMENTATION
#define EXTERN
#define EXTERN_SET(x,y) x = y
#else
#define EXTERN extern
#define EXTERN_SET(x,y) extern x
#endif

struct ScwmDecor;		/* definition in screen.h */

extern char NoName[];
extern char NoClass[];
extern char NoResource[];


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


struct ScwmWindowConstraintInfo;




typedef struct gravity_info_tag {
  unsigned short x:2; /* how many times the border width difference should we 
			 move the frame window east? */
  unsigned short y:2; /* how many times the border width difference should we 
			 move the frame window north? */
  unsigned short t:2; /* how many times (half the title bar height) should we
			 move the frame window north? */
} gravity_info;


#define GRAV_X_ADJUSTMENT(psw) ((psw)->grav.x * ((psw)->old_bw - (psw)->bw - (psw)->xboundary_width))

#define GRAV_Y_ADJUSTMENT(psw) (((psw)->grav.y * ((psw)->old_bw - (psw)->bw - (psw)->boundary_width)) - ((psw)->grav.t * ((psw)->title_height) / 2))


/* for each window that is on the display, one of these structures
 * is allocated and linked into a list 
 * AddWindow initializes parts of this struct
 */
struct ScwmWindow {
  struct ScwmWindow *next;	/* next scwm window */
  struct ScwmWindow *prev;	/* prev scwm window */
  Window w;			/* the child window */
  int old_bw;			/* border width before reparenting */
  Window frame;			/* the frame window */
  Window Parent;		/* Ugly Ugly Ugly - it looks like you
				 * HAVE to reparent the app window into
				 * a window whose size = app window,
				 * or else you can't keep xv and matlab
				 * happy at the same time! */
  Window title_w;		/* the title bar window */
  Window sides[4];              /* top, right, bottom, then left */
  Window corners[4];		/* nw, ne, sw, se  coreners */
  int nr_left_buttons;
  int nr_right_buttons;
  Window left_w[5];
  Window right_w[5];
  struct ScwmDecor *fl;
  Window icon_w;		/* the icon window */
  Window icon_pixmap_w;		/* the icon window */
  Bool fShaped;			/* is this a shaped window */

  int frame_x;                  /* x position of frame */
  int frame_y;                  /* y position of frame */
  int frame_width;              /* width of frame */
  int frame_height;             /* height of frame */
  struct ScwmWindowConstraintInfo *pswci; /* Constraint information for this scheme window */
                                /* NULL if built w/o cassowary support */

  int boundary_width;           /* the decoration vertical boundary's width, in pixels */
  int xboundary_width;          /* the decoration horizontal boundary's width, in pixels */
  int corner_width;             /* the width of the decoration handles, in pixels */
  int bw;                       /* the border_width for the frame, w, and Parent, in pixels */
  int title_x;
  int title_y;
  int title_height;		/* height of the title bar */
  int title_width;		/* width of the title bar */
  int icon_x_loc;		/* icon window x coordinate */
  int icon_xl_loc;		/* icon label window x coordinate */
  int icon_y_loc;		/* icon window y coordiante */
  int icon_w_width;		/* width of the icon window */
  int icon_w_height;		/* height of the icon window */
  int icon_t_width;		/* width of the icon title window */
  int icon_p_height;            /* height of the icon pixmap window */
  int icon_p_width;             /* width of the icon pixmap window */
  char *name;			/* name of the window */
  char *icon_name;		/* name of the icon */
  XWindowAttributes attr;	/* the child window attributes */
  XSizeHints hints;		/* normal hints */
  XWMHints *wmhints;		/* WM hints */
  XClassHint classhint;
  int Desk;			/* Tells which desktop this window is on */
  int StartDesk;		/* Tells which desktop this window is on */
  int FocusDesk;		/* Where (if at all) was it focussed */
  int DeIconifyDesk;		/* Desk to deiconify to, for StubbornIcons */
  Window transientfor;

  /* FIXMS: We need to add comments to document what all these darned
     flags mean. */

  /* The "common" flags */
  PackedBool(fStartIconic);
  PackedBool(fOnTop);
  PackedBool(fSticky);
  PackedBool(fWindowListSkip);
  PackedBool(fSuppressIcon);
  PackedBool(fNoIconTitle);
  PackedBool(fLenience);
  PackedBool(fStickyIcon);
  PackedBool(fCirculateSkip);
  PackedBool(fCirculateSkipIcon);
  PackedBool(fClickToFocus);
  PackedBool(fSloppyFocus);
  PackedBool(fShowOnMap);

  PackedBool(fBorder);
  PackedBool(fTitle);
  PackedBool(fMapped);
  PackedBool(fIconified);
  PackedBool(fTransient);
  PackedBool(fRaised);
  PackedBool(fVisible);
  PackedBool(fIconOurs);
  PackedBool(fPixmapOurs);
  PackedBool(fShapedIcon);
  PackedBool(fMaximized);
  PackedBool(fDoesWmTakeFocus);
  PackedBool(fDoesWmDeleteWindow);
  PackedBool(fIconMoved);	/* has the icon been moved by the user? */

  /* was the icon unmapped, even though
     the window is still iconified (Transients) */
  PackedBool(fIconUnmapped);

  /* Sent an XMapWindow, but didn't receive a MapNotify yet. */
  PackedBool(fMapPending);
  PackedBool(fHintOverride);
  PackedBool(fMWMButtons);
  PackedBool(fMWMBorders);
  PackedBool(fMWMFunctions);
  PackedBool(fMWMDecor);
  PackedBool(fDecorateTransient);
  PackedBool(fWindowShaded);
  PackedBool(fStartsOnDesk);
  PackedBool(fRandomPlace);
  PackedBool(fSmartPlace);
  PackedBool(fOLDecorHint);
  PackedBool(fNoPPosition);
  PackedBool(fForceIcon);       

  SCM mini_icon_image;          /* A Scheme image object to use for the 
				   mini-icon. */
  SCM icon_req_image;		/* the icon picture requested */
  SCM icon_image;               /* the icon picture used */

  int orig_width;               /* unshaded/unmaximized window width */
  int orig_height;               /* unshaded/unmaximized window height */

  gravity_info grav;            /* Decoded gravity information. */

  int *mwm_hints;
  int ol_hints;
  int functions;                /* was enum wm_client_functions, 
                                   but causes problems for C++ since it is
                                   not used as an enumeration --06/24/98 gjb */
  Window *cmap_windows;		/* Colormap windows property */
  int number_cmap_windows;	/* Should generally be 0 */
  SCM ReliefColor;
  SCM ShadowColor;
  SCM TextColor;
  SCM BackColor;
  /* Now colors for when this is the highlighted window
     (i.e., for when it's got the focus) */
  SCM HiReliefColor;            /* only partially used now */
  SCM HiShadowColor;            /* only partially used now */
  SCM HiTextColor;
  SCM HiBackColor;
  unsigned long buttons;
  int IconBox[4];

  SCM other_properties;

  SCM schwin;
};

/* FIXJTL: This is ugly, but needed to make window_fwd work at all; is
   it worth it? */
#ifndef WINDOW_FWD_H__
typedef struct ScwmWindow ScwmWindow;
#endif

/* FIXGJB: fWindowListSkip, fCirculateSkipIcon, fCirculateSkip are unused */


/* Window viewport offsets to correct for the viewport
   position only if not a sticky window */
#define WIN_VP_OFFSET_X(psw) ((psw)->fSticky? 0: Scr.Vx)
#define WIN_VP_OFFSET_Y(psw) ((psw)->fSticky? 0: Scr.Vy)
#define ICON_VP_OFFSET_X(psw) ((psw)->fStickyIcon? 0: Scr.Vx)
#define ICON_VP_OFFSET_Y(psw) ((psw)->fStickyIcon? 0: Scr.Vy)

void ResetCommonFlags(ScwmWindow *psw);
void ResetAllFlags(ScwmWindow *psw);
void CopyCommonFlags(ScwmWindow *psw, const ScwmWindow *pswSrc);
void CopyAllFlags(ScwmWindow *psw, const ScwmWindow *pswSrc);
void CopySetAllFlags(ScwmWindow *psw, const ScwmWindow *pswSrc);
void CopySetCommonFlags(ScwmWindow *psw, const ScwmWindow *pswSrc);
unsigned long FlagsBitsFromSw(ScwmWindow *psw);


#define SHOW_TITLE_P(psw) ((psw)->fTitle && (!(psw)->fTransient || (psw)->fDecorateTransient))

#define SHADED_P(psw) ((psw)->fWindowShaded)
#define SET_UNSHADED(psw) do { (psw)->fWindowShaded = False; } while (0)
#define SET_SHADED(psw) do { (psw)->fWindowShaded = True; } while (0)

/* flags to suppress/enable title bar buttons */
#define BUTTON1     (1<<0)
#define BUTTON2     (1<<1)
#define BUTTON3     (1<<2)
#define BUTTON4     (1<<3)
#define BUTTON5     (1<<4)
#define BUTTON6     (1<<5)
#define BUTTON7     (1<<6)
#define BUTTON8     (1<<7)
#define BUTTON9     (1<<8)
#define BUTTON10    (1<<9)

SCM ensure_valid(SCM win, int n, char *subr, SCM kill_p, SCM release_p);

#define VALIDATE(win,subr)  if(((win=ensure_valid(win,1,subr,SCM_BOOL_F, SCM_BOOL_T)))==SCM_BOOL_F) return SCM_BOOL_F

#define VALIDATEKILL(win,subr)  if(((win=ensure_valid(win,1,subr,SCM_BOOL_T, SCM_BOOL_T)))==SCM_BOOL_F) return SCM_BOOL_F

#define VALIDATEN(win,n,subr)  if(((win=ensure_valid(win,n,subr,SCM_BOOL_F, SCM_BOOL_T)))==SCM_BOOL_F) return SCM_BOOL_F

#define VALIDATE_PRESS_ONLY(win,subr)  if(((win=ensure_valid(win,1,subr,SCM_BOOL_F, SCM_BOOL_F)))==SCM_BOOL_F) return SCM_BOOL_F

typedef struct {
  ScwmWindow *psw;
  int valid;
} scwm_window;

EXTERN long scm_tc16_scwm_window;
EXTERN_SET(SCM window_context,SCM_UNDEFINED);

EXTERN SCM invalid_interaction_hook;
EXTERN SCM cannot_grab_hook;

#define WINDOWP(X) (SCM_NIMP(X) && (gh_car(X) == (SCM)scm_tc16_scwm_window))
#define WINDOW(X)  ((scwm_window *)gh_cdr(X))
/* SCWMWINDOW should disappear-- PSWFROMSCMWIN is a better name, IMO--07/17/98 gjb*/
/* #define SCWMWINDOW(X) (((scwm_window *)gh_cdr(X))->psw) */
#define PSWFROMSCMWIN(X) (((scwm_window *)gh_cdr(X))->psw)
#define VALIDWINP(X) (((scwm_window *)gh_cdr(X))->valid)

#define set_window_context(X) window_context=X;
#define unset_window_context() window_context=SCM_UNDEFINED;

ScwmWindow *PswFromWindow(Display *dpy, Window w);
ScwmWindow *PswFromPointerLocation(Display *dpy);
ScwmWindow *PswSelectInteractively(Display *dpy);

void MapIt(ScwmWindow *psw);

void free_window_names(ScwmWindow *psw, Bool nukename, Bool nukeicon);

void DestroyScwmWindow(ScwmWindow *psw);
void UnmapScwmWindow(ScwmWindow * psw);
void RaiseWindow(ScwmWindow *psw);
void LowerWindow(ScwmWindow *psw);

void KeepOnTop();
void MovePswToCurrentPosition(const ScwmWindow *psw);
void MovePswIconToCurrentPosition(const ScwmWindow *psw);
void ResizePswToCurrentSize(ScwmWindow *psw);

/* do not call these functions -- they are exported for cassowary to call,
   and used in the non-cassowary standin "Suggest..." functions */
void SetScwmWindowGeometry(ScwmWindow *psw, int x, int y, int w, int h, 
                           Bool fOpaque);
void SetScwmWindowPosition(ScwmWindow *psw, int x, int y, 
                           Bool fOpaque);

void FocusOn(ScwmWindow *psw);
void WarpOn(ScwmWindow *psw, int warp_x, int x_unit, int warp_y, int y_unit);

void MoveTo(ScwmWindow *psw, int x, int y);
void ResizeTo(ScwmWindow *psw, int width, int height);
void MoveResizeTo(ScwmWindow *psw, int x, int y, int width, int height);

Bool GrabEm(enum cursor);
void UngrabEm(void);

void invalidate_window(SCM schwin);
SCM make_window(ScwmWindow *psw);
void move_finalize(Window w, ScwmWindow * psw, int x, int y);
void move_finalize_virt(Window w, ScwmWindow * psw, int x, int y);

void set_window_internal_title_height(ScwmWindow *psw, int nh);

/* FIXGJB: this primitive should not be exposed in the interface, 
   but needs to be for resetting the relief decor, color.c */
SCM set_window_background_x(SCM bg, SCM win);

Bool FIsPartiallyInViewport(const ScwmWindow *psw);

#endif /* WINDOW_H__ */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
