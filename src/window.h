/* $Id$
 * Copyright (C) 1997-1999 Maciej Stachowiak and Greg J. Badros
 */

#ifndef WINDOW_H__
#define WINDOW_H__

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <libguile.h>

#include <assert.h>
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


extern SCM sym_click, sym_root_window;


/* See validate.h for a bit of description about these macros.
   These macros use FUNC_NAME as the name of the function 
   to report as being in error;  be sure that is correct.

   Sample Usage:

   VALIDATE_WIN_USE_CONTEXT(win);   
   [assume arg position 1, default to window context]

   VALIDATE_ARG_WIN(2, win);
   [specify arg position explicitly]
*/


#define VALIDATE_WIN_USE_CONTEXT(win) \
  do { if ((win = ensure_valid(win,1,FUNC_NAME, SCM_BOOL_T, SCM_BOOL_F)) == SCM_BOOL_F) \
          return SCM_BOOL_F; } while (0)

#define VALIDATE_WIN_COPY_USE_CONTEXT(win,psw) \
  do { if ((win = ensure_valid(win,1,FUNC_NAME, SCM_BOOL_T, SCM_BOOL_F)) == SCM_BOOL_F) \
          return SCM_BOOL_F; \
       else psw = PSWFROMSCMWIN(win); } while (0)

#define VALIDATE_ARG_WIN_COPY_USE_CONTEXT(pos,win,psw) \
  do { if ((win = ensure_valid(win,pos,FUNC_NAME, SCM_BOOL_T, SCM_BOOL_F)) == SCM_BOOL_F) \
          return SCM_BOOL_F; \
       else psw = PSWFROMSCMWIN(win); } while (0)

#define VALIDATE_WIN_COPY(arg,psw) \
  do { if (!WINDOWP(arg)) scm_wrong_type_arg(FUNC_NAME,1,arg); \
       else psw = PSWFROMSCMWIN(arg); } while (0)

#define VALIDATEKILL(win) \
  do { if ((win = ensure_valid(win,1,FUNC_NAME, SCM_BOOL_T, SCM_BOOL_T)) == SCM_BOOL_F) \
          return SCM_BOOL_F; } while (0)

#define VALIDATE_ARG_WIN_USE_CONTEXT(n,win) \
  do { if ((win = ensure_valid(win,n,FUNC_NAME, SCM_BOOL_T, SCM_BOOL_F)) == SCM_BOOL_F) \
           return SCM_BOOL_F; } while (0)

#define VALIDATE_PRESS_ONLY(win) \
  do { if ((win = ensure_valid(win,1,FUNC_NAME, SCM_BOOL_F, SCM_BOOL_F)) == SCM_BOOL_F) \
           return SCM_BOOL_F; } while (0)

/* VALIDATE_ARG_WIN does *not* allow interactive selection of a window
   like the above VALIDATE{,KILL,N,_PRESS_ONLY} macros, and it errors
   if the argument is not a window (instead of just returning SCM_BOOL_F 
   Also note that the order of arguments are reversed for VALIDATE_WIN_ARG
   relative to the old VALIDATEN -- this is in accordance with a "gjb-new" convention
   of the argument number being first in the argument checking macros --
   like VALIDATE_ARG_COLOR */
#define VALIDATE_ARG_WIN(pos,arg) \
  do { if (!WINDOWP(arg)) scm_wrong_type_arg(FUNC_NAME,pos,arg); } while (0)

#define VALIDATE_ARG_WIN_USE_F(pos,arg) \
  do { if (UNSET_SCM(arg)) arg = SCM_BOOL_F; \
       else if (!WINDOWP(arg)) scm_wrong_type_arg(FUNC_NAME,pos,arg); } while (0)

#define VALIDATE_ARG_WINVALID(pos,arg) \
  do { if (!WINDOWP(arg)) scm_wrong_type_arg(FUNC_NAME,pos,arg); \
       else if (!VALIDWINP(arg)) scm_misc_error(FUNC_NAME,"Window is not valid",SCM_EOL); } while (0)

#define VALIDATE_ARG_WINVALID_COPY(pos,arg,psw) \
  do { if (!WINDOWP(arg)) scm_wrong_type_arg(FUNC_NAME,pos,arg); \
       else if (!VALIDWINP(arg)) scm_misc_error(FUNC_NAME,"Window is not valid",SCM_EOL); \
       else psw = PSWFROMSCMWIN(arg); } while (0)


#define VALIDATE_ARG_WIN_COPY(pos,arg,psw) \
  do { if (!WINDOWP(arg)) scm_wrong_type_arg(FUNC_NAME,pos,arg); \
       else psw = PSWFROMSCMWIN(arg); } while (0)

#define VALIDATE_ARG_WIN_ROOTSYM_OR_NUM_COPY(pos,arg,w) \
  do {  if (arg == sym_root_window) w = Scr.Root;                 \
        else if (gh_number_p(arg)) {                              \
          assert(sizeof(Window) == sizeof(unsigned long));        \
          w = gh_scm2ulong(arg);                                  \
        } else if (WINDOWP(arg)) {                                \
          w = PSWFROMSCMWIN(arg)->w;                              \
        } else {                                                  \
          w = None;                                               \
          SCWM_WRONG_TYPE_ARG(pos, arg);                          \
        }                                                         \
  } while (0)



struct ScwmDecor;		/* definition in screen.h */

extern char NoName[];
extern char NoClass[];
extern char NoResource[];

struct ScwmWindowConstraintInfo;

typedef struct gravity_info_tag {
  unsigned short x:2; /* how many times the border width difference should we 
			 move the frame window east? */
  unsigned short y:2; /* how many times the border width difference should we 
			 move the frame window north? */
  unsigned short t:2; /* how many times (half the title bar height) should we
			 move the frame window north? */
  SCM *psym;          /* the SCM symbol for this gravity */
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

  SCM corner_cursors[4];
  SCM side_cursors[4];
  SCM title_cursor;
  SCM sys_cursor;
  SCM frame_cursor;
  SCM icon_cursor;

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

  int saved_boundary_width;     /* the decoration vertical boundary's width, in pixels, if it had one */
  int boundary_width;           /* the decoration vertical boundary's width, in pixels */
  int xboundary_width;          /* the decoration horizontal boundary's width, in pixels */
  int corner_width;             /* the width of the decoration handles, in pixels */
  int bw;                       /* the border_width for the frame, w, and Parent, in pixels */
  int title_x;
  int title_y;
  int title_height;		/* height of the title bar */
  int title_width;		/* width of the title window */
  int tbar_right;               /* width of the full title bar */
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
  time_t ttCreated;             /* when the window was created */
  time_t ttLastFocussed;        /* when the window was last focussed */

  /* FIXMS: We need to add comments to document what all these darned
     flags mean. */

  /* The "common" flags */
  PackedBool(fStartIconic);
  PackedBool(fOnTop);
  PackedBool(fSticky);
  PackedBool(fSuppressIcon);
  PackedBool(fNoIconTitle);
  PackedBool(fLenience);
  PackedBool(fStickyIcon);
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
  PackedBool(fSquashedTitlebar);
  PackedBool(fFullyConstructed);

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
  unsigned long buttons;        /* bitmask of *disabled* buttons; 1==button is off */
  int IconBox[4];

  SCM other_properties;

  SCM schwin;
};

/* FIXJTL: This is ugly, but needed to make window_fwd work at all; is
   it worth it? */
#ifndef WINDOW_FWD_H__
typedef struct ScwmWindow ScwmWindow;
#endif

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
void PswUpdateFlags(ScwmWindow *psw, unsigned long flags);


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

SCM ensure_valid(SCM win, int n, const char *func_name, SCM kill_p, SCM release_p);

typedef struct {
  ScwmWindow *psw;
  Bool valid;
} scwm_window;

EXTERN long scm_tc16_scwm_window;
EXTERN_SET(SCM scm_window_context,SCM_UNDEFINED);

#define WINDOWP(X) (SCM_NIMP(X) && (gh_car(X) == (SCM)scm_tc16_scwm_window))
#define WINDOW(X)  ((scwm_window *)gh_cdr(X))

#define SCWMWINDOW_FROM_PSW(X) WINDOW(((X)->schwin))

/* SCWMWINDOW is just an accessor for setting/getting
   PSWFROMSCMWIN returns NULL for invalid windows
   and cannot be used as an lvalue

   MOST CODE SHOULD USE PSWFROMSCMWIN */

#define SCWMWINDOW(X) (((scwm_window *)gh_cdr(X))->psw)
#define PSWFROMSCMWIN(X) (VALIDWINP(X)?SCWMWINDOW(X):0)

/* I tried making VALIDWINP ensure WINDOWP first, but that
   failed miserably and strangely.... why? --04/12/99 gjb */
#define VALIDWINP(X) (((scwm_window *)gh_cdr(X))->valid)


#define SET_VALIDWIN_FLAG(X,f) do { ((scwm_window *)gh_cdr(X))->valid = f; } while (0)

#define set_window_context(X) do { scm_window_context = (X); } while (0)
#define unset_window_context() do { scm_window_context = SCM_UNDEFINED; } while (0)

ScwmWindow *PswFromWindow(Display *dpy, Window w);
ScwmWindow *PswFromAnyWindow(Display *dpy, Window w);
ScwmWindow *PswFromPointerLocation(Display *dpy);
ScwmWindow *PswSelectInteractively(Display *dpy);

void MapIt(ScwmWindow *psw);

void free_window_names(ScwmWindow *psw, Bool nukename, Bool nukeicon);

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

Bool GrabEm(Cursor);
void UngrabEm(void);

void ScwmSaveContextPsw(Display *dpy, Window w, ScwmWindow *psw);

void invalidate_window(SCM schwin);
SCM make_window(ScwmWindow *psw);
void move_finalize(Window w, ScwmWindow * psw, int x, int y);
void move_finalize_virt(Window w, ScwmWindow * psw, int x, int y);

SCM convert_move_data(SCM x, SCM y, SCM win, const char *func_name, 
                       int *pStartX, int *pStartY,
                       int *pDestX, int *pDestY,
                       ScwmWindow **ppsw, Window *pw);

SCM list_stacking_order();

void set_window_internal_title_height(ScwmWindow *psw, int nh, Bool fInPlace);

/* GJB:FIXME:: this primitive should not be exposed in the interface, 
   but needs to be for resetting the relief decor, color.c */
SCM set_window_background_x(SCM bg, SCM win);

Bool FIsPartiallyInViewport(const ScwmWindow *psw);

void window_pixel_size_to_client_units(const ScwmWindow *psw,
                                       int width, int height, 
                                       int *px_units,
                                       int *py_units);

void notify_new_desk(ScwmWindow *psw, int desk, int old);

__inline__ int DecorationWidth(const ScwmWindow *psw);
__inline__ int DecorationHeight(const ScwmWindow *psw);


#endif /* WINDOW_H__ */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

