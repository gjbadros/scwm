/* $Id$
 * (C) 1997, 1998 Maciej Stachowiak and Greg J. Badros
 */

#ifndef WINDOW_H
#define WINDOW_H

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

#define PackedBool(x) unsigned short x:1

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

/* for each window that is on the display, one of these structures
 * is allocated and linked into a list 
 */
typedef struct ScwmWindow {
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
  int wShaped;			/* is this a shaped window */

  int frame_x;                  /* x position of frame */
  int frame_y;                  /* y position of frame */
  int frame_width;              /* width of frame */
  int frame_height;             /* height of frame */
  struct ScwmWindowConstraintInfo *pswci; /* Constraint information for this scheme window */
                                /* NULL if built w/o cassowary support */

  int boundary_width;
  int corner_width;
  int bw;
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

  int orig_x;			/* unmaximized x coordinate */
  int orig_y;			/* unmaximized y coordinate */
  int orig_wd;			/* unmaximized window width */
  int orig_ht;			/* unmaximized window height */

  int xdiff, ydiff;		/* used to restore window position on exit */
  int *mwm_hints;
  int ol_hints;
  int functions;  /* was enum wm_client_functions, but causes problems for C++ --06/24/98 gjb */
  Window *cmap_windows;		/* Colormap windows property */
  int number_cmap_windows;	/* Should generally be 0 */
  SCM ReliefColor;
  SCM ShadowColor;
  SCM TextColor;
  SCM BackColor;
  unsigned long buttons;
  int IconBox[4];

  /* Not used, but I'm not sure what it used to mean, so leaving it
     commented for now - MS 3-14-98 */
  /* int BoxFillMethod; */
  SCM schwin;
} ScwmWindow;

/* FIXGJB: fWindowListSkip, fCirculateSkipIcon, fCirculateSkip are unused */


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

SCM  ensure_valid(SCM win, int n, char *subr, SCM kill_p, SCM release_p);

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

#define WINDOWP(X) (SCM_NIMP(X) && (SCM_CAR(X) == (SCM)scm_tc16_scwm_window))
#define WINDOW(X)  ((scwm_window *)SCM_CDR(X))
/* SCWMWINDOW should disappear-- PSWFROMSCMWIN is a better name, IMO--07/17/98 gjb*/
/* #define SCWMWINDOW(X) (((scwm_window *)SCM_CDR(X))->psw) */
#define PSWFROMSCMWIN(X) (((scwm_window *)SCM_CDR(X))->psw)
#define VALIDWINP(X) (((scwm_window *)SCM_CDR(X))->valid)

#define set_window_context(X) window_context=X;
#define unset_window_context() window_context=SCM_UNDEFINED;

size_t free_window(SCM obj);
SCM mark_window(SCM obj);
int print_window(SCM obj, SCM port, scm_print_state * pstate);

void init_window();

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
void MovePswToCurrentPosition(ScwmWindow *psw);

void FocusOn(ScwmWindow *psw, int DeIconifyOnly);
void WarpOn(ScwmWindow *psw, int warp_x, int x_unit, int warp_y, int y_unit);

Bool GrabEm(enum cursor);
void UngrabEm(void);

SCM make_window(ScwmWindow *psw);
void invalidate_window(SCM schwin);
SCM window_p(SCM obj);

SCM get_window(SCM kill_p, SCM select_p, SCM release_p);
SCM current_window_with_focus();
SCM current_window_with_pointer();
SCM select_window_interactively();

SCM select_window(SCM kill_p, SCM release_p);
SCM delete_window(SCM win);
SCM destroy_window(SCM win);
SCM window_deletable_p(SCM win);
SCM focus(SCM win);
SCM unfocus();
SCM warp_to_window(SCM win);
SCM raise_window(SCM win);
SCM lower_window(SCM win);
SCM raised_p(SCM win);
SCM transient_p(SCM win);
SCM deiconify(SCM win);
SCM iconify(SCM win);
SCM iconified_p(SCM win);
SCM stick(SCM win);
SCM unstick(SCM win);
SCM sticky_p(SCM win);

SCM window_shade(SCM win, SCM animated_p);
SCM un_window_shade(SCM win, SCM animated_p);
SCM window_shaded_p(SCM win);


SCM set_animation_x(SCM vector);
SCM move_to(SCM x, SCM y, SCM win, SCM animated, SCM move_pointer_too);
SCM interactive_move(SCM win);
SCM resize_to(SCM w, SCM h, SCM win);
SCM interactive_resize(SCM win);
SCM refresh_window(SCM win);

SCM move_window_to_desk(SCM which, SCM win);
SCM window_position(SCM win);
SCM window_size(SCM win);
SCM window_id(SCM win);
SCM window_frame_id(SCM win);
SCM id_to_window(SCM window_id);
SCM frame_id_to_window(SCM window_id);

SCM window_desk(SCM win);
SCM window_title(SCM win);

SCM window_class(SCM win);
SCM window_resource(SCM win);

SCM list_all_windows();

SCM keep_on_top(SCM win);
SCM un_keep_on_top(SCM win);
SCM kept_on_top_p(SCM win);

SCM show_titlebar(SCM win);
SCM hide_titlebar(SCM win);
SCM titlebar_shown_p(SCM win);

SCM normal_border(SCM win);
SCM plain_border(SCM win);
SCM border_normal_p(SCM win);

SCM set_border_width_x(SCM width, SCM win);
SCM stick_icon(SCM win);
SCM unstick_icon(SCM win);
SCM icon_sticky_p(SCM win);

SCM set_random_placement_x(SCM val, SCM win);
SCM set_smart_placement_x(SCM val, SCM win);
SCM set_window_button_x(SCM butt, SCM val, SCM win);

SCM set_icon_box_x(SCM sx, SCM sy, SCM sw, SCM sh, SCM win);
SCM set_window_focus_x(SCM sym, SCM win);

SCM set_window_foreground_x(SCM fg, SCM win);
SCM set_window_background_x(SCM bg, SCM win);

SCM set_icon_title_x(SCM title, SCM win);

SCM set_mwm_buttons_x(SCM val, SCM win);
SCM set_mwm_border_x(SCM val, SCM win);

SCM set_show_icon_x (SCM flag, SCM win);
SCM set_force_icon_x (SCM flag, SCM win);
SCM set_icon_x(SCM val, SCM win);
SCM set_mini_icon_x(SCM val, SCM win);
SCM set_hint_override_x(SCM val, SCM w);
SCM set_decorate_transient_x(SCM val, SCM w);
SCM set_mwm_decor_hint_x(SCM val, SCM w);
SCM set_mwm_func_hint_x(SCM val, SCM w);
SCM set_PPosition_hint_x(SCM val, SCM w);
SCM set_OL_decor_hint_x(SCM val, SCM w);
SCM set_start_on_desk_x(SCM desk, SCM w);
SCM set_skip_mapping_x(SCM val, SCM w);
SCM set_lenience_x(SCM val, SCM win);



#endif /* WINDOW_H */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
