/* $Id$
 * window.c
 *
 * (C) 1998 Maciej Stachowiak and Greg J. Badros
 *
 * This module has been significantly modified by Maciej Stachowiak.
 * It may be used under the terms of the fvwm copyright (see COPYING.FVWM).
 *
 */

/* #define SCWM_DEBUG_MSGS */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <assert.h>
#include <stdio.h>
#include <X11/keysym.h>

#include <guile/gh.h>

#define WINDOW_IMPLEMENTATION
#include "window.h"

#include "scwm.h"
#include "screen.h"
#include "focus.h"
#include "move.h"
#include "icons.h"
#include "ICCCM.h"
#include "color.h"
#include "util.h"
#include "errors.h"
#include "Grab.h"
#include "resize.h"
#include "borders.h"
#include "decor.h"
#include "decorations.h"
#include "colors.h"
#include "colormaps.h"
#include "image.h"
#include "events.h"
#include "module-interface.h"
#include "virtual.h"
#include "font.h"
#include "xmisc.h"
#include "guile-compat.h"
#include "callbacks.h"
#include "add_window.h"
#include "dbug_resize.h"

/* also used by miscproc.c's set-colormap-focus! */
SCWM_GLOBAL_SYMBOL(sym_mouse , "mouse");


SCWM_SYMBOL(sym_sloppy , "sloppy");
SCWM_SYMBOL(sym_none , "none");

extern SCM sym_click, sym_root_window;

char NoName[] = "Untitled";	/* name if no name in XA_WM_NAME */
char NoClass[] = "NoClass";	/* Class if no res_class in class hints */
char NoResource[] = "NoResource";	/* Class if no res_name in class hints */

unsigned long 
FlagsBitsFromSw(ScwmWindow *psw)
{
  unsigned long flags = 0;
  int i = 0;

#define SET_BIT_FOR(x) do { if (psw->x) flags |= (1 << i); i++; } while (0)

#define SET_BIT_FOR_OBJ_PROP(s) \
do { \
  if (SCM_NFALSEP(scm_object_property(psw->schwin, \
                                     gh_symbol2scm(s)))) { \
/*    scwm_msg(DBG,__FUNCTION__,"fWinListSkip for %s",psw->name); */ \
    flags |= (1 << i); \
  } \
  i++; \
} while (0)

  SET_BIT_FOR(fStartIconic);
  SET_BIT_FOR(fOnTop);
  SET_BIT_FOR(fSticky);
  SET_BIT_FOR_OBJ_PROP("winlist-skip");
  SET_BIT_FOR(fSuppressIcon);
  SET_BIT_FOR(fNoIconTitle);
  SET_BIT_FOR(fLenience);
  SET_BIT_FOR(fSticky);
  SET_BIT_FOR_OBJ_PROP("circulate-skip-icon");
  SET_BIT_FOR_OBJ_PROP("circulate-skip");
  SET_BIT_FOR(fClickToFocus);
  SET_BIT_FOR(fSloppyFocus);
  SET_BIT_FOR(fShowOnMap);
  SET_BIT_FOR(fBorder);
  SET_BIT_FOR(fTitle);
  SET_BIT_FOR(fMapped);
  SET_BIT_FOR(fIconified);
  SET_BIT_FOR(fTransient);
  SET_BIT_FOR(fRaised);
  SET_BIT_FOR(fVisible);
  SET_BIT_FOR(fIconOurs);
  SET_BIT_FOR(fPixmapOurs);
  SET_BIT_FOR(fShapedIcon);
  SET_BIT_FOR_OBJ_PROP("maximize");
  SET_BIT_FOR(fDoesWmTakeFocus);
  SET_BIT_FOR(fDoesWmDeleteWindow);
  SET_BIT_FOR(fIconMoved);
  SET_BIT_FOR(fIconUnmapped);
  SET_BIT_FOR(fMapPending);
  SET_BIT_FOR(fHintOverride);
  SET_BIT_FOR(fMWMButtons);
  SET_BIT_FOR(fMWMBorders);
#undef SET_BIT_FOR
#undef SET_BIT_FOR_OBJ_PROP

  assert(i == 32);
  return flags;
}


void
ResetCommonFlags(ScwmWindow *psw)
{
  psw->fStartIconic =
    psw->fOnTop = 
    psw->fSticky = 
    psw->fWindowListSkip =
    psw->fSuppressIcon =
    psw->fNoIconTitle =
    psw->fLenience =
    psw->fStickyIcon =
    psw->fCirculateSkip =
    psw->fCirculateSkipIcon =
    psw->fClickToFocus =
    psw->fSloppyFocus =
    psw->fShowOnMap = False;
}

void
ResetAllFlags(ScwmWindow *psw)
{
  ResetCommonFlags(psw);

  psw->fBorder = 
    psw->fTitle =
    psw->fMapped =
    psw->fIconified =
    psw->fTransient =
    psw->fRaised =
    psw->fVisible =
    psw->fIconOurs =
    psw->fPixmapOurs =
    psw->fShapedIcon =
    psw->fMaximized =
    psw->fDoesWmTakeFocus =
    psw->fDoesWmDeleteWindow =
    psw->fIconMoved =
    psw->fIconUnmapped =
    psw->fMapPending =
    psw->fHintOverride =
    psw->fMWMButtons =
    psw->fMWMBorders =
    psw->fMWMFunctions =
    psw->fMWMDecor =
    psw->fDecorateTransient =
    psw->fWindowShaded =
    psw->fStartsOnDesk =
    psw->fOLDecorHint =
    psw->fNoPPosition =
    psw->fForceIcon = False;
}

void
CopyCommonFlags(ScwmWindow *psw, const ScwmWindow *pswSrc)
{
  psw->fStartIconic = pswSrc->fStartIconic;
  psw->fOnTop = pswSrc->fOnTop;
  psw->fSticky = pswSrc->fSticky;
  psw->fWindowListSkip = pswSrc->fWindowListSkip;
  psw->fSuppressIcon = pswSrc->fSuppressIcon;
  psw->fNoIconTitle = pswSrc->fNoIconTitle;
  psw->fLenience = pswSrc->fLenience;
  psw->fStickyIcon = pswSrc->fStickyIcon;
  psw->fCirculateSkip = pswSrc->fCirculateSkip;
  psw->fCirculateSkipIcon = pswSrc->fCirculateSkipIcon;
  psw->fClickToFocus = pswSrc->fClickToFocus;
  psw->fSloppyFocus = pswSrc->fSloppyFocus;
  psw->fShowOnMap = pswSrc->fShowOnMap;
}


/* Copy only the set common flags from pswSrc into psw
   i.e., this will not reset any of psw's flags */
void
CopySetCommonFlags(ScwmWindow *psw, const ScwmWindow *pswSrc)
{
  if (pswSrc->fStartIconic)
    psw->fStartIconic = True;
  if (pswSrc->fOnTop)
    psw->fOnTop = True;
  if (pswSrc->fSticky)
    psw->fSticky = True;
  if (pswSrc->fWindowListSkip)
    psw->fWindowListSkip = True;
  if (pswSrc->fSuppressIcon)
    psw->fSuppressIcon = True;
  if (pswSrc->fNoIconTitle)
    psw->fNoIconTitle = True;
  if (pswSrc->fLenience)
    psw->fLenience = True;
  if (pswSrc->fStickyIcon)
    psw->fStickyIcon = True;
  if (pswSrc->fCirculateSkip)
    psw->fCirculateSkip = True;
  if (pswSrc->fCirculateSkipIcon)
    psw->fCirculateSkipIcon = True;
  if (pswSrc->fClickToFocus)
    psw->fClickToFocus = True;
  if (pswSrc->fSloppyFocus)
    psw->fSloppyFocus = True;
  if (pswSrc->fShowOnMap)
    psw->fShowOnMap = True;
}

void
CopySetAllFlags(ScwmWindow *psw, const ScwmWindow *pswSrc)
{
  CopySetCommonFlags(psw,pswSrc);

  if ( pswSrc->fBorder )
    psw->fBorder = True;
  if ( pswSrc->fTitle )
    psw->fTitle = True;
  if ( pswSrc->fMapped )
    psw->fMapped = True;
  if ( pswSrc->fIconified )
    psw->fIconified = True;
  if ( pswSrc->fTransient )
    psw->fTransient = True;
  if ( pswSrc->fRaised )
    psw->fRaised = True;
  if ( pswSrc->fVisible )
    psw->fVisible = True;
  if ( pswSrc->fIconOurs )
    psw->fIconOurs = True;
  if ( pswSrc->fPixmapOurs )
    psw->fPixmapOurs = True;
  if ( pswSrc->fShapedIcon )
    psw->fShapedIcon = True;
  if ( pswSrc->fMaximized )
    psw->fMaximized = True;
  if ( pswSrc->fDoesWmTakeFocus )
    psw->fDoesWmTakeFocus = True;
  if ( pswSrc->fDoesWmDeleteWindow )
    psw->fDoesWmDeleteWindow = True;
  if ( pswSrc->fIconMoved )
    psw->fIconMoved = True;
  if ( pswSrc->fIconUnmapped )
    psw->fIconUnmapped = True;
  if ( pswSrc->fMapPending )
    psw->fMapPending = True;
  if ( pswSrc->fHintOverride )
    psw->fHintOverride = True;
  if ( pswSrc->fMWMButtons )
    psw->fMWMButtons = True;
  if ( pswSrc->fMWMBorders )
    psw->fMWMBorders = True;
  if ( pswSrc->fMWMFunctions )
    psw->fMWMFunctions = True;
  if ( pswSrc->fMWMDecor )
    psw->fMWMDecor = True;
  if ( pswSrc->fDecorateTransient )
    psw->fDecorateTransient = True;
  if ( pswSrc->fWindowShaded )
    psw->fWindowShaded = True;
  if ( pswSrc->fStartsOnDesk )
    psw->fStartsOnDesk = True;
  if ( pswSrc->fOLDecorHint )
    psw->fOLDecorHint = True;
  if ( pswSrc->fNoPPosition )
    psw->fNoPPosition = True;
  if ( pswSrc->fForceIcon )
    psw->fForceIcon = True;
}

void
CopyAllFlags(ScwmWindow *psw, const ScwmWindow *pswSrc)
{
  CopyCommonFlags(psw, pswSrc);

  psw->fBorder = pswSrc->fBorder;
  psw->fTitle = pswSrc->fTitle;
  psw->fMapped = pswSrc->fMapped;
  psw->fIconified = pswSrc->fIconified;
  psw->fTransient = pswSrc->fTransient;
  psw->fRaised = pswSrc->fRaised;
  psw->fVisible = pswSrc->fVisible;
  psw->fIconOurs = pswSrc->fIconOurs;
  psw->fPixmapOurs = pswSrc->fPixmapOurs;
  psw->fShapedIcon = pswSrc->fShapedIcon;
  psw->fMaximized = pswSrc->fMaximized;
  psw->fDoesWmTakeFocus = pswSrc->fDoesWmTakeFocus;
  psw->fDoesWmDeleteWindow = pswSrc->fDoesWmDeleteWindow;
  psw->fIconMoved = pswSrc->fIconMoved;
  psw->fIconUnmapped = pswSrc->fIconUnmapped;
  psw->fMapPending = pswSrc->fMapPending;
  psw->fHintOverride = pswSrc->fHintOverride;
  psw->fMWMButtons = pswSrc->fMWMButtons;
  psw->fMWMBorders = pswSrc->fMWMBorders;
  psw->fMWMFunctions = pswSrc->fMWMFunctions;
  psw->fMWMDecor = pswSrc->fMWMDecor;
  psw->fDecorateTransient = pswSrc->fDecorateTransient;
  psw->fWindowShaded = pswSrc->fWindowShaded;
  psw->fStartsOnDesk = pswSrc->fStartsOnDesk;
  psw->fOLDecorHint = pswSrc->fOLDecorHint;
  psw->fNoPPosition = pswSrc->fNoPPosition;
  psw->fForceIcon = pswSrc->fForceIcon;

}

/**CONCEPT: Windows 
  Windows are the most important scwm data type. A window object
represents an on-screen window that scwm is managing, and is used to
perform window management operations on the window, as well as to set
options and retrieve information about the window.
 */

size_t 
free_window(SCM obj)
{
  FREE(WINDOW(obj));
  return (0);
}

SCM 
mark_window(SCM obj)
{
  SCM_SETGC8MARK(obj);
    
  /* FIXGJB: revisit this */
  if (VALIDWINP(obj)) {
    ScwmWindow *psw = PSWFROMSCMWIN(obj);
    if (psw->fl != NULL) {
      scm_gc_mark(psw->fl->scmdecor);
    }
    scm_gc_mark(psw->mini_icon_image);
    scm_gc_mark(psw->icon_req_image);
    scm_gc_mark(psw->icon_image);
    scm_gc_mark(psw->ReliefColor);
    scm_gc_mark(psw->ShadowColor);
    scm_gc_mark(psw->TextColor);
    scm_gc_mark(psw->BackColor);
    scm_gc_mark(psw->HiReliefColor);
    scm_gc_mark(psw->HiShadowColor);
    scm_gc_mark(psw->HiTextColor);
    scm_gc_mark(psw->HiBackColor);
    GC_MARK_SCM_IF_SET(psw->HiReliefColor);
    GC_MARK_SCM_IF_SET(psw->HiShadowColor);
    GC_MARK_SCM_IF_SET(psw->HiTextColor);
    GC_MARK_SCM_IF_SET(psw->HiBackColor);
    scm_gc_mark(psw->other_properties);
  }

  return SCM_BOOL_F;
}


int 
print_window(SCM obj, SCM port, scm_print_state * pstate)
{
  scm_puts("#<window ", port);
  if (VALIDWINP(obj)) {
    ScwmWindow *psw = PSWFROMSCMWIN(obj);
    scm_write(gh_ulong2scm((unsigned long) (psw->w)), port);
    if (psw->name) {
      scm_puts(": \"",port);
      scm_puts(psw->name,port);
      scm_putc('"',port);
    }
  } else {
    scm_puts("(invalid)", port);
  }
  scm_putc('>', port);

  return 1;
}


SCM 
make_window(ScwmWindow * win)
{
  scwm_window *schwin;
  SCM answer;

  schwin = NEW(scwm_window);

  gh_defer_ints();

  SCWM_NEWCELL_SMOB(answer, scm_tc16_scwm_window, schwin);
  PSWFROMSCMWIN(answer) = win;
  VALIDWINP(answer) = 1;

  /* Warning, arbitrary constant, we really need growable hash
     tables. */

  win->other_properties=scm_make_vector(SCM_MAKINUM(5), SCM_EOL);
  scm_protect_object(answer);

  gh_allow_ints();
  return answer;
}

void 
invalidate_window(SCM schwin)
{
  VALIDWINP(schwin) = 0;
  PSWFROMSCMWIN(schwin) = NULL;
  scm_unprotect_object(schwin);
}


/**** ResizeTo, MoveResizeTo, MoveTo
 These three functions are the main way to reconfigure
 a top level window.  They will update the psw-> fields
 to the actual argument values directly if Cassowary is
 not in use, and will otherwise set the add the appropriate
 edit variables, set the constraint variables, and let
 ScwmResolve copy the new constraint variable values into
 the fields and then call ResizePswToCurrentSize or
 MovePswToCurrentPosition to make the X11 calls to
 actually move the window.

 These are not to be used from within interactive positioning as they
 encapsulate the adding and removing of the edit variables. Use the
 functions that these call after sandwiching between the cassowary
 edit variable adding and deleting: SuggestSizeWindowTo or
 SuggestMoveWindowTo.

 Note that resizing involves calling SetupFrame, which positions all
 the decoration windows appropriately.
*/

void
ResizeTo(ScwmWindow *psw, int width, int height)
{
  ConstrainSize(psw, 0, 0, &width, &height);
  CassowaryEditSize(psw);
  SuggestSizeWindowTo(psw, FRAME_X(psw), FRAME_Y(psw),
                      width, height, True);
  CassowaryEndEdit(psw);
}

/* x, y are virtual coordinates */
void
MoveResizeTo(ScwmWindow *psw, int x, int y, int width, int height)
{
  ConstrainSize(psw, 0, 0, &width, &height);
  CassowaryEditSize(psw);
  SuggestSizeWindowTo(psw, x, y, width, height, True);
  CassowaryEndEdit(psw);
}

/* x, y are virtual coordinates */
void
MoveTo(ScwmWindow *psw, int x, int y)
{
  CassowaryEditPosition(psw);
  SuggestMoveWindowTo(psw, x, y, True);
  CassowaryEndEdit(psw);
}


static
void
SendClientConfigureNotify(const ScwmWindow *psw)
{
  XEvent client_event;

  client_event.type = ConfigureNotify;
  client_event.xconfigure.display = dpy;
  client_event.xconfigure.event = psw->w;
  client_event.xconfigure.window = psw->w;
  
  client_event.xconfigure.x = FRAME_X_VP(psw) + psw->xboundary_width;
  client_event.xconfigure.y = FRAME_Y_VP(psw) + psw->title_height + psw->boundary_width;
  client_event.xconfigure.width = FRAME_WIDTH(psw) - 2 * psw->xboundary_width;
  client_event.xconfigure.height = FRAME_HEIGHT(psw) - 2 * psw->boundary_width - psw->title_height;
  
  client_event.xconfigure.border_width = psw->bw;
  /* Real ConfigureNotify events say we're above title window, so ... */
  /* what if we don't have a title ????? */
  client_event.xconfigure.above = psw->frame;
  client_event.xconfigure.override_redirect = False;
  DBUG_RESIZE((DBG,__FUNCTION__, "Sending configure event to %s",psw->name));
  XSendEvent(dpy, psw->w, False, StructureNotifyMask, &client_event);
}


/* Note that this will Broadcast the window information
   which will remove the icon window from the pager
   and display the window instead -- probably should
   be called only if !psw->fIconified */
void 
MovePswToCurrentPosition(const ScwmWindow *psw)
{
  int x = FRAME_X_VP(psw), y = FRAME_Y_VP(psw);
#ifndef NDEBUG
  if (psw->fSticky && !FIsPartiallyInViewport(psw)) {
    scwm_msg(ERR,__FUNCTION__,"Window %s is sticky but not on screen --\n\
correcting from %d,%d",
             psw->name,x,y);
    MoveTo((ScwmWindow *)psw,0,0);
  }
#endif
    
  XMoveWindow(dpy, psw->frame, x, y);
  SendClientConfigureNotify(psw);
  BroadcastConfig(M_CONFIGURE_WINDOW, psw);
}


/* Note that this will Broadcast the icon information
   which will remove the real mapped window from the pager
   and display the icon instead -- probably should
   be called only if psw->fIconified */
void 
MovePswIconToCurrentPosition(const ScwmWindow *psw)
{
  int x = ICON_X_VP(psw), y = ICON_Y_VP(psw);
  if (psw->icon_w != None)
    XMoveWindow(dpy, psw->icon_w, x, y + psw->icon_p_height);
  if (psw->icon_pixmap_w != None)
    XMoveWindow(dpy, psw->icon_pixmap_w, x, y);
  BroadcastIconInfo(M_ICON_LOCATION, psw);
}


/* ResizePswToCurrentSize may update psw->attr, so is not const */
void 
ResizePswToCurrentSize(ScwmWindow *psw)
{
  int w = FRAME_WIDTH(psw), h = FRAME_HEIGHT(psw);
  int x = FRAME_X_VP(psw), y = FRAME_Y_VP(psw);

  SendClientConfigureNotify(psw);

  if (SHADED_P(psw)) {
    h = psw->title_height + psw->boundary_width;
  }
  SetupFrame(psw,x,y,w,h,WAS_MOVED,WAS_RESIZED);
}


/* Similar to SetScwmWindowGeometry, below -- normal scwm functions
   don't call this -- it is called when cassowary is not re-solving
   for this window, so we must be sure that the window has really
   changed.  (Note that it may get called when cassowary is in use if
   there is no master solver yet) 
   x,y are virtual positions
*/
void
SetScwmWindowPosition(ScwmWindow *psw, int x, int y, Bool fOpaque) 
{
  if (x != FRAME_X(psw) || y != FRAME_Y(psw)) {
    SET_CVALUE(psw,frame_x,x);
    SET_CVALUE(psw,frame_y,y);
    if (fOpaque) 
      MovePswToCurrentPosition(psw);
    else {
      int h = FRAME_HEIGHT(psw);
      if (SHADED_P(psw)) {
        h = psw->title_height + psw->boundary_width;
      }
      RemoveRubberbandOutline(Scr.Root);
      RedrawOutlineAtNewPosition(Scr.Root, 
                                 FRAME_X_VP(psw), FRAME_Y_VP(psw),
                                 FRAME_WIDTH(psw), h);
    }
  }
}

/* Only resize if something has changed -- this gets called
   when cassowary is not re-solving for this window, so we must be sure that
   the window has really changed.
   (Note that it may get called when cassowary is in use if there is
   no master solver yet) 

   Normal scwm function never call this function -- they should call
   MoveTo or MoveResizeTo or ResizeTo, and this will get invoked as
   needed.

   x,y are virtual positions
  */
void
SetScwmWindowGeometry(ScwmWindow *psw, int x, int y, int w, int h, 
                      Bool fOpaque)
{
  Bool fNeedResize = (psw->frame_width != w || psw->frame_height != h);
  Bool fNeedMove = (psw->frame_x != x || psw->frame_y != y);
  if (fNeedMove || fNeedResize) {
    SET_CVALUE(psw,frame_x,x);
    SET_CVALUE(psw,frame_y,y);
    SET_CVALUE(psw,frame_width,w);
    SET_CVALUE(psw,frame_height,h);
    if (!fOpaque) {
      RemoveRubberbandOutline(Scr.Root);
      RedrawOutlineAtNewPosition(Scr.Root, 
                                 FRAME_X_VP(psw) - psw->bw, 
                                 FRAME_Y_VP(psw) - psw->bw,
                                 FRAME_WIDTH(psw) + 2 * psw->bw,
                                 FRAME_HEIGHT(psw) + 2 * psw->bw);
    } else {
      if (fNeedResize)
        ResizePswToCurrentSize(psw);
      else
        MovePswToCurrentPosition(psw);
    }
  }
}

/* This will involve cassowary as needed, through
   use of MoveTo 
   Same as MoveTo, except it knows about icons as well 
   x, y are viewport coordinates 
*/
void 
move_finalize(Window w, ScwmWindow * psw, int x, int y)
{
  DBUG((DBG,__FUNCTION__,"%d,%s, %d,%d",
        (w==psw->frame), psw->name, x,y));
  if (w == psw->frame) {
    x += WIN_VP_OFFSET_X(psw);
    y += WIN_VP_OFFSET_Y(psw);
  } else {
    x += ICON_VP_OFFSET_X(psw);
    y += ICON_VP_OFFSET_Y(psw);
  }
  DBUG((DBG,__FUNCTION__,"adjusted to %d,%d", x,y));
  move_finalize_virt(w,psw,x,y);
}



/* This will involve cassowary as needed, through
   use of MoveTo 
   Same as MoveTo, except it knows about icons as well 
   x, y are virtual coordinates
*/
void 
move_finalize_virt(Window w, ScwmWindow * psw, int x, int y)
{
  if (w == psw->frame) {
    MoveTo(psw,x,y);
  } else {			/* icon window */
    psw->fIconMoved = True;
    psw->icon_x_loc = x;
    psw->icon_xl_loc = y - (psw->icon_w_width - psw->icon_p_width) / 2;
    psw->icon_y_loc = y;
    MovePswIconToCurrentPosition(psw);
#if 0 /* FIXGJB */
    if (psw->icon_pixmap_w != None) {
      XMapWindow(dpy, psw->icon_w);
      XMapWindow(dpy, w);
    }
#endif
  }
}



static int DeferExecution(XEvent * eventp, Window * w, ScwmWindow **ppsw,
                          enum cursor cursor, int FinishEvent);


SCWM_PROC(window_p, "window?", 1, 0, 0,
          (SCM obj))
     /** Returns #t if OBJ is a window object, otherwise returns #f. */
#define FUNC_NAME s_window_p
{
  return SCM_BOOL_FromBool(WINDOWP(obj));
}
#undef FUNC_NAME

SCWM_PROC(select_window, "select-window", 0, 2, 0,
          (SCM kill_p, SCM release_p))
     /** Select a window interactively.
Use a special cursor and let the user click to select the window. The
optional arguments KILL? and RELEASE? indicate whether to use the
"skull and cross-bones" kill cursor (recommended for destructive
operations like delete-window and destroy-window), and whether to wait
for a mouse release or act immediately on the click. The former is a
place-holder until we have proper cursor support in scwm. */
#define FUNC_NAME s_select_window
{
  XEvent ev;
  Window w;
  ScwmWindow *psw;

  SCM_REDEFER_INTS;
  w = Scr.Root;

  psw = &Scr.ScwmRoot;

  if (kill_p == SCM_UNDEFINED) {
    kill_p = SCM_BOOL_F;
  } else if (!gh_boolean_p(kill_p)) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 1, kill_p);
  }

  if (release_p == SCM_UNDEFINED) {
    release_p = SCM_BOOL_T;
  } else if (!gh_boolean_p(release_p)) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 2, release_p);
  }


  if (DeferExecution(&ev, &w, &psw,
		     (kill_p != SCM_BOOL_F ? CURSOR_DESTROY : CURSOR_SELECT),
		     (release_p != SCM_BOOL_F ? ButtonRelease :
		      ButtonPress))) {
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }
  /* XXX - this needs to done right.  (Was != NULL before --10/24/97 gjb ) */
  if (psw && psw->schwin != SCM_UNDEFINED) {
    SCM_REALLOW_INTS;
    return (psw->schwin);
  } else {
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }
}
#undef FUNC_NAME


/**CONCEPT: The Window Context

  When actions are triggered by mouse or keyboard events, or menu
actions from menus originally popped up by mouse or keyboard events, a
context window is saved, which is used as the default for window
operations that are not passed their optional window argument. This
allows the user to easily bind actions to events without worrying
about passing around the window argument. 

  However, it is unclear whether behind-the-scenes magic like this is
a good idea.  The merit of this approach is still under consideration;
it may be changed entirely. */

/* FIXMS: consider reordering the arguments for get-window */

SCWM_PROC(get_window, "get-window", 0, 3, 0,
          (SCM kill_p, SCM select_p, SCM release_p))
     /** Retrieve the context window or select interactively.
If there is no context window, a window is selected interactively.
The optional boolean argument KILL?  (default #f) determines whether
to use the "kill" cursor when selecting interactively. The boolean
SELECT? argument (default #t) determines whether or not a window
should be selected interactively if there is no current context
window. And finally the RELEASE? argument (default #t) determines
whether or not interactive selection (if any) should wait for a mouse
release event or just a press. The latter behavior is useful if the
action being performed on the window is an interactive one involving
mouse drags. */
#define FUNC_NAME s_get_window
{
  if (kill_p == SCM_UNDEFINED) {
    kill_p = SCM_BOOL_F;
  } else if (!gh_boolean_p(kill_p)) {
    scm_wrong_type_arg(FUNC_NAME, 1, kill_p);
  }
  if (select_p == SCM_UNDEFINED) {
    select_p = SCM_BOOL_T;
  } else if (!gh_boolean_p(select_p)) {
    scm_wrong_type_arg(FUNC_NAME, 2, select_p);
  }
  if (release_p == SCM_UNDEFINED) {
    release_p = SCM_BOOL_T;
  } else if (!gh_boolean_p(release_p)) {
    scm_wrong_type_arg(FUNC_NAME, 3, release_p);
  }
  if (UNSET_SCM(window_context)) {
    if (select_p == SCM_BOOL_T) {
      return select_window(kill_p,release_p);
    } else {
      return SCM_BOOL_F;
    }
  }
  return window_context;
}
#undef FUNC_NAME


SCWM_PROC (set_window_context_x, "set-window-context!", 1, 0, 0,
           (SCM win))
     /** Set the current window context to WIN, return the old context.
WIN can be either a window, or #f, to reset the current window-context.
See also `with-window'. */
#define FUNC_NAME s_set_window_context_x
{
  int iarg = 1;
  SCM answer = window_context;
  if (win != SCM_BOOL_F && !WINDOWP(win)) {
    scm_wrong_type_arg(FUNC_NAME,iarg++,win);
  }
  window_context = win;
  if (answer == SCM_UNDEFINED)
    answer = SCM_BOOL_F;
  return answer;
}
#undef FUNC_NAME

SCWM_PROC(get_window_context, "window-context", 0, 0, 0,
          ())
     /** Returns the current window context, or #f if there is none.
See also `with-window' and `set-window-context!' */
#define FUNC_NAME s_get_window_context
{
  if (window_context == SCM_UNDEFINED)
    return SCM_BOOL_F;
  else
    return window_context;
}
#undef FUNC_NAME


SCWM_PROC(current_window_with_focus, "current-window-with-focus", 0, 0, 0,
          ())
     /** Return the window that currently has the input focus. */
#define FUNC_NAME s_current_window_with_focus
{
  return Scr.Hilite? Scr.Hilite->schwin : SCM_BOOL_F;
}
#undef FUNC_NAME


SCWM_PROC(current_window_with_pointer, "current-window-with-pointer", 0, 0, 0,
          ())
     /** Return the window that currently contains the mouse pointer. */
#define FUNC_NAME s_current_window_with_pointer
{
  ScwmWindow *psw = PswFromPointerLocation(dpy);
  return psw? psw->schwin: SCM_BOOL_F;
}
#undef FUNC_NAME


/* FIXGJB: it'd be nice to add an option to have the message window follow
   the pointer around! --07/25/98 gjb */
SCWM_PROC(select_window_interactively, "select-window-interactively", 0, 1, 0,
          (SCM msg))
     /** Returns a window selected interactively while displaying MSG.
Returns #f if no window was selected. Display no message if MSG not given. */
#define FUNC_NAME s_select_window_interactively
{
  ScwmWindow *psw = NULL;
  char *sz = NULL;
  Bool fWindow = False;
  if (!UNSET_SCM(msg) && !gh_string_p(msg)) {
    scm_wrong_type_arg(FUNC_NAME, 1, msg);
  }
  if (gh_string_p(msg)) {
    sz = gh_scm2newstr(msg, NULL);
    MapMessageWindow();
    DisplayMessage(sz, True);
    fWindow = True;
  }
  psw = PswSelectInteractively(dpy);
  if (fWindow) {
    FREE(sz);
    UnmapMessageWindow();
  }
  return psw? psw->schwin: SCM_BOOL_F;
}
#undef FUNC_NAME


/*
 * Keeps the "StaysOnTop" windows on the top of the pile.
 * This is achieved by clearing a flag for OnTop windows here, and waiting
 * for a visibility notify on the windows. Exeption: OnTop windows which are
 * obscured by other OnTop windows, which need to be raised here.
 */
void 
KeepOnTop()
{
  ScwmWindow *psw = Scr.ScwmRoot.next;

  /* flag that on-top windows should be re-raised */
  for ( ; psw != NULL; psw = psw->next) {
    if (psw->fOnTop && !psw->fVisible) {
      RaiseWindow(psw);
      psw->fRaised = False;
    } else
      psw->fRaised = True;
  }
}

Bool
FIsPartiallyInViewport(const ScwmWindow *psw)
{
  return ! (((FRAME_X_VP(psw) + FRAME_WIDTH(psw)) < 0) ||
            (FRAME_Y_VP(psw) + FRAME_HEIGHT(psw) < 0) ||
            (FRAME_X_VP(psw) > Scr.DisplayWidth) || 
            (FRAME_Y_VP(psw) > Scr.DisplayHeight));
}
     
/* pcx, pcy are output only */
static void
GetWindowVirtualCenter(const ScwmWindow *psw, int *pcx, int *pcy)
{
  int cx, cy;
  assert(psw);
  if (psw->fIconified) {
    cx = psw->icon_xl_loc + psw->icon_w_width / 2;
    cy = psw->icon_y_loc + psw->icon_p_height + ICON_HEIGHT / 2;
    if (psw->fStickyIcon) {
      cx += Scr.Vx;
      cy += Scr.Vy;
    }
  } else {
    /* not iconified */
    cx = FRAME_X_VP(psw) + FRAME_WIDTH(psw) / 2;
    cy = FRAME_Y_VP(psw) + FRAME_HEIGHT(psw) / 2;
    if (psw->fSticky) {
      cx += Scr.Vx;
      cy += Scr.Vy;
    }
  }
  *pcx = cx;
  *pcy = cy;
}

/* return the snapped viewport position (an even multiple of screen size)
   that contains vx,vy;
   viewport_x, viewport_y are output only
*/
static void
GetSnappedViewportPositionFor(int vx, int vy, int *viewport_x, int *viewport_y)
{
  /* go to an even multiple viewport */
  *viewport_x = (vx / Scr.DisplayWidth) * Scr.DisplayWidth;
  *viewport_y = (vy / Scr.DisplayHeight) * Scr.DisplayHeight;
}


/*
 * Moves focus to specified window 
 */
void 
FocusOn(ScwmWindow *psw)
{
  /* center coord of psw */
  int cx, cy;

  if (!psw)
    return;

  if (psw->Desk != Scr.CurrentDesk) {
    changeDesks(0, psw->Desk);
  }

  GetWindowVirtualCenter(psw,&cx,&cy);

  if (!FIsPartiallyInViewport(psw)) {
    int dx, dy;
    GetSnappedViewportPositionFor(cx,cy,&dx,&dy);
    MoveViewport(dx, dy, True);
  }

  KeepOnTop();

  if (!FIsPartiallyInViewport(psw)) {
    move_finalize(psw->frame,psw,0,0);
    if (!psw->fClickToFocus)
      XWarpPointer(dpy, None, Scr.Root, 0, 0, 0, 0, 2, 2);
  }
#if 0 /* FIXGJB --09/13/98 gjb */
  UngrabEm();
#endif
  SetFocus(psw->w, psw, False);
}




/*
 * Moves pointer to specified window 
 */
void 
WarpOn(ScwmWindow * psw, int warp_x, int x_unit, int warp_y, int y_unit)
{
  int cx, cy;
  int x, y;

  if (!psw || (psw->fIconified && psw->icon_w == None))
    return;

  if (psw->Desk != Scr.CurrentDesk) {
    changeDesks(0, psw->Desk);
  }

  GetWindowVirtualCenter(psw,&cx,&cy);

  if (!FIsPartiallyInViewport(psw)) {
    int dx, dy;
    GetSnappedViewportPositionFor(cx,cy,&dx,&dy);
    MoveViewport(dx, dy, True);
  }

  if (psw->fIconified) {
    x = psw->icon_xl_loc + psw->icon_w_width / 2 + 2;
    y = psw->icon_y_loc + psw->icon_p_height + ICON_HEIGHT / 2 + 2;
    if (!psw->fStickyIcon) {
      x -= Scr.Vx;
      y -= Scr.Vy;
    }
  } else {
    if (x_unit != Scr.DisplayWidth)
      x = FRAME_X_VP(psw) + 2 + warp_x;
    else
      x = FRAME_X_VP(psw) + 2 + (FRAME_WIDTH(psw) - 4) * warp_x / 100;
    if (y_unit != Scr.DisplayHeight)
      y = FRAME_Y_VP(psw) + 2 + warp_y;
    else
      y = FRAME_Y_VP(psw) + 2 + (FRAME_HEIGHT(psw) - 4) * warp_y / 100;
  }
  if (warp_x >= 0 && warp_y >= 0) {
    XWarpPointer(dpy, None, Scr.Root, 0, 0, 0, 0, x, y);
  }
  KeepOnTop();

  if (!FIsPartiallyInViewport(psw)) {
    move_finalize(psw->frame,psw,0,0);
    XWarpPointer(dpy, None, Scr.Root, 0, 0, 0, 0, 2, 2);
  }
#if 0 /* FIXGJB --09/13/98 gjb */
  UngrabEm();
#endif
}


/*
 * Grab the pointer and keyboard
 */
Bool 
GrabEm(enum cursor cursor)
{
  int i = 0, val = 0;
  unsigned int mask;

  XSync(dpy, 0);
  /* move the keyboard focus prior to grabbing the pointer to
   * eliminate the enterNotify and exitNotify events that go
   * to the windows */
  if (Scr.PreviousFocus == NULL)
    Scr.PreviousFocus = Scr.Focus;
  SetFocus(Scr.NoFocusWin, NULL, 0);
  mask = ButtonPressMask | ButtonReleaseMask | ButtonMotionMask | PointerMotionMask
    | EnterWindowMask | LeaveWindowMask;
  /*  FIXGJB: while ((i < 1000) &&  */
  while ((i < 5) && 
         (val = XGrabPointer(dpy, Scr.Root, True, mask,
                             GrabModeAsync, GrabModeAsync, Scr.Root,
                             Scr.ScwmCursors[cursor], 
                             CurrentTime) != GrabSuccess)) {
    i++;
    /* If you go too fast, other windows may not get a change to release
     * any grab that they have. */
    /* FIXGJB: fvwm2 must be doing the wrong thing here! --08/28/98 gjb */
    ms_sleep(1);
  }

  /* If we fall out of the loop without grabbing the pointer, its
     time to give up */
  XSync(dpy, 0);

  return (val == GrabSuccess);
}


/*
 * UnGrab the pointer and keyboard
 */
void 
UngrabEm()
{
  Window w;

  XSync(dpy, 0);
  XUngrabPointer(dpy, CurrentTime);

  if (Scr.PreviousFocus != NULL) {
    w = Scr.PreviousFocus->w;

    /* if the window still exists, focus on it */
    if (w) {
      SetFocus(w, Scr.PreviousFocus, 0);
    }
    Scr.PreviousFocus = NULL;
  }
  XSync(dpy, 0);
}


ScwmWindow *
PswFromWindow(Display *dpy, Window w)
{
  ScwmWindow *psw;
  if (XFindContext(dpy, w, ScwmContext, (caddr_t *) &psw) == XCNOENT) {
    return NULL;
  }
  return psw;
}

ScwmWindow *
PswFromPointerLocation(Display *dpy)
{
  Window wChild = WXGetPointerChild(Scr.Root);
  if (wChild == None) {
    return NULL;
  }
  return PswFromWindow(dpy,wChild);
}

ScwmWindow *
PswSelectInteractively(Display *dpy)
{
  /* FIXGJB: this should be the primitive that select_window calls,
     and this should replace DeferExecution. --07/25/98 gjb */
  SCM result = select_window(SCM_BOOL_F, SCM_BOOL_T);
  if (result == SCM_BOOL_F)
    return NULL;
  return PSWFROMSCMWIN(result);
}
  


extern Bool have_orig_position;

/*
 *
 *  Procedure:
 *	DeferExecution - return the interactively-selected window in *ppsw
 *
 *  Inputs:
 *      eventp  - pointer to XEvent to patch up
 *      w       - pointer to Window to patch up
 *      ppsw    - pointer to pointer to ScwmWindow that was selected
 *	cursor	- the cursor to display while waiting
 *      finishEvent - ButtonRelease or ButtonPress; tells what kind of event to
 *                    terminate on.
 *
 */
static int 
DeferExecution(XEvent * eventp, Window * w, ScwmWindow **ppsw,
	       enum cursor cursor, int FinishEvent)
{
  Bool fDone = False;
  Bool fFinished = False;
  Window original_w;

  original_w = *w;

  if (!GrabEm(cursor)) {
    call0_hooks(cannot_grab_hook);
    return True;
  }

  while (!fFinished) {
    fDone = False;
    XMaskEvent(dpy, ButtonPressMask | ButtonReleaseMask |
	       ExposureMask | KeyPressMask | VisibilityChangeMask |
	       ButtonMotionMask | PointerMotionMask, eventp);
    StashEventTime(eventp);

    if (eventp->type == KeyPress) {
      Keyboard_shortcuts(eventp, FinishEvent, 0, False);
    }
    if (eventp->type == FinishEvent) {
      fFinished = True;
    }
    if (eventp->type == ButtonPress && !fDone) {
      XAllowEvents(dpy, ReplayPointer, CurrentTime);
      fDone = True;
    }
    if (eventp->type == ButtonRelease || eventp->type == KeyPress) {
      XAllowEvents(dpy, ReplayPointer, CurrentTime);
      fDone = True;
    }
    if (!fDone) {
      /* copy the event to the global current event structure so the
	 right event is dispatched on. */
      Event = *eventp;
      DispatchEvent();
    }
  }

  *w = eventp->xany.window;
  if (((*w == Scr.Root) || (*w == Scr.NoFocusWin))
      && (eventp->xbutton.subwindow != (Window) 0)) {
    *w = eventp->xbutton.subwindow;
    eventp->xany.window = *w;
  }
  if (*w == Scr.Root) {
    call0_hooks(invalid_interaction_hook);
    UngrabEm();
    return True;
  }
  *ppsw = PswFromWindow(dpy,*w);
  if (*ppsw == NULL) {
    call0_hooks(invalid_interaction_hook);
    UngrabEm();
    return True;
  }
  if (*w == (*ppsw)->Parent)
    *w = (*ppsw)->w;

  if (original_w == (*ppsw)->Parent)
    original_w = (*ppsw)->w;

  /* this ugly mess attempts to ensure that the release and press
   * are in the same window. */
  if ((*w != original_w) && (original_w != Scr.Root) &&
      (original_w != None) && (original_w != Scr.NoFocusWin))
    if (!((*w == (*ppsw)->frame) &&
	  (original_w == (*ppsw)->w))) {
      call0_hooks(invalid_interaction_hook);
      UngrabEm();
      return True;
    }

  UngrabEm();
  /* interactive operations should not use the stashed mouse position
     if we just selected the window. */
  have_orig_position = False; 
  return False; 
}


/*
 * Unmaps a window on transition to a new desktop
 */
void 
UnmapScwmWindow(ScwmWindow * psw)
{
  XWindowAttributes winattrs;
  unsigned long eventMask;

  /*
   * Prevent the receipt of an UnmapNotify, since that would
   * cause a transition to the Withdrawn state.
   */
  XGetWindowAttributes(dpy, psw->w, &winattrs);
  eventMask = winattrs.your_event_mask;
  XSelectInput(dpy, psw->w, eventMask & ~StructureNotifyMask);
  if (psw->fIconified) {
    if (psw->icon_pixmap_w != None)
      XUnmapWindow(dpy, psw->icon_pixmap_w);
    if (psw->icon_w != None)
      XUnmapWindow(dpy, psw->icon_w);
  } else if (psw->fMapped || psw->fMapPending) {
    XUnmapWindow(dpy, psw->frame);
  }
  XSelectInput(dpy, psw->w, eventMask);
}

/*
 * Maps a window on transition to a new desktop
 */
void 
MapIt(ScwmWindow *psw)
{
  if (psw->fIconified) {
    if (psw->icon_pixmap_w != None)
      XMapWindow(dpy, psw->icon_pixmap_w);
    if (psw->icon_w != None)
      XMapWindow(dpy, psw->icon_w);
  } else if (psw->fMapped) {
    XMapWindow(dpy, psw->frame);
    psw->fMapPending = True;
    XMapWindow(dpy, psw->Parent);
  }
}

/*
 * Raise a window in the stacking order
 */ 
void 
RaiseWindow(ScwmWindow *psw)
{
  ScwmWindow *t2;
  int count, i;
  Window *wins;

  /* raise the target, at least */
  count = 1;
  Broadcast(M_RAISE_WINDOW, 3, psw->w, psw->frame, (unsigned long) psw, 0, 0, 0, 0);

  for (t2 = Scr.ScwmRoot.next; t2 != NULL; t2 = t2->next) {
    if (t2->fOnTop)
      count++;
    if (t2->fTransient && (t2->transientfor == psw->w) &&
	(t2 != psw)) {
      count++;
      Broadcast(M_RAISE_WINDOW, 3, t2->w, t2->frame, (unsigned long) t2,
		0, 0, 0, 0);
      if (t2->fIconified && !t2->fSuppressIcon) {
	count += 2;
      }
    }
  }
  if (psw->fIconified && !psw->fSuppressIcon) {
    count += 2;
  }
  wins = NEWC(count,Window);

  i = 0;

  /* fOnTop windows on top */
  for (t2 = Scr.ScwmRoot.next; t2 != NULL; t2 = t2->next) {
    if (t2->fOnTop) {
      Broadcast(M_RAISE_WINDOW, 3, t2->w, t2->frame, (unsigned long) t2,
		0, 0, 0, 0);
      wins[i++] = t2->frame;
    }
  }

  /* now raise transients */
  /* FIXGJB: this should be a runtime option */
#ifndef DONT_RAISE_TRANSIENTS
  for (t2 = Scr.ScwmRoot.next; t2 != NULL; t2 = t2->next) {
    if (t2->fTransient &&
	(t2->transientfor == psw->w) &&
	(t2 != psw) &&
	(!t2->fOnTop)) {
      wins[i++] = t2->frame;
      if (t2->fIconified && !t2->fSuppressIcon) {
	if (!t2->fNoIconTitle)
	  wins[i++] = t2->icon_w;
	if (!(t2->icon_pixmap_w))
	  wins[i++] = t2->icon_pixmap_w;
      }
    }
  }
#endif
  if (psw->fIconified && !psw->fSuppressIcon) {
    if (!psw->fNoIconTitle)
      wins[i++] = psw->icon_w;
    if (psw->icon_pixmap_w)
      wins[i++] = psw->icon_pixmap_w;
  }
  if (!psw->fOnTop) {
    wins[i++] = psw->frame;
    Scr.LastWindowRaised = psw;
  }

  if (i > 0)
    XRaiseWindow(dpy, wins[0]);

  XRestackWindows(dpy, wins, i);
  FREEC(wins);
  raisePanFrames();
}


void 
LowerWindow(ScwmWindow * psw)
{
  XLowerWindow(dpy, psw->frame);

  Broadcast(M_LOWER_WINDOW, 3, psw->w, psw->frame, (unsigned long) psw, 0, 0, 0, 0);

  if (psw->fIconified && !psw->fSuppressIcon) {
    XLowerWindow(dpy, psw->icon_w);
    XLowerWindow(dpy, psw->icon_pixmap_w);
  }
  Scr.LastWindowRaised = NULL;
}

/*
 * Releases dynamically allocated space used to store window/icon names
 */
void 
free_window_names(ScwmWindow *psw, Bool nukename, Bool nukeicon)
{
  if (!psw)
    return;

  if (nukename && nukeicon) {
    if (psw->name == psw->icon_name) {
      if (psw->name != NoName && psw->name != NULL)
	XFree(psw->name);
      psw->name = NULL;
      psw->icon_name = NULL;
    } else {
      if (psw->name != NoName && psw->name != NULL)
	XFree(psw->name);
      psw->name = NULL;
      if (psw->icon_name != NoName && psw->icon_name != NULL)
	XFree(psw->icon_name);
      psw->icon_name = NULL;
    }
  } else if (nukename) {
    if (psw->name != psw->icon_name
	&& psw->name != NoName
	&& psw->name != NULL)
      XFree(psw->name);
    psw->name = NULL;
  } else {			/* if (nukeicon) */
    if (psw->icon_name != psw->name
	&& psw->icon_name != NoName
	&& psw->icon_name != NULL)
      XFree(psw->icon_name);
    psw->icon_name = NULL;
  }

  return;
}


/*
 * Handles destruction of a window 
 * FIXGJB: maybe this should go right after AddWindow, in add_window.c
 * since it is most tightly couple to that code
 */
void 
DestroyScwmWindow(ScwmWindow *psw)
{
  int i;
  extern ScwmWindow *ButtonWindow;
  extern ScwmWindow *colormap_win;
  extern Bool PPosOverride;

  /*
   * Warning, this is also called by HandleUnmapNotify; if it ever needs to
   * look at the event, HandleUnmapNotify will have to mash the UnmapNotify
   * into a DestroyNotify.
   */
  if (!psw)
    return;

  XUnmapWindow(dpy, psw->frame);

  if (!PPosOverride)
    XSync(dpy, 0);

  if (psw == Scr.Hilite)
    Scr.Hilite = NULL;

  Broadcast(M_DESTROY_WINDOW, 3, psw->w, psw->frame,
	    (unsigned long) psw, 0, 0, 0, 0);

  if (Scr.PreviousFocus == psw)
    Scr.PreviousFocus = NULL;

  if (ButtonWindow == psw)
    ButtonWindow = NULL;

  if ((psw == Scr.Focus) && psw->fClickToFocus) {
    if (psw->next) {
      HandleHardFocus(psw->next);
    } else {
      SetFocus(Scr.NoFocusWin, NULL, 1);
    }
  } else if (Scr.Focus == psw) {
    SetFocus(Scr.NoFocusWin, NULL, 1);
  }

  if (psw == FocusOnNextTimeStamp)
    FocusOnNextTimeStamp = NULL;

  if (psw == Scr.Ungrabbed)
    Scr.Ungrabbed = NULL;

  if (psw == Scr.pushed_window)
    Scr.pushed_window = NULL;

  if (psw == colormap_win)
    colormap_win = NULL;

  XDestroyWindow(dpy, psw->frame);
  XDeleteContext(dpy, psw->frame, ScwmContext);

  XDestroyWindow(dpy, psw->Parent);

  XDeleteContext(dpy, psw->Parent, ScwmContext);

  XDeleteContext(dpy, psw->w, ScwmContext);

  if (psw->icon_w && psw->fPixmapOurs &&
      psw->icon_image != SCM_BOOL_F) {
    XFreePixmap(dpy, IMAGE(psw->icon_image)->image);
  }

  /* FIXGJB: these should check if the windows were created,
     not if the feature is currently turned on */
  if (psw->icon_w) {
    XDestroyWindow(dpy, psw->icon_w);
    XDeleteContext(dpy, psw->icon_w, ScwmContext);
  }
  if (psw->fIconOurs && (psw->icon_pixmap_w != None))
    XDestroyWindow(dpy, psw->icon_pixmap_w);
  if (psw->icon_pixmap_w != None)
    XDeleteContext(dpy, psw->icon_pixmap_w, ScwmContext);

  if (psw->fTitle) {
    XDeleteContext(dpy, psw->title_w, ScwmContext);
    for (i = 0; i < Scr.nr_left_buttons; i++)
      XDeleteContext(dpy, psw->left_w[i], ScwmContext);
    for (i = 0; i < Scr.nr_right_buttons; i++)
      if (psw->right_w[i] != None)
	XDeleteContext(dpy, psw->right_w[i], ScwmContext);
  }
  if (psw->fBorder) {
    for (i = 0; i < 4; i++)
      XDeleteContext(dpy, psw->sides[i], ScwmContext);
    for (i = 0; i < 4; i++)
      XDeleteContext(dpy, psw->corners[i], ScwmContext);
  }
  psw->prev->next = psw->next;
  if (psw->next != NULL)
    psw->next->prev = psw->prev;
  free_window_names(psw, True, True);
  if (psw->wmhints)
    XFree(psw->wmhints);
  /* removing NoClass change for now... */
  if (psw->classhint.res_name && psw->classhint.res_name != NoResource)
    XFree(psw->classhint.res_name);
  if (psw->classhint.res_class && psw->classhint.res_class != NoClass)
    XFree(psw->classhint.res_class);
  if (psw->mwm_hints)
    XFree(psw->mwm_hints);

  if (psw->cmap_windows != (Window *) NULL)
    XFree(psw->cmap_windows);

  /* XSCM */
  invalidate_window(psw->schwin);
  XFree(psw->name);
  FREE(psw);

  if (!PPosOverride)
    XSync(dpy, 0);
  return;
}


SCWM_PROC(delete_window, "delete-window", 0, 1, 0,
          (SCM win))
     /** Request that WIN remove itself from the display. 

This is the friendly way of closing a window, but it will not work if
the application does not cooperate. WIN defaults to the window context
in the usual way if not specified. */
#define FUNC_NAME s_delete_window
{
  ScwmWindow *psw;

  SCM_REDEFER_INTS;

  VALIDATEKILL(win, FUNC_NAME);

  psw = PSWFROMSCMWIN(win);
  if (check_allowed_function(F_DELETE, psw) == 0) {
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }
  if (psw->fDoesWmDeleteWindow) {
    send_clientmessage(dpy, psw->w, XA_WM_DELETE_WINDOW, CurrentTime);
    SCM_REALLOW_INTS;
    return SCM_BOOL_T;
  }
  SCM_REALLOW_INTS;
  return SCM_BOOL_F;
}
#undef FUNC_NAME


SCWM_PROC(destroy_window, "destroy-window", 0, 1, 0,
          (SCM win))
     /** Forcibly remove WIN from the screen.  

This will kill the application without giving it a chance to save its
state or do any other shutdown, but is guaranteed to work. WIN
defaults to the window context in the usual way if not specified. */
#define FUNC_NAME s_destroy_window
{
  ScwmWindow *psw;

  SCM_REDEFER_INTS;
  VALIDATEKILL(win, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);
  if (check_allowed_function(F_DESTROY, psw) == 0) {
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }
  if (!FXWindowAccessible(dpy,psw->w)) {
    DestroyScwmWindow(psw);
  } else {
    XKillClient(dpy, psw->w);
  }
  XSync(dpy, 0);
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}
#undef FUNC_NAME



SCWM_PROC(window_deletable_p, "window-deletable?", 0, 1, 0,
          (SCM win))
     /** Return #t if WIN is able to be deleted, #f otherwise.
Scwm may call `delete-window' on WIN only if this procedure returns
#t. WIN defaults to the window context in the usual way if not
specified. */
#define FUNC_NAME s_window_deletable_p
{
  VALIDATEKILL(win, FUNC_NAME);
  return SCM_BOOL_FromBool(PSWFROMSCMWIN(win)->fDoesWmDeleteWindow);
}
#undef FUNC_NAME


SCWM_PROC(focus, "focus", 0, 1, 0,
          (SCM win))
     /** Give WIN the input focus. 

This will typically result in drawing WIN's frame in a special style
as well. WIN defaults to the window context in the usual way if not
specified. Note that WIN is not raised by giving it the focus;  see
`raise-window' if that is your intent. */
#define FUNC_NAME s_focus
{
  ScwmWindow *psw;

  SCM_REDEFER_INTS;
  VALIDATE(win, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);
  FocusOn(psw);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(unfocus, "unfocus", 0, 0, 0,
          ())
     /** Remove the input focus from any window that may have it. */
#define FUNC_NAME s_unfocus
{
  SCM_REDEFER_INTS;
  Unfocus();
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME  


SCWM_PROC(warp_to_window, "warp-to-window", 0, 1, 0,
          (SCM win))
     /** Move the mouse pointer to the upper left corner of WIN.  

If WIN is on a different desk or in a different viewport, these will
be changed appropriately so that the window is visible. WIN defaults
to the window context in the usual way if not specified.  Note that
the target window is not raised, so if the target window's upper
left corner is under another window, that other window may end up
with the keyboard focus. */
#define FUNC_NAME s_warp_to_window
{
  SCM_REDEFER_INTS;
  VALIDATE(win, FUNC_NAME);
  WarpOn(PSWFROMSCMWIN(win), 0, 0, 0, 0);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(raise_window, "raise-window", 0, 1, 0,
          (SCM win))
     /** Raise WIN to the top of the window stack.
Stays-on-top windows still take priority. WIN defaults to the window
context in the usual way if not specified. */
#define FUNC_NAME s_raise_window
{
  ScwmWindow *psw;

  SCM_REDEFER_INTS;
  VALIDATE(win, FUNC_NAME);

  psw = PSWFROMSCMWIN(win);

  RaiseWindow(psw);
  /* FIXMS darn, this is not going to do what we want it to -- must
     start keeping a general stays on top flag as well a currently on
     top flag in the window struct, only the latter of which is
     changed by raises and lowers. */
  KeepOnTop();
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(lower_window, "lower-window", 0, 1, 0,
          (SCM win))
     /** Lower WIN to the bottom of the window stack. WIN defaults to
the window context in the usual way if not specified. */

#define FUNC_NAME s_lower_window
{
  SCM_REDEFER_INTS;
  VALIDATE(win, FUNC_NAME);
  LowerWindow(PSWFROMSCMWIN(win));
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



SCWM_PROC(restack_windows, "restack-windows", 1, 0, 0,
          (SCM winlist))
     /** Restack the windows in WINLIST from front to back.
The first element of WINLIST will be kept in its current stacking
order, the remainder will be stacked immediately below it in the order
given. (Note: This will currently confuse the heck out of the pager
and possibly other legacy fvwm2 modules). */
#define FUNC_NAME s_restack_windows
{
  Window *windows;
  SCM p;
  int i, cnt;

  if (!gh_list_p(winlist)) {
    scm_wrong_type_arg(FUNC_NAME, 1, winlist);
  }

  /* FIXMS: Hmmm, do we really want to restack the icons of iconified
     windows? */

  for (p=winlist, cnt=0; SCM_EOL!=p; p=gh_cdr(p)) {
    ScwmWindow *psw;
    SCM cur=gh_car(p);

    if (!WINDOWP(cur)) {
      scm_wrong_type_arg(FUNC_NAME, 1, winlist);      
    }

    psw=PSWFROMSCMWIN(cur);

    cnt++;

    if (psw->fIconified && !psw->fSuppressIcon) {
      if (!psw->fNoIconTitle) {
	cnt++;
      }
      if (!(psw->icon_pixmap_w)) {
	cnt++;
      }
    }
  }

  windows = NEWC(cnt,Window);
  
  /* FIXMS: This doesn't properly handle transient windows (the way
     raise does), but I am unsure what the really right way to handle
     those is. Need to see if ICCCM really requires them to always be
     in front of the app. In fact, ICCCM says no such thing about
     transients, so we should probably implement the RaiseTransients
     functionality using a more general hook of some kind,
     ultimately. */

  for (p=winlist, i=0; SCM_EOL!=p; p=gh_cdr(p)) {
    SCM cur=gh_car(p);
    ScwmWindow *psw=PSWFROMSCMWIN(cur);

    if (!WINDOWP(cur)) {
      FREEC(windows);
      scm_wrong_type_arg(FUNC_NAME, 1, winlist);      
    }

    windows[i++]=PSWFROMSCMWIN(cur)->frame;    

    if (psw->fIconified && !psw->fSuppressIcon) {
      if (!psw->fNoIconTitle) {
	windows[i++]=psw->icon_w;
      }
      if (!(psw->icon_pixmap_w)) {
	windows[i++]=psw->icon_pixmap_w;
      }
    }
  }
  
  /* This will interact badly with the fvwm pager, ultimately, we will
     want to fake an appropriate set of M_RAISE_WINDOW and
     M_LOWER_WINDOW broadcasts. */


  XRestackWindows(dpy, windows, cnt);

  KeepOnTop();

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



SCWM_PROC(raised_p, "raised?", 0, 1, 0,
          (SCM win))
     /** Return #t if WIN is currently raised, #f if not.
WIN defaults to the window context in the usual way if not
specified. */
#define FUNC_NAME s_raised_p
{
  ScwmWindow *psw;

  VALIDATE(win, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);
  return SCM_BOOL_FromBool(psw == Scr.LastWindowRaised ||
			   psw->fVisible);
}
#undef FUNC_NAME


SCWM_PROC(transient_p, "transient?", 0, 1, 0,
          (SCM win))
     /** Return #t if WIN is transient, #f if not.
WIN defaults to the window context in the usual way if not
specified. */
#define FUNC_NAME s_transient_p
{
  ScwmWindow *psw;

  VALIDATE(win, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);
  return SCM_BOOL_FromBool(psw->fTransient);
}
#undef FUNC_NAME

SCWM_PROC(window_transient_for, "window-transient-for", 0, 1, 0,
          (SCM win))
     /** Return the window for which WIN is transient.
If WIN is transient, and the window it is transient for is
not the root window and is being managed by scwm, this returns the window
object of the window that WIN is transient for, otherwise return
#f. WIN defaults to the window context in the usual way if not
specified. */
#define FUNC_NAME s_window_transient_for
{
  ScwmWindow *psw, *tpsw;

  VALIDATE(win, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);

  if (psw->fTransient
      && (psw->transientfor != None) &&
      (psw->transientfor != Scr.Root) &&
      ((tpsw=PswFromWindow (dpy, psw->transientfor)) !=NULL)) {
    return tpsw->schwin;
  } else {
    return SCM_BOOL_F;
  }
}
#undef FUNC_NAME


/* FIXGJB: rename to window-iconify */

SCWM_PROC(iconify, "iconify", 0, 1, 0,
          (SCM win))
     /** Iconify WIN.
Iconifying unmaps the regular window, and map the window's icon
window. WIN defaults to the window context in the usual way if not
specified. */
#define FUNC_NAME s_iconify
{
  ScwmWindow *psw;

  SCM_REDEFER_INTS;
  VALIDATE(win, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);

  if (check_allowed_function(F_ICONIFY, psw) == 0) {
    return SCM_BOOL_F;
  }
  Iconify(psw, 0, 0);

  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* FIXGJB: rename to window-deiconify */

SCWM_PROC(deiconify, "deiconify", 0, 1, 0,
          (SCM win))
     /** Deiconify WIN.
Unmap its icon window, and map its regular
window. WIN defaults to the window context in the usual way if not
specified. */
#define FUNC_NAME s_deiconify
{
  SCM_REDEFER_INTS;
  VALIDATE(win, FUNC_NAME);
  DeIconify(PSWFROMSCMWIN(win));
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/* FIXGJB: rename to window-iconified? */

SCWM_PROC(iconified_p, "iconified?", 0, 1, 0,
          (SCM win))
     /** Return #t if WIN is iconified, otherwise return #f.
WIN defaults to the window context in the usual way if not
specified. */
#define FUNC_NAME s_iconified_p
{
  VALIDATE(win, FUNC_NAME);
  return SCM_BOOL_FromBool(PSWFROMSCMWIN(win)->fIconified);
}
#undef FUNC_NAME

/* FIXGJB: rename to window-stick */

SCWM_PROC(stick, "stick", 0, 1, 0,
          (SCM win))
     /** Make WIN "sticky" so that it stays stationary in viewport.
A sticky window will appear on all desktops, and will remain at the
same screen position regardless of scrolling within the current
desktop. WIN defaults to the window context in the usual way if not
specified. */
#define FUNC_NAME s_stick
{
  ScwmWindow *psw;

  SCM_REDEFER_INTS;
  VALIDATE(win, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);
  if (!psw->fSticky) {
    psw->fSticky = True;
    CassowaryEditPosition(psw);
    SuggestMoveWindowTo(psw,
                        (FRAME_X_VP(psw) % Scr.DisplayWidth),
                        (FRAME_Y_VP(psw) % Scr.DisplayHeight),
                        True);
    CassowaryEndEdit(psw);
    BroadcastConfig(M_CONFIGURE_WINDOW, psw);
    SetTitleBar(psw, (Scr.Hilite == psw), True);
  }
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/* FIXGJB: rename to window-unstick */

SCWM_PROC(unstick, "unstick", 0, 1, 0,
          (SCM win))
     /** Cause a window to no longer be "sticky", if it is.
See `stick' for an explanation. WIN defaults to the window context in
the usual way if not specified. */
#define FUNC_NAME s_unstick
{
  ScwmWindow *psw;

  SCM_REDEFER_INTS;
  VALIDATE(win, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);
  if (psw->fSticky) {
    psw->fSticky = False;
    CassowaryEditPosition(psw);
    SuggestMoveWindowTo(psw,
                        (FRAME_X(psw)+Scr.Vx),
                        (FRAME_Y(psw)+Scr.Vy),
                        True);
    CassowaryEndEdit(psw);
    BroadcastConfig(M_CONFIGURE_WINDOW, psw);
    SetTitleBar(psw, (Scr.Hilite == psw), True);
  }
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/* FIXGJB: rename to window-sticky? */

SCWM_PROC(sticky_p, "sticky?", 0, 1, 0,
          (SCM win))
     /** Return #t if WIN is "sticky", #f otherwise.
See `stick' for an explanation. WIN defaults to the
window context in the usual way if not specified. */
#define FUNC_NAME s_sticky_p
{
  VALIDATE(win, FUNC_NAME);
  return SCM_BOOL_FromBool(PSWFROMSCMWIN(win)->fSticky);
}
#undef FUNC_NAME


/*
 *  WindowShade -- shades or unshades a window
 *  Originally written for fvwm2 by Andrew V. <veliaa@rpi.edu>
 */

/* Modified for scwm by mstachow@mit.edu, 
   animation added by gjb@cs.washington.edu */

/* FIXMS: split out animated versions of this and move-to into their
own procs! 
  why? so they're separate add-ons?  that's reasonable-- just curious --07/24/98 gjb
*/

SCWM_PROC(window_shade, "window-shade", 0, 2, 0,
          (SCM win, SCM animated_p))
     /** Cause WIN to become "window-shaded".
That is, to roll up into just a titlebar. By default, the change takes
place instantaneously. However, if the optional ANIMATED? argument is
true, the window will be animated as it rolls up, producing a pleasing
visual effect. WIN defaults to the window context in the usual way if
not specified. */
#define FUNC_NAME s_window_shade
{
  ScwmWindow *psw;
  Bool fAnimated = False;

  SCM_REDEFER_INTS;
  VALIDATE(win, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);

  if (!psw->fTitle || psw->fMaximized) {
    return SCM_BOOL_F;
  }
#ifdef GJB_BE_ANAL_ABOUT_BOOLS
  /* FIXGJB: I took this code out so I can say:
     (window-shade (get-window) 'animated)
     Is there a better way? */
  if (!gh_boolean_p(animated_p)) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 2, animated_p);
  }
#endif
  if (animated_p == SCM_UNDEFINED) {
    /* FIXGJB: make an option for allowing the default to be animated */
    animated_p = SCM_BOOL_F;
  }
  fAnimated = gh_scm2bool(animated_p);

  SET_SHADED(psw);
  
  if (fAnimated) {
    AnimatedShadeWindow(psw,True /* roll up */, -1, NULL);
    /* discard resize events */
    while (XCheckMaskEvent(dpy,  ResizeRedirectMask, &Event))
      { }
    /* We discard events so we don't propagate a resize
       event that will call SetupFrame again */
    /* Note sometimes the event we're trying to discard won't be
       generated in time for the above to discard it, so I had to hack
       the HandleConfigureNotify() routine to avoid resizing the
       frame; I left the XSync in for performance, since there's no
       reason to propagate that event if we can avoid it; perhaps
       substructure redirection is a solution here, but I don't know
       much about it --11/11/97 gjb */

    /* need to reset the client window offset so that if
       if it's un-window-shaded w/o animation, things are ok */
    XMoveWindow(dpy,psw->w,0,0);
  } /* else { FIXGJB: ideally, avoid this call when animated,
     but we need it to ensure that different combinations of 
     animated/unanimated shading do the right thing */
    SetupFrame(psw, FRAME_X_VP(psw), FRAME_Y_VP(psw), FRAME_WIDTH(psw),
               psw->title_height + psw->boundary_width, 
               NOT_MOVED, WAS_RESIZED);
    /*  } */

  CoerceEnterNotifyOnCurrentWindow();
  Broadcast(M_WINDOWSHADE, 1, psw->w, 0, 0, 0, 0, 0, 0);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* FIXGJB: rename to window-unshade */

SCWM_PROC(un_window_shade, "un-window-shade", 0, 2, 0,
          (SCM win, SCM animated_p))
    /** Reverse the effect of `window-shade' on WIN.
By default, the change takes place instantaneously. However, if the
optional ANIMATED? argument is true, the window will be animated as it
rolls down, producing a pleasing visual effect. WIN defaults to the
window context in the usual way if not specified. */
#define FUNC_NAME s_un_window_shade
{
  ScwmWindow *psw;
  Bool fAnimated = False;

  SCM_REDEFER_INTS;
  VALIDATE(win, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);

#ifdef GJB_BE_ANAL_ABOUT_BOOLS
  /* FIXGJB: I took this code out so I can say:
     (window-shade (get-window) 'animated)
     Is there a better way? */
  if (!gh_boolean_p(animated_p)) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 2, animated_p);
  }
#endif
  if (animated_p == SCM_UNDEFINED) {
    /* FIXGJB: make an option for allowing the default to be animated */
    animated_p = SCM_BOOL_F;
  }
  fAnimated = gh_scm2bool(animated_p);

  SET_UNSHADED(psw);
  if (fAnimated) {
    AnimatedShadeWindow(psw,False /* !roll up */, -1, NULL);
  } /* else { FIXGJB: ideally, avoid this call when animated,
     but we need it to ensure that different combinations of 
     animated/unanimated shading do the right thing */
    SetupFrame(psw, FRAME_X_VP(psw), FRAME_Y_VP(psw), 
               psw->orig_width, psw->orig_height,
               NOT_MOVED, WAS_RESIZED);
    /*  } */
  Broadcast(M_DEWINDOWSHADE, 1, psw->w, 0, 0, 0, 0, 0, 0);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(window_shaded_p, "window-shaded?", 0, 1, 0,
          (SCM win))
    /** Return #t if WIN is shaded.
WIN defaults to the window context in the usual way if not
specified. */
#define FUNC_NAME s_window_shaded_p
{
  VALIDATE(win, FUNC_NAME);
  return SCM_BOOL_FromBool(SHADED_P(PSWFROMSCMWIN(win)));
}
#undef FUNC_NAME




extern float rgpctMovementDefault[32];
extern int cpctMovementDefault;
extern int cmsDelayDefault;
extern int c10msDelaysBeforePopup;


/* set animation parameters */
SCWM_PROC(set_animation_x, "set-animation!", 1,0,0,
          (SCM vector))
     /** Set the animation parameters to VECTOR. VECTOR is a vector of
floats which give the fractions of the final position that the window
should appear at. For instance, #(0.0 0.25 0.5 0.75 1.0 1.1 1.0) would
make the window appear at the initial position, 1/4 of the way, 1/2 of
the way, 3/4 of the way, overshoot the final position slightly, and
finally slide back into place. This parameter is used for both
animated window shades and animated moves. */
#define FUNC_NAME s_set_animation_x
{
  int citems;
  int i;
  int iarg = 1;
/*
  FIXGJB: make a scheme-variable move-animation-delay get used instead
  if (!gh_int_p(delay) && !gh_boolean_p(delay)) {
    scm_wrong_type_arg(FUNC_NAME,iarg++,delay);
  } */
  if (!gh_vector_p(vector)) {
    scm_wrong_type_arg(FUNC_NAME,iarg++,vector);
  }
/*
  if (gh_int_p(delay)) {
    cmsDelayDefault = gh_scm2int(delay);
  }
  */
  citems = gh_vector_length(vector);
  for (i=0; i<citems; i++) {
    SCM val = gh_vector_ref(vector,gh_int2scm(i));    
    if (!gh_number_p(val)) {
      scm_wrong_type_arg(FUNC_NAME,iarg-1,vector);
    }
    /* FIXGJB: also check < 2, perhaps (don't want to
      check < 1, since we might want to overshoot and then come back) */
    rgpctMovementDefault[i] = (float) gh_scm2double(val);
  }
  /* Ensure that we end up 100% of the way to our destination */
  if (i>0 && rgpctMovementDefault[i-1] != 1.0) {
    rgpctMovementDefault[i++] = 1.0;
  }
  return SCM_UNDEFINED;
}
#undef FUNC_NAME
 
/* FIXMS: move frame or window? How to deal w/ gravity? */

SCWM_PROC(move_window, "move-window", 2, 3, 0,
          (SCM x, SCM y, SCM win, SCM animated_p, SCM move_pointer_too_p))
     /** Move WIN to coordinates virtual coordinates X, Y.
If X is #f, then X defaults to the current X position of WIN.
If Y is #f, then Y defaults to the current Y position of WIN.
If ANIMATED? is specified and true, animate the motion of the window,
otherwise the move is instantaneous. If MOVE-POINTER-TOO? is specified
and true, move the mouse pointer by the same amount as the window,
animating the motion of the pointer along with the window if ANIMATED?
is true. WIN defaults to the window context in the usual way if not
specified. */
#define FUNC_NAME s_move_window
{
  ScwmWindow *psw;
  Window w;
  Bool fMovePointer = False;
  Bool fAnimated = False;
  int startX, startY;
  int destX, destY;

  SCM_REDEFER_INTS;
  VALIDATEN(win, 3, FUNC_NAME);
  if (x != SCM_BOOL_F && !gh_number_p(x)) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 1, x);
  }
  if (y != SCM_BOOL_F && !gh_number_p(y)) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 2, y);
  }
  if (x == SCM_BOOL_F && y == SCM_BOOL_F) {
    gh_allow_ints();
    scm_misc_error(FUNC_NAME,"Either X or Y must be a number",SCM_EOL);
  }
#ifdef GJB_BE_ANAL_ABOUT_BOOLS
  /* FIXGJB: I took this code out so I can say:
     (move-to x y (get-window) 'animated 'move-pointer)
     Is there a better way? */
  if (!gh_boolean_p(animated_p)) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 4, animated_p);
  }
  if (!gh_boolean_p(move_pointer_too_p)) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 5, move_pointer_too_p);
  }
#endif
  if (animated_p == SCM_UNDEFINED) {
    /* FIXGJB: make an option for allowing the default to be animated */
    animated_p = SCM_BOOL_F;
  }
  if (move_pointer_too_p == SCM_UNDEFINED) {
    /* This is the only sensible default */
    move_pointer_too_p = SCM_BOOL_F;
  }
  psw = PSWFROMSCMWIN(win);
  w = psw->frame;
  if (psw->fIconified) {
    if (psw->icon_pixmap_w != None) {
      XUnmapWindow(dpy, psw->icon_w);
      w = psw->icon_pixmap_w;
    } else
      w = psw->icon_w;
  }
  fMovePointer = gh_scm2bool(move_pointer_too_p);
  fAnimated = gh_scm2bool(animated_p);
  if (fMovePointer || fAnimated ||
      x == SCM_BOOL_F || y == SCM_BOOL_F) {
    FXGetWindowTopLeft(w,&startX, &startY);
  }
  if (x == SCM_BOOL_F)
    destX = startX + WIN_VP_OFFSET_X(psw);
  else 
    destX = gh_scm2int(x);

  if (y == SCM_BOOL_F)
    destY = startY + WIN_VP_OFFSET_Y(psw);
  else 
    destY = gh_scm2int(y);

  if (fAnimated) {
    SCM animation_ms_delay = gh_lookup("animation-ms-delay");
    int cmsDelay = -1;
    if (animation_ms_delay != SCM_UNDEFINED &&
	gh_number_p(animation_ms_delay)) {
      cmsDelay = gh_scm2int(animation_ms_delay);
    }
    /* use viewport coordinates */
    AnimatedMoveWindow(w,startX,startY,
                       destX - WIN_VP_OFFSET_X(psw),
                       destY - WIN_VP_OFFSET_Y(psw),
		       fMovePointer,cmsDelay,NULL);
  } else if (fMovePointer) {
    int x, y;
    WXGetPointerWindowOffsets(Scr.Root, &x, &y);
    XWarpPointer(dpy, Scr.Root, Scr.Root, 0, 0, Scr.DisplayWidth,
		 Scr.DisplayHeight, x + destX - startX, y + destY - startY);
  }

  move_finalize_virt(w, psw, destX, destY);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



/* FIXMS: would animated resizes be a good idea? */
/* FIXGJB: either resize-to or resize-frame-to should be written
   in scheme using the other */
/* FIXGJB: See resize-window  -- this primitive should be renamed to that */

SCWM_PROC(resize_to, "resize-to", 2, 1, 0,
          (SCM w, SCM h, SCM win))
     /** Resize WIN's client area to a size of W by H in pixels. 
The size does not include the window decorations -- only the client
application size. WIN defaults to the window
context in the usual way if not specified.*/
#define FUNC_NAME s_resize_to
{
  int width, height;
  ScwmWindow *psw;

  SCM_REDEFER_INTS;
  VALIDATEN(win, 3, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);

  if (check_allowed_function(F_RESIZE, psw) == 0
      || SHADED_P(psw)) {
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }
  psw->fMaximized = False;

  /* can't resize icons */
  if (psw->fIconified) {
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }
  width = gh_scm2int(w);
  height = gh_scm2int(h);

  width += (2*psw->boundary_width);
  height += (psw->title_height + 2*psw->boundary_width);
  ConstrainSize(psw, 0, 0, &width, &height);
  ResizeTo(psw,width,height);

  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}
#undef FUNC_NAME


SCWM_PROC(resize_frame_to, "resize-frame-to", 2, 1, 0,
          (SCM w, SCM h, SCM win))
     /** Resize WIN to a size of W by H in pixels. 
The size includes the window decorations. WIN defaults to the window
context in the usual way if not specified.*/
#define FUNC_NAME s_resize_frame_to
{
  int width, height;
  ScwmWindow *psw;

  SCM_REDEFER_INTS;
  VALIDATEN(win, 3, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);

  if (check_allowed_function(F_RESIZE, psw) == 0
      || SHADED_P(psw)) {
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }
  psw->fMaximized = False;

  /* can't resize icons */
  if (psw->fIconified) {
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }
  width = gh_scm2int(w);
  height = gh_scm2int(h);

  ConstrainSize(psw, 0, 0, &width, &height);
  ResizeTo(psw,width,height);

  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}
#undef FUNC_NAME

SCWM_PROC (window_size_hints, "window-size-hints", 1, 0, 0,
           (SCM win))
     /** Return a list of the window size hints associated with WIN.
The list returned contains 4 cons pairs containing:
'((min-width . max-width) (min-height . max-height) 
(width-inc . height-inc) (base-width . baseheight)) */
#define FUNC_NAME s_window_size_hints
{
  int iarg = 1;
  SCM answer = SCM_EOL;
  ScwmWindow *psw = NULL;
  if (!WINDOWP(win)) {
    scm_wrong_type_arg(FUNC_NAME,iarg++,win);
  }
  psw = PSWFROMSCMWIN(win);

  answer = gh_cons(gh_cons(gh_int2scm(psw->hints.base_width),
                           gh_int2scm(psw->hints.base_height)),answer);

  answer = gh_cons(gh_cons(gh_int2scm(psw->hints.width_inc),
                           gh_int2scm(psw->hints.height_inc)),answer);

  answer = gh_cons(gh_cons(gh_int2scm(psw->hints.min_height),
                           gh_int2scm(psw->hints.max_height)),answer);

  answer = gh_cons(gh_cons(gh_int2scm(psw->hints.min_width),
                           gh_int2scm(psw->hints.max_width)),answer);
  
  return answer;
}
#undef FUNC_NAME


SCWM_PROC(refresh_window, "refresh-window", 0, 1, 0,
          (SCM win))
     /** Refresh the decorations on window WIN.
Refreshing ensuring that everything, including the decorations is up
to date. `refresh' does this in a more efficient way for all windows,
as well as the root. WIN defaults to the window context in the usual
way if not specified. */
#define FUNC_NAME s_refresh_window
{
  ScwmWindow *psw;

  SCM_REDEFER_INTS;
  VALIDATE(win, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);

  refresh_common(psw->fIconified ?
		 (psw->icon_w) : (psw->frame));

  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(move_window_to_desk, "move-window-to-desk", 1, 1, 0,
          (SCM desk, SCM win))
     /** Move WIN to DESK. DESK is an integer desk identifier. WIN
defaults to the window context in the usual way if not specified. */
#define FUNC_NAME s_move_window_to_desk
{
  ScwmWindow *psw;
  int val1;

  SCM_REDEFER_INTS;
  if (!gh_number_p(desk) && desk != SCM_BOOL_F) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 1, desk);
  }
  VALIDATEN(win, 2, FUNC_NAME);

  psw = PSWFROMSCMWIN(win);

  val1 = gh_scm2int(desk);

  /* Mapping window on its new Desk,
     unmapping it from the old Desk */
  /* Only change mapping for non-sticky windows */
  if (!(psw->fIconified && psw->fStickyIcon) &&
      !psw->fSticky && !psw->fIconUnmapped) {
    if (psw->Desk == Scr.CurrentDesk) {
      psw->Desk = val1;
      if (val1 != Scr.CurrentDesk) {
	UnmapScwmWindow(psw);
      }
    } else if (val1 == Scr.CurrentDesk) {
      psw->Desk = val1;
      /* If its an icon, auto-place it */
      if (psw->fIconified)
	AutoPlace(psw);
      MapIt(psw);
    } else {
      psw->Desk = val1;
    }
  }
  BroadcastConfig(M_CONFIGURE_WINDOW, psw);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;;
}
#undef FUNC_NAME


SCWM_PROC(window_position, "window-position", 0, 1, 0,
          (SCM win))
     /** Return the position of WIN in pixels.
The position is returned as a list of the x coordinate and the y
coordinate in pixels. If the window is sticky, the position will
always be in the 0,0 viewport. WIN defaults to the window context in the usual
way if not specified.  See also `window-viewport-position'. */
#define FUNC_NAME s_window_position
{
  ScwmWindow *psw;

  VALIDATE(win, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);

  return gh_list(SCM_MAKINUM(FRAME_X(psw)),
                 SCM_MAKINUM(FRAME_Y(psw)),
                 SCM_UNDEFINED);
}
#undef FUNC_NAME


SCWM_PROC(icon_position, "icon-position", 0, 1, 0,
          (SCM win))
     /** Return the position of the icon for WIN. 
The position is returned as a list of the x coordinate and the y
coordinate in pixels.  If the icon is sticky, the position will
always be in the 0,0 viewport. WIN defaults to the window context in the usual
way if not specified. */
#define FUNC_NAME s_icon_position
{
  ScwmWindow *psw;

  VALIDATE(win, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);

  return gh_list(SCM_MAKINUM(ICON_X(psw)),
                 SCM_MAKINUM(ICON_Y(psw)),
                 SCM_UNDEFINED);
}
#undef FUNC_NAME


SCWM_PROC(window_frame_size, "window-frame-size", 0, 1, 0,
          (SCM win))
/** Return the size of the frame of WIN.
The position is returned as a list of the width and the height in
pixels. WIN defaults to the window context in the usual way if not
specified. */
#define FUNC_NAME s_window_frame_size
{
  ScwmWindow *psw;

  VALIDATE(win, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);

  return gh_list(SCM_MAKINUM(FRAME_WIDTH(psw)),
                 SCM_MAKINUM(FRAME_HEIGHT(psw)),
                 SCM_UNDEFINED);
}
#undef FUNC_NAME

SCWM_PROC(window_size, "window-size", 0, 1, 0,
          (SCM win))
/** Return the size of the application window of WIN.

WIN defaults to the window context in the usual way if not specified.
The position is returned as a list of four numbers. The first two are
the width and the height in pixels, the third and fourth are the width
and height in resize units (e.g., characters for an xterm).  */
#define FUNC_NAME s_window_size
{
  ScwmWindow *psw;
  int cpixX;
  int cpixY;
  int width;
  int height;

  VALIDATE(win, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);

  cpixX = FRAME_WIDTH(psw) - psw->xboundary_width*2;
  cpixY = FRAME_HEIGHT(psw) - psw->title_height - psw->boundary_width*2;

  /* these are the size in resize units */
  /* see also the code in DisplaySize */
  width = cpixX;
  height = cpixY;

  width -= psw->hints.base_width;
  height -= psw->hints.base_height;
  width /= psw->hints.width_inc;
  height /= psw->hints.height_inc;

  return gh_list(gh_int2scm(cpixX),gh_int2scm(cpixY),
                 gh_int2scm(width),gh_int2scm(height),
                 SCM_UNDEFINED);
}
#undef FUNC_NAME


SCWM_PROC (window_title_size, "window-title-size", 0, 1, 0,
           (SCM win))
     /** Return a list with the width and height of WIN's titlebar. */
#define FUNC_NAME s_window_title_size
{
  ScwmWindow *psw;
  VALIDATE(win, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);
  return gh_list(gh_int2scm(psw->title_width),
                 gh_int2scm(psw->title_height),
                 SCM_UNDEFINED);
}
#undef FUNC_NAME


SCWM_PROC (window_frame_border_width, "window-frame-border-width", 0, 1, 0,
           (SCM win))
     /** Return the width of WIN's frame's border. */
#define FUNC_NAME s_window_frame_border_width
{
  ScwmWindow *psw;
  VALIDATE(win, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);
  return gh_int2scm(psw->boundary_width);
}
#undef FUNC_NAME


SCWM_PROC(window_id, "window-id", 0, 1, 0,
          (SCM win))
     /** Return the X window id for WIN.
This is the X id for the actual application window. WIN defaults to
the window context in the usual way if not specified. */
#define FUNC_NAME s_window_id
{
  if (win == sym_root_window) {
    return SCM_MAKINUM(Scr.Root);
  }
  VALIDATE(win, FUNC_NAME);
  return SCM_MAKINUM(PSWFROMSCMWIN(win)->w);
}
#undef FUNC_NAME


SCWM_PROC(window_frame_id, "window-frame-id", 0, 1, 0,
          (SCM win))
     /** Return the X window id for the outermost frame window of WIN.
WIN defaults to the window context in the usual way if not
specified. */
#define FUNC_NAME s_window_frame_id
{
  VALIDATE(win, FUNC_NAME);
  return SCM_MAKINUM(PSWFROMSCMWIN(win)->frame);
}
#undef FUNC_NAME


SCWM_PROC(id_to_window, "id->window", 1, 0, 0,
          (SCM window_id))
     /** Return the window object corresponding to an application WINDOW-ID.
WINDOW-ID should be the X id of the application window. If there is no
such window object, return #f. */
#define FUNC_NAME s_id_to_window
{
  ScwmWindow *psw = NULL;
  Window w;

  if (!gh_number_p(window_id)) {
    scm_wrong_type_arg(FUNC_NAME, 1, window_id);
  }		   

  w =(Window) gh_scm2int(window_id);
  psw = PswFromWindow(dpy, w);

  return ((psw&& psw->w==w) ? psw->schwin : SCM_BOOL_F);
}
#undef FUNC_NAME

SCWM_PROC(frame_id_to_window, "frame-id->window", 1, 0, 0,
          (SCM window_id))
     /** Return the window object corresponding to a frame WINDOW-ID.
WINDOW-ID should be the X id of a scwm frame window. If there is no
such window object, return #f. */
#define FUNC_NAME s_frame_id_to_window
{
  ScwmWindow *psw = NULL;
  Window w;

  if (!gh_number_p(window_id)) {
    scm_wrong_type_arg(FUNC_NAME, 1, window_id);
  }

  w =(Window) gh_scm2int(window_id);
  psw = PswFromWindow(dpy, w);

  return ((psw && psw->frame==w) ? psw->schwin : SCM_BOOL_F);
}
#undef FUNC_NAME



SCWM_PROC(window_desk, "window-desk", 0, 1, 0,
          (SCM win))
     /** Return the desk that WIN is currently on. WIN defaults to the
window context in the usual way if not specified. */
#define FUNC_NAME s_window_desk
{
  VALIDATE(win, FUNC_NAME);
  return SCM_MAKINUM(PSWFROMSCMWIN(win)->Desk);
}
#undef FUNC_NAME


SCWM_PROC(window_title, "window-title", 0, 1, 0,
          (SCM win))
     /** Return the window title of WIN, as requested by the application.
WIN defaults to the window context in the usual way if not specified. */
#define FUNC_NAME s_window_title
{
  VALIDATE(win, FUNC_NAME);
  return gh_str02scm(PSWFROMSCMWIN(win)->name);
}
#undef FUNC_NAME

SCWM_PROC(window_icon_title, "window-icon-title", 0, 1, 0,
          (SCM win))
     /** Return the icon window title of WIN.
This is the title as requested by the application . WIN defaults to
the window context in the usual way if not specified. */
#define FUNC_NAME s_window_icon_title
{
  VALIDATE(win, FUNC_NAME);
  return gh_str02scm(PSWFROMSCMWIN(win)->icon_name);
}
#undef FUNC_NAME

SCWM_PROC(window_class, "window-class", 0, 1, 0,
          (SCM win))
     /** Return the window resource class of WIN. 
WIN defaults to the window context in the usual way if not
specified. */
#define FUNC_NAME s_window_class
{
  VALIDATE(win, FUNC_NAME);
  return gh_str02scm(PSWFROMSCMWIN(win)->classhint.res_class);
}
#undef FUNC_NAME


SCWM_PROC(window_resource, "window-resource", 0, 1, 0,
          (SCM win))
     /** Return the window resource instance of WIN. WIN defaults to
the window context in the usual way if not specified. */
#define FUNC_NAME s_window_resource
{
  VALIDATE(win, FUNC_NAME);
  return gh_str02scm(PSWFROMSCMWIN(win)->classhint.res_name);
}
#undef FUNC_NAME


SCWM_PROC(list_all_windows, "list-all-windows", 0, 0, 0,
          ())
     /** Return a list of all of the top-level window objects. 

The list is in a semi-arbitrary order that is convenient for the sake
of circulation.*/
#define FUNC_NAME s_list_all_windows
{ 
  ScwmWindow *psw; SCM result = SCM_EOL;

  for (psw = Scr.ScwmRoot.next; NULL != psw; psw = psw->next) {
    result = gh_cons(psw->schwin, result);
  }

  return result;
}
#undef FUNC_NAME


SCWM_PROC (list_stacking_order, "list-stacking-order", 0, 0, 0,
           ())
     /** Return a list of all the top-level window objects, from top to bottom.
The order is the stacking order of the windows. The first element is
the topmost window, the last is the bottommost */
#define FUNC_NAME s_list_stacking_order
{
  SCM result = SCM_EOL;
  Window *rgw;
  int cw = 0;
  int iw = 0;
  
  if (!XQueryTree(dpy, Scr.Root, &JunkWindow, &JunkWindow, &rgw, &cw))
    return SCM_BOOL_F;

  for (; iw < cw; ++iw) {
    ScwmWindow *psw = PswFromWindow(dpy,rgw[iw]);
    if (psw && ((psw->fIconified && psw->icon_w == rgw[iw]) ||
		(!psw->fIconified && psw->frame == rgw[iw]))) {
      result = gh_cons(psw->schwin,result);
    }
  }

  if (rgw) XFree(rgw);
  return result;
}
#undef FUNC_NAME




SCWM_PROC(keep_on_top, "keep-on-top", 0, 1, 0,
          (SCM win))
     /** Ensure that WIN is kept on top of all other windows.
Obviously, other windows that are also on-top may obscure WIN.
WIN defaults to the window context in the usual way if not specified. */
#define FUNC_NAME s_keep_on_top
{
  ScwmWindow *psw;

  SCM_REDEFER_INTS;
  VALIDATE(win, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);
  psw->fOnTop = True;
  /* is this needed? */
  BroadcastConfig(M_CONFIGURE_WINDOW, psw);
  raise_window(win);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(un_keep_on_top, "un-keep-on-top", 0, 1, 0,
          (SCM win))
     /** Remove the on-top property from WIN, if it has it.
See `keep-on-top'. WIN defaults to the window context in the usual
way if not specified. */
#define FUNC_NAME s_un_keep_on_top
{
  ScwmWindow *psw;

  SCM_REDEFER_INTS;
  VALIDATE(win, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);
  psw->fOnTop = False;
  /* is this needed? */
  BroadcastConfig(M_CONFIGURE_WINDOW, psw);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(kept_on_top_p, "kept-on-top?", 0, 1, 0,
          (SCM win))
     /** Return #t if WIN is an on-top window, #f otherwise.
WIN defaults to the window context in the usual way if not
specified. */
#define FUNC_NAME s_kept_on_top_p
{
  VALIDATE(win, FUNC_NAME);
  return SCM_BOOL_FromBool(PSWFROMSCMWIN(win)->fOnTop);
}
#undef FUNC_NAME


/* maybe all of this can be replaced with set-title-height 
   (a per-window version) ? */


void set_window_internal_title_height(ScwmWindow *psw, int nh)
{
  int oldyadj, oldh;

  oldh = psw->title_height;
  if (oldh != nh) {
    oldyadj = GRAV_Y_ADJUSTMENT(psw);
    
    psw->title_height=nh;
    
    MoveResizeTo(psw,
		 FRAME_X(psw),
		 FRAME_Y(psw) + GRAV_Y_ADJUSTMENT(psw) - oldyadj,
		 FRAME_WIDTH(psw),
		 FRAME_HEIGHT(psw) + nh - oldh);
    BroadcastConfig(M_CONFIGURE_WINDOW, psw);
  }
}


SCWM_PROC(show_titlebar, "show-titlebar", 0, 1, 0,
          (SCM win))
     /** Cause WIN to be decorated with a titlebar. 
WIN defaults to the window context in the usual way if not
specified. */
#define FUNC_NAME s_show_titlebar
{
  ScwmWindow *psw;
  ScwmDecor *fl;

  VALIDATE(win, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);
  fl = psw->fl ? psw->fl : &Scr.DefaultDecor;


  if (!psw->fTitle) {
    psw->fTitle = True;
    set_window_internal_title_height(psw, fl->TitleHeight);
  }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(hide_titlebar, "hide-titlebar", 0, 1, 0,
          (SCM win))
     /** Cause WIN not to be decorated with a titlebar. 

WIN defaults to the window context in the usual way if not
specified. */
#define FUNC_NAME s_hide_titlebar
{
  ScwmWindow *psw;

  VALIDATE(win, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);

  if (psw->fTitle) {
    psw->fTitle = False;
    set_window_internal_title_height(psw, 0);
  }

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(titlebar_shown_p, "titlebar-shown?", 0, 1, 0,
          (SCM win))
     /** Return #t if WIN is decorated with a titlebar, #f otherwise.

WIN defaults to the window context in the usual way if not
specified. */
#define FUNC_NAME s_titlebar_shown_p
{
  VALIDATE(win, FUNC_NAME);
  return SCM_BOOL_FromBool(PSWFROMSCMWIN(win)->fTitle);
}
#undef FUNC_NAME


SCWM_PROC(normal_border, "normal-border", 0, 1, 0,
          (SCM win))
     /** Cause WIN to be decorated with a normal border. 

This means that there will be resize handles in the corners. WIN
defaults to the window context in the usual way if not specified. */
#define FUNC_NAME s_normal_border
{
  ScwmWindow *psw;
  ScwmDecor *fl;
  int i;

  SCM_REDEFER_INTS;
  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  VALIDATE(win, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);
  psw->fBorder = True;
  BroadcastConfig(M_CONFIGURE_WINDOW, psw);
  for (i = 0; i < 4; i++) {
    XMapWindow(dpy, psw->corners[i]);
    XMapWindow(dpy, psw->sides[i]);
  }

  SetBorderX(psw, (Scr.Hilite == psw), True, True, None, True);

  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(plain_border, "plain-border", 0, 1, 0,
          (SCM win))
     /** Cause WIN to be decorated with a plain border. 
This means that there will be no resize handles in the corners, and the
window . WIN defaults to the window context in the usual way if not
specified. */
#define FUNC_NAME s_plain_border
{
  ScwmWindow *psw;
  int i;

  SCM_REDEFER_INTS;

  VALIDATE(win, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);
  psw->fBorder = False;
  BroadcastConfig(M_CONFIGURE_WINDOW, psw);

  for (i = 0; i < 4; i++) {
    XUnmapWindow(dpy, psw->corners[i]);
    XUnmapWindow(dpy, psw->sides[i]);
  }

  SetBorderX(psw, (Scr.Hilite == psw), True, True, None, True);

  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(border_normal_p, "border-normal?", 0, 1, 0,
          (SCM win))
     /** Return #t if WIN has a normal border, #f otherwise.
WIN defaults to the window context in the usual way if not
specified. */
#define FUNC_NAME s_border_normal_p
{
  VALIDATE(win, FUNC_NAME);
  return SCM_BOOL_FromBool(PSWFROMSCMWIN(win)->fBorder);
}
#undef FUNC_NAME

SCWM_PROC(set_border_width_x, "set-border-width!", 1, 1, 0,
          (SCM width, SCM win))
     /** Set the border width of WIN's border to WIDTH pixels. 
WIN defaults to the window context in the usual way if not specified. */
#define FUNC_NAME s_set_border_width_x
{
  ScwmWindow *psw;
  ScwmDecor *fl;
  int cpix, oldw, oldxw;
  int oldxadj, oldyadj;

  SCM_REDEFER_INTS;
  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  if (!gh_number_p(width)) {
    scm_wrong_type_arg(FUNC_NAME, 1, width);
  }
  cpix = gh_scm2int(width);

  VALIDATEN(win, 2, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);

  oldw = psw->boundary_width;
  oldxw = psw->xboundary_width;

  oldxadj = GRAV_X_ADJUSTMENT(psw);
  oldyadj = GRAV_Y_ADJUSTMENT(psw);

#define NO_SIDE_DECORATIONS_P(psw) \
  SCM_NFALSEP( scm_object_property((psw)->schwin, gh_symbol2scm("no-side-decorations")))

  psw->boundary_width = cpix;
  if (!NO_SIDE_DECORATIONS_P(psw))
    psw->xboundary_width = psw->boundary_width;

  MoveResizeTo(psw, 
	       FRAME_X(psw) + GRAV_X_ADJUSTMENT(psw) - oldxadj,
	       FRAME_Y(psw) + GRAV_Y_ADJUSTMENT(psw) - oldyadj,
	       FRAME_WIDTH(psw) + 2 * (psw->xboundary_width - oldxw),
	       FRAME_HEIGHT(psw) + 2 * (psw->boundary_width - oldw));

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(stick_icon, "stick-icon", 0, 1, 0,
          (SCM win))
     /** Cause WIN's icon to become "sticky". See `stick'. WIN
defaults to the window context in the usual way if not specified. */
#define FUNC_NAME s_stick_icon
{
  ScwmWindow *psw;

  SCM_REDEFER_INTS;
  VALIDATE(win, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);
  psw->fStickyIcon = True;
  BroadcastConfig(M_CONFIGURE_WINDOW, psw);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(unstick_icon, "unstick-icon", 0, 1, 0,
          (SCM win))
     /** Cause WIN's icon to no longer by "sticky". See `stick-icon'
and `stick'. WIN defaults to the window context in the usual way if
not specified. */
#define FUNC_NAME s_unstick_icon
{
  ScwmWindow *psw;

  SCM_REDEFER_INTS;
  VALIDATE(win, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);
  psw->fStickyIcon = False;
  BroadcastConfig(M_CONFIGURE_WINDOW, psw);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(icon_sticky_p, "icon-sticky?", 0, 1, 0,
          (SCM win))
     /** Return #t if WIN is "sticky", #f otherwise.
See `stick-icon' and `stick'. WIN defaults to the window context in
the usual way if not specified. */
#define FUNC_NAME s_icon_sticky_p
{
  VALIDATE(win, FUNC_NAME);
  return SCM_BOOL_FromBool(PSWFROMSCMWIN(win)->fStickyIcon);
}
#undef FUNC_NAME


SCWM_PROC(set_icon_box_x, "set-icon-box!", 4, 1, 0,
          (SCM x, SCM y, SCM w, SCM h, SCM win))
     /** Set the icon box in which WIN's icon will be placed.
This set the box to the rectangle at coordinates X, Y with width W and
height H. WIN defaults to the window context in the usual way if not
specified. */
#define FUNC_NAME s_set_icon_box_x
{
  /* FIXMS - should probably move existing window icons */
  int cx, cy, cw, ch;
  ScwmWindow *psw;

  if (!gh_number_p(x)) {
    scm_wrong_type_arg(FUNC_NAME, 1, x);
  }
  if (!gh_number_p(y)) {
    scm_wrong_type_arg(FUNC_NAME, 2, y);
  }
  if (!gh_number_p(w)) {
    scm_wrong_type_arg(FUNC_NAME, 3, w);
  }
  if (!gh_number_p(h)) {
    scm_wrong_type_arg(FUNC_NAME, 4, h);
  }
  VALIDATEN(win, 5, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);
  cx = gh_scm2int(x);
  cy = gh_scm2int(y);
  cw = gh_scm2int(w);
  ch = gh_scm2int(h);
  psw->IconBox[0] = cx;
  psw->IconBox[1] = cy;
  psw->IconBox[2] = cx + cw;
  psw->IconBox[3] = cy + ch;
  return (SCM_BOOL_T);
}
#undef FUNC_NAME

/**CONCEPT: Focus Styles

   Scwm supports several focus styles, which are settable
per-window. A window with the 'click focus style is click-to-focus: it
requires that the user click on it before it will receive the input
focus, and will not lose it again until some other window gains
focus. The 'mouse focus style is mouse-focus in the traditional sense
- the window will gain and lose focus as the mouse enters and leaves
it. 

  'sloppy indicates the sloppy-focus style. This is like mouse-focus,
but the window will not lose the focus until another gains it. So if
you focus the window with the mouse and then let the pointer slide
into the root window or a window that has focus styles of 'click or
'none, the window will not lose the focus. This style of focus was
first introduced in fvwm.

  A focus style of 'none indicates that the window should never gain
the input focus, no matter what. This can be used for clocks,
mailboxes, and other desktop gadgets that never need keyboard input.
*/

SCWM_PROC(set_window_focus_x, "set-window-focus!", 1, 1, 0,
          (SCM sym, SCM win))
     /** Set the focus style of WIN to SYM. SYM may be 'click, 'mouse,
'sloppy or 'none. WIN defaults to the window context in the usual way
if not specified. */
#define FUNC_NAME s_set_window_focus_x
{
  ScwmWindow *psw;

  if (!gh_symbol_p(sym)) {
    scm_wrong_type_arg(FUNC_NAME, 1, sym);
  }
  VALIDATEN(win, 2, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);

  if (gh_eq_p(sym, sym_mouse)) {
    psw->fClickToFocus = False;
    psw->fSloppyFocus = False;
  } else if (gh_eq_p(sym, sym_click)) {
    psw->fClickToFocus = True;
    psw->fSloppyFocus = False;
  } else if (gh_eq_p(sym, sym_sloppy)) {
    psw->fClickToFocus = False;
    psw->fSloppyFocus = True;
  } else if (gh_eq_p(sym, sym_none)) {
    psw->fClickToFocus = True;
    psw->fSloppyFocus = True;
  } else {
    scwm_error(FUNC_NAME, "Window focus must be \'click, \'mouse, \'sloppy or \'none.");
  }
  return sym;
}
#undef FUNC_NAME


SCWM_PROC(get_window_colors, "get-window-colors", 0, 1, 0,
          (SCM win))
     /** Return a two-element list, "(fg bg)", the colors for WIN. */
#define FUNC_NAME s_get_window_colors
{
  ScwmWindow *psw;
  VALIDATEN(win, 1, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);

  return gh_list(psw->TextColor, psw->BackColor, SCM_UNDEFINED);
}
#undef FUNC_NAME


SCWM_PROC(get_window_highlight_colors, "get-window-highlight-colors", 0, 1, 0,
          (SCM win))
     /** Return a two-element list, "(fg bg)", the highlight colors for WIN. */
#define FUNC_NAME s_get_window_highlight_colors
{
  ScwmWindow *psw;
  VALIDATEN(win, 1, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);

  return gh_list(psw->HiTextColor, psw->HiBackColor, SCM_UNDEFINED);
}
#undef FUNC_NAME


SCWM_PROC(set_window_foreground_x, "set-window-foreground!", 1, 1, 0,
          (SCM fg, SCM win))
     /** Set the foreground color of WIN to FG. 
This color is used to draw the title text currently. In the future, it
may have other uses as well. WIN defaults to the window context in the
usual way if not specified. */
#define FUNC_NAME s_set_window_foreground_x
{
  ScwmWindow *psw;

  VALIDATE_COLOR(fg, FUNC_NAME, 1);

  VALIDATEN(win, 2, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);

  psw->TextColor = fg;
  SetBorderX(psw, (Scr.Hilite == psw), True, True, None, True);

  return SCM_UNSPECIFIED;  
}
#undef FUNC_NAME


SCWM_PROC(set_window_background_x, "set-window-background!", 1, 1, 0,
          (SCM bg, SCM win))
     /** Set the background color of WIN to BG. 
This color is used to draw most of the window decorations, along with
the relief colors generated from it, which are used to draw the
window's 3-D bevels.  WIN defaults to the window context in the usual
way if not specified. */
#define FUNC_NAME s_set_window_background_x
{
  ScwmDecor * fl;
  ScwmWindow *psw;

  VALIDATE_COLOR(bg, FUNC_NAME, 1);

  VALIDATEN(win, 2, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);
  fl = psw->fl ? psw->fl : &Scr.DefaultDecor;


  psw->BackColor = bg;
  psw->ShadowColor = adjust_brightness(psw->BackColor, fl->shadow_factor);
  psw->ReliefColor = adjust_brightness(psw->BackColor, fl->hilight_factor);

  SetBorderX(psw, (Scr.Hilite == psw), True, True, None, True);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(set_window_highlight_foreground_x, "set-window-highlight-foreground!", 1, 1, 0,
          (SCM fg, SCM win))
     /** Set the highlighted foreground color of WIN to FG.
This color is used to draw the title text when WIN has the focus.
In the future, it may have other uses
as well. WIN defaults to the window context in the usual way
if not specified. If FG is #f, then lets the decor highlight
foreground color be used (turns off a special highlight
color for WIN. */
#define FUNC_NAME s_set_window_highlight_foreground_x
{
  ScwmWindow *psw;

  if (fg != SCM_BOOL_F) {
    VALIDATE_COLOR(fg, FUNC_NAME, 1);
  }

  VALIDATEN(win, 2, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);

  psw->HiTextColor = fg;
  SetBorderX(psw, (Scr.Hilite == psw), True, True, None, True);

  return SCM_UNSPECIFIED;  
}
#undef FUNC_NAME


SCWM_PROC(set_window_highlight_background_x, "set-window-highlight-background!", 1, 1, 0,
          (SCM bg, SCM win))
     /** Set the highlighted background color of WIN to BG.
This color is used when WIN has the focus to draw most of the window
decorations, along with the relief colors generated from it, which are
used to draw the window's 3-D bevels.  WIN defaults to the window
context in the usual way if not specified. If BG is #f, then lets the decor highlight
background color be used (turns off a special highlight 
color for WIN.   */
#define FUNC_NAME s_set_window_highlight_background_x
{
  ScwmDecor * fl;
  ScwmWindow *psw;

  if (bg != SCM_BOOL_F)
    VALIDATE_COLOR(bg, FUNC_NAME, 1);

  VALIDATEN(win, 2, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);
  fl = psw->fl ? psw->fl : &Scr.DefaultDecor;


  psw->HiBackColor = bg;
#if 0 /* FIXGJB: these aren't used yet, and bg might be #f */
  psw->HiShadowColor = adjust_brightness(bg, fl->shadow_factor);
  psw->HiReliefColor = adjust_brightness(bg, fl->hilight_factor);
#endif

  SetBorderX(psw, (Scr.Hilite == psw), True, True, None, True);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(set_random_placement_x, "set-random-placement!", 1, 1, 0,
          (SCM flag, SCM win))
     /** Set the random-placement flag of WIN to boolean FLAG.
This flag only matters if the default placement procedure is
being used. WIN defaults to the window context in the usual way if not
specified. */
#define FUNC_NAME s_set_random_placement_x
{
  VALIDATEN(win, 2, FUNC_NAME);
  COPY_BOOL_OR_ERROR(PSWFROMSCMWIN(win)->fRandomPlace,flag,1,FUNC_NAME);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(set_smart_placement_x, "set-smart-placement!", 1, 1, 0,
          (SCM flag, SCM win))
     /** Set the smart-placement flag of WIN to boolean FLAG.
This flag only matters if the default placement procedure is
being used. WIN defaults to the window context in the usual way if not
specified. */
#define FUNC_NAME s_set_smart_placement_x
{
  VALIDATEN(win, 2, FUNC_NAME);
  COPY_BOOL_OR_ERROR(PSWFROMSCMWIN(win)->fSmartPlace,flag,1,FUNC_NAME);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(set_window_button_x, "set-window-button!", 2, 1, 0,
          (SCM n, SCM flag, SCM win))
     /** Set the visibility of button number N on window WIN.
If FLAG is #t, the button will be visible, otherwise it won't be
drawn.  WIN defaults to the window context in the usual way if not
specified. (Note: this code may be broken right now.) */
#define FUNC_NAME s_set_window_button_x
{
  VALIDATEN(win, 2, FUNC_NAME);
  if (flag == SCM_BOOL_T) {
    PSWFROMSCMWIN(win)->buttons &= ~(1 << (gh_scm2int(n) - 1));
  } else if (flag == SCM_BOOL_F) {
    PSWFROMSCMWIN(win)->buttons |= (1 << (gh_scm2int(n) - 1));
  } else {
    scm_wrong_type_arg(FUNC_NAME, 1, flag);
  }
  /* XXX - This won't really work for any case unless it is a hint.
     Handling of the number of buttons is kind of broken in
     general for now, but will be fixed. */

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(set_mwm_buttons_x, "set-mwm-buttons!", 1, 1, 0,
          (SCM flag, SCM win))
     /** Set the mwm-buttons flag of WIN to boolean FLAG.
The mwm-buttons flag controls whether any of this window's
flags obey their mwm-flags. See `set-button-mwm-flag!'. WIN defaults
to the window context in the usual way if not specified. */
#define FUNC_NAME s_set_mwm_buttons_x
{
  VALIDATEN(win, 2, FUNC_NAME);
  COPY_BOOL_OR_ERROR(PSWFROMSCMWIN(win)->fMWMButtons,flag,1,FUNC_NAME);
  /* FIXGJB: why here? SetBorder(psw,(Scr.Hilite==psw),True,True,None); */
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(set_mwm_border_x, "set-mwm-border!", 1, 1, 0,
          (SCM flag, SCM win))
     /** Set the mwm-border style flag of WIN to boolean FLAG.
The Mwm style has shallower bevels than the default scwm/fvwm2 style.
WIN defaults to the window context in the usual way if not
specified. */
#define FUNC_NAME s_set_mwm_border_x
{
  ScwmWindow *psw;
  VALIDATEN(win, 2, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);
  COPY_BOOL_OR_ERROR(psw->fMWMBorders,flag,1,FUNC_NAME);
  /* copied from SelectDecor */
  if (psw->fMWMBorders)
    psw->bw = 0;
  else if (psw->boundary_width <= 0) {
    psw->boundary_width = 0;
    psw->bw = 0;
  } else {
    psw->bw = BW;
    /*    psw->boundary_width -= 1; makes no sense --08/12/98 gjb */
  }
  SetupFrame(psw,FRAME_X_VP(psw),FRAME_Y_VP(psw),
             FRAME_WIDTH(psw),FRAME_HEIGHT(psw),
             WAS_MOVED, WAS_RESIZED);
  SetBorderX(psw, (Scr.Hilite == psw), True, True, None, True);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


void
force_icon_redraw (ScwmWindow *psw)
{
  if (None != psw->icon_w) {
    XDestroyWindow(dpy, psw->icon_w);
    psw->icon_w = None;
  }

  if (psw->fIconified) {
    Iconify(psw, 0, 0);
  }  
}


SCWM_PROC(set_icon_title_x, "set-icon-title!", 1, 1, 0,
          (SCM flag, SCM win))
     /** Set the visibility of WIN's icon title according to FLAG. WIN
defaults to the window context in the usual way if not specified. */
#define FUNC_NAME s_set_icon_title_x
{
  ScwmWindow *psw;
  /* Should changing the icon title string be allowed? */
  VALIDATEN(win, 2, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);
  COPY_INVERT_BOOL_OR_ERROR(psw->fNoIconTitle,flag,1,FUNC_NAME);
  force_icon_redraw (psw);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(set_force_icon_x, "set-force-icon!", 1, 1, 0,
          (SCM flag, SCM win))
     /** Set the window-manager-overriding property for WIN to boolean FLAG.
If #t, the icon specified for WIN by the user through scwm will override an
application-provided icon.  WIN defaults to the window context in the usual
way if not specified. */
#define FUNC_NAME s_set_force_icon_x
{
  ScwmWindow *psw;

  VALIDATEN(win, 2, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);

  if (flag== SCM_BOOL_F) {
    psw->fForceIcon=False; 
  } else if (flag== SCM_BOOL_T) {
    psw->fForceIcon=True; 
  } else {
    scm_wrong_type_arg(FUNC_NAME, 1, flag);
  }

  force_icon_redraw (psw);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(set_show_icon_x, "set-show-icon!", 1, 1, 0,
          (SCM flag, SCM win))
     /** Set whether or not the icon of WIN will be visible.  If FLAG
is #t, the icon will be displayed, if #f, it will not appear when the
window is iconified (it will still be in the window list, of course).
WIN defaults to the window context in the usual way if not
specified. */
#define FUNC_NAME s_set_show_icon_x
{ 
  ScwmWindow *psw;
  VALIDATEN(win, 2, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);
  COPY_INVERT_BOOL_OR_ERROR(psw->fSuppressIcon,flag,1,FUNC_NAME);
  force_icon_redraw (psw);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(set_icon_x, "set-icon!", 1, 1, 0,
          (SCM image, SCM win))
     /** Set the image to use for the icon of WIN to IMAGE. 
As usual, an image object or a filename string may be given. #f May
also be specified, indicating no icon image. WIN defaults to the window
context in the usual way if not specified. */
#define FUNC_NAME s_set_icon_x
{
  ScwmWindow *psw;
  char *icon_name;
  int length;

  VALIDATEN(win, 2, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);
  if (gh_string_p(image)) {
    psw->icon_req_image = make_image(image);
  } else if (IMAGE_P(image) || image == SCM_BOOL_F) {
    psw->icon_req_image = image;
  } else {
    scm_wrong_type_arg(FUNC_NAME, 1, image);
  }

  if (IMAGE_P(image)) {
    icon_name=gh_scm2newstr(IMAGE(psw->icon_req_image)->full_name, &length);
    /* FIXMS: This can't deal properly with app-specified icons! */
    BroadcastName(M_ICON_FILE, psw->w, psw->frame,
		  (unsigned long) psw, icon_name);   
    free (icon_name);
  }

  force_icon_redraw (psw);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(window_icon, "window-icon", 1, 0, 0,
          (SCM win))
     /** Get the icon image being used for WIN.
Returns #f if none is being used. WIN defaults to the window context
in the usual way if not specified. */
#define FUNC_NAME s_window_icon
{
  ScwmWindow *psw;

  VALIDATEN(win, 2, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);

  return psw->icon_req_image;
}
#undef FUNC_NAME


SCWM_PROC(set_mini_icon_x, "set-mini-icon!", 1, 1, 0,
          (SCM image, SCM win))
     /** Set the image to use for the mini-icon of WIN to IMAGE. As
usual, an image object or a filename string may be given. WIN defaults
to the window context in the usual way if not specified. */
#define FUNC_NAME s_set_mini_icon_x
{
  ScwmWindow *psw;

  VALIDATEN(win, 2, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);
  if (image == SCM_BOOL_F) {
    psw->mini_icon_image = SCM_BOOL_F;
  } else if (gh_string_p(image)) {
    psw->mini_icon_image = make_image(image);
  } else if (IMAGE_P(image)) {
    psw->mini_icon_image = image;
  } else {
    scm_wrong_type_arg(FUNC_NAME, 1, image);
  }

  /* FIXMS: this isn't right, fvwm2 has a separate SendMiniIcon which
     sends more info than that! */

  /* Broadcast the new mini-icon or something? */
  if (psw->mini_icon_image != SCM_BOOL_F) {
    BroadcastMiniIcon(M_MINI_ICON, psw);
  }

  SetBorderX(psw, Scr.Hilite == psw, True, psw->fMapped, None, True);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(window_mini_icon, "window-mini-icon", 1, 0, 0,
          (SCM win))
#define FUNC_NAME s_window_mini_icon
     /** Get the mini-icon image being used for WIN.
Returns #f if none is being used. WIN defaults to the window context
in the usual way if not specified. */
{
  ScwmWindow *psw;

  VALIDATEN(win, 2, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);

  return psw->mini_icon_image;
}
#undef FUNC_NAME


SCWM_PROC (window_shaped_p, "window-shaped?", 0, 1, 0,
           (SCM win))
     /** Return #t if WIN is a shaped window, #f otherwise. */
#define FUNC_NAME s_window_shaped_p
{
  ScwmWindow *psw;

  VALIDATEN(win, 2, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);

  return SCM_BOOL_FromBool(psw->fShaped);
}
#undef FUNC_NAME


SCWM_PROC (window_icon_shaped_p, "window-icon-shaped?", 0, 1, 0,
           (SCM win))
     /** Return #t if WIN has shaped icon, #f otherwise. */
#define FUNC_NAME s_window_icon_shaped_p
{
  ScwmWindow *psw;

  VALIDATEN(win, 2, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);

  return SCM_BOOL_FromBool(psw->fShapedIcon);
}
#undef FUNC_NAME



SCWM_PROC(set_hint_override_x, "set-hint-override!", 1, 1, 0,
          (SCM flag, SCM win))
     /** Set whether or not Mwm and Open Look function hints are used.
If FLAG is #t, the hints, which indicate what operations should be
allowed on a window, will be ignored for WIN.  If FLAG is #f, the hints will
be honoured. WIN defaults to the window context in the usual way if
not specified. */
#define FUNC_NAME s_set_hint_override_x
{
  VALIDATEN(win, 2, FUNC_NAME);
  COPY_BOOL_OR_ERROR(PSWFROMSCMWIN(win)->fHintOverride,flag,1,FUNC_NAME);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* FIXMS: seems silly in current framework. */

SCWM_PROC(set_decorate_transient_x, "set-decorate-transient!", 1, 1, 0,
          (SCM flag, SCM win))
     /** Set decoration of transients property on WIN.
If FLAG is #t, then if WIN is transient it will be fully
decorated. Transient windows that are not fully decorated will be
given only a border and no titlebar regardless of other settings. WIN
defaults to the window context in the usual way if not specified. */
#define FUNC_NAME s_set_decorate_transient_x
{
  VALIDATEN(win, 2, FUNC_NAME);
  COPY_BOOL_OR_ERROR(PSWFROMSCMWIN(win)->fDecorateTransient,flag,1,FUNC_NAME);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(set_mwm_decor_hint_x, "set-mwm-decor-hint!", 1, 1, 0,
          (SCM flag, SCM win))
     /** Set whether or not Motif decoration hints are used for WIN.
If FLAG is #t, the Mwm decor hint will be given for WIN.  WIN defaults
to the window context in the usual way if not specified. */
#define FUNC_NAME s_set_mwm_decor_hint_x
{
  VALIDATEN(win, 2, FUNC_NAME);
  COPY_BOOL_OR_ERROR(PSWFROMSCMWIN(win)->fMWMDecor,flag,1,FUNC_NAME);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(set_mwm_func_hint_x, "set-mwm-func-hint!", 1, 1, 0,
          (SCM flag, SCM win))
     /** Set whether or not Motif function hints are used for WIN.
If FLAG is #t, the Motif function hints are respected for WIN.
WIN defaults to the window context in the usual way if not
specified. */
#define FUNC_NAME s_set_mwm_func_hint_x
{
  VALIDATEN(win, 2, FUNC_NAME);
  COPY_BOOL_OR_ERROR(PSWFROMSCMWIN(win)->fMWMFunctions,flag,1,FUNC_NAME);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(set_PPosition_hint_x, "set-PPosition-hint!", 1, 1, 0,
          (SCM flag, SCM win))
     /** Set or reset the program-specified position hint for WIN.
If FLAG is #t, the hint will be set, otherwise reset.  This only
matters when using the default placement procedure. Some programs
allegedly set this hint to a useless value like (0,0) always, so
ignoring it is recommended. WIN defaults to the window context in the
usual way if not specified. */
#define FUNC_NAME s_set_PPosition_hint_x
{
  VALIDATEN(win, 2, FUNC_NAME);
  COPY_INVERT_BOOL_OR_ERROR(PSWFROMSCMWIN(win)->fNoPPosition,flag,1,FUNC_NAME);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(set_OL_decor_hint_x, "set-OL-decor-hint!", 1, 1, 0,
          (SCM flag, SCM win))
     /** Determine whether or not to respect Open Look decoration hints.
If FLAG is #t, the decoration hints will be respected for WIN. WIN
defaults to the window context in the usual way if not specified. */
#define FUNC_NAME s_set_OL_decor_hint_x
{
  VALIDATEN(win, 2, FUNC_NAME);
  COPY_BOOL_OR_ERROR(PSWFROMSCMWIN(win)->fOLDecorHint,flag,1,FUNC_NAME);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(set_start_on_desk_x, "set-start-on-desk!", 1, 1, 0,
          (SCM desk, SCM win))
     /** Make WIN start on DESK when first mapped. WIN defaults to the
window context in the usual way if not specified. */
#define FUNC_NAME s_set_start_on_desk_x
{
  ScwmWindow *psw;

  VALIDATEN(win, 2, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);
  if (desk == SCM_BOOL_F) {
    psw->fStartsOnDesk = False;
  } else if (gh_number_p(desk)) {
    DBUG((DBG,__FUNCTION__,"setting fStartsOnDesk"));
    psw->fStartsOnDesk = True;
    psw->StartDesk = gh_scm2int(desk);
  } else {
    scm_wrong_type_arg(FUNC_NAME, 1, desk);
  }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(set_skip_mapping_x, "set-skip-mapping!", 1, 1, 0,
          (SCM flag, SCM win))
     /** Set or reset whether scwm should not change desktops on a map.
This only affect the behaviour upon initial mapping of WIN. If FLAG is
#t, the virtual desktop will not be changed when WIN is mapped.  WIN
defaults to the window context in the usual way if not specified. */
#define FUNC_NAME s_set_skip_mapping_x
{
  VALIDATEN(win, 2, FUNC_NAME);
  COPY_BOOL_OR_ERROR(PSWFROMSCMWIN(win)->fShowOnMap,flag,1,FUNC_NAME);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(set_lenience_x, "set-lenience!", 1, 1, 0,
          (SCM flag, SCM win))
     /** Set or reset the input focus lenience flag.
Determine whether or not to try to give WIN the input focus
when asked, even if the window claims according to hints that it
cannot receive the input focus, according to the boolean value
FLAG. WIN defaults to the window context in the usual way if not
specified. */
#define FUNC_NAME s_set_lenience_x
{
  VALIDATEN(win, 2, FUNC_NAME);
  COPY_BOOL_OR_ERROR(PSWFROMSCMWIN(win)->fLenience,flag,1,FUNC_NAME);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(set_window_property_x, "set-window-property!", 3, 0, 0,
          (SCM win, SCM prop, SCM val))
     /** Set window property PROP of WIN to VAL.
PROP should be a symbol. VAL may be any Scheme object. This name/value
pair will be associated with the window, and may be retrieved with
`window-property'. Passing #f as the value will delete the property
instead. Soon, some properties will have magical meanings, altering
particular fields in the window structure. Also, a
window-property-change-hook mechanism will soon be implemented for
notification of all window property changes. This is not yet done. The
window property primitives should be considered in flux. */
#define FUNC_NAME s_set_window_property_x
{
  ScwmWindow *psw = NULL;
  if (!WINDOWP(win) || !VALIDWINP(win)) {
    scm_wrong_type_arg (FUNC_NAME, 1, win);
  }

  if (!gh_symbol_p(prop)) {
    scm_wrong_type_arg (FUNC_NAME, 2, prop);
  }

  psw = PSWFROMSCMWIN(win);
  if (val==SCM_BOOL_F) {
    scm_hashq_remove_x(psw->other_properties, prop);
  } else {
    scm_hashq_set_x(psw->other_properties, prop, val);
  }

  /* FIXMS: notification needs to go here. */

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



SCWM_PROC(window_property, "window-property", 2, 0, 0,
          (SCM win, SCM prop))
     /** Retrieve window property PROP of WIN.

PROP should be a symbol. #f will be returned if the property does not
exist (wether set by `set-window-property!' or otherwise). Soon, some
properties will have magical meanings, accessing particular fields in
the window structure. Also, a window-property-change-hook mechanism
will soon be implemented for notification of all window property
changes. This is not yet done. The window property primitives should
be considered in flux. */
#define FUNC_NAME s_window_property
{
  if (!WINDOWP(win) || !VALIDWINP(win)) {
    scm_wrong_type_arg (FUNC_NAME, 1, win);
  }

  if (!gh_symbol_p(prop)) {
    scm_wrong_type_arg (FUNC_NAME, 2, prop);
  }

  return scm_hashq_ref(PSWFROMSCMWIN(win)->other_properties, prop, SCM_BOOL_F);
}
#undef FUNC_NAME



MAKE_SMOBFUNS(window);


SCM 
ensure_valid(SCM win, int n, char *subr, SCM kill_p, SCM release_p)
{
  if (UNSET_SCM(win)) {
    win = get_window(kill_p, SCM_BOOL_T, release_p);
    if (win == SCM_BOOL_F || win == SCM_UNDEFINED) {
      return SCM_BOOL_F;
    }
  }
  if (!WINDOWP(win)) {
    gh_allow_ints();
    scm_wrong_type_arg(subr, n, win);
  }
  if (!VALIDWINP(win)) {
    gh_allow_ints();
    scwm_error(subr, "Window no longer valid.");
    /* maybe should just return SCM_BOOL_F; */
  }
  return (win);
}


void 
init_window()
{
  REGISTER_SCWMSMOBFUNS(window);

  SCWM_HOOK(invalid_interaction_hook,"invalid-interaction-hook");
  /** This hook is invoked with no arguments when the user hits an invalid
key or performs an invalid mouse action during an interactive
operation like `interactive-resize' or `interactive-move'. `beep' is
one example of a procedure to use here. */

  SCWM_HOOK(cannot_grab_hook,"cannot-grab-hook");
  /** This hook is invoked with no arguments whenever scwm cannot
successfully grab the X server. `beep' is one example of a procedure
to use here.  */

#ifndef SCM_MAGIC_SNARFER
#include "window.x"
#endif
}



/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
