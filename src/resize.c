/* $Id$
 * Copyright (C) 1998-1999, Maciej Stachowiak and Greg J. Badros
 *
 * This module is derived from code by Rob Nation 
 * Copyright 1993, Robert Nation
 *     You may use this code for any purpose, as long as the original
 *     copyright remains in the source code and all documentation
 */

/*
 * some window resizing code borrowed from the "wm" window manager
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <X11/keysym.h>

#include "resize.h"

#include "scwm.h"
#include "screen.h"
#include "borders.h"
#include "font.h"
#include "decorations.h"
#include "colormaps.h"
#include "callbacks.h"
#include "Grab.h"
#include "focus.h"
#include "move.h"
#include "virtual.h"
#include "events.h"
#include "xmisc.h"
#include "util.h"
#include "cursor.h"

SCWM_HOOK(interactive_resize_start_hook,"interactive-resize-start-hook",3,
"This hook is invoked at the start of an interactive resize.
It is called with three arguments: WINDOW, XMOTION, YMOTION.
XMOTION and YMOTION are -1, 0, or 1, indicating motion in that dimension
can happen on the right/bottom side, not at all, or the top/left side,
respectively.");

SCWM_HOOK(interactive_resize_new_size_hook,"interactive-resize-new-size-hook",7,
"This hook is invoked during an interactive resize.  
It is called with seven arguments, WINDOW, X-POSITION, Y-POSITION,
NEW-WIDTH-PIXELS, NEW-HEIGHT-PIXELS, NEW-WIDTH-UNITS, and
NEW-HEIGHT-UNITS whenever the window is changed to a new size.  The
first five arguments refer to the size and position of the frame
window (not the client window). The -UNITS arguments refer to the size
of the client window and are in client units (e.g., characters for
Emacsen and XTerms). ");

SCWM_HOOK(interactive_resize_finish_hook,"interactive-resize-finish-hook",1,
"This hook is invoked at the end of an interactive resize.
It is called with one argument, WINDOW.");

extern SCM cannot_grab_hook;


static __inline__
int
makemult(int a, int b)
{
  return a - (a % b);
}

void
ComputeDeltaForResize(ScwmWindow *psw, int *pdx, int *pdy, int width, int height)
{
  int grav_x = psw->grav.x;
  int grav_y = psw->grav.y;
  int dx = width - FRAME_WIDTH(psw);   /* wider => positive */
  int dy = height - FRAME_HEIGHT(psw); /* taller => positive */
  *pdx = dx * (double) grav_x/2.0;
  *pdy = dy * (double) grav_y/2.0;
}

/* *px and *py and in/out parameters
   Correct the position of psw for being resized to width/height
   while honouring its gravity setting */
void
ComputePositionForResize(ScwmWindow *psw, int *px, int *py, int width, int height)
{
  int dx = 0;
  int dy = 0;
  ComputeDeltaForResize(psw,&dx,&dy,width,height);
  *px -= dx;
  *py -= dy;
}


/*
 * ConstrainSize - adjust the given width and height to account for the
 *              constraints imposed by size hints
 *
 *      The general algorithm, especially the aspect ratio stuff, is
 *      borrowed from uwm's CheckConsistency routine.
 */
void 
ConstrainSize(ScwmWindow *psw, int xmotion, int ymotion, 
              /* input and output to/from: */ int *widthp, int *heightp)
{
#define FUNC_NAME "ConstrainSize"
  int minWidth, minHeight, maxWidth, maxHeight, xinc, yinc, delta;
  int baseWidth, baseHeight;
  int dwidth = *widthp, dheight = *heightp;
  DBUG((DBG,FUNC_NAME,"initially %d x %d",dwidth,dheight));

  dwidth -= 2 * psw->xboundary_width;
  dheight -= (psw->title_height + 2 * psw->boundary_width);

  minWidth = MinFrameWidth(psw);
  minHeight = MinFrameHeight(psw);

  baseWidth = psw->hints.base_width;
  baseHeight = psw->hints.base_height;

  maxWidth = MaxFrameWidth(psw);
  maxHeight = MaxFrameHeight(psw);

  /* Could bound the max dimensions by Scr.VxMax + Scr.DisplayWidth
     and Scr.VyMax + Scr.DisplayHeight */

  xinc = psw->hints.width_inc;
  yinc = psw->hints.height_inc;

  /*
   * First, clamp to min and max values
   */
  if (dwidth < minWidth)
    dwidth = minWidth;
  if (dheight < minHeight)
    dheight = minHeight;

  if (dwidth > maxWidth)
    dwidth = maxWidth;
  if (dheight > maxHeight)
    dheight = maxHeight;


  /*
   * Second, fit to base + N * inc
   */
  if (xinc != 0) 
    dwidth = ((dwidth - baseWidth) / xinc * xinc) + baseWidth;
  else
    scwm_msg(WARN,FUNC_NAME,"xinc == 0");

  if (yinc != 0)
    dheight = ((dheight - baseHeight) / yinc * yinc) + baseHeight;
  else
    scwm_msg(WARN,FUNC_NAME,"yinc == 0");


  /*
   * Third, adjust for aspect ratio
   */
#define maxAspectX psw->hints.max_aspect.x
#define maxAspectY psw->hints.max_aspect.y
#define minAspectX psw->hints.min_aspect.x
#define minAspectY psw->hints.min_aspect.y
  /*
   * The math looks like this:
   *
   * minAspectX    dwidth     maxAspectX
   * ---------- <= ------- <= ----------
   * minAspectY    dheight    maxAspectY
   *
   * If that is multiplied out, then the width and height are
   * invalid in the following situations:
   *
   * minAspectX * dheight > minAspectY * dwidth
   * maxAspectX * dheight < maxAspectY * dwidth
   * 
   */

  if (psw->hints.flags & PAspect &&
      minAspectX != 0 && minAspectY != 0 && maxAspectX != 0 && maxAspectY != 0) {
    if ((minAspectX * dheight > minAspectY * dwidth) && (xmotion == 0)) {
      /* Change width to match */
      delta = makemult(minAspectX * dheight / minAspectY - dwidth,
		       xinc);
      if (dwidth + delta <= maxWidth)
	dwidth += delta;
    }
    if (minAspectX * dheight > minAspectY * dwidth) {
      delta = makemult(dheight - dwidth * minAspectY / minAspectX,
		       yinc);
      if (dheight - delta >= minHeight)
	dheight -= delta;
      else {
	delta = makemult(minAspectX * dheight / minAspectY - dwidth,
			 xinc);
	if (dwidth + delta <= maxWidth)
	  dwidth += delta;
      }
    }
    if ((maxAspectX * dheight < maxAspectY * dwidth) && (ymotion == 0)) {
      delta = makemult(dwidth * maxAspectY / maxAspectX - dheight,
		       yinc);
      if (dheight + delta <= maxHeight)
	dheight += delta;
    }
    if ((maxAspectX * dheight < maxAspectY * dwidth)) {
      delta = makemult(dwidth - maxAspectX * dheight / maxAspectY,
		       xinc);
      if (dwidth - delta >= minWidth)
	dwidth -= delta;
      else {
	delta = makemult(dwidth * maxAspectY / maxAspectX - dheight,
			 yinc);
	if (dheight + delta <= maxHeight)
	  dheight += delta;
      }
    }
  }
  /*
   * Fourth, account for border width and title height
   */
  *widthp = dwidth + 2 * psw->xboundary_width;
  *heightp = dheight + psw->title_height + 2 * psw->boundary_width;

  DBUG((DBG,FUNC_NAME,"constrained to %d x %d",*widthp,*heightp));
  return;
}
#undef FUNC_NAME

void
ComputeNewGeometryOnResize(ScwmWindow *psw, 
                           int x_orig, int y_orig,
                           int w_orig, int h_orig,
                           int x_root, int y_root,
                           int *psgnXmotion, int *psgnYmotion,
                           int *pxReturn, int *pyReturn,
                           int *pwReturn, int *phReturn)
{
  Bool fChangedX = False;
  Bool fChangedY = False;
  int dragx = x_root, dragy = y_root;
  int dragWidth, dragHeight;

  if ((y_root <= y_orig) ||
      ((*psgnYmotion == 1) && (y_root < y_orig + h_orig - 1))) {
    dragy = y_root;
    dragHeight = y_orig + h_orig - y_root;
    *psgnYmotion = 1;
    fChangedY = True;
  } else if ((y_root >= y_orig + h_orig - 1) ||
	     ((*psgnYmotion == -1) && (y_root > y_orig))) {
    dragy = y_orig;
    dragHeight = 1 + y_root - dragy;
    *psgnYmotion = -1;
    fChangedY = True;
  }
  if ((x_root <= x_orig) ||
      ((*psgnXmotion == 1) && (x_root < x_orig + w_orig - 1))) {
    dragx = x_root;
    dragWidth = x_orig + w_orig - x_root;
    *psgnXmotion = 1;
    fChangedX = True;
  } else if ((x_root >= x_orig + w_orig - 1) ||
      ((*psgnXmotion == -1) && (x_root > x_orig))) {
    dragx = x_orig;
    dragWidth = 1 + x_root - x_orig;
    *psgnXmotion = -1;
    fChangedX = True;
  }
  if (fChangedX || fChangedY) {
    ConstrainSize(psw, *psgnXmotion, *psgnYmotion, &dragWidth, &dragHeight);
    if (*psgnXmotion == 1)
      dragx = x_orig + w_orig - dragWidth;
    if (*psgnYmotion == 1)
      dragy = y_orig + h_orig - dragHeight;
  }
  if (fChangedX) {
    *pxReturn = dragx;
    *pwReturn = dragWidth;
  }
  if (fChangedY) {
    *pyReturn = dragy;
    *phReturn = dragHeight;
  }
}


static void
InitializeOutlineRects(XRectangle rects[], int lastx, int lasty, int lastWidth, int lastHeight)
{
  rects[0].x = lastx;
  rects[0].y = lasty;
  rects[0].width = lastWidth;
  rects[0].height = lastHeight;
  rects[1].x = lastx + 1;
  rects[1].y = lasty + 1;
  rects[1].width = lastWidth - 2;
  rects[1].height = lastHeight - 2;
  rects[2].x = lastx + 2;
  rects[2].y = lasty + 2;
  rects[2].width = lastWidth - 4;
  rects[2].height = lastHeight - 4;
  rects[3].x = lastx + 3;
  rects[3].y = lasty + 3 + (lastHeight - 6) / 3;
  rects[3].width = lastWidth - 6;
  rects[3].height = (lastHeight - 6) / 3;
  rects[4].x = lastx + 3 + (lastWidth - 6) / 3;
  rects[4].y = lasty + 3;
  rects[4].width = (lastWidth - 6) / 3;
  rects[4].height = (lastHeight - 6);
}

static GC DrawRubberBandGC;

/* MS:FIXME:: Can't easily be a color w/o overlay planes-- needs to be really 
   fast to erase */
SCWM_PROC(set_rubber_band_mask_x, "set-rubber-band-mask!", 1, 0, 0,
          (SCM value),
"Set the rubber band mask used when dragging or resizing.
VALUE is XORed with the background when dragging non-opaque move or
resize frames. VALUE should be an integer.")
#define FUNC_NAME s_set_rubber_band_mask_x
{
  XGCValues gcv;
  unsigned long gcm;
  int v;

  VALIDATE_ARG_INT_MIN_COPY(1,value,0,v);
  gcm = GCFunction | GCLineWidth | GCForeground | GCSubwindowMode;
  gcv.function = GXxor;
  gcv.line_width = 0;
  gcv.foreground = v;
  gcv.subwindow_mode = IncludeInferiors;
  if (NULL != DrawRubberBandGC) {
    XFreeGC(dpy, DrawRubberBandGC);
  }
  DrawRubberBandGC = XCreateGC(dpy, Scr.Root, gcm, &gcv);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



/*
 * RedrawOutlineAtNewPosition
 *
 *  Inputs:
 *	x	    - upper left x coordinate
 *	y	    - upper left y coordinate
 * (x,y are viewport positions)
 *	width	    - the width of the rectangle
 *	height	    - the height of the rectangle
 */
void 
RedrawOutlineAtNewPosition(int x, int y, int width, int height)
{
  /* GJB:FIXME:: Ugh. no statics! */
  static int lastx = 0;
  static int lasty = 0;
  static int lastWidth = 0;
  static int lastHeight = 0;
  XRectangle rects[5];

  if (x == lastx && y == lasty && width == lastWidth && height == lastHeight)
    return;

  /* undraw the old one, if any */
  if (lastWidth || lastHeight) {
    InitializeOutlineRects(rects,lastx,lasty,lastWidth,lastHeight);
    XDrawRectangles(dpy, Scr.Root, DrawRubberBandGC, rects, 5);
  }

  /* save the new position in the static variables */
  lastx = x;
  lasty = y;
  lastWidth = width;
  lastHeight = height;

  /* draw the new one, if any */
  if (lastWidth || lastHeight) {
    InitializeOutlineRects(rects,lastx,lasty,lastWidth,lastHeight);
    XDrawRectangles(dpy, Scr.Root, DrawRubberBandGC, rects, 5);
  }
}


/* GJB:FIXME:: InteractiveResize uses the global var PressedW 
   and the global Event to figure out how the resize should work. 
   This is bad! */
Bool
InteractiveResize(ScwmWindow *psw, Bool fOpaque, int *pwidthReturn, int *pheightReturn)
{
  extern Window PressedW;       /* GJB:FIXME:: ugly! */
  extern XEvent Event;		/* GJB:FIXME:: this too! */

  int dragx;			/* all these variables are used */
  int dragy;			/* in resize operations */
  int dragWidth;
  int dragHeight;

  int origx;
  int origy;
  int origWidth;
  int origHeight;
  int yoffset = 0;

  int x_win_offset = 0;
  int y_win_offset = 0;

  int ymotion = 0, xmotion = 0;
  Bool finished = False, done = False;
  int x, y, delta_x, delta_y;

  /* save state of edge wrap */
  Bool fEdgeWrapX = Scr.fEdgeWrapX;
  Bool fEdgeWrapY = Scr.fEdgeWrapY;

  CassowaryModifyOpaqueFlag(&fOpaque);

  InstallRootColormap();
  if (!GrabEm(XCursorByNumber(XC_fleur))) {
    call0_hooks(cannot_grab_hook);
    return False;
  }
  if (!fOpaque) {
    XGrabServer_withSemaphore(dpy);
  }

  /* handle problems with edge-wrapping while resizing */
  Scr.fEdgeWrapX = Scr.fEdgeWrapY = False;

  XGetGeometry(dpy, psw->frame, &JunkRoot, &dragx, &dragy, 
               (unsigned int *) &dragWidth, (unsigned int *) &dragHeight,
               &JunkBW, &JunkDepth);

  origx = dragx;
  origy = dragy;
  origWidth = dragWidth;
  origHeight = dragHeight;
  ymotion = xmotion = 0;

  if ((PressedW != Scr.Root) && (PressedW != None)) {
    if (PressedW == psw->sides[0])	/* top */
      ymotion = 1;
    if (PressedW == psw->sides[1])	/* right */
      xmotion = -1;
    if (PressedW == psw->sides[2])	/* bottom */
      ymotion = -1;
    if (PressedW == psw->sides[3])	/* left */
      xmotion = 1;
    if (PressedW == psw->corners[0]) {	/* upper-left */
      ymotion = 1;
      xmotion = 1;
    }
    if (PressedW == psw->corners[1]) {	/* upper-right */
      if (psw->fSquashedTitlebar)
        yoffset = psw->title_height;
      xmotion = -1;
      ymotion = 1;
    }
    if (PressedW == psw->corners[2]) {	/* lower left */
      ymotion = -1;
      xmotion = 1;
    }
    if (PressedW == psw->corners[3]) {	/* lower right */
      ymotion = -1;
      xmotion = -1;
    }
    if (PressedW == psw->Parent) { /* the window itself */
      if ((Event.type == ButtonPress) ||
	  (Event.type == ButtonRelease))
      {
	/* Figure out which nonant the mouse was clicked in and use
	 * this to determine which direction to move. */

	if (Event.xbutton.x < (origWidth / 3))
	{
	  /* anchor against left */
	  x_win_offset = Event.xbutton.x;
	  xmotion = 1;
	}
	else if (Event.xbutton.x > ((2 * origWidth) / 3))
	{
	  /* anchor against right */
	  x_win_offset = Event.xbutton.x - origWidth;
	  xmotion = -1;
	}
	if (Event.xbutton.y < (origHeight / 3))
	{
	  /* anchor against top */
	  y_win_offset = Event.xbutton.y;
	  ymotion = 1;
	}
	else if (Event.xbutton.y > ((2 * origHeight) / 3))
	{
	  /* anchor against right */
	  y_win_offset = Event.xbutton.y - origHeight;
	  ymotion = -1;
	}
      }
    }
  }

  { /* scope */
    int x_units, y_units;
    window_pixel_size_to_client_units(psw,
                                      dragWidth - DecorationWidth(psw),
                                      dragHeight - DecorationHeight(psw),
                                      &x_units, &y_units);

    call3_hooks(interactive_resize_start_hook, psw->schwin,
                gh_int2scm(xmotion), gh_int2scm(ymotion));

    /* same hook is called identically on each iteration; see below */
    call7_hooks(interactive_resize_new_size_hook, psw->schwin,
                gh_int2scm(dragx), gh_int2scm(dragy),
                gh_int2scm(dragWidth), gh_int2scm(dragHeight),
                gh_int2scm(x_units), gh_int2scm(y_units));
  }
  
  /* pop up a resize dimensions window */

  CassowaryEditSize(psw);

  /* draw the rubber-band window */
  if (!fOpaque) {
    RedrawOutlineAtNewPosition(dragx - psw->bw, dragy - psw->bw,
                               dragWidth + 2 * psw->bw,
                               dragHeight + 2 * psw->bw);
  }

  /* loop to resize */
  while (!finished) {
    XEvent ResizeEvent;
    while (XCheckMaskEvent(dpy,
                           ButtonPressMask | ButtonReleaseMask | KeyPressMask |
                           ButtonMotionMask | PointerMotionMask | ExposureMask | VisibilityChangeMask,
                           &ResizeEvent) == False) {
#ifndef NOT_MORE_RESPONSIVE
      NoEventsScwmUpdate(False);
#else
      ms_sleep(10);
#endif
    }
    /* fallen through, so we got an event we're interested in */
    StashEventTime(&ResizeEvent);

    if (ResizeEvent.type == MotionNotify)
      /* discard any extra motion events before a release */
      while (XCheckMaskEvent(dpy, ButtonMotionMask | ButtonReleaseMask |
			     PointerMotionMask, &ResizeEvent)) {
	StashEventTime(&ResizeEvent);
	if (ResizeEvent.type == ButtonRelease)
	  break;
      }
    done = False;
    /* Handle a limited number of key press events to allow mouseless
     * operation */
    if (ResizeEvent.type == KeyPress)
      Keyboard_shortcuts(&ResizeEvent, ButtonRelease, psw, True);
    switch (ResizeEvent.type) {
    case ButtonPress:
      XAllowEvents(dpy, ReplayPointer, CurrentTime);
    case KeyPress:
      /* simple code to bag out of move - CKH */
      if (XLookupKeysym(&(ResizeEvent.xkey), 0) == XK_Escape) {
	dragx = origx;
	dragy = origy;
	dragWidth = origWidth;
	dragHeight = origHeight;
	finished = True;
      }
      done = True;
      break;

    case ButtonRelease:
      finished = True;
      done = True;
      break;

    case MotionNotify:
      x = ResizeEvent.xmotion.x_root - x_win_offset;
      y = ResizeEvent.xmotion.y_root - yoffset - y_win_offset;
      /* resize before paging request to prevent resize from lagging mouse - mab */
      ComputeNewGeometryOnResize(psw,origx,origy,origWidth,origHeight,
                                 x, y, &xmotion, &ymotion, 
                                 &dragx, &dragy, &dragWidth, &dragHeight);

      if (SuggestSizeWindowTo(psw,
                              WIN_VP_OFFSET_X(psw) + dragx, 
                              WIN_VP_OFFSET_Y(psw) + dragy,
                              dragWidth,dragHeight, fOpaque)) {
        int x_units, y_units;
        window_pixel_size_to_client_units(psw, 
                                          dragWidth - DecorationWidth(psw),
                                          dragHeight - DecorationHeight(psw),
                                          &x_units, &y_units);
        /* two calls to this hook exist.
           see the other above */
        call7_hooks(interactive_resize_new_size_hook, psw->schwin,
                    gh_int2scm(dragx), gh_int2scm(dragy),
                    gh_int2scm(dragWidth), gh_int2scm(dragHeight),
                    gh_int2scm(x_units), gh_int2scm(y_units));
      }
      

      GenerateEdgeEvents();
      if (FNeedsPaging(Scr.EdgeScrollX, Scr.EdgeScrollY, x, y)) {
        /* need to move the viewport */
        HandlePaging(Scr.EdgeScrollX, Scr.EdgeScrollY, &x, &y,
                     &delta_x, &delta_y, False);
        /* redraw outline if we paged - mab */
        if ((delta_x != 0) || (delta_y != 0)) {
          origx -= delta_x;
          origy -= delta_y;
          dragx -= delta_x;
          dragy -= delta_y;
          
          if (!fOpaque) {
            RedrawOutlineAtNewPosition(dragx - psw->bw, dragy - psw->bw,
                                       dragWidth + 2 * psw->bw, 
                                       dragHeight + 2 * psw->bw);
          }
        }
      }
      done = True;
    default:
      break;
    }
    if (!done) {
      extern XEvent Event;
      Event = ResizeEvent;
      DispatchEvent();
    }
  }

  if (!fOpaque) {
    RemoveRubberbandOutline();
  }

  SuggestSizeWindowTo(psw,WIN_VP_OFFSET_X(psw)+dragx,WIN_VP_OFFSET_Y(psw)+dragy,
                      dragWidth,dragHeight, True);
  CassowaryEndEdit(psw);
  call1_hooks(interactive_resize_finish_hook, psw->schwin);
  
  UninstallRootColormap();

  if (!fOpaque) {
    XUngrabServer_withSemaphore(dpy);
  }
  UngrabEm();
  xmotion = 0;
  ymotion = 0;

  /* restore edge wrap state */
  Scr.fEdgeWrapX = fEdgeWrapX;
  Scr.fEdgeWrapY = fEdgeWrapY;

  if (pwidthReturn)
    *pwidthReturn = dragWidth;

  if (pheightReturn)
    *pheightReturn = dragHeight;

  return True;
}

SCWM_PROC(rubber_band_resize, "rubber-band-resize", 0, 1, 0,
          (SCM win),
"Resize WIN interactively, using a rubber band frame.
Returns a list '(width height) that is the new size of WIN.
This allows the user to drag a rubber band frame to set the size of
the window. WIN defaults to the window context in the usual way if not
specified. ")
#define FUNC_NAME s_rubber_band_resize
{
  ScwmWindow *psw;
  int width, height;            /* not used, now */

  VALIDATE_PRESS_ONLY(win);
  psw = PSWFROMSCMWIN(win);

  if (check_allowed_function(F_RESIZE, psw) == 0
      || SHADED_P(psw)) {
    return SCM_BOOL_F;
  }

  if (psw->fIconified) {
    return SCM_BOOL_F;
  }

  InteractiveResize(psw, False, &width, &height);

  return gh_list(gh_int2scm(width),gh_int2scm(height),SCM_UNDEFINED);
}
#undef FUNC_NAME



SCWM_PROC(opaque_resize, "opaque-resize", 0, 1, 0,
          (SCM win),
"Resize WIN interactively, opaquely.
Returns a list '(width height) that is the new size of WIN.
This allows the user to drag the boundaries of the window to set its
size. WIN defaults to the window context in the usual way if not
specified. The window is updated immediately as the size changes take
place.")
#define FUNC_NAME s_opaque_resize
{
  ScwmWindow *psw;
  int width, height;

  VALIDATE_PRESS_ONLY(win);
  psw = PSWFROMSCMWIN(win);

  if (check_allowed_function(F_RESIZE, psw) == 0
      || SHADED_P(psw)) {
    return SCM_BOOL_F;
  }

  if (psw->fIconified) {
    return SCM_BOOL_F;
  }

  InteractiveResize(psw, True, &width, &height);

  return gh_list(gh_int2scm(width),gh_int2scm(height),SCM_UNDEFINED);
}
#undef FUNC_NAME

void
init_resize_gcs()
{
  XGCValues gcv;
  unsigned long gcm = GCFunction | GCLineWidth | GCForeground | GCSubwindowMode;
  gcv.function = GXxor;
  gcv.line_width = 0;
  gcv.foreground = 0xaaaaaa;
  gcv.subwindow_mode = IncludeInferiors;
  DrawRubberBandGC = XCreateGC(dpy, Scr.Root, gcm, &gcv);
}


void 
init_resize()
{
#ifndef SCM_MAGIC_SNARFER
#include "resize.x"
#endif
}



/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

