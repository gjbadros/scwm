/* $Id$
 * (C) 1998 Maciej Stachowiak and Greg J. Badros
 */ 

/*
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
#include "scwm.h"
#include "screen.h"
#include "resize.h"
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


/* Create the small window to show the interactively-moved/resized window's
   size/position */
Window
CreateMessageWindow(Pixel fg, Pixel bg) {
  Window w;
  const int width = 50; /* just some starting place-- DisplayMessage resizes */
  XSetWindowAttributes attributes;
  unsigned long valuemask = (CWBorderPixel | CWBackPixel | CWBitGravity);
  attributes.border_pixel = fg;
  attributes.background_pixel = bg;
  attributes.bit_gravity = NorthWestGravity;

  w = XCreateWindow(dpy, Scr.Root,
                    0, 0, width, 
                    (FONTHEIGHT(Scr.msg_window_font) + SIZE_VINDENT * 2),
                    0, 0, CopyFromParent, (Visual *) CopyFromParent,
                    valuemask, &attributes);
  return w;
}

SCWM_PROC(set_message_window_attributes_x, "set-message-window-attributes!", 3, 0, 0,
          (SCM font, SCM fg_color, SCM bg_color))
    /** Set the attributes to be used for the message window.
The font will be FONT, foreground color FG-COLOR, and background color BG-COLOR.
This the window which is used to display the current size or position of the window
being moved or resized interactively. */
#define FUNC_NAME s_set_message_window_attributes_x
{
  int iarg = 1;
  if (gh_string_p(font)) {
    font = make_font(font);
  }
  if (!FONT_P(font)) {
    scm_wrong_type_arg(FUNC_NAME, iarg++, font);
  }
  VALIDATE_COLOR (fg_color, FUNC_NAME, iarg++);
  VALIDATE_COLOR (bg_color, FUNC_NAME, iarg++);
  Scr.msg_window_font=font;
  Scr.msg_window_fg = fg_color;
  Scr.msg_window_bg = bg_color;

  XSetWindowBorder(dpy,Scr.MsgWindow,XCOLOR(Scr.msg_window_fg));
  XSetWindowBackground(dpy,Scr.MsgWindow,XCOLOR(Scr.msg_window_bg));
  return SCM_UNDEFINED;
}
#undef FUNC_NAME


/*
 *  Procedure:
 *      DisplaySize - display the size in the dimensions window
 *
 *  Inputs:
 *      psw - the current scwm window
 *      width   - the width of the rubber band
 *      height  - the height of the rubber band
 */
static void 
DisplaySize(ScwmWindow *psw, int width, int height, Bool fRelief)
{
  char sz[30];
  int dheight = height - psw->title_height - 2*psw->boundary_width;
  int dwidth = width - 2*psw->boundary_width;
  
  dwidth -= psw->hints.base_width;
  dheight -= psw->hints.base_height;
  dwidth /= psw->hints.width_inc;
  dheight /= psw->hints.height_inc;

  sprintf(sz, " %4d x %-4d ", dwidth, dheight);
  DisplayMessage(sz,fRelief);
}


static int
makemult(int a, int b)
{
  if (b==1)
    return a;
  else {
    int t = a/b;
    return t * (b + (a/b - t >= .5? 1: 0));
  }
}

/*
 * ConstrainSize - adjust the given width and height to account for the
 *              constraints imposed by size hints
 *
 *      The general algorithm, especially the aspect ratio stuff, is
 *      borrowed from uwm's CheckConsistency routine.
 */
void 
ConstrainSize(ScwmWindow *psw, int xmotion, int ymotion, int *widthp, int *heightp)
{
  int minWidth, minHeight, maxWidth, maxHeight, xinc, yinc, delta;
  int baseWidth, baseHeight;
  int dwidth = *widthp, dheight = *heightp;
  DBUG(__FUNCTION__,"initially %d x %d",dwidth,dheight);

  dwidth -= 2 * psw->xboundary_width;
  dheight -= (psw->title_height + 2 * psw->boundary_width);

  minWidth = psw->hints.min_width;
  minHeight = psw->hints.min_height;

  baseWidth = psw->hints.base_width;
  baseHeight = psw->hints.base_height;

  maxWidth = psw->hints.max_width;
  maxHeight = psw->hints.max_height;

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
  dwidth = ((dwidth - baseWidth) / xinc * xinc) + baseWidth;
  dheight = ((dheight - baseHeight) / yinc * yinc) + baseHeight;


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

  if (psw->hints.flags & PAspect) {
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

  DBUG(__FUNCTION__,"constrained to %d x %d",*widthp,*heightp);
  return;
}

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
  int dragx, dragy, dragWidth, dragHeight;

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

/* MSFIX: Can't easily be a color w/o overlay planes-- needs to be really 
   fast to erase */
SCWM_PROC(set_rubber_band_mask_x, "set-rubber-band-mask!", 1, 0, 0,
          (SCM value))
     /** Set the rubber band mask used when dragging or resizing.
VALUE is XORed with the background when dragging non-opaque move or
resize frames. VALUE should be an integer. */
#define FUNC_NAME s_set_rubber_band_mask_x
{
  XGCValues gcv;
  unsigned long gcm;

  SCM_REDEFER_INTS;

  if (!gh_number_p(value)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg(FUNC_NAME, 1, value);
  }
  gcm = GCFunction | GCLineWidth | GCForeground | GCSubwindowMode;
  gcv.function = GXxor;
  gcv.line_width = 0;
  gcv.foreground = gh_scm2long(value);
  gcv.subwindow_mode = IncludeInferiors;
  if (NULL != DrawRubberBandGC) {
    XFreeGC(dpy, DrawRubberBandGC);
  }
  DrawRubberBandGC = XCreateGC(dpy, Scr.Root, gcm, &gcv);
  SCM_REALLOW_INTS;
  return (value);
}
#undef FUNC_NAME



/*
 * RedrawOutlineAtNewPosition
 *
 *  Inputs:
 *	root	    - the window we are outlining
 *	x	    - upper left x coordinate
 *	y	    - upper left y coordinate
 *	width	    - the width of the rectangle
 *	height	    - the height of the rectangle
 */
void 
RedrawOutlineAtNewPosition(Window root, int x, int y, int width, int height)
{
  /* Ugh. no statics! FIXGJB */
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


/* FIXGJB: InteractiveResize uses the global var PressedW 
   to figure out how the resize should work. This is bad! */
Bool
InteractiveResize(ScwmWindow *psw, Bool fOpaque, int *pwidthReturn, int *pheightReturn)
{
  extern Window PressedW;       /* FIXGJB: ugly! */

  int dragx;			/* all these variables are used */
  int dragy;			/* in resize operations */
  int dragWidth;
  int dragHeight;

  int origx;
  int origy;
  int origWidth;
  int origHeight;

  int ymotion = 0, xmotion = 0;
  Bool finished = False, done = False, abort = False;
  int x, y, delta_x, delta_y;

  /* save state of edge wrap */
  Bool fEdgeWrapX = Scr.fEdgeWrapX;
  Bool fEdgeWrapY = Scr.fEdgeWrapY;

  CassowaryModifyOpaqueFlag(&fOpaque);

  psw->fMaximized = False;

  InstallRootColormap();
  if (!GrabEm(CURSOR_MOVE)) {
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

  /* pop up a resize dimensions window */
  MapMessageWindow();
  DisplaySize(psw, origWidth, origHeight, True);

  CassowaryEditSize(psw);


  /* Get the current position to determine which border to resize 
     FIXGJB: this is ugly -- perhaps should pass in
     resize directions? */
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
  }
  /* draw the rubber-band window */
  if (!fOpaque) {
    RedrawOutlineAtNewPosition(Scr.Root, dragx - psw->bw, dragy - psw->bw,
                               dragWidth + 2 * psw->bw,
                               dragHeight + 2 * psw->bw);
  }

  /* loop to resize */
  while (!finished) {
    XEvent ResizeEvent;
    XMaskEvent(dpy, ButtonPressMask | ButtonReleaseMask | KeyPressMask |
	       ButtonMotionMask | PointerMotionMask | ExposureMask, &ResizeEvent);
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
	abort = True;
	finished = True;
      }
      done = True;
      break;

    case ButtonRelease:
      finished = True;
      done = True;
      break;

    case MotionNotify:
      x = ResizeEvent.xmotion.x_root;
      y = ResizeEvent.xmotion.y_root;
      /* resize before paging request to prevent resize from lagging mouse - mab */
      ComputeNewGeometryOnResize(psw,origx,origy,origWidth,origHeight,
                                 x, y, &xmotion, &ymotion, 
                                 &dragx, &dragy, &dragWidth, &dragHeight);

      SuggestSizeWindowTo(psw,dragx,dragy,dragWidth,dragHeight, fOpaque);
            
      DisplaySize(psw, dragWidth, dragHeight, True);
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
            RedrawOutlineAtNewPosition(Scr.Root, dragx - psw->bw, dragy - psw->bw,
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

      /* limit ourselves to legitimate sizes */
      ConstrainSize(psw, xmotion, ymotion, &dragWidth, &dragHeight);

      SuggestSizeWindowTo(psw,dragx,dragy,dragWidth,dragHeight, fOpaque);
    }
  }

  if (!fOpaque) {
    RemoveRubberbandOutline(Scr.Root);
  }

  /* pop down the size window */
  UnmapMessageWindow();

  SuggestSizeWindowTo(psw,dragx,dragy,dragWidth,dragHeight, True);
  CassowaryEndEdit(psw);

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

  return True;
}


SCWM_PROC(interactive_resize, "interactive-resize", 0, 2, 0,
          (SCM win, SCM opaque_p))
     /** Resize WIN interactively.
This allows the user to drag a rubber band frame to set the size of
the window. WIN defaults to the window context in the usual way if not
specified. If OPAQUE? is #t, the resize will be done
"opaquely", moving the actual X window, if #f a rubberband will be
used instead to save on server computation (note that the rubberband
requires a server "grab" which means that nothing else changes on
screen while the non-opaque resize takes place. */
#define FUNC_NAME s_interactive_resize
{
  ScwmWindow *psw;
  int width, height;            /* not used, now */
  Bool fOpaque;

  VALIDATE_PRESS_ONLY(win, FUNC_NAME);
  COPY_BOOL_OR_ERROR_DEFAULT_FALSE(fOpaque,opaque_p,2,FUNC_NAME);
  psw = PSWFROMSCMWIN(win);

  if (check_allowed_function(F_RESIZE, psw) == 0
      || SHADED_P(psw)) {
    return SCM_BOOL_F;
  }

  if (psw->fIconified) {
    return SCM_BOOL_F;
  }

  InteractiveResize(psw, fOpaque, &width, &height);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

void
init_resize_gcs()
{
  XGCValues gcv;
  unsigned long gcm = GCFunction | GCLineWidth | GCForeground | GCSubwindowMode;
  gcv.function = GXxor;
  gcv.line_width = 0;
  gcv.foreground = 37; /* randomish */
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
