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

static int dragx;			/* all these variables are used */
static int dragy;			/* in resize operations */
static int dragWidth;
static int dragHeight;

static int origx;
static int origy;
static int origWidth;
static int origHeight;

static int ymotion = 0, xmotion = 0;
static int last_width, last_height;



/*
 *  Procedure:
 *      DoResize - move the rubberband around.  This is called for
 *                 each motion event when we are resizing
 *
 *  Inputs:
 *      x_root  - the X corrdinate in the root window
 *      y_root  - the Y corrdinate in the root window
 *      psw - the current scwm window
 */
void 
DoResize(int x_root, int y_root, ScwmWindow * psw)
{
  int action = 0;

  if ((y_root <= origy) || ((ymotion == 1) && (y_root < origy + origHeight - 1))) {
    dragy = y_root;
    dragHeight = origy + origHeight - y_root;
    action = 1;
    ymotion = 1;
  } else if ((y_root >= origy + origHeight - 1) ||
	     ((ymotion == -1) && (y_root > origy))) {
    dragy = origy;
    dragHeight = 1 + y_root - dragy;
    action = 1;
    ymotion = -1;
  }
  if ((x_root <= origx) ||
      ((xmotion == 1) && (x_root < origx + origWidth - 1))) {
    dragx = x_root;
    dragWidth = origx + origWidth - x_root;
    action = 1;
    xmotion = 1;
  }
  if ((x_root >= origx + origWidth - 1) ||
      ((xmotion == -1) && (x_root > origx))) {
    dragx = origx;
    dragWidth = 1 + x_root - origx;
    action = 1;
    xmotion = -1;
  }
  if (action) {
    ConstrainSize(psw, &dragWidth, &dragHeight);
    if (xmotion == 1)
      dragx = origx + origWidth - dragWidth;
    if (ymotion == 1)
      dragy = origy + origHeight - dragHeight;

    RedrawOutlineAtNewPosition(Scr.Root, dragx - psw->bw, dragy - psw->bw,
                               dragWidth + 2 * psw->bw, dragHeight + 2 * psw->bw);
  }
  DisplaySize(psw, dragWidth, dragHeight, False);
}


/* Create the small window to show the interactively-moved/resized window's
   size/position */
Window
CreateMessageWindow(Pixel fg, Pixel bg, Bool fMWMLike) {
  Window w;
  XSetWindowAttributes attributes;
  unsigned long valuemask = (CWBorderPixel | CWBackPixel | CWBitGravity);
#ifdef I18N
  XRectangle dummy,log_ret;

  XmbTextExtents(XFONT(Scr.msg_window_font)," +999999x999999", 15,&dummy,&log_ret);
  Scr.EntryHeight = log_ret.height + HEIGHT_EXTRA;
  Scr.SizeStringWidth = log_ret.width;
#else
  Scr.EntryHeight = FONTHEIGHT(Scr.msg_window_font) + HEIGHT_EXTRA;
  Scr.SizeStringWidth = XTextWidth(XFONT(Scr.msg_window_font),
					 " +999999x999999", 15);
#endif
  attributes.border_pixel = fg;
  attributes.background_pixel = bg;
  attributes.bit_gravity = NorthWestGravity;

  if (!fMWMLike) {
    w = XCreateWindow(dpy, Scr.Root,
                      0, 0, (Scr.SizeStringWidth + SIZE_HINDENT * 2),
                      (FONTHEIGHT(Scr.msg_window_font) + SIZE_VINDENT * 2),
                      0, 0, CopyFromParent, (Visual *) CopyFromParent,
                      valuemask, &attributes);
  } else {
    w = XCreateWindow(dpy, Scr.Root,
                      Scr.MyDisplayWidth / 2 - 
                      (Scr.SizeStringWidth + SIZE_HINDENT * 2) / 2,
                      Scr.MyDisplayHeight / 2 -
                      (FONTHEIGHT(Scr.msg_window_font) + SIZE_VINDENT * 2) / 2,
                      (Scr.SizeStringWidth + SIZE_HINDENT * 2),
                      (FONTHEIGHT(Scr.msg_window_font) + SIZE_VINDENT * 2),
                      0, 0, CopyFromParent, (Visual *) CopyFromParent,
                      valuemask, &attributes);
  }
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
  XDestroyWindow(dpy,Scr.SizeWindow);
  Scr.SizeWindow = CreateMessageWindow( XCOLOR(Scr.msg_window_fg), 
                                        XCOLOR(Scr.msg_window_bg), Scr.flags & MWMMenus);
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
void 
DisplaySize(ScwmWindow *psw, int width, int height, Bool fInitializeRelief)
{
  char sz[30];
  sprintf(sz, " %4d x %-4d ", width, height);
  DisplayMessage(psw,sz,fInitializeRelief);
}

/*
 *  Procedure:
 *      ConstrainSize - adjust the given width and height to account for the
 *              constraints imposed by size hints
 *
 *      The general algorithm, especially the aspect ratio stuff, is
 *      borrowed from uwm's CheckConsistency routine.
 */

void 
ConstrainSize(ScwmWindow *psw, int *widthp, int *heightp)
{
#define makemult(a,b) ((b==1) ? (a) : (((int)((a)/(b))) * (b)) )
  int minWidth, minHeight, maxWidth, maxHeight, xinc, yinc, delta;
  int baseWidth, baseHeight;
  int dwidth = *widthp, dheight = *heightp;

  dwidth -= 2 * psw->boundary_width;
  dheight -= (psw->title_height + 2 * psw->boundary_width);

  minWidth = psw->hints.min_width;
  minHeight = psw->hints.min_height;

  baseWidth = psw->hints.base_width;
  baseHeight = psw->hints.base_height;

  maxWidth = psw->hints.max_width;
  maxHeight = psw->hints.max_height;

/*    maxWidth = Scr.VxMax + Scr.MyDisplayWidth;
   maxHeight = Scr.VyMax + Scr.MyDisplayHeight; */

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
  *widthp = dwidth + 2 * psw->boundary_width;
  *heightp = dheight + psw->title_height + 2 * psw->boundary_width;
  return;
}
#undef makemult


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
    XDrawRectangles(dpy, Scr.Root, Scr.DrawGC, rects, 5);
  }

  /* save the new position in the static variables */
  lastx = x;
  lasty = y;
  lastWidth = width;
  lastHeight = height;

  /* draw the new one, if any */
  if (lastWidth || lastHeight) {
    InitializeOutlineRects(rects,lastx,lasty,lastWidth,lastHeight);
    XDrawRectangles(dpy, Scr.Root, Scr.DrawGC, rects, 5);
  }
}


SCWM_PROC(interactive_resize, "interactive-resize", 0, 1, 0,
          (SCM win))
     /** Resize WIN interactively.
This allows the user to drag a rubber band frame to set the size of
the window. WIN defaults to the window context in the usual way if not
specified. */
#define FUNC_NAME s_interactive_resize
{
  extern Window PressedW;
  ScwmWindow *psw;
  Bool finished = False, done = False, abort = False;
  int x, y, delta_x, delta_y;
  Window ResizeWindow;
  Bool flags;

  VALIDATE_PRESS_ONLY(win, FUNC_NAME);
  psw = PSWFROMSCMWIN(win);


  if (check_allowed_function(F_RESIZE, psw) == 0
      || SHADED_P(psw)) {
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }
  psw->fMaximized = False;

  if (psw->fIconified) {
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }
  ResizeWindow = psw->frame;


  InstallRootColormap();
  if (!GrabEm(CURSOR_MOVE)) {
    call0_hooks(cannot_grab_hook);
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }
  XGrabServer_withSemaphore(dpy);

  /* handle problems with edge-wrapping while resizing */
  flags = Scr.flags;
  Scr.flags &= ~(EdgeWrapX | EdgeWrapY);

  XGetGeometry(dpy, (Drawable) ResizeWindow, &JunkRoot,
	       &dragx, &dragy, (unsigned int *) &dragWidth,
	       (unsigned int *) &dragHeight, &JunkBW, &JunkDepth);

  dragx += psw->bw;
  dragy += psw->bw;
  origx = dragx;
  origy = dragy;
  origWidth = dragWidth;
  origHeight = dragHeight;
  ymotion = xmotion = 0;

  /* pop up a resize dimensions window */
  MapSizePositionWindow();
  last_width = 0;
  last_height = 0;
  DisplaySize(psw, origWidth, origHeight, True);

  /* Get the current position to determine which border to resize */
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
  RedrawOutlineAtNewPosition(Scr.Root, dragx - psw->bw, dragy - psw->bw,
                             dragWidth + 2 * psw->bw,
                             dragHeight + 2 * psw->bw);

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
      Keyboard_shortcuts(&ResizeEvent, ButtonRelease);
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
      DoResize(x, y, psw);
      /* need to move the viewport */
      HandlePaging(Scr.EdgeScrollX, Scr.EdgeScrollY, &x, &y,
		   &delta_x, &delta_y, False);
      /* redraw outline if we paged - mab */
      if ((delta_x != 0) || (delta_y != 0)) {
	origx -= delta_x;
	origy -= delta_y;
	dragx -= delta_x;
	dragy -= delta_y;

	DoResize(x, y, psw);
      }
      done = True;
    default:
      break;
    }
    if (!done) {
      extern XEvent Event;
      RemoveRubberbandOutline(Scr.Root);
      Event = ResizeEvent;
      DispatchEvent();

      RedrawOutlineAtNewPosition(Scr.Root, dragx - psw->bw, dragy - psw->bw,
                                 dragWidth + 2 * psw->bw, dragHeight + 2 * psw->bw);

    }
  }

  RemoveRubberbandOutline(Scr.Root);

  /* pop down the size window */
  XUnmapWindow(dpy, Scr.SizeWindow);

  if (!abort) {
    ConstrainSize(psw, &dragWidth, &dragHeight);
    SetupFrame(psw, dragx - psw->bw,
	       dragy - psw->bw, dragWidth, dragHeight, False,
               NOT_MOVED, WAS_RESIZED);
  }
  UninstallRootColormap();
  ResizeWindow = None;
  XUngrabServer_withSemaphore(dpy);
  UngrabEm();
  xmotion = 0;
  ymotion = 0;

  Scr.flags |= flags & (EdgeWrapX | EdgeWrapY);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


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
