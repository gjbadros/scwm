/* $Id$
 * (C) 1998 Maciej Stachowiak and Greg J. Badros
 */ 

/****************************************************************************
 * This module is derived from code by Rob Nation 
 * Copyright 1993, Robert Nation
 *     You may use this code for any purpose, as long as the original
 *     copyright remains in the source code and all documentation
 ****************************************************************************/

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

int dragx;			/* all these variables are used */
int dragy;			/* in resize operations */
int dragWidth;
int dragHeight;

int origx;
int origy;
int origWidth;
int origHeight;

int ymotion = 0, xmotion = 0;
int last_width, last_height;
extern int menuFromFrameOrWindowOrTitlebar;
extern Window PressedW;


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

    MoveOutline(Scr.Root, dragx - psw->bw, dragy - psw->bw,
		dragWidth + 2 * psw->bw, dragHeight + 2 * psw->bw);
  }
  DisplaySize(psw, dragWidth, dragHeight, False);
}



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
DisplaySize(ScwmWindow * psw, int width, int height, Bool Init)
{
  char str[100];
  int dwidth, dheight, offset;
#ifdef I18N
  XRectangle dummy,log_ret;
#endif

  if (last_width == width && last_height == height)
    return;

  last_width = width;
  last_height = height;

  dheight = height - psw->title_height - 2 * psw->boundary_width;
  dwidth = width - 2 * psw->boundary_width;

  dwidth -= psw->hints.base_width;
  dheight -= psw->hints.base_height;
  dwidth /= psw->hints.width_inc;
  dheight /= psw->hints.height_inc;

  (void) sprintf(str, " %4d x %-4d ", dwidth, dheight);
#ifdef I18N
  XmbTextExtents(XFONT(Scr.menu_font),"WWWWWWWWWWWWWWW",15,&dummy,&log_ret);
  offset = (Scr.SizeStringWidth + SIZE_HINDENT * 2 - log_ret.width) / 2;
  if (Init) {
    XClearWindow(dpy, Scr.SizeWindow);
    if (Scr.d_depth >= 2)
      RelieveWindow(psw,
                    Scr.SizeWindow, 0, 0, Scr.SizeStringWidth + SIZE_HINDENT * 2,
		    log_ret.height + SIZE_VINDENT * 2,
		    Scr.MenuReliefGC, Scr.MenuShadowGC, FULL_HILITE);
  } else {
    XClearArea(dpy, Scr.SizeWindow, SIZE_HINDENT, SIZE_VINDENT, Scr.SizeStringWidth,
	       log_ret.height, False);
  }

  XmbDrawString(dpy, Scr.SizeWindow, XFONT(Scr.menu_font), Scr.MenuGC,
	      offset, FONTY(Scr.menu_font) + SIZE_VINDENT, str, 13);
#else
  offset = (Scr.SizeStringWidth + SIZE_HINDENT * 2
	    - XTextWidth(XFONT(Scr.menu_font), str, strlen(str))) / 2;
  if (Init) {
    XClearWindow(dpy, Scr.SizeWindow);
    if (Scr.d_depth >= 2)
      RelieveWindow(psw,
	       Scr.SizeWindow, 0, 0, Scr.SizeStringWidth + SIZE_HINDENT * 2,
		    FONTHEIGHT(Scr.menu_font) + SIZE_VINDENT * 2,
		    Scr.MenuReliefGC, Scr.MenuShadowGC, FULL_HILITE);
  } else {
    XClearArea(dpy, Scr.SizeWindow, SIZE_HINDENT, SIZE_VINDENT, Scr.SizeStringWidth,
	       FONTHEIGHT(Scr.menu_font), False);
  }

  XDrawString(dpy, Scr.SizeWindow, Scr.MenuGC,
	      offset, FONTY(Scr.menu_font) + SIZE_VINDENT, str, 13);
#endif

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


/*
 *  Procedure:
 *	MoveOutline - move a window outline
 *
 *  Inputs:
 *	root	    - the window we are outlining
 *	x	    - upper left x coordinate
 *	y	    - upper left y coordinate
 *	width	    - the width of the rectangle
 *	height	    - the height of the rectangle
 */
void 
MoveOutline(Window root, int x, int y, int width, int height)
{
  static int lastx = 0;
  static int lasty = 0;
  static int lastWidth = 0;
  static int lastHeight = 0;
  XRectangle rects[5];

  if (x == lastx && y == lasty && width == lastWidth && height == lastHeight)
    return;

  /* undraw the old one, if any */
  if (lastWidth || lastHeight) {
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
    XDrawRectangles(dpy, Scr.Root, Scr.DrawGC, rects, 5);
  }
  lastx = x;
  lasty = y;
  lastWidth = width;
  lastHeight = height;

  /* draw the new one, if any */
  if (lastWidth || lastHeight) {
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
    XDrawRectangles(dpy, Scr.Root, Scr.DrawGC, rects, 5);
  }
}

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
