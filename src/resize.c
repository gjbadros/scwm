/* $Id$ */

/****************************************************************************
 * This module is all original code 
 * by Rob Nation 
 * Copyright 1993, Robert Nation
 *     You may use this code for any purpose, as long as the original
 *     copyright remains in the source code and all documentation
 ****************************************************************************/


/***********************************************************************
 *
 * window resizing borrowed from the "wm" window manager
 *
 ***********************************************************************/
#include <config.h>

#include <stdio.h>
#include <X11/keysym.h>
#include "scwm.h"
#include "misc.h"
#include "screen.h"
#include "parse.h"

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


/***********************************************************************
 *
 *  Procedure:
 *      DoResize - move the rubberband around.  This is called for
 *                 each motion event when we are resizing
 *
 *  Inputs:
 *      x_root  - the X corrdinate in the root window
 *      y_root  - the Y corrdinate in the root window
 *      tmp_win - the current scwm window
 *
 ************************************************************************/
void 
DoResize(int x_root, int y_root, ScwmWindow * tmp_win)
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
    ConstrainSize(tmp_win, &dragWidth, &dragHeight);
    if (xmotion == 1)
      dragx = origx + origWidth - dragWidth;
    if (ymotion == 1)
      dragy = origy + origHeight - dragHeight;

    MoveOutline(Scr.Root, dragx - tmp_win->bw, dragy - tmp_win->bw,
		dragWidth + 2 * tmp_win->bw, dragHeight + 2 * tmp_win->bw);
  }
  DisplaySize(tmp_win, dragWidth, dragHeight, False);
}



/***********************************************************************
 *
 *  Procedure:
 *      DisplaySize - display the size in the dimensions window
 *
 *  Inputs:
 *      tmp_win - the current scwm window
 *      width   - the width of the rubber band
 *      height  - the height of the rubber band
 *
 ***********************************************************************/
void 
DisplaySize(ScwmWindow * tmp_win, int width, int height, Bool Init)
{
  char str[100];
  int dwidth, dheight, offset;

  if (last_width == width && last_height == height)
    return;

  last_width = width;
  last_height = height;

  dheight = height - tmp_win->title_height - 2 * tmp_win->boundary_width;
  dwidth = width - 2 * tmp_win->boundary_width;

  dwidth -= tmp_win->hints.base_width;
  dheight -= tmp_win->hints.base_height;
  dwidth /= tmp_win->hints.width_inc;
  dheight /= tmp_win->hints.height_inc;

  (void) sprintf(str, " %4d x %-4d ", dwidth, dheight);
  offset = (Scr.SizeStringWidth + SIZE_HINDENT * 2
	    - XTextWidth(Scr.StdFont.font, str, strlen(str))) / 2;
  if (Init) {
    XClearWindow(dpy, Scr.SizeWindow);
    if (Scr.d_depth >= 2)
      RelieveWindow(tmp_win,
	       Scr.SizeWindow, 0, 0, Scr.SizeStringWidth + SIZE_HINDENT * 2,
		    Scr.StdFont.height + SIZE_VINDENT * 2,
		    Scr.MenuReliefGC, Scr.MenuShadowGC, FULL_HILITE);
  } else {
    XClearArea(dpy, Scr.SizeWindow, SIZE_HINDENT, SIZE_VINDENT, Scr.SizeStringWidth,
	       Scr.StdFont.height, False);
  }

  XDrawString(dpy, Scr.SizeWindow, Scr.MenuGC,
	      offset, Scr.StdFont.font->ascent + SIZE_VINDENT, str, 13);

}

/***********************************************************************
 *
 *  Procedure:
 *      ConstrainSize - adjust the given width and height to account for the
 *              constraints imposed by size hints
 *
 *      The general algorithm, especially the aspect ratio stuff, is
 *      borrowed from uwm's CheckConsistency routine.
 * 
 ***********************************************************************/

void 
ConstrainSize(ScwmWindow * tmp_win, int *widthp, int *heightp)
{
#define makemult(a,b) ((b==1) ? (a) : (((int)((a)/(b))) * (b)) )
#define _min(a,b) (((a) < (b)) ? (a) : (b))
  int minWidth, minHeight, maxWidth, maxHeight, xinc, yinc, delta;
  int baseWidth, baseHeight;
  int dwidth = *widthp, dheight = *heightp;

  dwidth -= 2 * tmp_win->boundary_width;
  dheight -= (tmp_win->title_height + 2 * tmp_win->boundary_width);

  minWidth = tmp_win->hints.min_width;
  minHeight = tmp_win->hints.min_height;

  baseWidth = tmp_win->hints.base_width;
  baseHeight = tmp_win->hints.base_height;

  maxWidth = tmp_win->hints.max_width;
  maxHeight = tmp_win->hints.max_height;

/*    maxWidth = Scr.VxMax + Scr.MyDisplayWidth;
   maxHeight = Scr.VyMax + Scr.MyDisplayHeight; */

  xinc = tmp_win->hints.width_inc;
  yinc = tmp_win->hints.height_inc;

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
#define maxAspectX tmp_win->hints.max_aspect.x
#define maxAspectY tmp_win->hints.max_aspect.y
#define minAspectX tmp_win->hints.min_aspect.x
#define minAspectY tmp_win->hints.min_aspect.y
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

  if (tmp_win->hints.flags & PAspect) {
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
  *widthp = dwidth + 2 * tmp_win->boundary_width;
  *heightp = dheight + tmp_win->title_height + 2 * tmp_win->boundary_width;
  return;
}


/***********************************************************************
 *
 *  Procedure:
 *	MoveOutline - move a window outline
 *
 *  Inputs:
 *	root	    - the window we are outlining
 *	x	    - upper left x coordinate
 *	y	    - upper left y coordinate
 *	width	    - the width of the rectangle
 *	height	    - the height of the rectangle
 *
 ***********************************************************************/
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
