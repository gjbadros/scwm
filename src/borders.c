/* $Id$
 * borders.c
 * Copyright (C) 1997, 1998, 1999, 2000 By Maciej Stachowiak and Greg J. Badros
 *
 * This module is derived from code 
 * by Rob Nation 
 * Copyright 1993, Robert Nation
 *     You may use this code for any purpose, as long as the original
 *     copyright remains in the source code and all documentation
 *
 */
 
/*
 * Scwm window border drawing code
 */

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include <stdio.h>
#include <signal.h>
#include <string.h>
#include <assert.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#ifdef HAVE_SHAPE
#include <X11/extensions/shape.h>
#endif

#include <guile/gh.h>
#include "guile-compat.h"

#include "borders.h"

#include "scwm.h"
#include "icons.h"
#include "screen.h"
#include "image.h"
#include "module-interface.h"
#include "font.h"
#include "xmisc.h"
#include "scwm-constraints.h"
#include "dbug_resize.h"

#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif

extern Window PressedW;

SCWM_SYMBOL(sym_no_top_border_decoration, "no-top-border-decoration");

/* Also used in window.c */
extern SCM sym_maximized, sym_no_side_decorations;

/* macro rules to get button state */
/* GJB:FIXME:: ugh! dynamic scoping in a macro! --07/26/98 gjb */
#define GetButtonState(window)						\
        (fHighlightOn ? ((PressedW == (window)) ? ActiveDown : ActiveUp) \
        : Inactive)


/* GJB:FIXME:: ugh! dynamic scoping in a macro now here, too! --09/19/99 gjb */
/* macro to change window background color/pixmap */
#define ChangeWindowColor(window,valuemask) \
    do { if (NewColor) { \
           XChangeWindowAttributes(dpy,(window),(valuemask), &attributes); \
           XClearWindow(dpy,(window)); \
          } \
    } while(0)

/*
 * Sets up the shaped window borders 
 * This is used when psw->fShaped (i.e., client window is a shaped window)
 */
void 
SetShape(ScwmWindow *psw, int w)
{
#ifdef HAVE_SHAPE
  if (ShapesSupported) {
    XRectangle rect;

    XShapeCombineShape(dpy, psw->frame, ShapeBounding,
		       psw->xboundary_width,
		       psw->title_height + psw->boundary_width,
		       psw->w,
		       ShapeBounding, ShapeSet);
    if (SHOW_TITLE_P(psw)) {
      /* windows w/ titles */
      rect.x = psw->xboundary_width;
      rect.y = psw->title_y;
      rect.width = w - 2 * psw->xboundary_width + psw->bw;
      rect.height = psw->title_height;

      XShapeCombineRectangles(dpy, psw->frame, ShapeBounding,
			      0, 0, &rect, 1, ShapeUnion, Unsorted);
    }
  }
#endif
}

void 
SetShapedTitlebar(ScwmWindow *psw, int w)
{
#ifdef HAVE_SHAPE
  if (ShapesSupported) {
    Boolean fSet = False;
    XRectangle rect;
    rect.x = 0;
    rect.y = 0;
    rect.width  = FRAME_WIDTH (psw);
    rect.height = FRAME_HEIGHT(psw) - psw->title_height;

    if (!SHADED_P(psw)) {
      XShapeCombineRectangles(dpy, psw->frame, ShapeBounding,
                              0, psw->title_height,
                              &rect, 1, ShapeSet, Unsorted);
      fSet = True;
    }
    if (SHOW_TITLE_P(psw)) {
      /* windows w/ titles */
      rect.x = 0;
      rect.y = 0;
      rect.width = w;
      rect.height = psw->title_height;

      XShapeCombineRectangles(dpy, psw->frame, ShapeBounding,
			      0, 0, &rect, 1, fSet? ShapeUnion: ShapeSet, Unsorted);
    }
  }
#endif
}


/* GJB:FIXME:: Where are good docs about the XShape extension?
   this cannot be right! */
static void 
UnsetShapedTitlebar(ScwmWindow *psw)
{
  if (ShapesSupported) {
    XRectangle rect;
    rect.x = 0;
    rect.y = 0;
    rect.width = FRAME_WIDTH(psw);
    rect.height = FRAME_HEIGHT(psw);
    XShapeCombineRectangles(dpy, psw->frame, ShapeBounding,
                            0, 0, &rect, 1, ShapeSet, Unsorted);
  }
}


/*
 *  Draws an arbitrary sequence of lines within a window (more complex)
 */
static void 
DrawLinePattern(Window win,
		GC ReliefGC,
		GC ShadowGC,
		struct vector_coords *coords,
		int w, int h)
{
  int i = 1;

  for (; i < coords->num; ++i) {
    XDrawLine(dpy, win,
	      coords->line_style[i] ? ReliefGC : ShadowGC,
	      w * coords->x[i - 1] / 100,
	      h * coords->y[i - 1] / 100,
	      w * coords->x[i] / 100,
	      h * coords->y[i] / 100);
  }
}

/* initialized in init_borders */
static GC TransMaskGC;

/*
 *  Redraws buttons (Derived from code by veliaa@rpi.edu)
 */
static void 
DrawButton(ScwmWindow *psw, Window win, int w, int h,
	   ButtonFace * bf, GC ReliefGC, GC ShadowGC,
	   Bool ARG_UNUSED(inverted), int stateflags)
{
#define FUNC_NAME "DrawButton"
  register int type = bf->style & ButtonFaceTypeMask;

  scwm_image *image;
  int border = 0;
  unsigned int width, height;
  int x, y;


  switch (type) {
  case SimpleButton:
    break;

  case SolidButton:
    XSetWindowBackground(dpy, win, XCOLOR(bf->u.back));
    flush_expose(win);
    XClearWindow(dpy, win);
    break;


    /* GJB:FIXME: using an object property below -- would prefer not doing that */
  case VectorButton:
    if ((psw->fMWMButtons)
	&& (stateflags & MWMButton)
	&& (SCM_NFALSEP
	    (scm_object_property
	     (SCM_FROM_PSW(psw), sym_maximized))))
      DrawLinePattern(win,
		      ShadowGC, ReliefGC,
		      &bf->vector,
		      w, h);
    else
      DrawLinePattern(win,
		      ReliefGC, ShadowGC,
		      &bf->vector,
		      w, h);
    break;

  case MiniIconButton:
  case PixmapButton:
    if (type == PixmapButton)
      image = SAFE_IMAGE(bf->u.image);
    else {
      if (psw->mini_icon_image==SCM_BOOL_F)
	break;
      image = SAFE_IMAGE(psw->mini_icon_image);
    }
    if (image == NULL) {
      scwm_msg(WARN,FUNC_NAME,"image is NULL");
      return;
    }
    if (bf->style & FlatButton)
      border = 0;
    else
      border = psw->fMWMBorders ? 1 : 2;
    width  = w - border * 2;
    height = h - border * 2;

    x = border;
    if (bf->style & HOffCenter) {
      if (bf->style & HRight)
	x += (int) (width - image->width);
    } else
      x += (int) (width - image->width) / 2;

    y = border;
    if (bf->style & VOffCenter) {
      if (bf->style & VBottom)
	y += (int) (height - image->height);
    } else
      y += (int) (height - image->height) / 2;

    if (x < border)
      x = border;
    if (y < border)
      y = border;
    if (width > image->width)
      width = image->width;
    if (height > image->height)
      height = image->height;
    if ((int) width > w - x - border)
      width = w - x - border;
    if ((int) height > h - y - border)
      height = h - y - border;

    XSetClipMask(dpy, TransMaskGC, image->mask);
    XSetClipOrigin(dpy, TransMaskGC, x, y);
    XCopyArea(dpy, image->image, win, TransMaskGC,
	      0, 0, width, height, x, y);
    break;

  case TiledPixmapButton:
    XSetWindowBackgroundPixmap(dpy, win, IMAGE(bf->u.image)->image);
    flush_expose(win);
    XClearWindow(dpy, win);
    break;

  case HGradButton:
  case VGradButton:
    {
      XRectangle bounds;

      bounds.x = bounds.y = 0;
      bounds.width = w;
      bounds.height = h;
      flush_expose(win);

      XSetClipMask(dpy, TransMaskGC, None);
      if (type == HGradButton) {
	register int i = 0, dw = bounds.width
	/ bf->u.grad.npixels + 1;

	while (i < bf->u.grad.npixels) {
	  unsigned short x = i * bounds.width / bf->u.grad.npixels;

	  XSetForeground(dpy, TransMaskGC, bf->u.grad.pixels[i++]);
	  XFillRectangle(dpy, win, TransMaskGC,
			 bounds.x + x, bounds.y,
			 dw, bounds.height);
	}
      } else {
	register int i = 0, dh = bounds.height
	/ bf->u.grad.npixels + 1;

	while (i < bf->u.grad.npixels) {
	  unsigned short y = i * bounds.height / bf->u.grad.npixels;

	  XSetForeground(dpy, TransMaskGC, bf->u.grad.pixels[i++]);
	  XFillRectangle(dpy, win, TransMaskGC,
			 bounds.x, bounds.y + y,
			 bounds.width, dh);
	}
      }
    }
    break;

  default:
    scwm_msg(ERR, "DrawButton", "unknown button type");
    break;
  }
}
#undef FUNC_NAME

/* SRL:FIXME:: The relief drawing code is hacked up and needs to be refactored
   and possibly rewritten. */

/****************************************************************************
 *
 *  Draws the relief pattern around a window for HiddenHandle borders
 *
 *  (veliaa@rpi.edu)
 *
 ****************************************************************************/
static void 
RelieveWindowHH(ScwmWindow *psw, Window win,
		int x, int y, int w, int h,
		GC ReliefGC, GC ShadowGC,
		int draw, int highlight)
{
#define FUNC_NAME "RelieveWindowHH"
  XSegment seg[4];
  int i = 0;
  int edge = 0, a = 0, b = 0;

  if (win == psw->sides[0]) {
    edge = 5;
    b = 1;
  } else if (win == psw->sides[1]) {
    a = 1;
    edge = 6;
  } else if (win == psw->sides[2]) {
    edge = 7;
    b = 1;
  } else if (win == psw->sides[3]) {
    edge = 8;
    a = 1;
  } else if (win == psw->corners[0])
    edge = 1;
  else if (win == psw->corners[1])
    edge = 2;
  else if (win == psw->corners[2])
    edge = 3;
  else if (win == psw->corners[3])
    edge = 4;

  DBUG((DBG,FUNC_NAME,"edge = %d",edge));

  if (draw & TOP_HILITE) {
    seg[i  ].x1 = x;
    seg[i  ].y1 = y;
    seg[i  ].x2 = w + x - 1;
    seg[i++].y2 = y;

    /* SRL:FIXME::edge < 1 is the same as edge==0 since edge can't be negative
       and therefore the first test is entirely redundant with the second.
       Also, the way this code is called edge can't be equal to 0. */
    if (((psw->boundary_width > 2) || (edge == 0)) &&
	((psw->boundary_width > 3) || (edge <  1)) &&
	(!psw->fMWMBorders ||
         (((edge == 0) || (psw->boundary_width > 3)) && (highlight & TOP_HILITE)))) {
      seg[i  ].x1 = x + ((edge == 2) || b ? 0 : 1);
      seg[i  ].y1 = y + 1;
      seg[i  ].x2 = x + w - 1 - ((edge == 1) || b ? 0 : 1);
      seg[i++].y2 = y + 1;
    }
  }
  if (draw & LEFT_HILITE) {
    seg[i  ].x1 = x;
    seg[i  ].y1 = y;
    seg[i  ].x2 = x;
    seg[i++].y2 = h + y - 1;

    if (((psw->boundary_width > 2) || (edge == 0)) &&
	((psw->boundary_width > 3) || (edge < 1)) &&
	(!psw->fMWMBorders ||
	 (((edge == 0) || 
	   (psw->boundary_width > 3)) && (highlight & LEFT_HILITE)))) {
      seg[i  ].x1 = x + 1;
      seg[i  ].y1 = y + ((edge == 3) || a ? 0 : 1);
      seg[i  ].x2 = x + 1;
      seg[i++].y2 = y + h - 1 - ((edge == 1) || a ? 0 : 1);
    }
  }
  DBUG((DBG,FUNC_NAME,"i = %d",i));
  XDrawSegments(dpy, win, ReliefGC, seg, i);

  i = 0;

  if (draw & BOTTOM_HILITE) {
    seg[i  ].x1 = x;
    seg[i  ].y1 = y + h - 1;
    seg[i  ].x2 = w + x - 1;
    seg[i++].y2 = y + h - 1;

    if (((psw->boundary_width > 2) || (edge == 0)) &&
	(!psw->fMWMBorders ||
	 (((edge == 0) || (psw->boundary_width > 3)) && (highlight & BOTTOM_HILITE)))) {
      seg[i  ].x1 = x + (b || (edge == 4) ? 0 : 1);
      seg[i  ].y1 = y + h - 2;
      seg[i  ].x2 = x + w - ((edge == 3) ? 0 : 1);
      seg[i++].y2 = y + h - 2;
    }
  }
  if (draw & RIGHT_HILITE) {
    seg[i  ].x1 = x + w - 1;
    seg[i  ].y1 = y;
    seg[i  ].x2 = x + w - 1;
    seg[i++].y2 = y + h - 1;

    if (((psw->boundary_width > 2) || (edge == 0)) &&
	(!psw->fMWMBorders ||
         (((edge == 0) || (psw->boundary_width > 3)) && (highlight & RIGHT_HILITE)))) {
      seg[i  ].x1 = x + w - 2;
      seg[i  ].y1 = y + (a || (edge == 4) ? 0 : 1);
      seg[i  ].x2 = x + w - 2;
      seg[i++].y2 = y + h - 1 - ((edge == 2) || a ? 0 : 1);
    }
  }
  DBUG((DBG,FUNC_NAME,"i = %d",i));
  XDrawSegments(dpy, win, ShadowGC, seg, i);
}
#undef FUNC_NAME

static void 
RelieveParts(ScwmWindow *psw, int i, GC hor, GC vert)
{
  XSegment seg[2];
  int n = 0, hh = i & HH_HILITE;

  i &= FULL_HILITE;

  if (psw->fMWMBorders || (psw->boundary_width < 3)) {
    switch (i) {
    case 0:
      seg[0].x1 = psw->xboundary_width - 1;
      seg[0].x2 = psw->corner_width;
      seg[0].y1 = psw->boundary_width - 1;
      seg[0].y2 = psw->boundary_width - 1;
      n = 1;
      break;
    case 1:
      seg[0].x1 = 0;
      seg[0].x2 = psw->corner_width - psw->xboundary_width /* -1 */ ;
      seg[0].y1 = psw->boundary_width - 1;
      seg[0].y2 = psw->boundary_width - 1;
      n = 1;
      break;
    case 2:
      seg[0].x1 = psw->xboundary_width - 1;
      seg[0].x2 = psw->corner_width - (hh ? 1 : 2);
      seg[0].y1 = psw->corner_width - psw->boundary_width + psw->bw;
      seg[0].y2 = psw->corner_width - psw->boundary_width + psw->bw;
      n = 1;
      break;
    case 3:
      seg[0].x1 = 0;
      seg[0].x2 = psw->corner_width - (psw->xboundary_width ? psw->xboundary_width : 2);
      seg[0].y1 = psw->corner_width - psw->boundary_width + psw->bw;
      seg[0].y2 = psw->corner_width - psw->boundary_width + psw->bw;
      n = 1;
      break;
    }
    XDrawSegments(dpy, psw->corners[i], hor, seg, n);
    switch (i) {
    case 0:
      seg[0].y1 = psw->boundary_width - 1;
      seg[0].y2 = psw->corner_width;
      seg[0].x1 = psw->xboundary_width - 1;
      seg[0].x2 = psw->xboundary_width - 1;
      n = 1;
      break;
    case 1:
      seg[0].y1 = psw->boundary_width - 1;
      seg[0].y2 = psw->corner_width - (hh ? 0 : 2);
      seg[0].x1 = psw->corner_width - psw->xboundary_width;
      seg[0].x2 = psw->corner_width - psw->xboundary_width;
      n = 1;
      break;
    case 2:
      seg[0].y1 = 0;
      seg[0].y2 = psw->corner_width - psw->boundary_width;
      seg[0].x1 = psw->xboundary_width - 1;
      seg[0].x2 = psw->xboundary_width - 1;
      n = 1;
      break;
    case 3:
      seg[0].y1 = 0;
      seg[0].y2 = psw->corner_width - psw->boundary_width + psw->bw;
      seg[0].x1 = psw->corner_width - psw->xboundary_width;
      seg[0].x2 = psw->corner_width - psw->xboundary_width;
      n = 1;
      break;
    }
    XDrawSegments(dpy, psw->corners[i], vert, seg, 1);
  } else {
    switch (i) {
    case 0:
      seg[0].x1 = psw->xboundary_width ? psw->xboundary_width - 2 : 1;
      seg[0].x2 = psw->corner_width;
      seg[0].y1 = psw->boundary_width - 2;
      seg[0].y2 = psw->boundary_width - 2;

      seg[1].x1 = psw->xboundary_width - 2;
      seg[1].x2 = psw->corner_width;
      seg[1].y1 = psw->boundary_width - 1;
      seg[1].y2 = psw->boundary_width - 1;
      n = 2;
      break;
    case 1:
      seg[0].x1 = (hh ? 0 : 1);
      seg[0].x2 = psw->corner_width - psw->xboundary_width;
      seg[0].y1 = psw->boundary_width - 2;
      seg[0].y2 = psw->boundary_width - 2;

      seg[1].x1 = 0;
      seg[1].x2 = psw->corner_width - psw->xboundary_width - 1;
      seg[1].y1 = psw->boundary_width - 1;
      seg[1].y2 = psw->boundary_width - 1;
      n = 2;
      break;
    case 2:
      seg[0].x1 = psw->xboundary_width ? psw->xboundary_width - 1 : 0;
      seg[0].x2 = psw->corner_width - (hh ? 1 : 2);
      seg[0].y1 = psw->corner_width - psw->boundary_width;
      seg[0].y2 = psw->corner_width - psw->boundary_width;
      n = 1;
      if (psw->boundary_width > 3) {
	seg[1].x1 = psw->xboundary_width ? psw->xboundary_width - 2 : 0;
	seg[1].x2 = psw->corner_width - (hh ? 1 : 3);
	seg[1].y1 = psw->corner_width - psw->boundary_width + 1;
	seg[1].y2 = psw->corner_width - psw->boundary_width + 1;
	n = 2;
      }
      break;
    case 3:
      seg[0].x1 = 0;
      seg[0].x2 = psw->corner_width - 
        (psw->xboundary_width ? psw->xboundary_width : 2);

      seg[0].y1 = psw->corner_width - psw->boundary_width;
      seg[0].y2 = psw->corner_width - psw->boundary_width;
      n = 1;
      if (psw->boundary_width > 3) {
	seg[0].x2 = psw->corner_width - 
          (psw->xboundary_width ? psw->xboundary_width + 1 : 2);

	seg[1].x1 = 0;
	seg[1].x2 = psw->corner_width - 
          (psw->xboundary_width ? psw->xboundary_width + 1 : 3);
	seg[1].y1 = psw->corner_width - psw->boundary_width + 1;
	seg[1].y2 = psw->corner_width - psw->boundary_width + 1;
	n = 2;
      }
      break;
    }
    XDrawSegments(dpy, psw->corners[i], hor, seg, n);
    switch (i) {
    case 0:
      seg[0].y1 = psw->boundary_width - 2;
      seg[0].y2 = psw->corner_width;
      seg[0].x1 = psw->xboundary_width - 2;
      seg[0].x2 = psw->xboundary_width - 2;

      seg[1].y1 = psw->boundary_width - 2;
      seg[1].y2 = psw->corner_width;
      seg[1].x1 = psw->xboundary_width - 1;
      seg[1].x2 = psw->xboundary_width - 1;
      n = 2;
      break;
    case 1:
      seg[0].y1 = psw->boundary_width - 1;
      seg[0].y2 = psw->corner_width - (hh ? 1 : 2);
      seg[0].x1 = psw->corner_width - psw->xboundary_width;
      seg[0].x2 = psw->corner_width - psw->xboundary_width;
      n = 1;
      if (psw->boundary_width > 3) {
	seg[1].y1 = psw->boundary_width - 2;
	seg[1].y2 = psw->corner_width - (hh ? 1 : 3);
	seg[1].x1 = psw->corner_width - psw->xboundary_width + 1;
	seg[1].x2 = psw->corner_width - psw->xboundary_width + 1;
	n = 2;
      }
      break;
    case 2:
      seg[0].y1 = (hh ? 0 : 1);
      seg[0].y2 = psw->corner_width - psw->boundary_width + 1;
      seg[0].x1 = psw->xboundary_width - 2;
      seg[0].x2 = psw->xboundary_width - 2;
      n = 1;

      if (psw->boundary_width > 3) {
	seg[1].y1 = 0;
	seg[1].y2 = psw->corner_width - psw->boundary_width;
	seg[1].x1 = psw->xboundary_width - 1;
	seg[1].x2 = psw->xboundary_width - 1;
	n=2;
      }
      break;
    case 3:
      seg[0].y1 = 0;
      seg[0].y2 = psw->corner_width - psw->boundary_width;
      seg[0].x1 = psw->corner_width - psw->xboundary_width;
      seg[0].x2 = psw->corner_width - psw->xboundary_width;
      n = 1;

      if (psw->boundary_width > 3) {
	seg[0].y2 = psw->corner_width - psw->boundary_width + 1;
	seg[1].y1 = 0;
	seg[1].y2 = psw->corner_width - psw->boundary_width + 1;
	seg[1].x1 = psw->corner_width - psw->xboundary_width + 1;
	seg[1].x2 = psw->corner_width - psw->xboundary_width + 1;
	n = 2;
      }
      break;
    }
    XDrawSegments(dpy, psw->corners[i], vert, seg, n);
  }
}

/* Return a window-specific highlight foreground color,
   or use the decors if no window-specific one has been set */
static Pixel
PixelHiTextFromPsw(const ScwmWindow *psw)
{
  if (UNSET_SCM(psw->HiTextColor) || !COLOR_P(psw->HiTextColor))
    return XCOLOR(GET_DECOR(psw, HiColors.fg));
  else return XCOLOR(psw->HiTextColor);
}


/* Return a window-specific highlight background color,
   or use the decors if no window-specific one has been set */
static Pixel
PixelHiBackFromPsw(const ScwmWindow *psw)
{
  if (UNSET_SCM(psw->HiBackColor) || !COLOR_P(psw->HiBackColor))
    return XCOLOR(GET_DECOR(psw, HiColors.bg));
  else return XCOLOR(psw->HiBackColor);
}


int
CLeftButtons(const ScwmWindow *psw)
{
  unsigned long buttons = psw->buttons;
  int c = 0;
  int i = 0;
  while (i < Scr.nr_left_buttons) {
    /* bitmask is for suppressing buttons, 
       so if the bit is off, we want to display the button */
    if ((buttons & 1) == 0 && psw->left_w[i] != None) {
      ++c;
    }
    ++i;
    buttons >>= 2;
  }
  DBUG((DBG,"CLeftButtons","buttons = %ld",buttons));
  DBUG((DBG,"CLeftButtons","Returning %d instead of %d",c,Scr.nr_left_buttons));
  return c;
}

int
CRightButtons(const ScwmWindow *psw)
{
  unsigned long buttons = psw->buttons;
  int c = 0;
  int i = 0;
  buttons >>= 1;
  while (i < Scr.nr_right_buttons) {
    /* bitmask is for suppressing buttons, 
       so if the bit is off, we want to display the button */
    if ((buttons & 1) == 0 && psw->right_w[i] != None) {
      ++c;
    }
    buttons >>= 2;
    ++i;
  }
  DBUG((DBG,"CRightButtons","buttons = %ld",buttons));
  DBUG((DBG,"CRightButtons","Returning %d instead of %d",c,Scr.nr_right_buttons));
  return c;
}

/* Set Border just calls SetBorderX with really_force == False; .*/
void
SetBorder(ScwmWindow *psw, Bool fHighlightOn, Bool force, Bool Mapped,
	  Window expose_win)
{
  SetBorderX(psw, fHighlightOn, force, Mapped, expose_win, False);
}

/* This sets Scr.Hilite to psw;  do not set it prematurely, as
   then this code will opt out of the border redraw as an optimization */
void 
SetBorderX(ScwmWindow *psw, Bool fHighlightOn, Bool force, Bool Mapped,
	   Window expose_win, Bool really_force)
{
  int y, i, x;
  GC ReliefGC, ShadowGC;
  Pixel BorderColor, BackColor;
  Pixmap BackPixmap, TextColor;

  Pixmap TexturePixmap = None;
  XSetWindowAttributes notex_attributes;
  unsigned long notex_valuemask;

  Bool NewColor = False;
  XSetWindowAttributes attributes;
  unsigned long valuemask;
  static unsigned int corners[4];
  const Bool fSquashedTitlebar = psw? psw->fSquashedTitlebar: FALSE;
  Window w;

  if (!psw)
    return;

  corners[0] = TOP_HILITE    | LEFT_HILITE;
  corners[1] = TOP_HILITE    | RIGHT_HILITE;
  corners[2] = BOTTOM_HILITE | LEFT_HILITE;
  corners[3] = BOTTOM_HILITE | RIGHT_HILITE;

  if (fHighlightOn) {
    /* don't re-draw just for kicks */
    if (!force && (Scr.Hilite == psw))
      return;

    if (Scr.Hilite != psw || really_force)
      NewColor = True;

    /* make sure that the previously highlighted window got unhighlighted */
    if ((Scr.Hilite != psw) && (Scr.Hilite != NULL))
      SetBorder(Scr.Hilite, False, False, True, None);

    /* are we using textured borders? */
    if ((GET_DECOR(psw, BorderStyle.active->style)
	 & ButtonFaceTypeMask) == TiledPixmapButton) {
      SCM scmImage = GET_DECOR(psw, BorderStyle.active->u.image);
      if (IMAGE_P(scmImage)) {
        scwm_image *simage = IMAGE(scmImage);
	if (simage)
	  TexturePixmap = simage->image;
      }
    }

    /* set the keyboard focus */
    if (Mapped && psw->fMapped && (Scr.Hilite != psw))
      w = psw->w;
    else if (psw->fIconified && (Scr.Hilite != psw) && !psw->fSuppressIcon)
      w = psw->icon_w;
    Scr.Hilite = psw;
    Scr.Focus  = psw;

    TextColor   = PixelHiTextFromPsw(psw);
    BackPixmap = Scr.gray_pixmap;
    BackColor   = PixelHiBackFromPsw(psw);
    ReliefGC    = GET_DECOR(psw, HiReliefGC);
    ShadowGC    = GET_DECOR(psw, HiShadowGC);
    BorderColor = XCOLOR(GET_DECOR(psw, HiRelief.bg));
  } else /* this case for !fHighlightOn */ {
    /* don't re-draw just for kicks */
    if (!force && (Scr.Hilite != psw))
      return;

    if (Scr.Hilite == psw) {
      Scr.Hilite = NULL;
      NewColor = True;
    }
    if (really_force) {
      NewColor = True;
    }
    if ((GET_DECOR(psw, BorderStyle.inactive->style)
	 & ButtonFaceTypeMask) == TiledPixmapButton) {
      SCM img = GET_DECOR(psw, BorderStyle.inactive->u.image);
      if (IMAGE_P(img))
        TexturePixmap = IMAGE(img)->image;
    }

    TextColor = XCOLOR(psw->TextColor);
    BackPixmap = Scr.light_gray_pixmap;
    if (psw->fSticky)
      BackPixmap = Scr.sticky_gray_pixmap;
    BackColor = XCOLOR(psw->BackColor);
    SetGCFg(ReliefGC = Scr.ScratchGC1,XCOLOR(psw->ReliefColor));
    SetGCFg(ShadowGC = Scr.ScratchGC2,XCOLOR(psw->ShadowColor));
    BorderColor = XCOLOR(psw->ShadowColor);
  }

  if (psw->fIconified) {
    DrawIconWindow(psw);
    return;
  }

  valuemask =
    notex_valuemask =
    CWBorderPixel;

  attributes.border_pixel =
    notex_attributes.border_pixel =
    BorderColor;

  if (TexturePixmap) {
    attributes.background_pixmap = TexturePixmap;
    valuemask |= CWBackPixmap;
    if (Scr.d_depth < 2) {
      notex_attributes.background_pixmap = BackPixmap;
      notex_valuemask |= CWBackPixmap;
    } else {
      notex_attributes.background_pixel = BackColor;
      notex_valuemask |= CWBackPixel;
    }
  } else {
    /* !TexturePixmap */
    if (Scr.d_depth < 2) {
      attributes.background_pixmap = BackPixmap;
      valuemask |= CWBackPixmap;
      notex_attributes.background_pixmap = BackPixmap;
      notex_valuemask |= CWBackPixmap;
    } else {
      attributes.background_pixel = BackColor;
      valuemask |= CWBackPixel;
      notex_attributes.background_pixel = BackColor;
      notex_valuemask |= CWBackPixel;
    }
  }

  if (psw->fBorder || SHOW_TITLE_P(psw)) {
    XSetWindowBorder(dpy, psw->Parent, BorderColor);
    XSetWindowBorder(dpy, psw->frame,  BorderColor);
  }

  if (SHOW_TITLE_P(psw)) {
    unsigned long buttons = psw->buttons;
    ChangeWindowColor(psw->title_w, valuemask);
    for (i = 0; i < Scr.nr_left_buttons; ++i) {
      if (psw->left_w[i] != None && !(buttons & (1 << (i*2)))) {
	enum ButtonState bs = GetButtonState(psw->left_w[i]);
	ButtonFace *bf = GET_DECOR(psw, left_buttons[i].state[bs]);

	if (flush_expose(psw->left_w[i]) || (expose_win == psw->left_w[i]) ||
	    (expose_win == None) || NewColor) {
	  int inverted = PressedW == psw->left_w[i];

	  if (bf->style & UseBorderStyle)
	    XChangeWindowAttributes(dpy, psw->left_w[i],
				    valuemask, &attributes);
	  else
	    XChangeWindowAttributes(dpy, psw->left_w[i],
				    notex_valuemask, &notex_attributes);
	  XClearWindow(dpy, psw->left_w[i]);
	  if (bf->style & UseTitleStyle) {
	    ButtonFace *tsbf = GET_DECOR(psw, titlebar.state[bs]);

	    for (; tsbf; tsbf = tsbf->next)
	      DrawButton(psw, psw->left_w[i],
			 psw->title_height, psw->title_height,
			 tsbf, ReliefGC, ShadowGC,
			 inverted, GET_DECOR(psw, left_buttons[i].flags));
	  }
	  for (; bf; bf = bf->next)
	    DrawButton(psw, psw->left_w[i],
		       psw->title_height, psw->title_height,
		       bf, ReliefGC, ShadowGC,
		       inverted, GET_DECOR(psw, left_buttons[i].flags));

	  if (!(GET_DECOR(psw, left_buttons[i].state[bs]->style) & FlatButton)) {
	    if (GET_DECOR(psw, left_buttons[i].state[bs]->style) & SunkButton)
	      RelieveWindow(psw, psw->left_w[i], 0, 0,
			    psw->title_height, psw->title_height,
			    (inverted ? ReliefGC : ShadowGC),
			    (inverted ? ShadowGC : ReliefGC),
			    BOTTOM_HILITE);
	    else
	      RelieveWindow(psw, psw->left_w[i], 0, 0,
			    psw->title_height, psw->title_height,
			    (inverted ? ShadowGC : ReliefGC),
			    (inverted ? ReliefGC : ShadowGC),
			    BOTTOM_HILITE);
	  }
	}
      }
    }
    for (i = 0; i < Scr.nr_right_buttons; ++i) {
      if (psw->right_w[i] != None && !(buttons & (1 << (i*2+1)))) {
	enum ButtonState bs = GetButtonState(psw->right_w[i]);
	ButtonFace *bf = GET_DECOR(psw, right_buttons[i].state[bs]);

	if (flush_expose(psw->right_w[i]) || (expose_win == psw->right_w[i]) ||
	    (expose_win == None) || NewColor ) {
	  int inverted = PressedW == psw->right_w[i];

	  if (bf->style & UseBorderStyle)
	    XChangeWindowAttributes(dpy, psw->right_w[i],
				    valuemask, &attributes);
	  else
	    XChangeWindowAttributes(dpy, psw->right_w[i],
				    notex_valuemask, &notex_attributes);
	  XClearWindow(dpy, psw->right_w[i]);
	  if (bf->style & UseTitleStyle) {
	    ButtonFace *tsbf = GET_DECOR(psw, titlebar.state[bs]);

	    for (; tsbf; tsbf = tsbf->next)
	      DrawButton(psw, psw->right_w[i],
			 psw->title_height, psw->title_height,
			 tsbf, ReliefGC, ShadowGC,
			 inverted, GET_DECOR(psw, right_buttons[i].flags));
	  }
	  for (; bf; bf = bf->next)
	    DrawButton(psw, psw->right_w[i],
		       psw->title_height, psw->title_height,
		       bf, ReliefGC, ShadowGC,
		       inverted, GET_DECOR(psw, right_buttons[i].flags));

	  if (!(GET_DECOR(psw, right_buttons[i].state[bs]->style) & FlatButton)) {
	    if (GET_DECOR(psw, right_buttons[i].state[bs]->style) & SunkButton)
	      RelieveWindow(psw, psw->right_w[i], 0, 0,
			    psw->title_height, psw->title_height,
			    (inverted ? ReliefGC : ShadowGC),
			    (inverted ? ShadowGC : ReliefGC),
			    BOTTOM_HILITE);
	    else
	      RelieveWindow(psw, psw->right_w[i], 0, 0,
			    psw->title_height, psw->title_height,
			    (inverted ? ShadowGC : ReliefGC),
			    (inverted ? ReliefGC : ShadowGC),
			    BOTTOM_HILITE);
	  }
	}
      }
    }
    SetTitleBar(psw, fHighlightOn, False);

  }
  if (psw->fBorder) {
    /* draw relief lines */
    y = FRAME_HEIGHT(psw) - 2 * psw->corner_width;
    x = FRAME_WIDTH (psw) - 2 * psw->corner_width + psw->bw;

    for (i = 0; i < 4; i++) {
      int vertical  = i % 2;
      int northeast = i / 2;

      int flags = fHighlightOn
        ? GET_DECOR(psw, BorderStyle.active  ->style)
        : GET_DECOR(psw, BorderStyle.inactive->style);
      int top_side_relief_offset_when_squashed = 
        psw->tbar_right - psw->corner_width - psw->xboundary_width;

      int hn = psw->highlighted_nonant;

      if ((((hn == 1) || (hn == SCWM_NONANT_TOP   ) || (hn == SCWM_NONANT_HCENTER)) && i == 0) ||
          (((hn == 3) || (hn == SCWM_NONANT_LEFT  ) || (hn == SCWM_NONANT_VMIDDLE)) && i == 3) ||
          (((hn == 5) || (hn == SCWM_NONANT_RIGHT ) || (hn == SCWM_NONANT_VMIDDLE)) && i == 1) ||
          (((hn == 7) || (hn == SCWM_NONANT_BOTTOM) || (hn == SCWM_NONANT_HCENTER)) && i == 2)) {
        XSetWindowBackground(dpy,psw->sides[i],XCOLOR(Scr.nonant_highlight_color));
        XClearWindow(dpy,psw->sides[i]);
      } else {
        ChangeWindowColor(psw->sides[i], valuemask);
      }
      if ((flush_expose(psw->sides[i])) || (expose_win == psw->sides[i]) ||
	  (expose_win == None)) {
	GC sgc, rgc;

	sgc = ShadowGC;
	rgc = ReliefGC;
	if (!psw->fMWMButtons && (PressedW == psw->sides[i])) {
	  sgc = ReliefGC;
	  rgc = ShadowGC;
	}
	/* index    side
	 * 0        TOP
	 * 1        RIGHT
	 * 2        BOTTOM
	 * 3        LEFT
	 */

	if (flags & HiddenHandles) {
	  if (flags & NoInset) {
            /* no inset, hidden handles */
	    RelieveWindowHH(psw, psw->sides[i], 
                            (i==0 && fSquashedTitlebar) ? top_side_relief_offset_when_squashed : 0,
                            0,
			    (vertical ? psw->boundary_width : x),
			    (vertical ? y : psw->boundary_width),
			    rgc, sgc,
                            vertical
			    ? (i == 3 ? LEFT_HILITE   : RIGHT_HILITE)
			    : (i      ? BOTTOM_HILITE : TOP_HILITE),
			    (0x0001 << i));
	  } else {
            /* with inset, hidden handles */
	    RelieveWindowHH(psw, psw->sides[i], 
                            (i==0 && fSquashedTitlebar) ? top_side_relief_offset_when_squashed : 0,
                            0,
			    (vertical ? psw->boundary_width : x),
			    (vertical ? y : psw->boundary_width),
			    rgc, sgc,
                            vertical
			    ? (LEFT_HILITE | RIGHT_HILITE)
			    : (TOP_HILITE  | BOTTOM_HILITE),
			    (0x0001 << i));
	  }
	} else {
          /* not hidden handles (visible handles) */
	  RelieveWindow(psw, psw->sides[i], 
                        (i==0 && fSquashedTitlebar) ? top_side_relief_offset_when_squashed : 0,
                        0,
			(vertical ? psw->boundary_width : x),
			(vertical ? y : psw->boundary_width),
			rgc, sgc, (0x0001 << i));
	}
      }
      if ((((hn == 0) || (hn == SCWM_NONANT_TOP   ) || (hn == SCWM_NONANT_LEFT )) && i == 0) ||
          (((hn == 2) || (hn == SCWM_NONANT_TOP   ) || (hn == SCWM_NONANT_RIGHT)) && i == 1) ||
          (((hn == 6) || (hn == SCWM_NONANT_BOTTOM) || (hn == SCWM_NONANT_LEFT )) && i == 2) ||
          (((hn == 8) || (hn == SCWM_NONANT_BOTTOM) || (hn == SCWM_NONANT_RIGHT)) && i == 3)) {
        XSetWindowBackground(dpy,psw->corners[i],XCOLOR(Scr.nonant_highlight_color));
        XClearWindow(dpy,psw->corners[i]);
      } else {
        ChangeWindowColor(psw->corners[i], valuemask);
      }
      if ((flush_expose(psw->corners[i])) || (expose_win == psw->corners[i]) ||
	  (expose_win == None)) {
	GC rgc, sgc;

	rgc = ReliefGC;
	sgc = ShadowGC;
	if (!psw->fMWMButtons && (PressedW == psw->corners[i])) {
	  sgc = ReliefGC;
	  rgc = ShadowGC;
	}
	if (flags & HiddenHandles) {
	  RelieveWindowHH(psw, psw->corners[i], 0, 0,
                          psw->corner_width,
                          (northeast ? psw->corner_width + psw->bw : psw->corner_width),
			  rgc, sgc, corners[i], corners[i]);

	  if (!(flags & NoInset)) {
	    if (psw->boundary_width > 1)
	      RelieveParts(psw, i | HH_HILITE,
			   (northeast ? rgc : sgc), (vertical ? rgc : sgc));
	    else if (psw->boundary_width > 0)
	      RelieveParts(psw, i | HH_HILITE,sgc,sgc);
          }
	} else {
	  RelieveWindow(psw, psw->corners[i], 0, 0,
                        psw->corner_width,
                        (northeast ? psw->corner_width + psw->bw : psw->corner_width),
			rgc, sgc, corners[i]);
	  if (psw->boundary_width > 1)
	    RelieveParts(psw, i, (northeast ? rgc : sgc), (vertical ? rgc : sgc));
	  else if (psw->boundary_width > 0)
	    RelieveParts(psw, i, sgc,sgc);
	}
      }
    }
  } else {			/* no decorative border */
    /* for mono - put a black border on 
     * for color, make it the color of the decoration background */
    if (psw->boundary_width < 2) {
      flush_expose(psw->frame);
      if (Scr.d_depth < 2) {
	XSetWindowBorder(dpy, psw->frame,  TextColor);
	XSetWindowBorder(dpy, psw->Parent, TextColor);
	XSetWindowBackgroundPixmap(dpy, psw->frame,  BackPixmap);
	XClearWindow(dpy, psw->frame);
	XSetWindowBackgroundPixmap(dpy, psw->Parent, BackPixmap);
	XClearWindow(dpy, psw->Parent);
      } else {
	XSetWindowBackgroundPixmap(dpy, psw->frame, TexturePixmap);
	XSetWindowBorder(dpy, psw->frame,  BorderColor);
	XClearWindow(dpy, psw->frame);
	XSetWindowBackground(dpy, psw->Parent, BorderColor);
	XSetWindowBorder(dpy, psw->Parent, BorderColor);
	XClearWindow(dpy, psw->Parent);
	XSetWindowBorder(dpy, psw->w,      BorderColor);
      }
    } else {
      GC rgc, sgc;

      XSetWindowBorder(dpy, psw->Parent, BorderColor);
      XSetWindowBorder(dpy, psw->frame,  BorderColor);

      rgc = ReliefGC;
      sgc = ShadowGC;
      if (!psw->fMWMButtons && (PressedW == psw->frame)) {
	sgc = ReliefGC;
	rgc = ShadowGC;
      }
      ChangeWindowColor(psw->frame, valuemask);
      if ((flush_expose(psw->frame)) || (expose_win == psw->frame) ||
	  (expose_win == None)) {
	if (psw->boundary_width > 2) {
	  RelieveWindow(psw, psw->frame,
                        psw->boundary_width - 1 - psw->bw,
			psw->boundary_width - 1 - psw->bw,
			FRAME_WIDTH (psw) -
			(psw->boundary_width * 2) + 2 + 3 * psw->bw,
			FRAME_HEIGHT(psw) -
			(psw->boundary_width * 2) + 2 + 3 * psw->bw,
			sgc, rgc,
			TOP_HILITE   | LEFT_HILITE | 
                        RIGHT_HILITE | BOTTOM_HILITE);
	  RelieveWindow(psw, psw->frame, 0, 0,
                        FRAME_WIDTH (psw) + psw->bw,
			FRAME_HEIGHT(psw) + psw->bw, rgc, sgc,
			TOP_HILITE   | LEFT_HILITE | 
                        RIGHT_HILITE | BOTTOM_HILITE);
	} else {
	  RelieveWindow(psw, psw->frame, 0, 0, FRAME_WIDTH(psw) + psw->bw,
			FRAME_HEIGHT(psw) + psw->bw, rgc, rgc,
			TOP_HILITE   | LEFT_HILITE | 
                        RIGHT_HILITE | BOTTOM_HILITE);
	}
      } else {
	XSetWindowBackground(dpy, psw->Parent, BorderColor);
      }
    }
  }
  /* Sync to make the border-color change look fast! */
  XSync(dpy, False);
}


/*
 *  Redraws just the title bar
 */
void 
SetTitleBar(ScwmWindow *psw, Bool fHighlightOn, Bool ARG_UNUSED(NewTitle))
{
  /* GJB:FIXME:: NewTitle parameter is unused */
  int hor_off, w, i;
  enum ButtonState title_state;
  ButtonFaceStyle tb_style;
  int tb_flags;
  GC ReliefGC, ShadowGC, tGC;
  Pixel Forecolor, BackColor;

  if (!psw)
    return;
  if (!SHOW_TITLE_P(psw))
    return;

  if (fHighlightOn) {
    Forecolor = PixelHiTextFromPsw(psw);
    BackColor = PixelHiBackFromPsw(psw);
    ReliefGC = GET_DECOR(psw, HiReliefGC);
    ShadowGC = GET_DECOR(psw, HiShadowGC);
  } else {
    Forecolor = XCOLOR(psw->TextColor);
    BackColor = XCOLOR(psw->BackColor);
    SetGCFg(ReliefGC = Scr.ScratchGC1,XCOLOR(psw->ReliefColor));
    SetGCFg(ShadowGC = Scr.ScratchGC2,XCOLOR(psw->ShadowColor));
  }
  if (PressedW == psw->title_w) {
    tGC      = ShadowGC;
    ShadowGC = ReliefGC;
    ReliefGC = tGC;
  }
  flush_expose(psw->title_w);

  if (psw->name != (char *) NULL) {
    w = ComputeXTextWidth(XFONT(GET_DECOR(psw, window_font)), psw->name, -1);
    if (w > psw->title_width - 12)
      w = psw->title_width - 4;
    if (w < 0)
      w = 0;
  } else
    w = 0;

  title_state = GetButtonState(psw->title_w);
  tb_style = GET_DECOR(psw, titlebar.state[title_state]->style);
  tb_flags = GET_DECOR(psw, titlebar.flags);
  if (tb_flags & HOffCenter) {
    if (tb_flags & HRight)
      hor_off = psw->title_width - w - 10;
    else
      hor_off = 10;
  } else
    hor_off = (psw->title_width - w) / 2;

  NewFontAndColor(Scr.ScratchGC3,XFONTID(GET_DECOR(psw, window_font)), Forecolor, BackColor);

  /* the next bit tries to minimize redraw based upon compilation options (veliaa@rpi.edu) */
  /* we need to check for UseBorderStyle for the titlebar */
  { /* scope */
    ButtonFace *bf = fHighlightOn
      ? GET_DECOR(psw, BorderStyle.active)
      : GET_DECOR(psw, BorderStyle.inactive);

    if ((tb_style & UseBorderStyle)
	&& ((bf->style & ButtonFaceTypeMask) == TiledPixmapButton))
      XSetWindowBackgroundPixmap(dpy, psw->title_w, IMAGE(bf->u.image)->image);
  }
  XClearWindow(dpy, psw->title_w);

  /* for mono, we clear an area in the title bar where the window
   * title goes, so that its more legible. For color, no need */
  if (Scr.d_depth < 2) {
    RelieveWindow(psw, psw->title_w, 0, 0, hor_off - 2, psw->title_height,
		  ReliefGC, ShadowGC, BOTTOM_HILITE);
    RelieveWindow(psw, psw->title_w, hor_off + w + 2, 0,
		  psw->title_width - w - hor_off - 2, psw->title_height,
		  ReliefGC, ShadowGC, BOTTOM_HILITE);
    XFillRectangle(dpy, psw->title_w,
		   (PressedW == psw->title_w ? ShadowGC : ReliefGC),
		   hor_off - 2, 0, w + 4, psw->title_height);

    XDrawLine(dpy, psw->title_w, ShadowGC, hor_off + w + 1, 0, hor_off + w + 1,
	      psw->title_height);
    if (psw->name != (char *) NULL) 
#ifdef I18N
      XmbDrawString(dpy, psw->title_w,XFONT(GET_DECOR(psw,window_font)),
		    Scr.ScratchGC3, hor_off,
		    GET_DECOR(psw, window_font_y) + 1,
		    psw->name, strlen(psw->name));
#else
      XDrawString(dpy, psw->title_w, Scr.ScratchGC3, hor_off,
		  GET_DECOR(psw, window_font_y) + 1,
		  psw->name, strlen(psw->name));
#endif
  } else {
    ButtonFace *bf = GET_DECOR(psw, titlebar.state[title_state]);

    /* draw compound titlebar (veliaa@rpi.edu) */
    if (PressedW == psw->title_w) {
      for (; bf; bf = bf->next)
	DrawButton(psw, psw->title_w, psw->title_width, psw->title_height,
		   bf, ShadowGC, ReliefGC, True, 0);
    } else {
      for (; bf; bf = bf->next)
	DrawButton(psw, psw->title_w, psw->title_width, psw->title_height,
		   bf, ReliefGC, ShadowGC, False, 0);
    }

    if (!(tb_style & FlatButton)) {
      if (tb_style & SunkButton)
	RelieveWindow(psw, psw->title_w, 0, 0, psw->title_width, psw->title_height,
		      ShadowGC, ReliefGC, BOTTOM_HILITE);
      else
	RelieveWindow(psw, psw->title_w, 0, 0, psw->title_width, psw->title_height,
		      ReliefGC, ShadowGC, BOTTOM_HILITE);
    }
    if (psw->name != (char *) NULL) {
#ifdef I18N
      XmbDrawString(dpy, psw->title_w,XFONT(GET_DECOR(psw,window_font)),
		    Scr.ScratchGC3, hor_off,
		    GET_DECOR(psw, window_font_y) + 1,
		    psw->name, strlen(psw->name));
#else
      XDrawString(dpy, psw->title_w, Scr.ScratchGC3, hor_off,
		  GET_DECOR(psw, window_font_y) + 1,
		  psw->name, strlen(psw->name));
#endif
    }
  }
  /* now, draw lines in title bar if it's a sticky window */
  if (psw->fSticky) {
    for (i = 0; i < psw->title_height / 2 - 3; i += 4) {
      XDrawLine(dpy, psw->title_w, ShadowGC, 4, psw->title_height / 2 - i - 1,
		hor_off - 6, psw->title_height / 2 - i - 1);
      XDrawLine(dpy, psw->title_w, ShadowGC, 6 + hor_off + w, psw->title_height / 2 - i - 1,
		psw->title_width - 5, psw->title_height / 2 - i - 1);
      XDrawLine(dpy, psw->title_w, ReliefGC, 4, psw->title_height / 2 - i,
		hor_off - 6, psw->title_height / 2 - i);
      XDrawLine(dpy, psw->title_w, ReliefGC, 6 + hor_off + w, psw->title_height / 2 - i,
		psw->title_width - 5, psw->title_height / 2 - i);

      XDrawLine(dpy, psw->title_w, ShadowGC, 4, psw->title_height / 2 + i - 1,
		hor_off - 6, psw->title_height / 2 + i - 1);
      XDrawLine(dpy, psw->title_w, ShadowGC, 6 + hor_off + w, psw->title_height / 2 + i - 1,
		psw->title_width - 5, psw->title_height / 2 + i - 1);
      XDrawLine(dpy, psw->title_w, ReliefGC, 4, psw->title_height / 2 + i,
		hor_off - 6, psw->title_height / 2 + i);
      XDrawLine(dpy, psw->title_w, ReliefGC, 6 + hor_off + w, psw->title_height / 2 + i,
		psw->title_width - 5, psw->title_height / 2 + i);
    }
  }
  XFlush(dpy);
}




/****************************************************************************
 *
 *  Draws the relief pattern around a window
 *
 ****************************************************************************/
void 
RelieveWindow(ScwmWindow *psw, Window win,
	      int x, int y, int w, int h,
	      GC ReliefGC, GC ShadowGC, int highlight)
{
#define FUNC_NAME "RelieveWindow"
  XSegment seg[4];
  int i = 0;
  int edge = 0;

  DBUG((DBG,FUNC_NAME,"x = %d, y = %d;   w = %d, h = %d",x,y,w,h));

  if ((win == psw->sides[0]) || (win == psw->sides[1]) ||
      (win == psw->sides[2]) || (win == psw->sides[3]))
    edge = -1;
  else if (win == psw->corners[0])
    edge = 1;
  else if (win == psw->corners[1])
    edge = 2;
  else if (win == psw->corners[2])
    edge = 3;
  else if (win == psw->corners[3])
    edge = 4;


  DBUG((DBG,FUNC_NAME,"edge = %d",edge));

  seg[i  ].x1 = x;
  seg[i  ].y1 = y;
  seg[i  ].x2 = w + x - 1;
  seg[i++].y2 = y;

  seg[i  ].x1 = x;
  seg[i  ].y1 = y;
  seg[i  ].x2 = x;
  seg[i++].y2 = h + y - 1;

  if (((psw->boundary_width > 2) || (edge == 0)) &&
      ((psw->boundary_width > 3) || (edge < 1)) &&
      (!psw->fMWMBorders ||
       (((edge == 0) || (psw->boundary_width > 3)) && (highlight & TOP_HILITE)))) {
    seg[i  ].x1 = x + 1;
    seg[i  ].y1 = y + 1;
    seg[i  ].x2 = x + w - 2;
    seg[i++].y2 = y + 1;
  }
  if (((psw->boundary_width > 2) || (edge == 0)) &&
      ((psw->boundary_width > 3) || (edge < 1)) &&
      (!psw->fMWMBorders ||
       (((edge == 0) || (psw->boundary_width > 3)) && (highlight & LEFT_HILITE)))) {
    seg[i  ].x1 = x + 1;
    seg[i  ].y1 = y + 1;
    seg[i  ].x2 = x + 1;
    seg[i++].y2 = y + h - 2;
  }
  DBUG((DBG,FUNC_NAME,"i = %d",i));
  XDrawSegments(dpy, win, ReliefGC, seg, i);

  i = 0;
  seg[i  ].x1 = x;
  seg[i  ].y1 = y + h - 1;
  seg[i  ].x2 = w + x - 1;
  seg[i++].y2 = y + h - 1;

  if (((psw->boundary_width > 2) || (edge == 0)) &&
      (!psw->fMWMBorders ||
       (((edge == 0) ||
	 (psw->boundary_width > 3)) && (highlight & BOTTOM_HILITE)))) {
    seg[i  ].x1 = x + 1;
    seg[i  ].y1 = y + h - 2;
    seg[i  ].x2 = x + w - 2;
    seg[i++].y2 = y + h - 2;
  }
  seg[i  ].x1 = x + w - 1;
  seg[i  ].y1 = y;
  seg[i  ].x2 = x + w - 1;
  seg[i++].y2 = y + h - 1;

  if (((psw->boundary_width > 2) || (edge == 0)) &&
      (!psw->fMWMBorders ||
       (((edge == 0) || 
	 (psw->boundary_width > 3)) && (highlight & RIGHT_HILITE)))) {
    seg[i  ].x1 = x + w - 2;
    seg[i  ].y1 = y + 1;
    seg[i  ].x2 = x + w - 2;
    seg[i++].y2 = y + h - 2;
  }
  DBUG((DBG,FUNC_NAME,"i = %d",i));
  XDrawSegments(dpy, win, ShadowGC, seg, i);
}
#undef FUNC_NAME


/*
 * Setupframe - configure the decoration window sizes and positions
 * 
 * Unlike in fvwm2, this is *not* the correct function to call
 * when you want to move a ScwmWindow -- see the functions
 * in window.c, such as MoveTo, ResizeTo, and MoveResizeTo
 *
 *
 *  Inputs:
 *      psw - the ScwmWindow pointer
 *      x       - the x coordinate of the upper-left outer corner of the frame
 *      y       - the y coordinate of the upper-left outer corner of the frame
 * (x,y are in viewport coordinates)
 *      w       - the width of the frame window w/o border
 *      h       - the height of the frame window w/o border
 *      sendEvent  - True if we want to force an event to be sent reflecting the change
 *                   (it may get sent anyway --- see ICCCM comment below)
 *      fMoved  - set if the window was moved
 *      fResized - set if the window was resized
 *
 *
 *  Special Considerations:
 *      This routine will check to make sure the window is not completely
 *      off the display, if it is, it'll bring some of it back on.
 *
 *      The psw->frame_XXX variables should NOT be updated with the
 *      values of x,y,w,h prior to calling this routine, since the new
 *      values are compared against the old to see whether a synthetic
 *      ConfigureNotify event should be sent.  (It should be sent if the
 *      window was moved but not resized.)
 */

void 
SetupFrame(ScwmWindow *psw, int x, int y, int w, int h,
           Bool fMoved, Bool fResized)
{
#define FUNC_NAME "SetupFrame"
  XWindowChanges xwc;
  unsigned long xwcm;
  int i;
  int tbar_right = 0;
  const Bool shaded = SHADED_P(psw);
  const Bool fNoSideDecorations = NO_SIDE_DECORATIONS_P(psw);
  /*  Bool fNoTopDecoration = NO_TOP_BORDER_DECORATION_P(psw); */
  const Bool fSquashedTitlebar = psw->fSquashedTitlebar;

  assert(!fMoved   || fMoved   == WAS_MOVED);
  assert(!fResized || fResized == WAS_RESIZED);

  /* if windows is not shaded, save size for when unshaded
     This used to apply for maximization, too, but Maciej
     made those window properties --07/26/98 gjb */
  if (/* !psw->fMaximized && GJB:FIXME:: */ !shaded) {
    psw->orig_width = w;
    psw->orig_height = h;
  }

  if (fNoSideDecorations)
    psw->xboundary_width = 0;
  else
    psw->xboundary_width = psw->boundary_width;

  { /* scope */
    /* 16 pixels is the amount to force the window onto
       the virtual desktop if it is completely off */
    const int cpixForceOn = 16;
    if (x >= Scr.DisplayWidth + Scr.VxMax - WIN_VP_OFFSET_X(psw) - cpixForceOn)
      x = Scr.DisplayWidth + Scr.VxMax - WIN_VP_OFFSET_X(psw) - cpixForceOn;
    if (y >= Scr.DisplayHeight + Scr.VyMax - WIN_VP_OFFSET_Y(psw) - cpixForceOn)
      y = Scr.DisplayHeight + Scr.VyMax - WIN_VP_OFFSET_Y(psw) - cpixForceOn;
  }


#ifndef NDEBUG
  if ((w != FRAME_WIDTH(psw)) || ((h != FRAME_HEIGHT(psw)) && !fResized)) {
    DBUG((DBG,FUNC_NAME,"Width/height changed but not fResized"));
    ;
  }

  if ((x != FRAME_X(psw) || y != FRAME_Y(psw)) && !fMoved) {
    DBUG((DBG,FUNC_NAME,"Coords changed but not fMoved"));
    ;
  }
#endif

  if (fResized) {
    /* make the decoration buttons square */
    int button_width  = psw->title_height;
    int button_height = psw->title_height;

    int left  = CLeftButtons (psw);
    int right = CRightButtons(psw);

    DBUG((DBG,FUNC_NAME,"Resized to x=%d, y=%d;  w=%d,h=%d",x,y,w,h));

    psw->title_width = (w - (left + right) * button_width
                        - 2 * psw->xboundary_width + psw->bw);

    if (psw->title_width < 1)
      psw->title_width = 1;

    tbar_right = w;
    if (psw->fTitle && fSquashedTitlebar) {
      int tw = ComputeXTextWidth(XFONT(GET_DECOR(psw, window_font)), psw->name, -1);
      /* SRL:FIXME::Magic number! */
      tw += psw->xboundary_width + psw->bw + 18;
      if (psw->title_width > tw) {
        /* title is wider than it needs to be */
	tbar_right = w - (psw->title_width - tw);
	psw->title_width = tw;
      }
    }
    psw->tbar_right = tbar_right;

    if (SHOW_TITLE_P(psw)) {
      unsigned long buttons = psw->buttons;

      psw->title_x = psw->xboundary_width + (left * button_width);
      if (psw->title_x >= w - psw->xboundary_width)
	psw->title_x = -10;
      psw->title_y = fSquashedTitlebar ? 0 : psw->boundary_width;

      XMoveResizeWindow(dpy, psw->title_w,
			psw->title_x, psw->title_y,
			psw->title_width, psw->title_height);
      /* GJB:FIXME:: this reduces flicker a bit,
         but it'd be better to make resizing a frame not go through
         all of the SetupFrame routine and instead be optimized to change
         only what needs to be changed */
      SetTitleBar(psw,(Scr.Hilite==psw),False);
      XMapWindow(dpy,psw->title_w);

      xwcm = CWX | CWY | CWHeight | CWWidth;
      xwc.height = button_height;
      xwc.width  = button_width;
      xwc.y = fSquashedTitlebar ? 0 : psw->boundary_width;
      xwc.x = psw->xboundary_width;
      for (i = 0; i < Scr.nr_left_buttons; i++) {
	if (psw->left_w[i] != None) {
          if (buttons & (1 << (i*2))) {
            /* suppress that button */
            XUnmapWindow(dpy,psw->left_w[i]);
          } else {
            if (xwc.x + button_width < w - psw->boundary_width)
              XConfigureWindow(dpy, psw->left_w[i], xwcm, &xwc);
            else {
              xwc.x = -button_width;
              XConfigureWindow(dpy, psw->left_w[i], xwcm, &xwc);
            }
            XMapWindow(dpy,psw->left_w[i]);
          }
          xwc.x += button_width;
        }
      }

      xwc.x = tbar_right - psw->xboundary_width + psw->bw;
      for (i = 0; i < Scr.nr_right_buttons; i++) {
	if (psw->right_w[i] != None) {
          xwc.x -= button_width;
          if (buttons & (1 << (i*2+1))) {
            /* suppress that button */
            XUnmapWindow(dpy,psw->right_w[i]);
          } else {
            if (xwc.x > psw->boundary_width)
              XConfigureWindow(dpy, psw->right_w[i], xwcm, &xwc);
            else {
              xwc.x = -button_width;
              XConfigureWindow(dpy, psw->right_w[i], xwcm, &xwc);
            }
            XMapWindow(dpy,psw->right_w[i]);
          }
        }
      }
    } else { /* from if (SHOW_TITLE_P(psw)) */
      /* no title bar, so unmap button windows! */
      XUnmapWindow(dpy,psw->title_w);

      for (i = 0; i < Scr.nr_left_buttons; i++) {
	if (psw->left_w[i] != None) {
          XUnmapWindow(dpy,psw->left_w[i]);
        }
      }
      for (i = 0; i < Scr.nr_right_buttons; i++) {
	if (psw->right_w[i] != None) {
          XUnmapWindow(dpy,psw->right_w[i]);
        }
      }
    }

    if (psw->fBorder) {
      DBUG((DBG,FUNC_NAME,"Has border!"));
      psw->corner_width = psw->title_height + psw->bw + psw->boundary_width;

      /* GJB:FIXME: pretty arbitrary minimum size */
      if (psw->corner_width < 12)
        psw->corner_width = 12;

      if (w < 2 * psw->corner_width)
	psw->corner_width = w / 3;
      if ((h < 2 * psw->corner_width) && !shaded)
	psw->corner_width = h / 3;

      xwcm = CWWidth | CWHeight | CWX | CWY;

      if (psw->boundary_width + psw->bw > 0) {
        int ywidth = h - 2 * psw->corner_width;
        if (ywidth < 2)
          ywidth = 2;
        /* position the four sides */
        for (i = 0; i < 4; i++) {
          if (i == 0) { /* top side */
            xwc.x = psw->corner_width;
            xwc.y = (fSquashedTitlebar? psw->title_height : 0);
            xwc.width  = (fSquashedTitlebar? w: tbar_right)- 2 * psw->corner_width + psw->bw;
            xwc.height = psw->boundary_width;
          } else if (i == 1) { /* right side */
            xwc.x = w - psw->boundary_width + psw->bw;
            xwc.y = psw->corner_width;
            xwc.width  = psw->boundary_width;
            xwc.height = ywidth;
          } else if (i == 2) { /* bottom side */
            xwc.x = psw->corner_width;
            xwc.y = h - psw->boundary_width + psw->bw;
            xwc.width  = w - 2 * psw->corner_width + psw->bw;
            xwc.height = psw->boundary_width; /* GJB:FIXME:: + psw->bw; */
          } else { /* left side */
            xwc.x = 0;
            xwc.y = psw->corner_width;
            xwc.width  = psw->boundary_width;
            xwc.height = ywidth;
          }
          if (fNoSideDecorations && (i == 1 || i == 3)) {
            XUnmapWindow(dpy,psw->sides[i]);
          } else {
            /* SRL:FIXME:: Check this logic, seems suspect! Shouldn't it be i==2 ? */
            if (!shaded || (i != 2 && !fSquashedTitlebar)) { 
              /* show the sides -- we do not show them if 
                 the window is shaded; except the bottom
                 one, which we should show but may not GJB:FIXME:: */
              XConfigureWindow(dpy, psw->sides[i], xwcm, &xwc);
              /* make sure sides are mapped */
              XMapWindow(dpy,psw->sides[i]);
              XLowerWindow(dpy,psw->sides[i]); /* lower these so corners are on top */
            } else {
              XUnmapWindow(dpy,psw->sides[i]);
            }
          }
        }
      }

      xwcm = CWX | CWY | CWWidth | CWHeight;
      xwc.width = psw->corner_width;
      xwc.height = psw->corner_width;

      /* now position the four corners 
         0 = NW, 1 = NE, 2 = SW, 3 = SE */
      for (i = 0; i < 4; i++) {

	if (i == 1) /* right corner: NE */
	  xwc.x = (fSquashedTitlebar? w : tbar_right) - psw->corner_width + psw->bw;
        else if (i == 3) /* right corner: SE */
	  xwc.x = w - psw->corner_width + psw->bw;
	else /* Left corners: NW, SW */
	  xwc.x = 0;

	if (i == 2 || i == 3) /* Bottom corners: SW, SE */
	  xwc.y = h - xwc.height;
	else /* Top corners: NW, NE */
          xwc.y = (fSquashedTitlebar? psw->title_height : 0);

	if (!shaded || i==0 || (i == 1 && !fSquashedTitlebar)) { 
          /* do top left always, top right when shaded and not fSquashedTitlebar */
	  XConfigureWindow(dpy, psw->corners[i], xwcm, &xwc);
          XMapWindow(dpy,psw->corners[i]);
        } else {
          XUnmapWindow(dpy,psw->corners[i]);
        }
      }
    } else {
      /* no boundary/border, so no side windows visible */
      for (i=0; i<4; ++i) {
        XUnmapWindow(dpy,psw->sides[i]);
        XUnmapWindow(dpy,psw->corners[i]);
      }
    }
  }
  psw->attr.width  = w                     - 2 * psw->xboundary_width;
  psw->attr.height = h - psw->title_height - 2 * psw->boundary_width;
  
  { /* scope */
    int cx = psw->xboundary_width - psw->bw;
    int cy = psw->title_height + psw->boundary_width - psw->bw;

    if (!shaded && (fMoved || fResized)) {
      XResizeWindow    (dpy, psw->w, psw->attr.width, psw->attr.height);
      XMoveResizeWindow(dpy, psw->Parent, cx, cy,
                        psw->attr.width, psw->attr.height);
    }
  }

  /* fix up frame and assign size/location values in psw */
  DBUG_RESIZE((DBG,FUNC_NAME,"w = %d, h = %d", w, h));

  XMoveResizeWindow(dpy, psw->frame, x, y, w, h);

  if (ShapesSupported) {
    if (fResized) {
      if (psw->fShaped) {
        SetShape(psw, w);
      } else {
        if (fSquashedTitlebar) {
          SetShapedTitlebar(psw, tbar_right - psw->xboundary_width);
        } else {
          UnsetShapedTitlebar(psw);
        }
      }
    }
  }

  XSetWindowBorderWidth(dpy,psw->frame,psw->bw);
  XSetWindowBorderWidth(dpy,psw->Parent,psw->bw);

  XSync(dpy, False);

  BroadcastConfig(M_CONFIGURE_WINDOW, psw);
}
#undef FUNC_NAME


/* no primitives here, no .x file */
void
init_borders()
{
  XGCValues gcv;
  gcv.line_width = 0;
  gcv.function = GXcopy;
  gcv.plane_mask = AllPlanes;
  gcv.graphics_exposures = False;
  TransMaskGC = XCreateGC(dpy, 
                          Scr.Root, 
                          GCFunction | GCPlaneMask | GCGraphicsExposures | GCLineWidth,
                          &gcv);

#ifndef SCM_MAGIC_SNARFER
#include "borders.x"
#endif
}

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

