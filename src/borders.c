/* $Id$
 * borders.c
 *

/****************************************************************************
 * This module is derived from code 
 * by Rob Nation 
 * Copyright 1993, Robert Nation
 *     You may use this code for any purpose, as long as the original
 *     copyright remains in the source code and all documentation
 ****************************************************************************/

/* #define SCWM_DEBUG_MSGS */

/***********************************************************************
 *
 * scwm window border drawing code
 *
 ***********************************************************************/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <signal.h>
#include <string.h>
#include <assert.h>

#include "scwm.h"
#include "icons.h"
#include "screen.h"
#include "borders.h"
#include <guile/gh.h>
#include "image.h"
#include <X11/extensions/shape.h>
#include "module-interface.h"
#include "font.h"
#include "xmisc.h"
#include "scwm-constraints.h"

#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif

extern Window PressedW;

/* macro rules to get button state */
#define GetButtonState(window)						\
        (onoroff ? ((PressedW == (window)) ? ActiveDown : ActiveUp)	\
        : Inactive)


/* macro to change window background color/pixmap */
#define ChangeWindowColor(window,valuemask) {				\
        if(NewColor)							\
        {								\
          XChangeWindowAttributes(dpy,window,valuemask, &attributes);	\
          XClearWindow(dpy,window);					\
        }								\
      }

/****************************************************************************
 *
 *  Draws a little pattern within a window (more complex)
 *
 ****************************************************************************/
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

/****************************************************************************
 *
 *  Redraws buttons (veliaa@rpi.edu)
 *
 ****************************************************************************/
static void 
DrawButton(ScwmWindow * t, Window win, int w, int h,
	   ButtonFace * bf, GC ReliefGC, GC ShadowGC,
	   Boolean inverted, int stateflags)
{
  register int type = bf->style & ButtonFaceTypeMask;

  scwm_image *image;
  int border = 0;
  int width, height, x, y;


  switch (type) {
  case SimpleButton:
    break;

  case SolidButton:
    XSetWindowBackground(dpy, win, XCOLOR(bf->u.back));
    flush_expose(win);
    XClearWindow(dpy, win);
    break;


    /* FIXGJB: is this using an object property?  Cool! :-) */
  case VectorButton:
    if ((t->fMWMButtons)
	&& (stateflags & MWMButton)
	&& (t->fMaximized || (SCM_NFALSEP
			      (scm_object_property
			       (t->schwin,
				gh_symbol2scm("maximized"))))))
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
      image = IMAGE (bf->u.image);
    else {
      if (t->mini_icon_image==SCM_BOOL_F)
	break;
      image = IMAGE (t->mini_icon_image);
    }
    if (bf->style & FlatButton)
      border = 0;
    else
      border = t->fMWMBorders ? 1 : 2;
    width = w - border * 2;
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
    if (width > w - x - border)
      width = w - x - border;
    if (height > h - y - border)
      height = h - y - border;

    XSetClipMask(dpy, Scr.TransMaskGC, image->mask);
    XSetClipOrigin(dpy, Scr.TransMaskGC, x, y);
    XCopyArea(dpy, image->image, win, Scr.TransMaskGC,
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

      XSetClipMask(dpy, Scr.TransMaskGC, None);
      if (type == HGradButton) {
	register int i = 0, dw = bounds.width
	/ bf->u.grad.npixels + 1;

	while (i < bf->u.grad.npixels) {
	  unsigned short x = i * bounds.width / bf->u.grad.npixels;

	  XSetForeground(dpy, Scr.TransMaskGC, bf->u.grad.pixels[i++]);
	  XFillRectangle(dpy, win, Scr.TransMaskGC,
			 bounds.x + x, bounds.y,
			 dw, bounds.height);
	}
      } else {
	register int i = 0, dh = bounds.height
	/ bf->u.grad.npixels + 1;

	while (i < bf->u.grad.npixels) {
	  unsigned short y = i * bounds.height / bf->u.grad.npixels;

	  XSetForeground(dpy, Scr.TransMaskGC, bf->u.grad.pixels[i++]);
	  XFillRectangle(dpy, win, Scr.TransMaskGC,
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

/****************************************************************************
 *
 *  Draws the relief pattern around a window for HiddenHandle borders
 *
 *  (veliaa@rpi.edu)
 *
 ****************************************************************************/
static void 
RelieveWindowHH(ScwmWindow * t, Window win,
		int x, int y, int w, int h,
		GC ReliefGC, GC ShadowGC,
		int draw, int hilite)
{
  XSegment seg[4];
  int i = 0;
  int edge = 0, a = 0, b = 0;

  if (win == t->sides[0]) {
    edge = 5;
    b = 1;
  } else if (win == t->sides[1]) {
    a = 1;
    edge = 6;
  } else if (win == t->sides[2]) {
    edge = 7;
    b = 1;
  } else if (win == t->sides[3]) {
    edge = 8;
    a = 1;
  } else if (win == t->corners[0])
    edge = 1;
  else if (win == t->corners[1])
    edge = 2;
  else if (win == t->corners[2])
    edge = 3;
  else if (win == t->corners[3])
    edge = 4;

  if (draw & TOP_HILITE) {
    seg[i].x1 = x;
    seg[i].y1 = y;
    seg[i].x2 = w + x - 1;
    seg[i++].y2 = y;

    if (((t->boundary_width > 2) || (edge == 0)) &&
	((t->boundary_width > 3) || (edge < 1)) &&
	(!t->fMWMBorders ||
     (((edge == 0) || (t->boundary_width > 3)) && (hilite & TOP_HILITE)))) {
      seg[i].x1 = x + ((edge == 2) || b ? 0 : 1);
      seg[i].y1 = y + 1;
      seg[i].x2 = x + w - 1 - ((edge == 1) || b ? 0 : 1);
      seg[i++].y2 = y + 1;
    }
  }
  if (draw & LEFT_HILITE) {
    seg[i].x1 = x;
    seg[i].y1 = y;
    seg[i].x2 = x;
    seg[i++].y2 = h + y - 1;

    if (((t->boundary_width > 2) || (edge == 0)) &&
	((t->boundary_width > 3) || (edge < 1)) &&
	(!t->fMWMBorders ||
	 (((edge == 0) || 
	   (t->boundary_width > 3)) && (hilite & LEFT_HILITE)))) {
      seg[i].x1 = x + 1;
      seg[i].y1 = y + ((edge == 3) || a ? 0 : 1);
      seg[i].x2 = x + 1;
      seg[i++].y2 = y + h - 1 - ((edge == 1) || a ? 0 : 1);
    }
  }
  XDrawSegments(dpy, win, ReliefGC, seg, i);

  i = 0;

  if (draw & BOTTOM_HILITE) {
    seg[i].x1 = x;
    seg[i].y1 = y + h - 1;
    seg[i].x2 = w + x - 1;
    seg[i++].y2 = y + h - 1;

    if (((t->boundary_width > 2) || (edge == 0)) &&
	(!t->fMWMBorders ||
	 (((edge == 0) || (t->boundary_width > 3)) && (hilite & BOTTOM_HILITE)))) {
      seg[i].x1 = x + (b || (edge == 4) ? 0 : 1);
      seg[i].y1 = y + h - 2;
      seg[i].x2 = x + w - ((edge == 3) ? 0 : 1);
      seg[i++].y2 = y + h - 2;
    }
  }
  if (draw & RIGHT_HILITE) {
    seg[i].x1 = x + w - 1;
    seg[i].y1 = y;
    seg[i].x2 = x + w - 1;
    seg[i++].y2 = y + h - 1;

    if (((t->boundary_width > 2) || (edge == 0)) &&
	(!t->fMWMBorders ||
    (((edge == 0) || (t->boundary_width > 3)) && (hilite & RIGHT_HILITE)))) {
      seg[i].x1 = x + w - 2;
      seg[i].y1 = y + (a || (edge == 4) ? 0 : 1);
      seg[i].x2 = x + w - 2;
      seg[i++].y2 = y + h - 1 - ((edge == 2) || a ? 0 : 1);
    }
  }
  XDrawSegments(dpy, win, ShadowGC, seg, i);
}

static void 
RelieveParts(ScwmWindow * t, int i, GC hor, GC vert)
{
  XSegment seg[2];
  int n = 0, hh = i & HH_HILITE;

  i &= FULL_HILITE;

  if (t->fMWMBorders || (t->boundary_width < 3)) {
    switch (i) {
    case 0:
      seg[0].x1 = t->boundary_width - 1;
      seg[0].x2 = t->corner_width;
      seg[0].y1 = t->boundary_width - 1;
      seg[0].y2 = t->boundary_width - 1;
      n = 1;
      break;
    case 1:
      seg[0].x1 = 0;
      seg[0].x2 = t->corner_width - t->boundary_width /* -1 */ ;
      seg[0].y1 = t->boundary_width - 1;
      seg[0].y2 = t->boundary_width - 1;
      n = 1;
      break;
    case 2:
      seg[0].x1 = t->boundary_width - 1;
      seg[0].x2 = t->corner_width - (hh ? 1 : 2);
      seg[0].y1 = t->corner_width - t->boundary_width + t->bw;
      seg[0].y2 = t->corner_width - t->boundary_width + t->bw;
      n = 1;
      break;
    case 3:
      seg[0].x1 = 0;
      seg[0].x2 = t->corner_width - t->boundary_width;
      seg[0].y1 = t->corner_width - t->boundary_width + t->bw;
      seg[0].y2 = t->corner_width - t->boundary_width + t->bw;
      n = 1;
      break;
    }
    XDrawSegments(dpy, t->corners[i], hor, seg, n);
    switch (i) {
    case 0:
      seg[0].y1 = t->boundary_width - 1;
      seg[0].y2 = t->corner_width;
      seg[0].x1 = t->boundary_width - 1;
      seg[0].x2 = t->boundary_width - 1;
      n = 1;
      break;
    case 1:
      seg[0].y1 = t->boundary_width - 1;
      seg[0].y2 = t->corner_width - (hh ? 0 : 2);
      seg[0].x1 = t->corner_width - t->boundary_width;
      seg[0].x2 = t->corner_width - t->boundary_width;
      n = 1;
      break;
    case 2:
      seg[0].y1 = 0;
      seg[0].y2 = t->corner_width - t->boundary_width;
      seg[0].x1 = t->boundary_width - 1;
      seg[0].x2 = t->boundary_width - 1;
      n = 1;
      break;
    case 3:
      seg[0].y1 = 0;
      seg[0].y2 = t->corner_width - t->boundary_width + t->bw;
      seg[0].x1 = t->corner_width - t->boundary_width;
      seg[0].x2 = t->corner_width - t->boundary_width;
      n = 1;
      break;
    }
    XDrawSegments(dpy, t->corners[i], vert, seg, 1);
  } else {
    switch (i) {
    case 0:
      seg[0].x1 = t->boundary_width - 2;
      seg[0].x2 = t->corner_width;
      seg[0].y1 = t->boundary_width - 2;
      seg[0].y2 = t->boundary_width - 2;

      seg[1].x1 = t->boundary_width - 2;
      seg[1].x2 = t->corner_width;
      seg[1].y1 = t->boundary_width - 1;
      seg[1].y2 = t->boundary_width - 1;
      n = 2;
      break;
    case 1:
      seg[0].x1 = (hh ? 0 : 1);
      seg[0].x2 = t->corner_width - t->boundary_width;
      seg[0].y1 = t->boundary_width - 2;
      seg[0].y2 = t->boundary_width - 2;

      seg[1].x1 = 0;
      seg[1].x2 = t->corner_width - t->boundary_width - 1;
      seg[1].y1 = t->boundary_width - 1;
      seg[1].y2 = t->boundary_width - 1;
      n = 2;
      break;
    case 2:
      seg[0].x1 = t->boundary_width - 1;
      seg[0].x2 = t->corner_width - (hh ? 1 : 2);
      seg[0].y1 = t->corner_width - t->boundary_width + 1;
      seg[0].y2 = t->corner_width - t->boundary_width + 1;
      n = 1;
      if (t->boundary_width > 3) {
	seg[1].x1 = t->boundary_width - 2;
	seg[1].x2 = t->corner_width - (hh ? 1 : 3);
	seg[1].y1 = t->corner_width - t->boundary_width + 2;
	seg[1].y2 = t->corner_width - t->boundary_width + 2;
	n = 2;
      }
      break;
    case 3:
      seg[0].x1 = 0;
      seg[0].x2 = t->corner_width - t->boundary_width;
      seg[0].y1 = t->corner_width - t->boundary_width + 1;
      seg[0].y2 = t->corner_width - t->boundary_width + 1;
      n = 1;
      if (t->boundary_width > 3) {
	seg[0].x2 = t->corner_width - t->boundary_width + 1;

	seg[1].x1 = 0;
	seg[1].x2 = t->corner_width - t->boundary_width + 1;
	seg[1].y1 = t->corner_width - t->boundary_width + 2;
	seg[1].y2 = t->corner_width - t->boundary_width + 2;
	n = 2;
      }
      break;
    }
    XDrawSegments(dpy, t->corners[i], hor, seg, n);
    switch (i) {
    case 0:
      seg[0].y1 = t->boundary_width - 2;
      seg[0].y2 = t->corner_width;
      seg[0].x1 = t->boundary_width - 2;
      seg[0].x2 = t->boundary_width - 2;

      seg[1].y1 = t->boundary_width - 2;
      seg[1].y2 = t->corner_width;
      seg[1].x1 = t->boundary_width - 1;
      seg[1].x2 = t->boundary_width - 1;
      n = 2;
      break;
    case 1:
      seg[0].y1 = t->boundary_width - 1;
      seg[0].y2 = t->corner_width - (hh ? 1 : 2);
      seg[0].x1 = t->corner_width - t->boundary_width;
      seg[0].x2 = t->corner_width - t->boundary_width;
      n = 1;
      if (t->boundary_width > 3) {
	seg[1].y1 = t->boundary_width - 2;
	seg[1].y2 = t->corner_width - (hh ? 1 : 3);
	seg[1].x1 = t->corner_width - t->boundary_width + 1;
	seg[1].x2 = t->corner_width - t->boundary_width + 1;
	n = 2;
      }
      break;
    case 2:
      seg[0].y1 = (hh ? 0 : 1);
      seg[0].y2 = t->corner_width - t->boundary_width + 1;
      seg[0].x1 = t->boundary_width - 2;
      seg[0].x2 = t->boundary_width - 2;
      n = 1;

      if (t->boundary_width > 3) {
	seg[1].y1 = 0;
	seg[1].y2 = t->corner_width - t->boundary_width;
	seg[1].x1 = t->boundary_width - 1;
	seg[1].x2 = t->boundary_width - 1;
      }
      break;
    case 3:
      seg[0].y1 = 0;
      seg[0].y2 = t->corner_width - t->boundary_width + 1;
      seg[0].x1 = t->corner_width - t->boundary_width;
      seg[0].x2 = t->corner_width - t->boundary_width;
      n = 1;

      if (t->boundary_width > 3) {
	seg[0].y2 = t->corner_width - t->boundary_width + 2;
	seg[1].y1 = 0;
	seg[1].y2 = t->corner_width - t->boundary_width + 2;
	seg[1].x1 = t->corner_width - t->boundary_width + 1;
	seg[1].x2 = t->corner_width - t->boundary_width + 1;
	n = 2;
      }
      break;
    }
    XDrawSegments(dpy, t->corners[i], vert, seg, n);
  }
}

void
SetBorder(ScwmWindow * t, Bool onoroff, Bool force, Bool Mapped,
	  Window expose_win)
{
  SetBorderX(t, onoroff, force, Mapped, expose_win, False);
}


void 
SetBorderX(ScwmWindow * t, Bool onoroff, Bool force, Bool Mapped,
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
  Window w;

  corners[0] = TOP_HILITE | LEFT_HILITE;
  corners[1] = TOP_HILITE | RIGHT_HILITE;
  corners[2] = BOTTOM_HILITE | LEFT_HILITE;
  corners[3] = BOTTOM_HILITE | RIGHT_HILITE;

  if (!t)
    return;

  if (onoroff) {
    /* don't re-draw just for kicks */
    if ((!force) && (Scr.Hilite == t))
      return;

    if (Scr.Hilite != t)
      NewColor = True;

    if (really_force) {
      NewColor = True;
    }
    /* make sure that the previously highlighted window got unhighlighted */
    if ((Scr.Hilite != t) && (Scr.Hilite != NULL))
      SetBorder(Scr.Hilite, False, False, True, None);

    /* are we using textured borders? */
    if ((GetDecor(t, BorderStyle.active->style)
	 & ButtonFaceTypeMask) == TiledPixmapButton) {
      SCM scmImage = GetDecor(t, BorderStyle.active->u.image);
      if (IMAGE_P(scmImage)) {
        scwm_image *simage = IMAGE(scmImage);
	if (simage)
	  TexturePixmap = simage->image;
      }
    }

    /* set the keyboard focus */
    if (Mapped && t->fMapped && (Scr.Hilite != t))
      w = t->w;
    else if (t->fIconified && (Scr.Hilite != t) && !t->fSuppressIcon)
      w = t->icon_w;
    Scr.Hilite = t;

    TextColor = XCOLOR(GetDecor(t, HiColors.fg));
    BackPixmap = Scr.gray_pixmap;
    BackColor = XCOLOR(GetDecor(t, HiColors.bg));
    ReliefGC = GetDecor(t, HiReliefGC);
    ShadowGC = GetDecor(t, HiShadowGC);
    BorderColor = XCOLOR(GetDecor(t, HiRelief.bg));
  } else {
    /* don't re-draw just for kicks */
    if ((!force) && (Scr.Hilite != t))
      return;

    if (Scr.Hilite == t) {
      Scr.Hilite = NULL;
      NewColor = True;
    }
    if (really_force) {
      NewColor = True;
    }
    if ((GetDecor(t, BorderStyle.inactive->style)
	 & ButtonFaceTypeMask) == TiledPixmapButton)
      TexturePixmap = IMAGE(GetDecor(t, BorderStyle.inactive->u.image))->image;

    TextColor = XCOLOR(t->TextColor);
    BackPixmap = Scr.light_gray_pixmap;
    if (t->fSticky)
      BackPixmap = Scr.sticky_gray_pixmap;
    BackColor = XCOLOR(t->BackColor);
    Globalgcv.foreground = XCOLOR(t->ReliefColor);
    Globalgcm = GCForeground;
    XChangeGC(dpy, Scr.ScratchGC1, Globalgcm, &Globalgcv);
    ReliefGC = Scr.ScratchGC1;

    Globalgcv.foreground = XCOLOR(t->ShadowColor);
    XChangeGC(dpy, Scr.ScratchGC2, Globalgcm, &Globalgcv);
    ShadowGC = Scr.ScratchGC2;
    BorderColor = XCOLOR(t->ShadowColor);
  }

  if (t->fIconified) {
    DrawIconWindow(t);
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
  } else
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

  if (t->fBorder || SHOW_TITLE_P(t)) {
    XSetWindowBorder(dpy, t->Parent, BorderColor);
    XSetWindowBorder(dpy, t->frame, BorderColor);
  }

  if (SHOW_TITLE_P(t)) {
    ChangeWindowColor(t->title_w, valuemask);
    for (i = 0; i < Scr.nr_left_buttons; ++i) {
      if (t->left_w[i] != None) {
	enum ButtonState bs = GetButtonState(t->left_w[i]);
	ButtonFace *bf = GetDecor(t, left_buttons[i].state[bs]);

	if (flush_expose(t->left_w[i]) || (expose_win == t->left_w[i]) ||
	    (expose_win == None)
	    || NewColor
	  ) {
	  int inverted = PressedW == t->left_w[i];

	  if (bf->style & UseBorderStyle)
	    XChangeWindowAttributes(dpy, t->left_w[i],
				    valuemask, &attributes);
	  else
	    XChangeWindowAttributes(dpy, t->left_w[i],
				    notex_valuemask, &notex_attributes);
	  XClearWindow(dpy, t->left_w[i]);
	  if (bf->style & UseTitleStyle) {
	    ButtonFace *tsbf = GetDecor(t, titlebar.state[bs]);

	    for (; tsbf; tsbf = tsbf->next)
	      DrawButton(t, t->left_w[i],
			 t->title_height, t->title_height,
			 tsbf, ReliefGC, ShadowGC,
			 inverted, GetDecor(t, left_buttons[i].flags));
	  }
	  for (; bf; bf = bf->next)
	    DrawButton(t, t->left_w[i],
		       t->title_height, t->title_height,
		       bf, ReliefGC, ShadowGC,
		       inverted, GetDecor(t, left_buttons[i].flags));

	  if (!(GetDecor(t, left_buttons[i].state[bs]->style) & FlatButton)) {
	    if (GetDecor(t, left_buttons[i].state[bs]->style) & SunkButton)
	      RelieveWindow(t, t->left_w[i], 0, 0,
			    t->title_height, t->title_height,
			    (inverted ? ReliefGC : ShadowGC),
			    (inverted ? ShadowGC : ReliefGC),
			    BOTTOM_HILITE);
	    else
	      RelieveWindow(t, t->left_w[i], 0, 0,
			    t->title_height, t->title_height,
			    (inverted ? ShadowGC : ReliefGC),
			    (inverted ? ReliefGC : ShadowGC),
			    BOTTOM_HILITE);
	  }
	}
      }
    }
    for (i = 0; i < Scr.nr_right_buttons; ++i) {
      if (t->right_w[i] != None) {
	enum ButtonState bs = GetButtonState(t->right_w[i]);
	ButtonFace *bf = GetDecor(t, right_buttons[i].state[bs]);

	if (flush_expose(t->right_w[i]) || (expose_win == t->right_w[i]) ||
	    (expose_win == None)
	    || NewColor
	  ) {
	  int inverted = PressedW == t->right_w[i];

	  if (bf->style & UseBorderStyle)
	    XChangeWindowAttributes(dpy, t->right_w[i],
				    valuemask, &attributes);
	  else
	    XChangeWindowAttributes(dpy, t->right_w[i],
				    notex_valuemask, &notex_attributes);
	  XClearWindow(dpy, t->right_w[i]);
	  if (bf->style & UseTitleStyle) {
	    ButtonFace *tsbf = GetDecor(t, titlebar.state[bs]);

	    for (; tsbf; tsbf = tsbf->next)
	      DrawButton(t, t->right_w[i],
			 t->title_height, t->title_height,
			 tsbf, ReliefGC, ShadowGC,
			 inverted, GetDecor(t, right_buttons[i].flags));
	  }
	  for (; bf; bf = bf->next)
	    DrawButton(t, t->right_w[i],
		       t->title_height, t->title_height,
		       bf, ReliefGC, ShadowGC,
		       inverted, GetDecor(t, right_buttons[i].flags));

	  if (!(GetDecor(t, right_buttons[i].state[bs]->style) & FlatButton)) {
	    if (GetDecor(t, right_buttons[i].state[bs]->style) & SunkButton)
	      RelieveWindow(t, t->right_w[i], 0, 0,
			    t->title_height, t->title_height,
			    (inverted ? ReliefGC : ShadowGC),
			    (inverted ? ShadowGC : ReliefGC),
			    BOTTOM_HILITE);
	    else
	      RelieveWindow(t, t->right_w[i], 0, 0,
			    t->title_height, t->title_height,
			    (inverted ? ShadowGC : ReliefGC),
			    (inverted ? ReliefGC : ShadowGC),
			    BOTTOM_HILITE);
	  }
	}
      }
    }
    SetTitleBar(t, onoroff, False);

  }
  if (t->fBorder) {
    /* draw relief lines */
    y = FRAME_HEIGHT(t) - 2 * t->corner_width;
    x = FRAME_WIDTH(t) - 2 * t->corner_width + t->bw;

    for (i = 0; i < 4; i++) {
      int vertical = i % 2;

      int flags = onoroff
      ? GetDecor(t, BorderStyle.active->style)
      : GetDecor(t, BorderStyle.inactive->style);


      ChangeWindowColor(t->sides[i], valuemask);
      if ((flush_expose(t->sides[i])) || (expose_win == t->sides[i]) ||
	  (expose_win == None)) {
	GC sgc, rgc;

	sgc = ShadowGC;
	rgc = ReliefGC;
	if (!t->fMWMButtons && (PressedW == t->sides[i])) {
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
	    RelieveWindowHH(t, t->sides[i], 0, 0,
			    ((vertical) ? t->boundary_width : x),
			    ((vertical) ? y : t->boundary_width),
			    rgc, sgc, vertical
			    ? (i == 3 ? LEFT_HILITE : RIGHT_HILITE)
			    : (i ? BOTTOM_HILITE : TOP_HILITE),
			    (0x0001 << i)
	      );
	  } else {
	    RelieveWindowHH(t, t->sides[i], 0, 0,
			    ((vertical) ? t->boundary_width : x),
			    ((vertical) ? y : t->boundary_width),
			    rgc, sgc, vertical
			    ? (LEFT_HILITE | RIGHT_HILITE)
			    : (TOP_HILITE | BOTTOM_HILITE),
			    (0x0001 << i)
	      );
	  }
	} else {
	  RelieveWindow(t, t->sides[i], 0, 0,
			((i % 2) ? t->boundary_width : x),
			((i % 2) ? y : t->boundary_width),
			rgc, sgc, (0x0001 << i));
	}
      }
      ChangeWindowColor(t->corners[i], valuemask);
      if ((flush_expose(t->corners[i])) || (expose_win == t->corners[i]) ||
	  (expose_win == None)) {
	GC rgc, sgc;

	rgc = ReliefGC;
	sgc = ShadowGC;
	if (!t->fMWMButtons && (PressedW == t->corners[i])) {
	  sgc = ReliefGC;
	  rgc = ShadowGC;
	}
	if (flags & HiddenHandles) {
	  RelieveWindowHH(t, t->corners[i], 0, 0, t->corner_width,
		      ((i / 2) ? t->corner_width + t->bw : t->corner_width),
			  rgc, sgc, corners[i], corners[i]);

	  if (!(flags & NoInset))
	    if (t->boundary_width > 1)
	      RelieveParts(t, i | HH_HILITE,
			   ((i / 2) ? rgc : sgc), (vertical ? rgc : sgc));
	    else
	      RelieveParts(t, i | HH_HILITE,
			   ((i / 2) ? sgc : sgc), (vertical ? sgc : sgc));
	} else {
	  RelieveWindow(t, t->corners[i], 0, 0, t->corner_width,
		      ((i / 2) ? t->corner_width + t->bw : t->corner_width),
			rgc, sgc, corners[i]);

	  if (t->boundary_width > 1)
	    RelieveParts(t, i, ((i / 2) ? rgc : sgc), (vertical ? rgc : sgc));
	  else
	    RelieveParts(t, i, ((i / 2) ? sgc : sgc), (vertical ? sgc : sgc));
	}
      }
    }
  } else {			/* no decorative border */
    /* for mono - put a black border on 
     * for color, make it the color of the decoration background */
    if (t->boundary_width < 2) {
      flush_expose(t->frame);
      if (Scr.d_depth < 2) {
	XSetWindowBorder(dpy, t->frame, TextColor);
	XSetWindowBorder(dpy, t->Parent, TextColor);
	XSetWindowBackgroundPixmap(dpy, t->frame, BackPixmap);
	XClearWindow(dpy, t->frame);
	XSetWindowBackgroundPixmap(dpy, t->Parent, BackPixmap);
	XClearWindow(dpy, t->Parent);
      } else {
	XSetWindowBackgroundPixmap(dpy, t->frame, TexturePixmap);
	XSetWindowBorder(dpy, t->frame, BorderColor);
	XClearWindow(dpy, t->frame);
	XSetWindowBackground(dpy, t->Parent, BorderColor);
	XSetWindowBorder(dpy, t->Parent, BorderColor);
	XClearWindow(dpy, t->Parent);
	XSetWindowBorder(dpy, t->w, BorderColor);
      }
    } else {
      GC rgc, sgc;

      XSetWindowBorder(dpy, t->Parent, BorderColor);
      XSetWindowBorder(dpy, t->frame, BorderColor);

      rgc = ReliefGC;
      sgc = ShadowGC;
      if (!t->fMWMButtons && (PressedW == t->frame)) {
	sgc = ReliefGC;
	rgc = ShadowGC;
      }
      ChangeWindowColor(t->frame, valuemask);
      if ((flush_expose(t->frame)) || (expose_win == t->frame) ||
	  (expose_win == None)) {
	if (t->boundary_width > 2) {
	  RelieveWindow(t, t->frame, t->boundary_width - 1 - t->bw,
			t->boundary_width - 1 - t->bw,
			FRAME_WIDTH(t) -
			(t->boundary_width << 1) + 2 + 3 * t->bw,
			FRAME_HEIGHT(t) -
			(t->boundary_width << 1) + 2 + 3 * t->bw,
			sgc, rgc,
			TOP_HILITE | LEFT_HILITE | RIGHT_HILITE |
			BOTTOM_HILITE);
	  RelieveWindow(t, t->frame, 0, 0, FRAME_WIDTH(t) + t->bw,
			FRAME_HEIGHT(t) + t->bw, rgc, sgc,
			TOP_HILITE | LEFT_HILITE | RIGHT_HILITE |
			BOTTOM_HILITE);
	} else {
	  RelieveWindow(t, t->frame, 0, 0, FRAME_WIDTH(t) + t->bw,
			FRAME_HEIGHT(t) + t->bw, rgc, rgc,
			TOP_HILITE | LEFT_HILITE | RIGHT_HILITE |
			BOTTOM_HILITE);
	}
      } else {
	XSetWindowBackground(dpy, t->Parent, BorderColor);
      }
    }
  }
  /* Sync to make the border-color change look fast! */
  XSync(dpy, 0);

}



/****************************************************************************
 *
 *  Redraws just the title bar
 *
 ****************************************************************************/
void 
SetTitleBar(ScwmWindow * t, Bool onoroff, Bool NewTitle)
{
  int hor_off, w, i;
  enum ButtonState title_state;
  ButtonFaceStyle tb_style;
  int tb_flags;
  GC ReliefGC, ShadowGC, tGC;
  Pixel Forecolor, BackColor;
#ifdef I18N
  XRectangle dummy,log_ret;
#endif

  if (!t)
    return;
  if (!SHOW_TITLE_P(t))
    return;

  if (onoroff) {
    Forecolor = XCOLOR(GetDecor(t, HiColors.fg));
    BackColor = XCOLOR(GetDecor(t, HiColors.bg));
    ReliefGC = GetDecor(t, HiReliefGC);
    ShadowGC = GetDecor(t, HiShadowGC);
  } else {
    Forecolor = XCOLOR(t->TextColor);
    BackColor = XCOLOR(t->BackColor);
    Globalgcv.foreground = XCOLOR(t->ReliefColor);
    Globalgcm = GCForeground;
    XChangeGC(dpy, Scr.ScratchGC1, Globalgcm, &Globalgcv);
    ReliefGC = Scr.ScratchGC1;

    Globalgcv.foreground = XCOLOR(t->ShadowColor);
    XChangeGC(dpy, Scr.ScratchGC2, Globalgcm, &Globalgcv);
    ShadowGC = Scr.ScratchGC2;
  }
  if (PressedW == t->title_w) {
    tGC = ShadowGC;
    ShadowGC = ReliefGC;
    ReliefGC = tGC;
  }
  flush_expose(t->title_w);

  if (t->name != (char *) NULL) {
#ifdef I18N
    XmbTextExtents(XFONT(GetDecor(t,window_font)),
		   t->name, strlen(t->name), &dummy, &log_ret);
    w = log_ret.width;
#else
    w = XTextWidth(XFONT(GetDecor(t, window_font)), t->name, strlen(t->name));
#endif
    if (w > t->title_width - 12)
      w = t->title_width - 4;
    if (w < 0)
      w = 0;
  } else
    w = 0;

  title_state = GetButtonState(t->title_w);
  tb_style = GetDecor(t, titlebar.state[title_state]->style);
  tb_flags = GetDecor(t, titlebar.flags);
  if (tb_flags & HOffCenter) {
    if (tb_flags & HRight)
      hor_off = t->title_width - w - 10;
    else
      hor_off = 10;
  } else
    hor_off = (t->title_width - w) / 2;

#ifdef I18N
  NewFontAndColor(FONT(GetDecor(t, window_font))->xfs->fid, Forecolor, BackColor);
#else
  NewFontAndColor(XFONT(GetDecor(t, window_font))->fid, Forecolor, BackColor);
#endif

  /* the next bit tries to minimize redraw based upon compilation options (veliaa@rpi.edu) */
  /* we need to check for UseBorderStyle for the titlebar */
  {
    ButtonFace *bf = onoroff
      ? GetDecor(t, BorderStyle.active)
      : GetDecor(t, BorderStyle.inactive);

    if ((tb_style & UseBorderStyle)
	&& ((bf->style & ButtonFaceTypeMask) == TiledPixmapButton))
      XSetWindowBackgroundPixmap(dpy, t->title_w, IMAGE(bf->u.image)->image);
  }
  XClearWindow(dpy, t->title_w);

  /* for mono, we clear an area in the title bar where the window
   * title goes, so that its more legible. For color, no need */
  if (Scr.d_depth < 2) {
    RelieveWindow(t, t->title_w, 0, 0, hor_off - 2, t->title_height,
		  ReliefGC, ShadowGC, BOTTOM_HILITE);
    RelieveWindow(t, t->title_w, hor_off + w + 2, 0,
		  t->title_width - w - hor_off - 2, t->title_height,
		  ReliefGC, ShadowGC, BOTTOM_HILITE);
    XFillRectangle(dpy, t->title_w,
		   (PressedW == t->title_w ? ShadowGC : ReliefGC),
		   hor_off - 2, 0, w + 4, t->title_height);

    XDrawLine(dpy, t->title_w, ShadowGC, hor_off + w + 1, 0, hor_off + w + 1,
	      t->title_height);
    if (t->name != (char *) NULL) 
#ifdef I18N
      XmbDrawString(dpy, t->title_w,XFONT(GetDecor(t,window_font)),
		    Scr.ScratchGC3, hor_off,
		    GetDecor(t, window_font_y) + 1,
		    t->name, strlen(t->name));
#else
      XDrawString(dpy, t->title_w, Scr.ScratchGC3, hor_off,
		  GetDecor(t, window_font_y) + 1,
		  t->name, strlen(t->name));
#endif
  } else {
    ButtonFace *bf = GetDecor(t, titlebar.state[title_state]);

    /* draw compound titlebar (veliaa@rpi.edu) */
    if (PressedW == t->title_w) {
      for (; bf; bf = bf->next)
	DrawButton(t, t->title_w, t->title_width, t->title_height,
		   bf, ShadowGC, ReliefGC, True, 0);
    } else {
      for (; bf; bf = bf->next)
	DrawButton(t, t->title_w, t->title_width, t->title_height,
		   bf, ReliefGC, ShadowGC, False, 0);
    }

    if (!(tb_style & FlatButton)) {
      if (tb_style & SunkButton)
	RelieveWindow(t, t->title_w, 0, 0, t->title_width, t->title_height,
		      ShadowGC, ReliefGC, BOTTOM_HILITE);
      else
	RelieveWindow(t, t->title_w, 0, 0, t->title_width, t->title_height,
		      ReliefGC, ShadowGC, BOTTOM_HILITE);
    }
    if (t->name != (char *) NULL) {
#ifdef I18N
      XmbDrawString(dpy, t->title_w,XFONT(GetDecor(t,window_font)),
		    Scr.ScratchGC3, hor_off,
		    GetDecor(t, window_font_y) + 1,
		    t->name, strlen(t->name));
#else
      XDrawString(dpy, t->title_w, Scr.ScratchGC3, hor_off,
		  GetDecor(t, window_font_y) + 1,
		  t->name, strlen(t->name));
#endif
    }
  }
  /* now, draw lines in title bar if it's a sticky window */
  if (t->fSticky) {
    for (i = 0; i < t->title_height / 2 - 3; i += 4) {
      XDrawLine(dpy, t->title_w, ShadowGC, 4, t->title_height / 2 - i - 1,
		hor_off - 6, t->title_height / 2 - i - 1);
      XDrawLine(dpy, t->title_w, ShadowGC, 6 + hor_off + w, t->title_height / 2 - i - 1,
		t->title_width - 5, t->title_height / 2 - i - 1);
      XDrawLine(dpy, t->title_w, ReliefGC, 4, t->title_height / 2 - i,
		hor_off - 6, t->title_height / 2 - i);
      XDrawLine(dpy, t->title_w, ReliefGC, 6 + hor_off + w, t->title_height / 2 - i,
		t->title_width - 5, t->title_height / 2 - i);

      XDrawLine(dpy, t->title_w, ShadowGC, 4, t->title_height / 2 + i - 1,
		hor_off - 6, t->title_height / 2 + i - 1);
      XDrawLine(dpy, t->title_w, ShadowGC, 6 + hor_off + w, t->title_height / 2 + i - 1,
		t->title_width - 5, t->title_height / 2 + i - 1);
      XDrawLine(dpy, t->title_w, ReliefGC, 4, t->title_height / 2 + i,
		hor_off - 6, t->title_height / 2 + i);
      XDrawLine(dpy, t->title_w, ReliefGC, 6 + hor_off + w, t->title_height / 2 + i,
		t->title_width - 5, t->title_height / 2 + i);
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
RelieveWindow(ScwmWindow * t, Window win,
	      int x, int y, int w, int h,
	      GC ReliefGC, GC ShadowGC, int hilite)
{
  XSegment seg[4];
  int i;
  int edge;

  edge = 0;
  if ((win == t->sides[0]) || (win == t->sides[1]) ||
      (win == t->sides[2]) || (win == t->sides[3]))
    edge = -1;
  if (win == t->corners[0])
    edge = 1;
  if (win == t->corners[1])
    edge = 2;
  if (win == t->corners[2])
    edge = 3;
  if (win == t->corners[3])
    edge = 4;

  i = 0;
  seg[i].x1 = x;
  seg[i].y1 = y;
  seg[i].x2 = w + x - 1;
  seg[i++].y2 = y;

  seg[i].x1 = x;
  seg[i].y1 = y;
  seg[i].x2 = x;
  seg[i++].y2 = h + y - 1;

  if (((t->boundary_width > 2) || (edge == 0)) &&
      ((t->boundary_width > 3) || (edge < 1)) &&
      (!t->fMWMBorders ||
       (((edge == 0) || (t->boundary_width > 3)) && (hilite & TOP_HILITE)))) {
    seg[i].x1 = x + 1;
    seg[i].y1 = y + 1;
    seg[i].x2 = x + w - 2;
    seg[i++].y2 = y + 1;
  }
  if (((t->boundary_width > 2) || (edge == 0)) &&
      ((t->boundary_width > 3) || (edge < 1)) &&
      (!t->fMWMBorders ||
       (((edge == 0) || (t->boundary_width > 3)) && (hilite & LEFT_HILITE)))) {
    seg[i].x1 = x + 1;
    seg[i].y1 = y + 1;
    seg[i].x2 = x + 1;
    seg[i++].y2 = y + h - 2;
  }
  XDrawSegments(dpy, win, ReliefGC, seg, i);

  i = 0;
  seg[i].x1 = x;
  seg[i].y1 = y + h - 1;
  seg[i].x2 = w + x - 1;
  seg[i++].y2 = y + h - 1;

  if (((t->boundary_width > 2) || (edge == 0)) &&
      (!t->fMWMBorders ||
       (((edge == 0) ||
	 (t->boundary_width > 3)) && (hilite & BOTTOM_HILITE)))) {
    seg[i].x1 = x + 1;
    seg[i].y1 = y + h - 2;
    seg[i].x2 = x + w - 2;
    seg[i++].y2 = y + h - 2;
  }
  seg[i].x1 = x + w - 1;
  seg[i].y1 = y;
  seg[i].x2 = x + w - 1;
  seg[i++].y2 = y + h - 1;

  if (((t->boundary_width > 2) || (edge == 0)) &&
      (!t->fMWMBorders ||
       (((edge == 0) || 
	 (t->boundary_width > 3)) && (hilite & RIGHT_HILITE)))) {
    seg[i].x1 = x + w - 2;
    seg[i].y1 = y + 1;
    seg[i].x2 = x + w - 2;
    seg[i++].y2 = y + h - 2;
  }
  XDrawSegments(dpy, win, ShadowGC, seg, i);
}

/*
 *  Procedure:
 *      Setupframe - set window sizes
 *           This is called from lots and lots of places!
 *
 *  Inputs:
 *      psw - the ScwmWindow pointer
 *      x       - the x coordinate of the upper-left outer corner of the frame
 *      y       - the y coordinate of the upper-left outer corner of the frame
 *      w       - the width of the frame window w/o border
 *      h       - the height of the frame window w/o border
 *      sendEvent  - True if we want to force an event to be sent reflecting the change
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
SetupFrame(ScwmWindow * psw, int x, int y, int w, int h, Bool sendEvent,
           Bool fMoved, Bool fResized)
{
  XEvent client_event;
  XWindowChanges xwc;
  unsigned long xwcm;
  int cx, cy, i;
  int xwidth, ywidth, left, right;
  Bool shaded = SHADED_P(psw);

  assert(!fMoved || fMoved == WAS_MOVED);
  assert(!fResized || fResized == WAS_RESIZED);

  /* FIXMS: I think this can be safely removed, check RSN. */
  /* if windows is not being maximized, save size in case of maximization */
  if (!psw->fMaximized && !shaded) {
    psw->orig_x = x;
    psw->orig_y = y;
    psw->orig_wd = w;
    psw->orig_ht = h;
  }

  if (x >= Scr.MyDisplayWidth + Scr.VxMax - Scr.Vx - 16)
    x = Scr.MyDisplayWidth + Scr.VxMax - Scr.Vx - 16;
  if (y >= Scr.MyDisplayHeight + Scr.VyMax - Scr.Vy - 16)
    y = Scr.MyDisplayHeight + Scr.VyMax - Scr.Vy - 16;


#ifndef NDEBUG
  if ((w != FRAME_WIDTH(psw)) || (h != FRAME_HEIGHT(psw)) && !fResized)
    DBUG(__FUNCTION__,"Width/height changed but not fResized");

  if ((x != FRAME_X(psw) || y != FRAME_Y(psw)) && !fMoved)
    DBUG(__FUNCTION__,"Coords changed but not fMoved");
#endif

  /*
   * According to the July 27, 1988 ICCCM draft, we should send a
   * "synthetic" ConfigureNotify event to the client if the window
   * was moved but not resized.
   */
  if (fMoved && !fResized)
    sendEvent = True;

  if (fResized) {
    int button_width;
    DBUG(__FUNCTION__,"Resized!");
    left = psw->nr_left_buttons;
    right = psw->nr_right_buttons;

    if (SHOW_TITLE_P(psw)) {
      DBUG(__FUNCTION__,"Has title!");
      psw->title_height = GetDecor(psw, TitleHeight) + psw->bw;
      DBUG(__FUNCTION__,"Reset height to %d",psw->title_height);
    }
    /* make the decoration buttons square */
    button_width = psw->title_height;

    psw->title_width = w -
      (left + right) * button_width
      - 2 * psw->boundary_width + psw->bw;

    if (psw->title_width < 1)
      psw->title_width = 1;

    if (SHOW_TITLE_P(psw)) {
      psw->title_x = psw->boundary_width +
	(left * button_width);
      if (psw->title_x >= w - psw->boundary_width)
	psw->title_x = -10;
      psw->title_y = psw->boundary_width;

      XMoveResizeWindow(dpy, psw->title_w,
			psw->title_x, psw->title_y,
			psw->title_width, psw->title_height);

      xwcm = CWX | CWY | CWHeight | CWWidth;
      xwc.height = psw->title_height;
      xwc.width = button_width;
      xwc.y = psw->boundary_width;
      xwc.x = psw->boundary_width;
      for (i = 0; i < Scr.nr_left_buttons; i++) {
	if (psw->left_w[i] != None) {
	  if (xwc.x + button_width < w - psw->boundary_width)
	    XConfigureWindow(dpy, psw->left_w[i], xwcm, &xwc);
	  else {
	    xwc.x = -button_width;
	    XConfigureWindow(dpy, psw->left_w[i], xwcm, &xwc);
	  }
	  xwc.x += button_width;
	}
      }

      xwc.x = w - psw->boundary_width + psw->bw;
      for (i = 0; i < Scr.nr_right_buttons; i++) {
	if (psw->right_w[i] != None) {
	  xwc.x -= button_width;
	  if (xwc.x > psw->boundary_width)
	    XConfigureWindow(dpy, psw->right_w[i], xwcm, &xwc);
	  else {
	    xwc.x = -button_width;
	    XConfigureWindow(dpy, psw->right_w[i], xwcm, &xwc);
	  }
	}
      }
    }
    if (psw->fBorder) {
      DBUG(__FUNCTION__,"Has border!");
      psw->corner_width = GetDecor(psw, TitleHeight) + psw->bw +
	psw->boundary_width;

      if (w < 2 * psw->corner_width)
	psw->corner_width = w / 3;
      if ((h < 2 * psw->corner_width) && !shaded)
	psw->corner_width = h / 3;
      xwidth = w - 2 * psw->corner_width + psw->bw;
      ywidth = h - 2 * psw->corner_width;
      xwcm = CWWidth | CWHeight | CWX | CWY;
      if (xwidth < 2)
	xwidth = 2;
      if (ywidth < 2)
	ywidth = 2;

      for (i = 0; i < 4; i++) {
	if (i == 0) {
	  xwc.x = psw->corner_width;
	  xwc.y = 0;
	  xwc.height = psw->boundary_width;
	  xwc.width = xwidth;
	} else if (i == 1) {
	  xwc.x = w - psw->boundary_width + psw->bw;
	  xwc.y = psw->corner_width;
	  xwc.height = ywidth;
	  xwc.width = psw->boundary_width;
	} else if (i == 2) {
	  xwc.x = psw->corner_width;
	  xwc.y = h - psw->boundary_width + psw->bw;
	  xwc.height = psw->boundary_width + psw->bw;
	  xwc.width = xwidth;
	} else {
	  xwc.x = 0;
	  xwc.y = psw->corner_width;
	  xwc.width = psw->boundary_width;
	  xwc.height = ywidth;
	}
	if (!shaded || (i < 2)) /* do top corners even when shaded */
	  XConfigureWindow(dpy, psw->sides[i], xwcm, &xwc);
      }

      xwcm = CWX | CWY | CWWidth | CWHeight;
      xwc.width = psw->corner_width;
      xwc.height = psw->corner_width;
      for (i = 0; i < 4; i++) {
	if (i == 1 || i == 3)
	  xwc.x = w - psw->corner_width + psw->bw;
	else
	  xwc.x = 0;
	if (i == 2 || i == 3)
	  xwc.y = h - psw->corner_width;
	else
	  xwc.y = 0;
	if (!shaded || (i < 2)) /* do top corners even when shaded */
	  XConfigureWindow(dpy, psw->corners[i], xwcm, &xwc);
      }
    }
  }
  psw->attr.width = w - 2 * psw->boundary_width;
  psw->attr.height = h - psw->title_height
    - 2 * psw->boundary_width;
  /* may need to omit the -1 for shaped windows, next two lines */
  cx = psw->boundary_width - psw->bw;
  cy = psw->title_height + psw->boundary_width - psw->bw;

  if (!shaded) {
    XResizeWindow(dpy, psw->w, psw->attr.width,
		  psw->attr.height);
    XMoveResizeWindow(dpy, psw->Parent, cx, cy,
		      psw->attr.width, psw->attr.height);
  }

  /* 
   * fix up frame and assign size/location values in psw
   */
  DBUG(__FUNCTION__,"w = %d, h = %d", w, h);

#ifdef USE_CASSOWARY
  solver
    .setEditedValue(psw->frame_x,x)
    .setEditedValue(psw->frame_y,y)
    .setEditedValue(psw->frame_width,w)
    .setEditedValue(psw->frame_height,h);
#else
  FRAME_X(psw) = x;
  FRAME_Y(psw) = y;
  FRAME_WIDTH(psw) = w;
  FRAME_HEIGHT(psw) = h;
#endif
  XMoveResizeWindow(dpy, psw->frame, x, y, w, h);

  if (ShapesSupported) {
    if (fResized && psw->wShaped) {
      SetShape(psw, w);
    }
  }
  XSync(dpy, False);
  if (sendEvent && !shaded) {
    client_event.type = ConfigureNotify;
    client_event.xconfigure.display = dpy;
    client_event.xconfigure.event = psw->w;
    client_event.xconfigure.window = psw->w;

    client_event.xconfigure.x = x + psw->boundary_width;
    client_event.xconfigure.y = y + psw->title_height +
      psw->boundary_width;
    client_event.xconfigure.width = w - 2 * psw->boundary_width;
    client_event.xconfigure.height = h - 2 * psw->boundary_width -
      psw->title_height;

    client_event.xconfigure.border_width = psw->bw;
    /* Real ConfigureNotify events say we're above title window, so ... */
    /* what if we don't have a title ????? */
    client_event.xconfigure.above = psw->frame;
    client_event.xconfigure.override_redirect = False;
    XSendEvent(dpy, psw->w, False, StructureNotifyMask, &client_event);
  }
  BroadcastConfig(M_CONFIGURE_WINDOW, psw);
}


/****************************************************************************
 *
 * Sets up the shaped window borders 
 * 
 ****************************************************************************/
void 
SetShape(ScwmWindow * psw, int w)
{
  if (ShapesSupported) {
    XRectangle rect;

    XShapeCombineShape(dpy, psw->frame, ShapeBounding,
		       psw->boundary_width,
		       psw->title_height + psw->boundary_width,
		       psw->w,
		       ShapeBounding, ShapeSet);
    if (psw->title_w) {
      /* windows w/ titles */
      rect.x = psw->boundary_width;
      rect.y = psw->title_y;
      rect.width = w - 2 * psw->boundary_width + psw->bw;
      rect.height = psw->title_height;


      XShapeCombineRectangles(dpy, psw->frame, ShapeBounding,
			      0, 0, &rect, 1, ShapeUnion, Unsorted);
    }
  }
}

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
