/* $Id$
 * draw-pie-menu.c
 * By Todd Larason - 15 October 1998

 * Closely based on algorithms and code by Don Hopkins
 * (hopkins+@cs.cmu.edu, don@toad.com), as embodied in piewm
 * (<URL:http://www.crynwr.com/piewm/>).  For more information on pie
 * menus, see <URL:http://www.catalog.com/hopkins/piemenus>

 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License; either version 2
 * of the License, or (at your option) any later version.  In
 * addition, this code may be used with scwm or any other twm-derived
 * window manager distributed under the twm Evans and Sutherland
 * license or substantially similar licenses from other organizations.

 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <math.h>

#include <guile/gh.h>

#include "drawmenu.h"

#include "scwm.h"
#include "menu.h"
#include "menulook.h"
#include "screen.h"
#include "font.h"
#include "xmisc.h"

#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif

static SCM pie_menu_look = SCM_UNDEFINED;

extern SCM sym_top, sym_center, sym_bottom;

/* FIXGJB: comment these! */

/* Minimum radius for labels */
#define MENU_LABEL_RADIUS_MIN 36

/* Increase label radius this amount */
#define MENU_LABEL_RADIUS_STEP 12

/* Add this amount above smallest that works */
#define MENU_LABEL_RADIUS_EXTRA 6

/* Extra border to leave on all borders */
#define MENU_PIE_BORDER 2
/* Space to leave above and below menu contents */
#define MENU_EDGE_VERT_SPACING 2
#define MENU_EDGE_HORIZ_SPACING 4
#define MENU_TEXT_SPACING 8
#define MENU_ITEM_PICTURE_EXTRA_VERT_SPACE 8

/* Space to leave for menu labels, in addition to font height */
#define MENU_ITEM_LABEL_EXTRA_VERT_SPACE 2
#define MENU_ITEM_PICTURE_EXTRA_HORIZ_SPACE 8
#define MENU_SIDE_IMAGE_SPACING 10
#define MENU_ITEM_RR_SPACE 4
#define MENU_POPUP_ARROW_WIDTH 16
#define MENU_HEIGHT_SEPARATOR 8

static GC MenuGC;
static GC MenuStippleGC;
static GC MenuReliefGC;
static GC MenuShadowGC;

struct MenuDrawingInfo_tag
{
  int cpixXCenter;		/* horizontal center of pie, in X coords */
  int cpixYCenter;		/* vertical center of pie, in X coords */
  int cpixLabelRadius;		/* distance from center of pie for labels */
  int cpixInactiveRadius;	/* radius of inactive region in center */
  
  
//int cpixItemOffset;		/* how far from the left edge are items */
//int cpixLeftPicWidth;		/* how wide is the left image */
//int cpixTextWidth;		/* how wide are the text items */
//int cpixExtraTextWidth;	/* how wide are the text items */
//int cpixSideImage;		/* how wide is the side image */
  Pixel BGColor;		/* the background color */
//Pixel SideBGColor;		/* the side image bg color */
  Pixel TextColor;		/* the text color */
  Pixel StippleColor;		/* the stipple color */
  scwm_font *scfont;		/* To use scwm_font instead of XFont */
  void *p;			/* extra information needed by the client drawing code */
};

struct MenuItemDrawingInfo_tag
{
  int cpixLabelXOffset;		/* left x offset of the item */
  int cpixLabelYOffset;		/* top y offset of the item */
  int cpixLabelWidth;		/* width of label */
  int cpixLabelHeight;		/* height of label (needed?) */
  double rSliceCenter;		/* the center of this slice, in radians */
  double aSubtend;		/* angle subtended by this slice */
  float fDx, fDy;		/* cos and sin of rSliceCenter */
  int quadrant;			/* quadrant of leading edge */
  double fSlope;		/* slope of leading edge */
//Bool fOnTopEdge;		/* is this item on the top edge? */
//Bool fOnBottomEdge;		/* is this item on the bottom edge?  */
};

static void 
InitGCs()
{
  XGCValues gcv;
  unsigned long gcm;
  
  gcm = GCFunction | GCPlaneMask | GCGraphicsExposures | GCLineWidth | GCFillStyle;
  gcv.fill_style = FillSolid;
  gcv.plane_mask = AllPlanes;
  gcv.function = GXcopy;
  gcv.graphics_exposures = False;
  gcv.line_width = 0;
  MenuReliefGC = XCreateGC(dpy, Scr.Root, gcm, &gcv);
  MenuShadowGC = XCreateGC(dpy, Scr.Root, gcm, &gcv);
  MenuGC = XCreateGC(dpy, Scr.Root, gcm, &gcv);
  MenuStippleGC = XCreateGC(dpy, Scr.Root, gcm, &gcv);
}

static void
MakeGCs(DynamicMenu *pmd, scwm_font *scfont)
{
  static Pixel LastBGColor;
  static double last_hilight_factor, last_shadow_factor;
  unsigned long gcm;
  XGCValues gcv;
  Pixel Bright, Dim;
  static Bool GCs_initted;

  if (!GCs_initted) {
    InitGCs();
    GCs_initted = True;
  }

  if (pmd->pmdi->BGColor != LastBGColor ||
      menu_hilight_factor_val != last_hilight_factor ||
      menu_shadow_factor_val != last_shadow_factor) {
    /* Relief.fg */
    Bright = adjust_pixel_brightness(pmd->pmdi->BGColor,
				     menu_hilight_factor_val);
    /* Relief.bg */
    Dim = adjust_pixel_brightness(pmd->pmdi->BGColor,
				  menu_shadow_factor_val);
		  
    gcm = GCForeground | GCBackground;
    gcv.foreground = Bright;
    gcv.background = Dim;
    XChangeGC(dpy, MenuReliefGC, gcm, &gcv);

    gcm = GCForeground | GCBackground;
    gcv.foreground = Dim;
    gcv.background = Bright;
    XChangeGC(dpy, MenuShadowGC, gcm, &gcv);

    LastBGColor = pmd->pmdi->BGColor;
    last_hilight_factor = menu_hilight_factor_val;
    last_shadow_factor = menu_shadow_factor_val;
  }
  
  gcm = GCForeground | GCBackground;
  gcv.foreground = pmd->pmdi->TextColor;
  gcv.background = pmd->pmdi->BGColor;
#ifndef I18N
  gcm |= GCFont;
  gcv.font = scfont->xfs->fid;
#endif
  XChangeGC(dpy, MenuGC, gcm, &gcv);

  gcm = GCForeground | GCBackground;
/*  gcv.foreground = pmd->pmdi->TextColor; */
  gcv.foreground = pmd->pmdi->StippleColor;
  gcv.background = pmd->pmdi->BGColor;
  if (Scr.d_depth < 2) {
    gcm |= GCStipple | GCFillStyle;
    gcv.foreground = XCOLOR(BLACK_COLOR);
    gcv.stipple = Scr.gray_bitmap;
    gcv.fill_style = FillStippled;
  }
#ifndef I18N
  gcm |= GCFont;
  gcv.font = scfont->xfs->fid;
#endif
  XChangeGC(dpy, MenuStippleGC, gcm, &gcv);
}

static
scwm_font *
PscwmFontForMenuItem(SCM scmFont)
{
  scwm_font *scfont = DYNAMIC_SAFE_FONT(scmFont);
  if (!scfont) {
    scfont = FONT(scmFixedFont);
  }
  return scfont;
}

#if 0
static void
PaintSideImage(Window w, Pixel bg, int cpixHeight, scwm_image *psimg,
	       SCM align)
{
  int cpixDstYoffset, cpixSrcYoffset;
  int height;
  
  if (!psimg) {
    scwm_msg(ERR,__FUNCTION__,"psimg is NULL");
    return;
  }
  SetGCFg(Scr.ScratchGC1,bg);
  XFillRectangle(dpy, w, Scr.ScratchGC1, 
		 MENU_ITEM_RR_SPACE, MENU_ITEM_RR_SPACE,
		 psimg->width, cpixHeight - 2*MENU_ITEM_RR_SPACE);

  height = psimg->height;
  if (height > cpixHeight - 2*MENU_ITEM_RR_SPACE)
    height = cpixHeight - 2*MENU_ITEM_RR_SPACE;
  
  if (align == sym_top) {
    cpixDstYoffset = MENU_ITEM_RR_SPACE;
    cpixSrcYoffset = 0;
  } else if (align == sym_center) {
    if (psimg->height > height) {
      cpixDstYoffset = MENU_ITEM_RR_SPACE;
      cpixSrcYoffset = (psimg->height - height)/2;
    } else {
      cpixDstYoffset = (cpixHeight - height)/2;
      cpixSrcYoffset = 0;
    }
  } else {
    if (psimg->height > height) {
      cpixDstYoffset = MENU_ITEM_RR_SPACE;
      cpixSrcYoffset = psimg->height - height;
    } else {
      cpixDstYoffset = cpixHeight - height - MENU_ITEM_RR_SPACE;
      cpixSrcYoffset = 0;
    }
  }
  
  DrawSubImage(w, psimg,
	       MENU_ITEM_RR_SPACE, cpixDstYoffset,
	       0, cpixSrcYoffset,
	       psimg->width, height,
	       NULL);
}

/*
 * RelieveHalfRectangle - add relief lines to the sides only of a
 *      rectangular window
 */
static
void
RelieveHalfRectangle(Window win,int x,int y,int w,int h,
		     GC Hilite,GC Shadow)
{
  XDrawLine(dpy, win, Hilite, x, y-1, x, h+y);
  XDrawLine(dpy, win, Hilite, x+1, y, x+1, h+y-1);
  XDrawLine(dpy, win, Shadow, w+x-1, y-1, w+x-1, h+y);
  XDrawLine(dpy, win, Shadow, w+x-2, y, w+x-2, h+y-1);
}

/*
 *  Draws a two-pixel wide horizontal line to form a separator
 */
static
void 
DrawSeparator(Window w, GC TopGC, GC BottomGC, int x1, int x2, int y,
	      int extra_off)
{
  XDrawLine(dpy, w, TopGC, x1, y, x2, y);
  XDrawLine(dpy, w, BottomGC, x1 - extra_off, y + 1, x2 + extra_off, y + 1);
}

/*
 *  Draws a little Triangle pattern within a window
 */
static
void 
DrawTrianglePattern(Window w, GC GC1, GC GC2, GC GC3, int l, int u, int r, int b)
{
  int m;
  m = (u + b) / 2;
  XDrawLine(dpy, w, GC1, l, u, l, b);
  XDrawLine(dpy, w, GC2, l, b, r, m);
  XDrawLine(dpy, w, GC3, r, m, l, u);
}

#endif

/*
 * Procedure:
 *	DrawUnderline() - Underline a character in a string
 *
 * Calculate the pixel offsets to the start of the character position we
 * want to underline and to the next character in the string.  Shrink by
 * one pixel from each end and the draw a line that long two pixels below
 * the character...
 *
 */

static
void
DrawUnderline(Window w, scwm_font *scfont, GC gc, char *sz, int x, int y, int posn) 
{
  int cpixStart = ComputeXTextWidth(XFONT_FONTTYPE(scfont), sz, posn);
  int cpixEnd = ComputeXTextWidth(XFONT_FONTTYPE(scfont), sz, posn + 1) - 1;
  XDrawLine(dpy, w, gc, x + cpixStart, y + 2, x + cpixEnd, y + 2);
}


static
void
PaintMenuItemLabel(Window w, DynamicMenu *pmd, MenuItemInMenu *pmiim)
{
  /*  Menu *pmenu = pmd->pmenu; */
  MenuDrawingInfo *pmdi = pmd->pmdi;
  scwm_font *scfont = pmdi->scfont;
  int label_font_height = scfont->height;
  MenuItem *pmi = pmiim->pmi;
  int label_y_offset = pmiim->pmidi->cpixLabelYOffset;
  int label_x_offset = pmiim->pmidi->cpixLabelXOffset;
  int label_width = pmiim->pmidi->cpixLabelWidth;
  int label_height = pmiim->pmidi->cpixLabelHeight;
  GC ShadowGC = MenuShadowGC;
  GC ReliefGC = Scr.d_depth<2? MenuShadowGC: MenuReliefGC;
  GC currentGC;
//scwm_image *psimgLeft = SAFE_IMAGE(pmi->scmImgLeft);
//scwm_image *psimgAbove = SAFE_IMAGE(pmi->scmImgAbove);
  menu_item_state mis = pmiim->mis;
  int cpixExtraYOffset;
  
  /* FIXJTL: how expensive is XChangeGC()?  If bad, there should be a
     way of skipping this if they're still set up right from the last
     menu item; maybe the exported PaintMenuItem should set up the GCs
     and then call the real PaintMenuItem, and PaintMenu should set
     them up before looping over the real PaintMenuItem */
  
  MakeGCs(pmd, scfont);
  
  /* Erase any old reliefs indicated selectedness */
  XClearArea(dpy, w,
	     label_x_offset-MENU_ITEM_RR_SPACE-1,
	     label_y_offset-MENU_ITEM_RR_SPACE-1,
	     label_width+MENU_ITEM_RR_SPACE+1,
	     label_height+MENU_ITEM_RR_SPACE+1,
	     False);

  /* Only highlight if the item has an action */
  if (mis == MIS_Selected && !UNSET_SCM(pmi->scmAction)) {
    RelieveRectangle(w,
		     label_x_offset-MENU_ITEM_RR_SPACE,
		     label_y_offset-MENU_ITEM_RR_SPACE,
		     label_width+MENU_ITEM_RR_SPACE,
		     label_height+MENU_ITEM_RR_SPACE,
		     ReliefGC,ShadowGC);
  }

  if (pmi->fIsSeparator) {
    /* FIXJTL: what do separators even mean for pie menus?  1/3 width
       empty areas maybe? */
#if 0
    DrawSeparator(w,ShadowGC,ReliefGC,
		  x_offset-MENU_ITEM_RR_SPACE, width-2*MENU_ITEM_RR_SPACE,
		  y_offset-1+MENU_HEIGHT_SEPARATOR/2,0);
#endif
  } else {
#if 0
    if (psimgAbove) {
      int x = (width - x_offset - psimgAbove->width) / 2 + x_offset;
      if (!psimgLeft && pmi->cchLabel == 0 && pmi->cchExtra == 0) {
	/* center psimgAbove vertically in the item_height */
	y_offset += (item_height - psimgAbove->height)/2;
      }
      DBUG((DBG,__FUNCTION__,"Drawing psimgAbove"));
      DrawImage(w, psimgAbove, x, y_offset, MenuGC);
      y_offset += psimgAbove->height;
    }
#endif
#if 0
    /* center image vertically */
    if (psimgLeft) {
      cpixExtraYOffset = (item_height - psimgLeft->height) / 2;
      DrawImage(w, psimgLeft, x_offset, y_offset + cpixExtraYOffset, MenuGC);
    }
#endif  
    if (mis == MIS_Grayed) {
      currentGC = MenuStippleGC;
    } else {
      currentGC = MenuGC;
    }

    cpixExtraYOffset = ((pmiim->pmidi->cpixLabelHeight-label_font_height)/2)-2;
    
    if (pmi->szLabel) {
#ifdef I18N
      XmbDrawString(dpy, w, scfont->fontset, currentGC,
		  x_offset,y_offset + label_font_height + cpixExtraYOffset, 
		  pmi->szLabel, pmi->cchLabel);
#else
      XDrawString(dpy, w, currentGC,
		  label_x_offset,
		  label_y_offset + label_font_height + cpixExtraYOffset, 
		  pmi->szLabel, pmi->cchLabel);
#endif
    }

    /* highlight the shortcut key */
    if (pmiim->ichShortcutOffset >= 0) {
      DrawUnderline(w, scfont, currentGC, pmi->szLabel,
		    label_x_offset,
		    label_y_offset + label_font_height + cpixExtraYOffset,
		    pmiim->ichShortcutOffset);
    }

#if 0
    label_x_offset += pmdi->cpixTextWidth;

    if (pmi->szExtra) {
#ifdef I18N
      XmbDrawString(dpy, w, scfont->fontset, currentGC,
		  x_offset, y_offset + label_font_height + cpixExtraYOffset,
		  pmi->szExtra, pmi->cchExtra);
#else
      XDrawString(dpy, w, currentGC,
		  x_offset, y_offset + label_font_height + cpixExtraYOffset,
		  pmi->szExtra, pmi->cchExtra);
#endif
    }

    x_offset += pmdi->cpixExtraTextWidth;
#endif
#if 0
    if (pmiim->fShowPopupArrow) {
      int d = (item_height-7)/2; /* FIXGJB: magic numbers! */
      if (mis != MIS_Enabled) {
	DrawTrianglePattern(w, ShadowGC, ReliefGC, ShadowGC, /* ReliefGC, */
			    width-d-8, y_offset+d-1, width-d-1, y_offset+d+7);
      } else {
	DrawTrianglePattern(w, ReliefGC, ShadowGC, ReliefGC, /* ShadowGC, */
			    width-d-8, y_offset+d-1, width-d-1, y_offset+d+7);
      }
    }
#endif
  }
  return;
}

static
void 
PaintDynamicMenu(DynamicMenu *pmd, XEvent *pxe)
{
  Window w = pmd->w;
  MenuDrawingInfo *pmdi = pmd->pmdi;
  MenuItemInMenu **rgpmiim = pmd->rgpmiim;
  int cmiim = pmd->cmiim;
  int imiim = 0;

  for (imiim = 0; imiim < cmiim; imiim++) {
    MenuItemInMenu *pmiim = rgpmiim[imiim];
    if (pxe->xexpose.y < (pmiim->pmidi->cpixLabelYOffset +
			  pmiim->pmidi->cpixLabelHeight) &&
	((pxe->xexpose.y + pxe->xexpose.height) >
	 pmiim->pmidi->cpixLabelYOffset)) {
      DBUG((DBG,__FUNCTION__,"Painting menu item Label"));
      PaintMenuItemLabel(w,pmd,pmiim);
    }
  }
  /* FIXJTL: score lines */
  
#if 0
  { /* scope */
    scwm_image *psimgSide = SAFE_IMAGE(pmd->pmenu->scmImgSide);
    if (psimgSide) {
      DBUG((DBG,__FUNCTION__,"Painting side image"));
      PaintSideImage(w,pmdi->SideBGColor,pmd->cpixHeight,psimgSide,
		     pmd->pmenu->scmSideAlign);
    }
  }
#endif
  XSync(dpy,0);
}

static
void
SetPopupMenuPositionFromMenuItem(DynamicMenu *pmdNew, 
				 MenuItemInMenu *pmiimSelected)
{
  /* FIXJTL: do 'flyright' menus have any reasonable implementation for
     pie menus:  if so, should probably be something like:

     GetPointerPosition(&x, &y);
     pmdNew->x = x - pmdNew->pmdi->cpixCenterX;
     pmdNew->y = y - pmdNew->pmdi->cpixCenterY;
  */
  pmdNew->x = 0;
  pmdNew->y = 0;
}

static
void
WarpPointerToPmiim(MenuItemInMenu *pmiim)
{
  DynamicMenu *pmd; 
  int x, y;

  if (!pmiim)
    return;

  pmd = pmiim->pmd;

  /* FIXJTL: center for straight up or down?  or along center line,
     2 * inactive_radius out? */
  x = pmd->x + pmiim->pmidi->cpixLabelXOffset;
  y = pmd->y + pmiim->pmidi->cpixLabelYOffset;
  XWarpPointer(dpy, 0, pmd->w, 0, 0, 0, 0, x, y);
}

static
void
CalculateQuadrantSlope(float fDx, float fDy, int * piQuadrant,
		       float * pfNumerator, float * pfDenominator)
{
  int iQuadrant;
  float fNumerator, fDenominator;
  
  if (fDy > 0) {
    if (fDx > 0) {
      iQuadrant = 0;
    } else {
      iQuadrant = 1;
    }
  } else if (fDy < 0) {
    if (fDx < 0) {
      iQuadrant = 2;
    } else {
      iQuadrant = 3;
    }
  } else {
    if (fDx > 0) {
      iQuadrant = 0;
    } else {
      iQuadrant = 2;
    }
  }
  if (iQuadrant & 1) {
    fNumerator = ABS(fDx);
    fDenominator = ABS(fDy);
  } else {
    fNumerator = ABS(fDy);
    fDenominator = ABS(fDx);
  }

  *piQuadrant = iQuadrant;
  *pfNumerator = fNumerator;
  *pfDenominator = fDenominator;
}

static
MenuItemInMenu *
PmiimFromPmdXY(DynamicMenu *pmd, int x, int y)
{
  int ipmiim;
  MenuDrawingInfo *pmdi = pmd->pmdi;
  MenuItemInMenu * pmiimLast;
  int cpixRadius;
  float numerator, denominator;
  int quadrant, order, last_order;
  
  /* Translate x and y to pie coordinants */
  /* FIXJTL: piewm has x +1 and y -1; why? */
  x = x - pmdi->cpixXCenter;
  y = pmdi->cpixYCenter - y;

  /* Special case: no menu items */
  if (pmd->cmiim == 0)
    return NULL;

  /* Special case: in inactive region in center */
  if ((x * x) + (y * y) <
      (pmdi->cpixInactiveRadius * pmdi->cpixInactiveRadius))
    return NULL;

  /* Special case: one item */
  if (pmd->cmiim == 1)
    return pmd->rgpmiim[0];

  /* quadrant/slope algorithm by Don Hopkins:
   *
   * This pie menu tracking code determines the slice the cursor 
   * is in by representing slice edge angles as (quadrant, slope) 
   * pairs that can be quickly computed and compared. 
   *
   * The slope is defined such that it is greater than or equal to zero,
   * less than infinity, and increasing counter-clockwise around the menu. 
   * Each of the four quadrants encompasses one range of slope.
   *
   *                 Y
   *               ^
   *               |     x>0, y>=0
   *  x<=0, y>0 <--+       y/x
   *    -x/y       |        ^
   *        quad 1 | quad 0 |     X
   * -----+--------+--------+----> 
   *      | quad 2 | quad 3
   *      V        |      -x/y
   *   x<0, y<=0   +--> x>=0, y<0
   *     y/x       |
   *               |
   * 
   * The quadrants and slopes of the item edges are all precalculated,
   * during menu layout.
   * The quadrant and slope of the cursor must be calculated frequently
   * during menu tracking, so we just calculate the numerator and
   * denominator of the slope, and avoid an unnecessary division.
   * Instead of calculating "slope = numerator / denominator" then
   * testing "slope < it->slope", every time the cursor moves, we can
   * just test "numerator < (denominator * it->slope)".
   *
   * This algorithm works in a right-side-up coordinate space, but the final
   * results are tranformed into X-windows's up-side-down coordinate system 
   * by subtracting the y values from the window height. 
   */

  CalculateQuadrantSlope(x, y, &quadrant,
			 &numerator, &denominator);
  /* FIXJTL: Possible optimization: look at current selection first,
     then neighbors, then further away */

  pmiimLast = NULL;
  last_order = -1;

  /* FIXJTL: this goes through two more iterations than the piewm one
     does, I think, but my understanding seems to say it's neccessary;
     what am I missing? */
  for (ipmiim = 0; ipmiim <= pmd->cmiim + 1; ipmiim++) {
    /* Legend: c = cursor, e = edge
       <cursor quad>,<edge quad>
             quad 1 | quad 0
	     -------+-------
	     quad 2 | quad 3

	order = 1 if shortest path from edge to cursor is counter clock wise
    */
    MenuItemInMenu *pmiim = pmd->rgpmiim[ipmiim % pmd->cmiim ];

    switch ((quadrant - pmiim->pmidi->quadrant) & 3) {
      case 0:
	/*
		 0,0	 1,1	 2,2	 3,3 
		  |ce	ce|	  |	  |  
		--+--	--+--	--+--	--+--
		  |	  |	ce|	  |ce
	*/
	/* slope >= it->slope */
	order = (numerator >= (denominator * pmiim->pmidi->fSlope));
	break;

      case 1:
	/*
		 1,0	 2,1	 3,2	 0,3 
		 c|e	 e|	  |	  |c 
		--+--	--+--	--+--	--+--
		  |	 c|	 e|c	  |e 
	*/
	order = 1;
	break;

      case 2:
	/*
		 2,0	 3,1	 0,2	 1,3
		  |e	 e|	  |c	 c|
		--+--	--+--	--+--	--+--
		 c|	  |c	 e|	  |e
	*/
	/* slope < it->slope */
	order = (numerator < (denominator * pmiim->pmidi->fSlope));
	break;

      case 3:
	/*
		 3,0	 0,1	 1,2	 2,3
		  |e	 e|c	 c|	  |
		--+--	--+--	--+--	--+--
		  |c	  |	 e|	 c|e
	*/
	order = 0;
	break;
    }
    /* If we were ccw of the last leading edge edge, and cw of this one,
       then we were in the last menu item */
    if ((last_order == 1) && (order == 0)) {
      return pmiimLast;
    }

    last_order = order;
    pmiimLast = pmiim;
  }
  return NULL;
}

static int
InPopupZone(MenuItemInMenu *pmiim, int cpixXoffset, int cpixYoffset)
{
  return False;
}

/* px and py are in & out parameters; return the x,y location for the
   top left of a child popup when the mouse was in a popup zone at
   x,y; This function doesn't have to concern itself with screen
   borders */
static void
GetChildPopupPosition(DynamicMenu * pmd, int *px, int *py)
{
  *px = *px - pmd->pmdi->cpixXCenter;
  *py = *py - pmd->pmdi->cpixYCenter;
}

/* px and py are in & out parameters; return the x,y location for the
   top left of the popup when the mouse was clicked at x,y; This function
   doesn't have to concern itself with screen borders or with being
   popped up from another menu or decoration */
static void
GetPreferredPopupPosition(DynamicMenu * pmd, int *px, int *py)
{
  *px = *px - pmd->pmdi->cpixXCenter;
  *py = *py - pmd->pmdi->cpixYCenter;
}

static
void
FreePmdi(MenuDrawingInfo * pmdi)
{
  FREE(pmdi);
}

static
void
FreePmidi(MenuItemDrawingInfo * pmidi)
{
  FREE(pmidi);
}


#define INCREASE_MAYBE(var,val) do { if (val > var) { var = val; } } while (0)
#define DECREASE_MAYBE(var,val) do { if (val < var) { var = val; } } while (0)
#define MIN(v1,v2) ((v1)<(v2) ? (v1) : (v2))
#define MAX(v1,v2) ((v1)>(v2) ? (v1) : (v2))

/* ConstructDynamicPieMenu should try to do all the computations for
   the paint routine -- little should be done in the painting, as it'd
   be really hard to maintain the two routines in synch.  pmd->pmdi
   and pmd->rgpmiim should have all the information needed for drawing
   in response to expose events */
void
ConstructDynamicPieMenu(DynamicMenu *pmd)
{
  float aSubtend, angle;
  int label_height, cpixRadius, cpixXMin, cpixXMax, cpixYMin, cpixYMax;
  MenuItemInMenu * pmiimLast;
  int imiim = 0;
  int cmiim = pmd->cmiim;
  MenuItemInMenu **rgpmiim = pmd->rgpmiim;
  
  if (pmd->pmdi != NULL)
    return;

  /* FIXJTL: Things left out for now:
     extra text
     left/right/above images
     side images
     certainly other things
  */
  /* remember how to paint this menu */
  pmd->pmdv = DYNAMIC_SAFE_MENULOOK(pmd->pmenu->scmMenuLook)->mdvt;

  { /* scope */
    Menu *pmenu = pmd->pmenu;
    scwm_font *scfont;
    int imiim = 0;
    int total_height = MENU_EDGE_VERT_SPACING;
    int label_font_height = 0;

    MenuDrawingInfo *pmdi = pmd->pmdi = NEW(MenuDrawingInfo);
    
    pmdi->BGColor = DYNAMIC_SAFE_COLOR(pmenu->scmBGColor);
//  pmdi->SideBGColor = DYNAMIC_SAFE_COLOR(pmenu->scmSideBGColor);
    pmdi->TextColor = DYNAMIC_SAFE_COLOR(pmenu->scmTextColor);
    pmdi->StippleColor = DYNAMIC_SAFE_COLOR(pmenu->scmStippleColor);
    
    scfont = pmdi->scfont = PscwmFontForMenuItem(pmenu->scmFont);
 
    label_font_height = scfont->height;
    
    aSubtend = (M_PI*2)/cmiim;

    label_height = label_font_height + MENU_ITEM_LABEL_EXTRA_VERT_SPACE;

    angle = 0 - aSubtend/2.0;
    for (imiim = 0; imiim < cmiim; imiim++) {
      /* angle is angle of leading edge here */
	 
      float fDxEdge, fDyEdge, fNumerator, fDenominator;
      int iQuadrant;
      MenuItemInMenu *pmiim = rgpmiim[imiim];
      MenuItem *pmi = pmiim->pmi;
//    scwm_image *psimgAbove = SAFE_IMAGE(pmi->scmImgAbove);
//    scwm_image *psimgLeft = SAFE_IMAGE(pmi->scmImgLeft);
      int text_width;
//    int extra_text_width = 0;
      int item_height = 0; // = MENU_ITEM_EXTRA_VERT_SPACE * 2;

      pmiim->pmidi = NEW(MenuItemDrawingInfo);
      pmiim->pmidi->aSubtend = aSubtend;
      
      angle += aSubtend/2.0;

      /* angle is angle of center of slice here */

      pmiim->pmidi->rSliceCenter = angle;
      pmiim->pmidi->fDx = cos(angle);
      pmiim->pmidi->fDy = sin(angle);

      fDxEdge = cos(angle - aSubtend/2.0);
      fDyEdge = sin(angle - aSubtend/2.0);

      CalculateQuadrantSlope(fDxEdge, fDyEdge, &iQuadrant,
			     &fNumerator, &fDenominator);
      pmiim->pmidi->quadrant = iQuadrant;
      pmiim->pmidi->fSlope = fNumerator/fDenominator;

      angle += aSubtend/2.0;
      /* angle is now angle of trailing edge */

      text_width = ComputeXTextWidth(XFONT_FONTTYPE(scfont), pmi->szLabel, pmi->cchLabel);

      pmiim->pmidi->cpixLabelWidth = text_width;
      pmiim->pmidi->cpixLabelHeight = label_height;
      
      DBUG((DBG,__FUNCTION__,"`%s' has width %d (%d chars)\n",
	   pmi->szLabel,text_width,pmi->cchLabel));
#if 0
      if (pmi->fIsSeparator) {
	item_height = MENU_HEIGHT_SEPARATOR;
      } else {
	/* szLabel we know is not null, but szExtra can be */
	if (pmi->szExtra) {
          extra_text_width = ComputeXTextWidth(XFONT_FONTTYPE(scfont), 
                                               pmi->szExtra, pmi->cchExtra);
	}
      
	/* These are easy when using only one column */
	pmiim->pmidi->fOnTopEdge = (imiim == 0);
	pmiim->pmidi->fOnBottomEdge = (imiim == (cmiim - 1));

	if (pmi->cchLabel != 0 || pmi->cchExtra != 0) {
	  item_height += label_font_height + MENU_ITEM_LABEL_EXTRA_VERT_SPACE;
	}

	INCREASE_MAYBE(max_text_width,text_width);
	INCREASE_MAYBE(max_extra_text_width,extra_text_width);
      
	if (psimgAbove) {
	  int height = psimgAbove->height + MENU_ITEM_PICTURE_EXTRA_VERT_SPACE;
	  item_height += height;
	  INCREASE_MAYBE(max_above_image_width,psimgAbove->width);
	}
	if (psimgLeft) {
	  int height = psimgLeft->height + MENU_ITEM_PICTURE_EXTRA_VERT_SPACE;
	  INCREASE_MAYBE(item_height,height);
	  INCREASE_MAYBE(max_left_image_width,
			 psimgLeft->width + MENU_ITEM_PICTURE_EXTRA_HORIZ_SPACE);
	}

	if (pmiim->fShowPopupArrow) {
	  INCREASE_MAYBE(max_right_image_width, MENU_POPUP_ARROW_WIDTH);
	}
      }
      pmiim->pmidi->cpixItemHeight = item_height;
      total_height += item_height;
#endif
    }

    cpixRadius = MENU_LABEL_RADIUS_MIN;

    pmiimLast = rgpmiim[cmiim - 1];
    for (imiim = 0; imiim <= cmiim && cmiim > 1; imiim++) {
      MenuItemInMenu * pmiim = rgpmiim[imiim % cmiim];
      float fDx, fDy, fDxLast, fDyLast;
      int cpixWidth, cpixHeight, cpixWidthLast, cpixHeightLast;

      fDx = pmiim->pmidi->fDx;
      fDy = pmiim->pmidi->fDy;
      cpixWidth = pmiim->pmidi->cpixLabelWidth;
      cpixHeight = pmiim->pmidi->cpixLabelHeight;

      fDxLast = pmiimLast->pmidi->fDx;
      fDyLast = pmiimLast->pmidi->fDy;
      cpixWidthLast = pmiim->pmidi->cpixLabelWidth;
      cpixHeightLast = pmiim->pmidi->cpixLabelHeight;

      while (1) {
	int cpixX, cpixY,
	  cpixXLast, cpixYLast,
	  cpixLeftMax, cpixTopMax,
	  cpixRightMin, cpixBottomMin;

	cpixX     = fDx     * cpixRadius; cpixY     = fDy     * cpixRadius;
	cpixXLast = fDxLast * cpixRadius; cpixYLast = fDyLast * cpixRadius;

	/* If top or bottom, center label */
	if (ABS(cpixX) <= 2) {
	  cpixX -= cpixWidth / 2;
	  if (cpixY < 0)
	    cpixY -= cpixHeight;
	} else {
	  /* shift left to put right edge in right place, if on the left
	     side of the pie */
	  if (cpixX < 0)
	    cpixX -= cpixWidth;
	  cpixY -= cpixHeight/2;
	}

	if (ABS(cpixXLast) <= 2) {
	  cpixXLast -= cpixWidthLast / 2;
	  if (cpixYLast < 0)
	    cpixYLast -= cpixHeightLast;
	} else {
	  /* shift left to put right edge in right place, if on the left
	     side of the pie */
	  if (cpixXLast < 0)
	    cpixXLast -= cpixWidthLast;
	  cpixYLast -= cpixHeightLast/2;
	}

	/* Do the rectangles overlap? */
	cpixLeftMax = MAX(cpixX, cpixXLast);
	cpixTopMax = MAX(cpixY, cpixYLast);
	cpixRightMin = MIN(cpixX + cpixWidth, cpixXLast + cpixWidthLast);
	cpixBottomMin = MIN(cpixY + cpixHeight, cpixYLast + cpixHeightLast);
	if (!(cpixLeftMax < cpixRightMin &&
	      cpixTopMax < cpixBottomMin))
	  /* They fit - go to the next */
	  break;

	/* They don't - move further out and try again */
	cpixRadius += MENU_LABEL_RADIUS_STEP;
      }
      pmiimLast = pmiim;
    }
  }

  cpixRadius += MENU_LABEL_RADIUS_EXTRA;
  pmd->pmdi->cpixLabelRadius = cpixRadius;

  /* One more loop, to set all the label positions */
  cpixXMin = cpixYMin = cpixXMax = cpixYMax = 0;
  for (imiim = 0; imiim < cmiim; imiim++) {
    MenuItemInMenu *pmiim = rgpmiim[imiim];

    pmiim->pmidi->cpixLabelXOffset = cpixRadius * pmiim->pmidi->fDx;
    pmiim->pmidi->cpixLabelYOffset = cpixRadius * pmiim->pmidi->fDy;

    if (ABS(pmiim->pmidi->cpixLabelXOffset <= 2) ) {
      pmiim->pmidi->cpixLabelXOffset -= pmiim->pmidi->cpixLabelWidth / 2;
      if (pmiim->pmidi->cpixLabelYOffset < 0)
	pmiim->pmidi->cpixLabelYOffset -= pmiim->pmidi->cpixLabelHeight;
    } else {
      if (pmiim->pmidi->cpixLabelXOffset < 0)
	pmiim->pmidi->cpixLabelXOffset -= pmiim->pmidi->cpixLabelWidth;
      pmiim->pmidi->cpixLabelYOffset -= pmiim->pmidi->cpixLabelHeight/2;
    }

    DECREASE_MAYBE(cpixXMin, pmiim->pmidi->cpixLabelXOffset);
    INCREASE_MAYBE(cpixXMax,
		   pmiim->pmidi->cpixLabelXOffset +
		   pmiim->pmidi->cpixLabelWidth);
    DECREASE_MAYBE(cpixYMin, pmiim->pmidi->cpixLabelYOffset);
    INCREASE_MAYBE(cpixYMax,
		   pmiim->pmidi->cpixLabelYOffset +
		   pmiim->pmidi->cpixLabelHeight);
  }
  
  /* FIXJTL: title */

  cpixXMin -= MENU_PIE_BORDER;
  cpixYMin -= MENU_PIE_BORDER;
  cpixXMax += MENU_PIE_BORDER;
  cpixYMax += MENU_PIE_BORDER;

  pmd->pmdi->cpixXCenter = -cpixXMin;
  pmd->pmdi->cpixYCenter = cpixYMax; /* Y is flipped */
  pmd->cpixWidth = cpixXMax - cpixXMin;
  pmd->cpixHeight = cpixYMax - cpixYMin;

  /* And ONE MORE TIME, with emphasis - rearrange coordinates, in
     relation to center */
  for (imiim = 0; imiim < cmiim; imiim++) {
    MenuItemInMenu *pmiim = rgpmiim[imiim];
    pmiim->pmidi->cpixLabelXOffset += pmd->pmdi->cpixXCenter;
    pmiim->pmidi->cpixLabelYOffset =
      (pmd->pmdi->cpixYCenter - pmiim->pmidi->cpixLabelYOffset) - pmiim->pmidi->cpixLabelHeight;
  }

  /* FIXJTL: segments */

  /* Now create the window */
  { /* scope */
    unsigned long valuemask = (CWBackPixel | CWCursor | CWSaveUnder);
    XSetWindowAttributes attributes;
    attributes.background_pixel = pmd->pmdi->BGColor;
    attributes.cursor = Scr.ScwmCursors[CURSOR_MENU];
    attributes.save_under = True;

    pmd->w = XCreateWindow(dpy, Scr.Root, 0, 0, pmd->cpixWidth,
			   pmd->cpixHeight, 0, CopyFromParent, InputOutput,
			   CopyFromParent, valuemask, &attributes);

//      if (psimgBackground) {
//	XSetWindowBackgroundPixmap(dpy, pmd->w, psimgBackground->image);
//      }
  }
}
#undef INCREASE_MAYBE
#undef DECREASE_MAYBE
#undef MIN
#undef MAX
void 
drawpiemenu_init_gcs()
{
  XGCValues gcv;
  unsigned long gcm;

  gcm = GCFunction | GCPlaneMask | GCGraphicsExposures | GCLineWidth | GCFillStyle;
  gcv.fill_style = FillSolid;
  gcv.plane_mask = AllPlanes;
  gcv.function = GXcopy;
  gcv.graphics_exposures = False;
  gcv.line_width = 0;
  MenuReliefGC = XCreateGC(dpy, Scr.Root, gcm, &gcv);
  MenuShadowGC = XCreateGC(dpy, Scr.Root, gcm, &gcv);
  MenuGC = XCreateGC(dpy, Scr.Root, gcm, &gcv);
  MenuStippleGC = XCreateGC(dpy, Scr.Root, gcm, &gcv);
}

static
void
init_draw_pie_menu()
{
  MenuDrawingVtable * pmdvt;
  
  pmdvt = NEW(MenuDrawingVtable);
  memset(pmdvt, 0, sizeof *pmdvt);
  
  pmdvt->fnConstructDynamicMenu = ConstructDynamicPieMenu;
  pmdvt->fnPaintDynamicMenu = PaintDynamicMenu;
  pmdvt->fnPaintMenuItem = PaintMenuItemLabel;
  pmdvt->fnSetPopupMenuPositionFromMenuItem = SetPopupMenuPositionFromMenuItem;
  pmdvt->fnGetChildPopupPosition = GetChildPopupPosition;
  pmdvt->fnGetPreferredPopupPosition = GetPreferredPopupPosition;
  pmdvt->fnWarpPointerToPmiim = WarpPointerToPmiim;
  pmdvt->fnPmiimFromPmdXY = PmiimFromPmdXY;
  pmdvt->fnInPopupZone = InPopupZone;
  pmdvt->fnFreePmdi = FreePmdi;
  pmdvt->fnFreePmidi = FreePmidi;
  
  pie_menu_look = make_menulook("pie-menu-look", SCM_BOOL_T, pmdvt);
  SCWM_VAR_READ_ONLY(NULL,"pie-menu-look",pie_menu_look);
#ifndef SCM_MAGIC_SNARFER
#include "draw-pie-menu.x"
#endif
}

void scm_init_app_scwm_pie_menus_module()
{
  scm_register_module_xxx("app scwm pie-menus", init_draw_pie_menu);
}

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
