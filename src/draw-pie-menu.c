/* $Id$
 * draw-pie-menu.c
 * By Greg J. Badros -- March 23, 1998
 * (C) 1998 Greg J. Badros and Maciej Stachowiak
 * This is the pie menu drawing code.
 * It currently is just the default menuing code with different #define-s
 * but will evolve as an example of a dynamically loadable menu look
 */

#include <config.h>

#include "scwm.h"
#include "scwmmenu.h"
#include "drawmenu.h"
#include "screen.h"
#include "font.h"
#include <guile/gh.h>
#include "xmisc.h"

/* FIXGJB: comment these! */
#define MENU_EDGE_VERT_SPACING 4
#define MENU_EDGE_HORIZ_SPACING 4
#define MENU_TEXT_SPACING 8
#define MENU_ITEM_PICTURE_EXTRA_VERT_SPACE 8
#define MENU_ITEM_EXTRA_VERT_SPACE 8
#define MENU_ITEM_LABEL_EXTRA_VERT_SPACE 4
#define MENU_ITEM_PICTURE_EXTRA_HORIZ_SPACE 8
#define MENU_SIDE_IMAGE_SPACING 10
#define MENU_ITEM_RR_SPACE 4
#define MENU_POPUP_ARROW_WIDTH 16
#define MENU_HEIGHT_SEPARATOR 8


#define INCREASE_MAYBE(var,val) do { if (val > var) { var = val; } } while (0)

static
XFontStruct *
PxfsFontForMenuItem(SCM scmFont)
{
  XFontStruct *pxfont = XFONT(Scr.menu_font);
  scwm_font *psfont = DYNAMIC_SAFE_FONT(scmFont);
  if (psfont) {
    pxfont = psfont->xfs;
  }
  return pxfont;
}


static void
PaintSideImage(Window w, Pixel bg, int cpixHeight, scwm_image *psimg)
{
  if (!psimg) {
    scwm_msg(ERR,__FUNCTION__,"psimg is NULL");
    return;
  }
  Globalgcv.foreground = bg;
  XChangeGC(dpy, Scr.ScratchGC1, GCForeground, &Globalgcv);
  XFillRectangle(dpy, w, Scr.ScratchGC1, 
		 MENU_ITEM_RR_SPACE, MENU_ITEM_RR_SPACE,
		 psimg->width, cpixHeight - 2*MENU_ITEM_RR_SPACE);
  DrawImage(w,psimg,MENU_ITEM_RR_SPACE,MENU_ITEM_RR_SPACE,NULL);
}

/*
 * RelieveRectangle - add relief lines to a rectangular window
 */
static
void
RelieveRectangle(Window win,int x,int y,int w, int h,GC Hilite,GC Shadow)
{
  XDrawLine(dpy, win, Hilite, x, y, w+x-1, y);
  XDrawLine(dpy, win, Hilite, x, y, x, h+y-1);
  XDrawLine(dpy, win, Shadow, x, h+y-1, w+x-1, h+y-1);
  XDrawLine(dpy, win, Shadow, w+x-1, y, w+x-1, h+y-1);
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
DrawUnderline(Window w, XFontStruct *pxfont, GC gc, char *sz, int x, int y, int posn) 
{
  int cpixStart = XTextWidth(pxfont, sz, posn);
  int cpixEnd = XTextWidth(pxfont, sz, posn + 1) - 1;
  XDrawLine(dpy, w, gc, x + cpixStart, y + 2, x + cpixEnd, y + 2);
}

static
void
PaintMenuItem(Window w, DynamicMenu *pmd, MenuItemInMenu *pmiim)
{
  /*  Menu *pmenu = pmd->pmenu; */
  MenuDrawingInfo *pmdi = pmd->pmdi;
  XFontStruct *pxfont = pmdi->pxfont;
  MenuItem *pmi = pmiim->pmi;
  int label_font_height = pxfont->ascent + pxfont->descent;
  int y_offset = pmiim->cpixOffsetY;
  int x_offset = pmdi->cpixItemOffset;
  int width = pmdi->cpixWidth;
  int item_height = pmiim->cpixItemHeight;
  GC ShadowGC = Scr.MenuShadowGC;
  GC ReliefGC = Scr.d_depth<2? Scr.MenuShadowGC: Scr.MenuReliefGC;
  GC currentGC = ShadowGC;
  GC gcImage = Scr.MenuGC;
  scwm_image *psimgLeft = SAFE_IMAGE(pmi->scmImgLeft);
  scwm_image *psimgAbove = SAFE_IMAGE(pmi->scmImgAbove);
  menu_item_state mis = pmiim->mis;

  /* code for FVWM menu look here -- should abstract for other options */

  /* Erase any old reliefs indicated selectedness */
  XClearArea(dpy, w,
	     x_offset-MENU_ITEM_RR_SPACE-1,
	     y_offset, width+MENU_ITEM_RR_SPACE+1, item_height, False);

  /* Draw the shadows for the absolute outside of the menus
     This stuff belongs in here, not in PaintMenu, since we only
     want to redraw it when we have too (i.e. on expose event) */

  /* Top of the menu */
  if (pmiim->fOnTopEdge) {
    DrawSeparator(w, ReliefGC,ReliefGC, 
		  0, width-1,0, -1);
  } 

  /* Bottom of the menu */
  if (pmiim->fOnBottomEdge) {
    DrawSeparator(w,ShadowGC,ShadowGC, 
		  0, width-1, y_offset + item_height,
		  1);
  }

  /* Only highlight if the item has an action */
  if (mis == MIS_Selected && !UNSET_SCM(pmi->scmAction)) {
    RelieveRectangle(w, x_offset-MENU_ITEM_RR_SPACE, y_offset,
		     width+MENU_ITEM_RR_SPACE, item_height,
		     ReliefGC,ShadowGC);
  }
  /* Add the markings for the left edge of the menu and 
     the right edge of the menu */
  RelieveHalfRectangle(w, 0, y_offset, 
		       width, item_height, 
		       ReliefGC, ShadowGC);

  if (pmi->fIsSeparator) {
    DrawSeparator(w,ShadowGC,ReliefGC,
		  x_offset-MENU_ITEM_RR_SPACE, width-2*MENU_ITEM_RR_SPACE,
		  y_offset-1+MENU_HEIGHT_SEPARATOR/2,0);
  } else {
    if (psimgAbove) {
      int x = (width - x_offset - psimgAbove->width) / 2 + x_offset;
      if (!psimgLeft && pmi->cchLabel == 0 && pmi->cchExtra == 0) {
	/* center psimgAbove vertically in the item_height */
	y_offset += (item_height - psimgAbove->height)/2;
      }
      DBUG(__FUNCTION__,"Drawing psimgAbove");
      DrawImage(w, psimgAbove, x, y_offset, gcImage);
      y_offset += psimgAbove->height;
    }

    /* center text vertically if the pixmap is taller */
    if (psimgLeft) {
      int cpixExtraYOffset = (item_height - psimgLeft->height) / 2;
      DrawImage(w, psimgLeft, x_offset, y_offset + cpixExtraYOffset, gcImage);
    }
       
    if (mis == MIS_Grayed) {
      currentGC = Scr.MenuStippleGC;
    } else {
      currentGC = Scr.MenuGC;
    }

    x_offset += pmdi->cpixLeftPicWidth + MENU_ITEM_PICTURE_EXTRA_HORIZ_SPACE;

    { /* FIXGJB: clean up this gc garbage --12/12/97 gjb */
      XGCValues gcv;
      gcv.font = pxfont->fid;
      gcv.foreground = pmdi->TextColor;
      XChangeGC(dpy,currentGC,GCFont | GCForeground,&gcv);
    }

    if (pmi->szLabel) {
      XDrawString(dpy, w, currentGC,
		  x_offset,y_offset + label_font_height, 
		  pmi->szLabel, pmi->cchLabel);
    }

    /* highlight the shortcut key */
    if (pmiim->ichShortcutOffset >= 0) {
      DrawUnderline(w, pxfont, currentGC, pmi->szLabel,
		    x_offset, y_offset+label_font_height, pmiim->ichShortcutOffset);
    }
  
    x_offset += pmdi->cpixTextWidth;

    if (pmi->szExtra) {
      XDrawString(dpy, w, currentGC,
		  x_offset, y_offset + label_font_height,
		  pmi->szExtra, pmi->cchExtra);
    }

    x_offset += pmdi->cpixExtraTextWidth;

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
  }
  return;
}

static
void 
PaintDynamicMenu(DynamicMenu *pmd, XEvent *pxe)
{
  Window w = pmd->pmdi->w;
  MenuDrawingInfo *pmdi = pmd->pmdi;
  MenuItemInMenu **rgpmiim = pmd->rgpmiim;
  int cmiim = pmd->cmiim;
  int imiim = 0;

  for (imiim = 0; imiim < cmiim; imiim++) {
    MenuItemInMenu *pmiim = rgpmiim[imiim];
    if (pxe->xexpose.y < (pmiim->cpixOffsetY  + pmiim->cpixItemHeight) &&
	(pxe->xexpose.y + pxe->xexpose.height) > pmiim->cpixOffsetY) {
      PaintMenuItem(w,pmd,pmiim);
    }
  }

  { /* scope */
    scwm_image *psimgSide = SAFE_IMAGE(pmd->pmenu->scmImgSide);
    if (psimgSide) {
      PaintSideImage(w,pmdi->SideBGColor,pmdi->cpixHeight,psimgSide);
    }
  }
  XSync(dpy,0);
}


/* ConstructDynamicMenu should try to do all the computations for the paint
   routine -- little should be done in the painting, as it'd be really
   hard to maintain the two routines in synch.  pmd->pmdi and pmd->rgpmiim should
   have all the information needed for drawing in response to expose events */
/* FIXGJB: this shouldn't have the _Pie suffix */
void
ConstructDynamicMenu(DynamicMenu *pmd)
{
  if (pmd->pmdi != NULL)
    return;
  /* remember how to paint this menu */
  pmd->fnPaintDynamicMenu = PaintDynamicMenu;
  pmd->fnPaintMenuItem = PaintMenuItem;
  { /* scope */
    Menu *pmenu = pmd->pmenu;
    scwm_image *psimgSide = SAFE_IMAGE(pmenu->scmImgSide);
    /*    scwm_image *psimgBackground = SAFE_IMAGE(pmenu->scmImgBackground);  */
    XFontStruct *pxfont;
    MenuItemInMenu **rgpmiim = pmd->rgpmiim;

    int cmiim = pmd->cmiim;
    int imiim = 0;
    int total_height = MENU_EDGE_VERT_SPACING;
    int max_text_width = 0;
    int max_extra_text_width = 0;
    int max_left_image_width = 0;
    int max_right_image_width = 0;
    int max_above_image_width = 0;
    int label_font_height = 0;
    int max_item_width = 0;

    MenuDrawingInfo *pmdi = pmd->pmdi = NEW(MenuDrawingInfo);

    pmdi->BGColor = DYNAMIC_SAFE_COLOR(pmenu->scmBGColor);
    pmdi->SideBGColor = DYNAMIC_SAFE_COLOR(pmenu->scmSideBGColor);
    pmdi->TextColor = DYNAMIC_SAFE_COLOR(pmenu->scmTextColor);
    pxfont = pmdi->pxfont = PxfsFontForMenuItem(pmenu->scmFont);
    /* FIXGJB:    MakeGcsForDynamicMenu(pmenu); */

    label_font_height = pxfont->ascent + pxfont->descent;
    
    pmdi->x = 0;		/* just init: gets set elsewhere */
    pmdi->y = 0;		/* just init: gets set elsewhere */
    pmdi->ccol = 1;

    for (imiim = 0; imiim < cmiim; imiim++) {
      MenuItemInMenu *pmiim = rgpmiim[imiim];
      MenuItem *pmi = pmiim->pmi;
      scwm_image *psimgAbove = SAFE_IMAGE(pmi->scmImgAbove);
      scwm_image *psimgLeft = SAFE_IMAGE(pmi->scmImgLeft);
      int text_width = XTextWidth(pxfont, pmi->szLabel, pmi->cchLabel);
      int extra_text_width = 0;
      int item_height = MENU_ITEM_EXTRA_VERT_SPACE * 2;
      pmiim->cpixOffsetY = total_height;

      DBUG(__FUNCTION__,"`%s' has width %d (%d chars)\n",
	   pmi->szLabel,text_width,pmi->cchLabel);

      if (pmi->fIsSeparator) {
	item_height = MENU_HEIGHT_SEPARATOR;
      } else {
	/* szLabel we know is not null, but szExtra can be */
	if (pmi->szExtra) {
	  extra_text_width = XTextWidth(pxfont, pmi->szExtra, pmi->cchExtra);
	}
      
	/* These are easy when using only one column */
	pmiim->fOnTopEdge = (imiim == 0);
	pmiim->fOnBottomEdge = (imiim == (cmiim - 1));

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
      pmiim->cpixItemHeight = item_height;
      total_height += item_height;
    }

    /* now set global menu drawing properties */
    /* Handle the side image, if any */
    pmdi->cpixItemOffset = MENU_ITEM_RR_SPACE + MENU_EDGE_HORIZ_SPACING;
    if (psimgSide) {
      pmdi->cpixSideImage = psimgSide->width + MENU_SIDE_IMAGE_SPACING;
      pmdi->cpixItemOffset += pmdi->cpixSideImage;
    }

    total_height += MENU_EDGE_VERT_SPACING;

    pmdi->cpixLeftPicWidth = max_left_image_width;
    pmdi->cpixTextWidth = max_text_width + MENU_TEXT_SPACING;
    pmdi->cpixExtraTextWidth = max_extra_text_width + MENU_TEXT_SPACING;
    pmdi->cpixHeight = total_height;

    /* use the width of the largest above image if it is greater than the sum of
       the widths of the other components */
    max_item_width = max(pmdi->cpixLeftPicWidth+
			 pmdi->cpixTextWidth+
			 pmdi->cpixExtraTextWidth+
			 max_right_image_width,
			 max_above_image_width);

    pmdi->cpixWidth = pmdi->cpixItemOffset + max_item_width +  
      MENU_ITEM_RR_SPACE*2 + MENU_EDGE_HORIZ_SPACING*2;

    DBUG(__FUNCTION__,"LeftPic = %d, Text = %d, ExtraText = %d, RightImage = %d; above = %d\n",
	 pmdi->cpixLeftPicWidth,pmdi->cpixTextWidth,pmdi->cpixExtraTextWidth,
	 max_right_image_width,max_above_image_width);

    /* Now create the window */
    { /* scope */
      unsigned long valuemask = (CWBackPixel | CWCursor | CWSaveUnder);
      XSetWindowAttributes attributes;
      attributes.background_pixel = pmdi->BGColor;
      attributes.cursor = Scr.ScwmCursors[CURSOR_MENU];
      attributes.save_under = True;

      pmdi->w = XCreateWindow(dpy, Scr.Root, 0, 0, pmdi->cpixWidth,
			      pmdi->cpixHeight, 0, CopyFromParent, InputOutput,
			      CopyFromParent, valuemask, &attributes);
    }
  }
}
#undef INCREASE_MAYBE

