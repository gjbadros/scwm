/* $Id$
 * drawmenu.c
 * By Greg J. Badros -- Nov 22, 1997
 * This is the default menu drawing code.
 * Later we can make this interface be implemented by
 * dynamically loaded functions
 */

#include "scwm.h"
#include "scwmmenu.h"
#include "drawmenu.h"
#include "system.h"
#include "screen.h"
#include "font.h"
#include "menus.h"
#include <guile/gh.h>
#include "xmisc.h"

#define MENU_EDGE_SPACING 2
#define MENU_TEXT_SPACING 4
#define MENU_ITEM_PICTURE_EXTRA_VERT_SPACE 4
#define MENU_ITEM_LABEL_EXTRA_VERT_SPACE 4
#define MENU_ITEM_PICTURE_EXTRA_HORIZ_SPACE 4
#define MENU_SIDE_IMAGE_SPACING 3
#define MENU_ITEM_RR_SPACE 2
#define MENU_POPUP_ARROW_WIDTH 8


#define INCREASE_MAYBE(var,val) do { if (val > var) { var = val; } } while (0)

/* ConstructDynamicMenu should try to do all the computations for the paint
   routine -- little should be done in the painting, as it'd be really
   hard to maintain the two routines in synch.  pmd->pmdi and pmd->rgpmiim should
   have all the information needed for drawing in response to expose events */
void
ConstructDynamicMenu(DynamicMenu *pmd)
{
  if (pmd->pmdi != NULL)
    return;
  { /* scope */
    Scwm_Menu *pmenu = pmd->pmenu;
    Picture *picSide = pmenu->picSide;
    Picture *picBackground = pmenu->picBackground;
    XFontStruct *pxfont;
    MenuItemInMenu **rgpmiim = pmd->rgpmiim;

    int cmiim = pmd->cmiim;
    int imiim = 0;
    int total_height = MENU_EDGE_SPACING;
    int max_text_width = 0;
    int max_extra_text_width = 0;
    int max_left_image_width = 0;
    int max_right_image_width = 0;
    int max_above_image_width = 0;
    int label_font_height = 0;
    int max_item_width = 0;

    MenuDrawingInfo *pmdi = pmd->pmdi = safemalloc(sizeof(MenuDrawingInfo));

    pmdi->BGColor = COLOR(pmenu->scmBGColor);
    pmdi->SideBGColor = COLOR(pmenu->scmSideBGColor);
    pmdi->TextColor = COLOR(pmenu->scmTextColor);

    pxfont = Scr.StdFont.font;
    if (FONTP(pmenu->scmFont)) {
      scwm_font *psfont = FONT(pmenu->scmFont);
      if (psfont) {
	pxfont = psfont->xfs;
      }
    }
    label_font_height = pxfont->ascent + pxfont->descent;
    
    pmdi->x = 0;		/* just init: gets set elsewhere */
    pmdi->y = 0;		/* just init: gets set elsewhere */
    pmdi->ccol = 1;

    for (imiim = 0; imiim < cmiim; imiim++) {
      MenuItemInMenu *pmiim = rgpmiim[imiim];
      Scwm_MenuItem *pmi = pmiim->pmi;
      int text_width = XTextWidth(pxfont, pmi->szLabel, pmi->cchLabel);
      int extra_text_width = XTextWidth(pxfont, pmi->szExtra, pmi->cchExtra);
      int item_height = 0;
      pmiim->cpixOffsetY = total_height;
      
      /* These are easy when using only one column */
      pmiim->fOnTopEdge = (imiim == 0);
      pmiim->fOnBottomEdge = (imiim == (cmiim - 1));

      item_height = label_font_height + MENU_ITEM_LABEL_EXTRA_VERT_SPACE;

      INCREASE_MAYBE(max_text_width,text_width);
      INCREASE_MAYBE(max_extra_text_width,extra_text_width);
      
      if (pmi->picAbove) {
	int height = pmi->picAbove->height + MENU_ITEM_PICTURE_EXTRA_VERT_SPACE;
	item_height += height;
	INCREASE_MAYBE(max_above_image_width,pmi->picAbove->width);
      }
      if (pmi->picLeft) {
	int height = pmi->picLeft->height + MENU_ITEM_PICTURE_EXTRA_VERT_SPACE;
	INCREASE_MAYBE(item_height,height);
	INCREASE_MAYBE(max_left_image_width,pmi->picLeft->width);
      }

      if (pmiim->fShowPopupArrow) {
	INCREASE_MAYBE(max_right_image_width, MENU_POPUP_ARROW_WIDTH);
      }

      pmiim->cpixItemHeight = item_height;
      total_height += item_height;
    }

    /* now set global menu drawing properties */
    /* Handle the side image, if any */
    if (picSide) {
      pmdi->cpixItemOffset = picSide->width + MENU_SIDE_IMAGE_SPACING*2;
    } else {
      pmdi->cpixItemOffset = 0;
    }

    total_height += MENU_EDGE_SPACING;

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

    pmdi->cpixWidth = pmdi->cpixItemOffset + max_item_width + MENU_EDGE_SPACING*2;

    /* Now create the window */
    { /* scope */
      unsigned long valuemask = (CWBackPixel | CWCursor | CWSaveUnder);
      XSetWindowAttributes attributes;
      attributes.background_pixel = pmdi->BGColor;
      attributes.cursor = Scr.ScwmCursors[MENU];
      attributes.save_under = True;

      pmdi->w = XCreateWindow(dpy, Scr.Root, 0, 0, pmdi->cpixWidth,
			      pmdi->cpixHeight, 0, CopyFromParent, InputOutput,
			      CopyFromParent, valuemask, &attributes);
    }
  }
}
#undef INCREASE_MAYBE

static void
PaintSideImage(Window w, Pixel bg, int cpixHeight, Picture *pic)
{
  Globalgcv.foreground = bg;
  XChangeGC(dpy, Scr.ScratchGC1, GCForeground, &Globalgcv);
  XFillRectangle(dpy, w, Scr.ScratchGC1, 
		 MENU_SIDE_IMAGE_SPACING, MENU_SIDE_IMAGE_SPACING,
		 pic->width, cpixHeight - 2*MENU_SIDE_IMAGE_SPACING);
  DrawImage(w,pic,MENU_SIDE_IMAGE_SPACING,MENU_SIDE_IMAGE_SPACING,NULL);
}

#ifdef FIXGJB_NEED_THIS_WHEN_NO_MENUS_C
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
#else
void RelieveRectangle(Window win, int x, int y, int w, int h, GC Hilite, GC Shadow);
void RelieveHalfRectangle(Window win, int x, int y, int w, int h, GC Hilite, GC Shadow);
void DrawTrianglePattern(Window, GC, GC, GC, int, int, int, int);
void DrawSeparator(Window, GC, GC, int, int, int, int, int);
#endif


static
void
PaintMenuItem(Window w, DynamicMenu *pmd, MenuItemInMenu *pmiim)
{
  Scwm_Menu *pmenu = pmd->pmenu;
  XFontStruct *pxfont = FONTP(pmenu->scmFont)? XFONT(pmenu->scmFont)
    : Scr.StdFont.font;
  MenuDrawingInfo *pmdi = pmd->pmdi;
  Scwm_MenuItem *pmi = pmiim->pmi;
  int label_font_height = pxfont->ascent + pxfont->descent;
  int y_offset = pmiim->cpixOffsetY;
  int x_offset = pmdi->cpixItemOffset;
  int width = pmdi->cpixWidth;
  int item_height = pmiim->cpixItemHeight;
  GC ShadowGC = Scr.MenuShadowGC;
  GC ReliefGC = Scr.d_depth<2? Scr.MenuShadowGC: Scr.MenuReliefGC;
  GC currentGC = ShadowGC;
  Picture *picLeft = pmi->picLeft;
  Picture *picAbove = pmi->picAbove;
  menu_item_state mis = pmiim->mis;

  /* code for FVWM menu look here -- should abstract for other options */

  /* Erase any old reliefs indicated selectedness */
  XClearArea(dpy, w, x_offset, y_offset, width, item_height, False);

  /* Draw the shadows for the absolute outside of the menus
     This stuff belongs in here, not in PaintMenu, since we only
     want to redraw it when we have too (i.e. on expose event) */

  /* Top of the menu */
  if (pmiim->fOnTopEdge) {
    DrawSeparator(w, ReliefGC,ReliefGC, 0,0, 
		  width-1,0, -1);
  } 

  /* Botton of the menu */
  if (pmiim->fOnBottomEdge) {
    DrawSeparator(w,ShadowGC,ShadowGC, 0,y_offset + item_height-1,
                  width-1, y_offset + item_height-1, 1);
  }

  if (mis == MIS_Selected) {
    RelieveRectangle(w, x_offset+MENU_ITEM_RR_SPACE, y_offset,
		     width-2*MENU_ITEM_RR_SPACE, item_height,
		     ReliefGC,ShadowGC);
  }
  /* Add the markings for the left edge of the menu and 
     the right edge of the menu */
  RelieveHalfRectangle(w, 0, y_offset, 
		       width, item_height, 
		       ReliefGC, ShadowGC);

  if (picAbove) {
    int x = (width - x_offset - picAbove->width) / 2 + x_offset;
    DrawImage(w, picAbove, x, y_offset, currentGC);
    y_offset += picAbove->height;
  }

  /* center text vertically if the pixmap is taller */
  if (picLeft) {
    int cpixPicTallerBy = picLeft->height - label_font_height;
    DrawImage(w, picLeft, x_offset, y_offset, currentGC);
    if (cpixPicTallerBy > 1) {
      y_offset += cpixPicTallerBy/2;
    }
  }
       
  if (mis == MIS_Grayed) {
    currentGC = Scr.MenuStippleGC;
  } else {
    currentGC = Scr.MenuGC;
  }

  x_offset += pmdi->cpixLeftPicWidth;

  if (pmi->szLabel) {
    XDrawString(dpy, w, currentGC,
		x_offset,y_offset + label_font_height, 
		pmi->szLabel, pmi->cchLabel);
  }

  x_offset += pmdi->cpixTextWidth;

  if (pmi->szExtra) {
    XDrawString(dpy, w, currentGC,
		x_offset, y_offset + label_font_height,
		pmi->szExtra, pmi->cchExtra);
  }

  /* FIXGJB: use DrawUnderline to highlight the shortcut key */

  x_offset += pmdi->cpixExtraTextWidth;

  if (pmiim->fShowPopupArrow) {
    int d = (item_height-7)/2; /* FIXGJB: magic numbers! */
    if (mis == MIS_Enabled) {
      DrawTrianglePattern(w, ShadowGC, ReliefGC, ShadowGC, /* ReliefGC, */
			  width-d-8, y_offset+d-1, width-d-1, y_offset+d+7);
    } else {
      DrawTrianglePattern(w, ReliefGC, ShadowGC, ReliefGC, /* ShadowGC, */
			  width-d-8, y_offset+d-1, width-d-1, y_offset+d+7);
    }
  }

  return;
}

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
    Picture *picSide = pmd->pmenu->picSide;
    if (picSide) {
      PaintSideImage(w,pmdi->SideBGColor,pmdi->cpixHeight,pmd->pmenu->picSide);
    }
  }
  XSync(dpy,0);
}
