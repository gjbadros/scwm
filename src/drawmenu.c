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

#ifndef HAVE_GH_LENGTH
#define gh_length gh_list_length
#endif /* HAVE_GH_LENGTH */

#define MENU_ITEM_PICTURE_EXTRA_VERT_SPACE 4
#define MENU_ITEM_LABEL_EXTRA_VERT_SPACE 4
#define MENU_ITEM_PICTURE_EXTRA_HORIZ_SPACE 4
#define MENU_SIDE_IMAGE_SPACING 3
#define MENU_ITEM_RR_SPACE 2

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
    int total_height = 0;
    int max_item_width = 0;
    int label_font_height;
    MenuDrawingInfo *pmdi = pmd->pmdi = safemalloc(sizeof(MenuDrawingInfo));

    pmdi->BGColor = COLOR(pmenu->scmBGColor);
    pmdi->SideBGColor = COLOR(pmenu->scmSideBGColor);
    pmdi->TextColor = COLOR(pmenu->scmTextColor);

    pxfont = Scr.StdFont.font;
    if (SAFE_FONTP(pmenu->scmFont)) {
      scwm_font *psfont = FONT(pmenu->scmFont);
      if (psfont) {
	pxfont = psfont->xfs;
      }
    }
    label_font_height = pxfont->ascent + pxfont->descent;
    
    pmdi->x = 0;		/* just init: gets set elsewhere */
    pmdi->y = 0;		/* just init: gets set elsewhere */
    pmdi->ccol = 1;

    /* Handle the side image, if any */
    if (picSide) {
      pmdi->cpixItemOffset = picSide->width + MENU_SIDE_IMAGE_SPACING*2;
    } else {
      pmdi->cpixItemOffset = 0;
    }

    for (imiim = 0; imiim < cmiim; imiim++) {
      MenuItemInMenu *pmiim = rgpmiim[imiim];
      Scwm_MenuItem *pmi = pmiim->pmi;
      int item_width = XTextWidth(pxfont, pmi->szLabel, pmi->cchLabel);
      int item_height = 0;
      pmiim->cpixLabelX = pmdi->cpixItemOffset;
      pmiim->cpixOffsetY = total_height;
      pmiim->cpixExtraX = 0;
      
      /* These are easy when using only one column */
      pmiim->fOnTopEdge = (imiim == 0);
      pmiim->fOnBottomEdge = (imiim == (cmiim - 1));

      item_width += pmiim->cpixExtraX;

      item_height = label_font_height + MENU_ITEM_LABEL_EXTRA_VERT_SPACE;
      
      if (pmi->picAbove) {
	int height = pmi->picAbove->height + MENU_ITEM_PICTURE_EXTRA_VERT_SPACE;
	item_height += height;
      }
      if (pmi->picLeft) {
	int height = pmi->picLeft->height + MENU_ITEM_PICTURE_EXTRA_VERT_SPACE;
	if (height > pmiim->cpixHeight) {
	  item_height = height;
	}
	item_width += pmi->picLeft->width + MENU_ITEM_PICTURE_EXTRA_HORIZ_SPACE;
      }

      pmiim->cpixHeight = item_height;
      total_height += item_height;

      if (item_width > max_item_width) {
	max_item_width = item_width;
      }
    }

    pmdi->cpixWidth = pmdi->cpixItemOffset + max_item_width;
    pmdi->cpixHeight = total_height;

    /* Now create the window */
    { /* scope */
      unsigned long valuemask = (CWBackPixel | CWEventMask | CWCursor | CWSaveUnder);
      XSetWindowAttributes attributes;
      attributes.background_pixel = pmdi->BGColor;
      attributes.cursor = Scr.ScwmCursors[MENU];
      attributes.event_mask = (ExposureMask | EnterWindowMask);
      attributes.save_under = True;

      pmdi->w = XCreateWindow(dpy, Scr.Root, 0, 0, pmdi->cpixWidth,
			      pmdi->cpixHeight, 0, CopyFromParent, InputOutput,
			      CopyFromParent, valuemask, &attributes);
    }
  }
}


static void
PaintSideImage(Window w, Pixel bg, int cpixHeight, Picture *pic)
{
  /* FIXGJB: These GCs are bogus */
  GC ReliefGC = Scr.d_depth < 2? Scr.MenuShadowGC : Scr.MenuReliefGC;
  GC TextGC = Scr.MenuGC;
  
  Globalgcv.foreground = bg;
  XChangeGC(dpy, Scr.ScratchGC1, GCForeground, &Globalgcv);
  XFillRectangle(dpy, w, Scr.ScratchGC1, 
		 MENU_SIDE_IMAGE_SPACING, MENU_SIDE_IMAGE_SPACING,
		 pic->width, cpixHeight - 2*MENU_SIDE_IMAGE_SPACING);
    
  if (pic->depth > 0) {
    /* a full pixmap (as opposed to just a bitmap) */
    Globalgcm = GCClipMask | GCClipXOrigin | GCClipYOrigin;
    Globalgcv.clip_mask = pic->mask;
    Globalgcv.clip_x_origin = 3;
    Globalgcv.clip_y_origin = pic->height - pic->height - MENU_SIDE_IMAGE_SPACING;
    
    XChangeGC(dpy,ReliefGC,Globalgcm,&Globalgcv);
    XCopyArea(dpy, pic->picture, w, ReliefGC, 0, 0,
	      pic->width, pic->height,
	      Globalgcv.clip_x_origin, Globalgcv.clip_y_origin);
    Globalgcm = GCClipMask;
    Globalgcv.clip_mask = None;
    XChangeGC(dpy,ReliefGC,Globalgcm,&Globalgcv);
  } else {
    /* just a bitmap */
    XCopyPlane(dpy, pic->picture, w, TextGC, 0, 0,
	       pic->width, pic->height,
	       1, cpixHeight - pic->height, 1 /* plane */);
  }
}

/*
static
void
PaintMenuItem(Window w, DynamicMenu *pmd, MenuItemInMenu *pmiim)
{
  Scwm_MenuItem *pmi = pmiim->pmi;
  GC gcText = Scr.MenuGC;

  if (pmi->szLabel) {
    XDrawString(dpy, w, gcText, pmiim->cpixLabelX, 
		pmiim->cpixOffsetY + pmiim->cpixHeight,
		pmi->szLabel, pmi->cchLabel);
  }
}
*/

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
  XFontStruct *pxfont = SAFE_FONTP(pmenu->scmFont)? XFONT(pmenu->scmFont)
    : Scr.StdFont.font;
  MenuDrawingInfo *pmdi = pmd->pmdi;
  Scwm_MenuItem *pmi = pmiim->pmi;
  int label_font_height = pxfont->ascent + pxfont->descent;
  int y_offset = pmiim->cpixOffsetY;
  int y_height = pmiim->cpixHeight;
  int y_offset_text = y_offset + label_font_height;
  int x_offset = pmiim->cpixLabelX;
  int width = pmdi->cpixWidth;
  GC ShadowGC = Scr.MenuShadowGC;
  GC ReliefGC = Scr.d_depth<2? Scr.MenuShadowGC: Scr.MenuReliefGC;
  GC currentGC;
  Picture *picLeft = pmi->picLeft;
  Picture *picAbove = pmi->picAbove;
  menu_item_state mis = pmiim->mis;

  if (picAbove) {
    y_offset += picAbove->height;
  }

  /* center text vertically if the pixmap is taller */
  if (picLeft) {
    int cpixPicTallerBy = picLeft->height - label_font_height;
    if (cpixPicTallerBy > 1) {
      y_offset_text += cpixPicTallerBy/2;
    }
  }

  /* code for FVWM menu look here -- should abstract for other options */
       
  XClearArea(dpy, w, x_offset, y_offset, width, y_height, False);
  if (mis == MIS_Enabled) {
    RelieveRectangle(w, x_offset+MENU_ITEM_RR_SPACE, y_offset,
		     width-2*MENU_ITEM_RR_SPACE, y_height,
		     ReliefGC,ShadowGC);
    RelieveHalfRectangle(w, 0, y_offset, 
			 width, y_height, 
			 ReliefGC, ShadowGC);
  } 

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
    DrawSeparator(w,ShadowGC,ShadowGC, 1,y_height-2,
                  width-2, y_height-2, 1);
  }

  if (mis == MIS_Grayed) {
    currentGC = Scr.MenuStippleGC;
  } else {
    currentGC = Scr.MenuGC;
  }

  if (pmi->szLabel) {
    XDrawString(dpy, w, currentGC,
		x_offset,y_offset_text, 
		pmi->szLabel, pmi->cchLabel);
  }
  if (pmi->szExtra) {
    XDrawString(dpy, w, currentGC,
		x_offset + pmiim->cpixExtraX, y_offset_text,
		pmi->szExtra, pmi->cchExtra);
  }

  /* FIXGJB: use DrawUnderline to highlight the shortcut key */

  if (pmiim->fShowPopupArrow) {
    int d = (y_height-7)/2; /* FIXGJB: magic numbers! */
    if (mis == MIS_Enabled) {
      DrawTrianglePattern(w, ShadowGC, ReliefGC, ShadowGC, /* ReliefGC, */
			  width-d-8, y_offset+d-1, width-d-1, y_offset+d+7);
    } else {
      DrawTrianglePattern(w, ReliefGC, ShadowGC, ReliefGC, /* ShadowGC, */
			  width-d-8, y_offset+d-1, width-d-1, y_offset+d+7);
    }
  }

  if(picAbove) {
    int x = (width - x_offset - picAbove->width) / 2 + x_offset;

    if (picAbove->depth > 0) {
      /* pixmap, not bitmap */
      Globalgcm = GCClipMask | GCClipXOrigin | GCClipYOrigin;
      Globalgcv.clip_mask = picAbove->mask;
      Globalgcv.clip_x_origin= x;
      Globalgcv.clip_y_origin = y_offset+1;
      XChangeGC(dpy,ReliefGC,Globalgcm,&Globalgcv);
      XCopyArea(dpy,picAbove->picture,w,ReliefGC, 0,0,
		picAbove->width, picAbove->height,
		x,y_offset+1);
      Globalgcm = GCClipMask;
      Globalgcv.clip_mask = None;
      XChangeGC(dpy,ReliefGC,Globalgcm,&Globalgcv);
    } else {
      XCopyPlane(dpy,picAbove->picture,w,
		 currentGC,0,0,picAbove->width,picAbove->height,
		 x,y_offset+1, 1 /* plane */);
    }
  }
  
  if (picLeft) {
    int lp_offset = 6;
    int y;
    
    if (picAbove && pmi->szLabel) {
      y = y_offset + y_height - picLeft->height-1;
    } else {
      y = y_offset + y_height/2 - picLeft->height/2;
    }

    if (picLeft->depth > 0) {
      /* pixmap, not bitmap */
      Globalgcm = GCClipMask | GCClipXOrigin | GCClipYOrigin;
      Globalgcv.clip_mask = picLeft->mask;
      Globalgcv.clip_x_origin= lp_offset + x_offset;
      Globalgcv.clip_y_origin = y;

      XChangeGC(dpy,ReliefGC,Globalgcm,&Globalgcv);
      XCopyArea(dpy,picLeft->picture,w,ReliefGC,0,0,
		picLeft->width, picLeft->height,
		lp_offset + x_offset, y);
      Globalgcm = GCClipMask;
      Globalgcv.clip_mask = None;
      XChangeGC(dpy,ReliefGC,Globalgcm,&Globalgcv);
    } else {
      /* bitmap, not pixmap */
      XCopyPlane(dpy,picLeft->picture,w,
		 currentGC,0,0,picLeft->width,picLeft->height,
		 lp_offset + x_offset,y, 1 /* plane */);
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
    if (pxe->xexpose.y < (pmiim->cpixOffsetY  + pmiim->cpixHeight) &&
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
