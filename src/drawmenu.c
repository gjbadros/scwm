/* $Id$
 * drawmenu.c
 * By Greg J. Badros -- Nov 22, 1997
 * This is the default menu drawing code.
 * Later we can make this interface be implemented by
 * dynamically loaded functions
 */

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include <guile/gh.h>
#include "guile-compat.h"

#include "drawmenu.h"

#include "menulook.h"
#include "scwm.h"
#include "menu.h"
#include "screen.h"
#include "font.h"
#include "xmisc.h"
#include "cursor.h"

#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif

static SCM drawmenu_menu_look = SCM_UNDEFINED;

extern SCM sym_top, sym_center, sym_bottom;

/* GJB:FIXME:: comment these! */
#define MENU_EDGE_VERT_SPACING 2
#define MENU_EDGE_HORIZ_SPACING 2
#define MENU_TEXT_SPACING 4
#define MENU_ITEM_PICTURE_EXTRA_VERT_SPACE 4
#define MENU_ITEM_EXTRA_VERT_SPACE 2
#define MENU_ITEM_LABEL_EXTRA_VERT_SPACE 2
#define MENU_ITEM_PICTURE_EXTRA_HORIZ_SPACE 4
#define MENU_SIDE_IMAGE_SPACING 5
#define MENU_ITEM_RR_SPACE 2
#define MENU_POPUP_ARROW_WIDTH 8
#define MENU_HEIGHT_SEPARATOR 4

static GC MenuGC;
static GC MenuStippleGC;
static GC MenuReliefGC;
static GC MenuShadowGC;

struct MenuDrawingInfo_tag
{
  int ccol;			/* the number of columns in the menu */
  int cpixItemOffset;		/* how far from the left edge are items */
  int cpixLeftPicWidth;		/* how wide are the left images */
  int cpixTextWidth;		/* how wide are the text items */
  int cpixExtraTextWidth;	/* how wide are the text items */
  int cpixSideImage;		/* how wide is the side image */
  /* cpixItemOffset + ccol * cpixItemWidth == cpixWidth */
  Pixel HLBGColor;		/* the highlight background color */
  Pixel HLTextColor;		/* the highlight text color */
  Pixel BGColor;		/* the background color */
  Pixel SideBGColor;		/* the side image bg color */
  Pixel StippleColor;		/* the stipple color */
};

struct MenuItemDrawingInfo_tag
{
  int cpixOffsetY;		/* top y offset of the item */
  int cpixItemHeight;		/* height for item */
  Bool fOnTopEdge;		/* is this item on the top edge? */
  Bool fOnBottomEdge;		/* is this item on the bottom edge?  */
  Pixel BGColor;		/* the background color-- inherited from menu's if 0 */
  Pixel TextColor;		/* the text/fg color, set from menu's if not given */
                                /* above colors are overridden by HL* colors when selected */
  scwm_font *scfont;		/* set from menu's if not given */
};

#define INCREASE_MAYBE(var,val) do { if (val > var) { var = val; } } while (0)


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
  static double last_highlight_factor, last_shadow_factor;
  unsigned long gcm;
  XGCValues gcv;
  Pixel Bright, Dim;
  static Bool GCs_initted;

  if (!GCs_initted) {
    InitGCs();
    GCs_initted = True;
  }
    
  if (pmd->pmdi->BGColor      != LastBGColor ||
      menu_highlight_factor_val != last_highlight_factor ||
      menu_shadow_factor_val  != last_shadow_factor) {
    /* Relief.fg */
    Bright = adjust_pixel_brightness(pmd->pmdi->BGColor,
				     menu_highlight_factor_val);
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
    last_highlight_factor = menu_highlight_factor_val;
    last_shadow_factor = menu_shadow_factor_val;
  }
  
  gcm = GCForeground | GCBackground;
  gcv.foreground = DYNAMIC_SAFE_COLOR(pmd->pmenu->scmTextColor);
  gcv.background = pmd->pmdi->BGColor;
#ifndef I18N
  gcm |= GCFont;
  gcv.font = scfont->xfs->fid;
#endif
  XChangeGC(dpy, MenuGC, gcm, &gcv);

  gcm = GCForeground | GCBackground;
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

/* use scmFont, but default to scfont, or scmFixedFont */
static
scwm_font *
PscwmFontForMenuItem(scwm_font *scfontDefault, SCM scmFont)
{
  scwm_font *scfont = DYNAMIC_SAFE_FONT(scmFont);
  if (!scfont) scfont = scfontDefault;
  if (!scfont) scfont = FONT(scmFixedFont);
  return scfont;
}

static void
PaintSideImage(Window w, Pixel bg, int cpixHeight, scwm_image *psimg,
	       SCM align)
{
#define FUNC_NAME "PaintSideImage"
  int cpixDstYoffset, cpixSrcYoffset;
  unsigned int height;
  
  if (!psimg) {
    scwm_msg(ERR,FUNC_NAME,"psimg is NULL");
    return;
  }
  SetGCFg(Scr.ScratchGC1,bg);
  XFillRectangle(dpy, w, Scr.ScratchGC1, 
		 MENU_ITEM_RR_SPACE, MENU_ITEM_RR_SPACE,
		 psimg->width, cpixHeight - 2*MENU_ITEM_RR_SPACE);

  height = psimg->height;
  if ((int) height > cpixHeight - 2*MENU_ITEM_RR_SPACE)
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
#undef FUNC_NAME

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
DrawUnderline(Window w, scwm_font *scfont, GC gc, char *sz, int x, int y, int posn) 
{
  int cpixStart = ComputeXTextWidth(XFONT_FONTTYPE(scfont), sz, posn);
  int cpixEnd = ComputeXTextWidth(XFONT_FONTTYPE(scfont), sz, posn + 1) - 1;
  XDrawLine(dpy, w, gc, x + cpixStart, y + 2, x + cpixEnd, y + 2);
}

static
void
PaintMenuItem(Window w, DynamicMenu *pmd, MenuItemInMenu *pmiim)
{
#define FUNC_NAME "PaintMenuItem"
  MenuDrawingInfo *pmdi = pmd->pmdi;
  MenuItem *pmi = pmiim->pmi;
  MenuItemDrawingInfo *pmidi = pmiim->pmidi;
  scwm_font *scfont = pmidi->scfont;
  int label_font_height = scfont->height;
  int y_offset = pmidi->cpixOffsetY;
  int x_offset = pmdi->cpixItemOffset;
  int width = pmd->cpixWidth;
  int item_height = pmidi->cpixItemHeight;
  GC ShadowGC;
  GC ReliefGC;
  GC currentGC;
  scwm_image *psimgLeft = DYNAMIC_SAFE_IMAGE(pmi->scmImgLeft);
  scwm_image *psimgAbove = DYNAMIC_SAFE_IMAGE(pmi->scmImgAbove);
  menu_item_state mis = pmiim->mis;
  Bool fSelected = (mis == MIS_Selected && !UNSET_SCM(pmi->scmAction));
  Pixel TextColorDefault = DYNAMIC_SAFE_COLOR(pmd->pmenu->scmTextColor);

  /* code for FVWM menu look here -- should abstract for other options */

  /* FIXJTL: how expensive is XChangeGC()?  If bad, there should be a
     way of skipping this if they're still set up right from the last
     menu item; maybe the exported PaintMenuItem should set up the GCs
     and then call the real PaintMenuItem, and PaintMenu should set
     them up before looping over the real PaintMenuItem */


  MakeGCs(pmd, scfont);

  ShadowGC = MenuShadowGC;
  ReliefGC = Scr.d_depth < 2 ? MenuShadowGC: MenuReliefGC;
  
  /* Erase any old reliefs indicated selectedness --
     Use ClearArea if the color to use is the same as the menu color */
  if (pmidi->BGColor == pmdi->BGColor && (!fSelected || pmdi->HLBGColor == pmdi->BGColor)) {
    /* inherit menu's bg color */
    XClearArea(dpy, w,
               x_offset-MENU_ITEM_RR_SPACE-1,
               y_offset, width+MENU_ITEM_RR_SPACE+1, item_height, False);
  } else {
    /* use the item-specific color, or the highlight color */
    Pixel bg = fSelected? pmdi->HLBGColor : pmidi->BGColor;
    SetGCFg(Scr.ScratchGC1,bg);
    XFillRectangle(dpy, w, Scr.ScratchGC1, 
                   x_offset-MENU_ITEM_RR_SPACE-1,
                   y_offset, width+MENU_ITEM_RR_SPACE+1, item_height);
  }

  /* Draw the shadows for the absolute outside of the menus
     This stuff belongs in here, not in PaintMenu, since we only
     want to redraw it when we have too (i.e. on expose event) */

  /* Top of the menu */
  if (pmidi->fOnTopEdge) {
    DrawSeparator(w, ReliefGC,ReliefGC, 
		  0, width-1,0, -1);
  } 

  /* Bottom of the menu */
  if (pmidi->fOnBottomEdge) {
    DrawSeparator(w, ShadowGC, ShadowGC, 
		  0, width-1, y_offset + item_height,
		  1);
  }

  /* Only highlight if the item has an action */
  if (fSelected) {
    /* see also below where we pick colors based on this guard */
    if (pmd->pmenu->fHighlightRelief) {
      RelieveRectangle(w, x_offset-MENU_ITEM_RR_SPACE, y_offset,
                       width+MENU_ITEM_RR_SPACE, item_height,
                       ReliefGC,ShadowGC);
    }
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
      DBUG((DBG,FUNC_NAME,"Drawing psimgAbove"));
      DrawImage(w, psimgAbove, x, y_offset, MenuGC);
      y_offset += psimgAbove->height;
    }

    /* center image vertically */
    if (psimgLeft) {
      /* FIXJTL: this doesn't work well with both left & above iamges; */
      int cpixExtraYOffset = (item_height - psimgLeft->height) / 2;
      DrawImage(w, psimgLeft, x_offset, y_offset + cpixExtraYOffset, MenuGC);
    }
    
    /* JTL:FIXME:: fix for proper menu title support */
    if (mis == MIS_Grayed && False) {
      currentGC = MenuStippleGC;
    } else {
      currentGC = MenuGC;
    }

    { /* scope */
      Pixel fg = pmidi->TextColor;
      Pixel bg = pmidi->BGColor;

      if (fSelected) {
        fg = pmdi->HLTextColor;
        bg = pmdi->HLBGColor;
      }
      
      if (fg != TextColorDefault || bg != pmdi->BGColor) {
        XGCValues gcv;
        unsigned long gcm = 0;
        XCopyGC(dpy,MenuGC,GCForeground | GCBackground | GCFont,Scr.ScratchGC1);

        if (fg != TextColorDefault) {
          gcm |= GCForeground;
          gcv.foreground = fg;
        }
        if (bg != pmdi->BGColor) { 
          gcm |= GCBackground;
          gcv.background = bg;
        }
        XChangeGC(dpy, Scr.ScratchGC1, gcm, &gcv);
        currentGC = Scr.ScratchGC1;
      }
    }
  
    x_offset += pmdi->cpixLeftPicWidth;

    if (pmi->szLabel) {
#ifdef I18N
      XmbDrawString(dpy, w, scfont->fontset, currentGC,
		  x_offset,y_offset + label_font_height, 
		  pmi->szLabel, pmi->cchLabel);
#else
      XDrawString(dpy, w, currentGC,
		  x_offset,y_offset + label_font_height, 
		  pmi->szLabel, pmi->cchLabel);
#endif
    }

    /* highlight the shortcut key */
    if (pmiim->ichShortcutOffset >= 0) {
      DrawUnderline(w, scfont, currentGC, pmi->szLabel,
		    x_offset, y_offset+label_font_height, pmiim->ichShortcutOffset);
    }
  
    x_offset += pmdi->cpixTextWidth;

    if (pmi->szExtra) {
#ifdef I18N
      XmbDrawString(dpy, w, scfont->fontset, currentGC,
		  x_offset, y_offset + label_font_height,
		  pmi->szExtra, pmi->cchExtra);
#else
      XDrawString(dpy, w, currentGC,
		  x_offset, y_offset + label_font_height,
		  pmi->szExtra, pmi->cchExtra);
#endif
    }

    x_offset += pmdi->cpixExtraTextWidth;

    if (pmiim->fShowPopupArrow) {
      int d = (item_height-7)/2; /* GJB:FIXME:: magic numbers! */
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
#undef FUNC_NAME

static
void 
PaintDynamicMenu(DynamicMenu *pmd, XEvent *pxe)
{
#define FUNC_NAME "PaintDynamicMenu"
  Window w = pmd->w;
  MenuDrawingInfo *pmdi = pmd->pmdi;
  MenuItemInMenu **rgpmiim = pmd->rgpmiim;
  int cmiim = pmd->cmiim;
  int imiim = 0;

  DBUG((DBG,FUNC_NAME,"Painting menu"));
  for (imiim = 0; imiim < cmiim; imiim++) {
    MenuItemInMenu *pmiim = rgpmiim[imiim];
    if (pxe->xexpose.y < (pmiim->pmidi->cpixOffsetY +
			  pmiim->pmidi->cpixItemHeight) &&
	(pxe->xexpose.y + pxe->xexpose.height) > pmiim->pmidi->cpixOffsetY) {
      DBUG((DBG,FUNC_NAME,"Painting menu item"));
      PaintMenuItem(w,pmd,pmiim);
    }
  }

  { /* scope */
    scwm_image *psimgSide = DYNAMIC_SAFE_IMAGE(pmd->pmenu->scmImgSide);
    if (psimgSide) {
      DBUG((DBG,FUNC_NAME,"Painting side image"));
      PaintSideImage(w,pmdi->SideBGColor,pmd->cpixHeight,psimgSide,
		     pmd->pmenu->scmSideAlign);
    }
  }
  XSync(dpy,0);
}
#undef FUNC_NAME

static
void
SetPopupMenuPositionFromMenuItem(DynamicMenu *pmdNew, 
				 MenuItemInMenu *pmiimSelected)
{
  DynamicMenu *pmdOld = pmiimSelected->pmd;
  MenuDrawingInfo *pmdiOld = pmdOld->pmdi;
  int cpixXmenu = pmdOld->x;
  int cpixYmenu = pmdOld->y;
  int cpixWidthMenu = pmdOld->cpixWidth;
  /*  MenuDrawingInfo * pmdiNew = pmdNew->pmdi; */
  int cpixWidthNewMenu = pmdNew->cpixWidth;

  if (cpixXmenu + cpixWidthMenu + cpixWidthNewMenu <= Scr.DisplayWidth) {
    pmdNew->x = cpixXmenu + cpixWidthMenu - 2;
  } else {
    /* pop to the left */
    pmdNew->x = cpixXmenu - cpixWidthNewMenu + pmdiOld->cpixSideImage;
  }
  pmdNew->y = cpixYmenu + pmiimSelected->pmidi->cpixOffsetY - 2;
  if (pmdNew->y + pmdNew->cpixHeight > Scr.DisplayHeight) {
    /* would go off the bottom edge of the screen;
       force it up from the bottom of the screen */
    pmdNew->y = Scr.DisplayHeight-pmdNew->cpixHeight;
  }
}

static
void
WarpPointerToPmiim(MenuItemInMenu *pmiim)
{
  DynamicMenu *pmd; 
  MenuDrawingInfo *pmdi;
  int x, y;

  if (!pmiim)
    return;

  pmd = pmiim->pmd;
  pmdi = pmd->pmdi;

  /* GJB:FIXME:: make fraction of menu that pointer goes to configurable */
  x = pmdi->cpixItemOffset + 2*(pmd->cpixWidth - pmdi->cpixItemOffset)/3;
  y = pmiim->pmidi->cpixOffsetY + pmiim->pmidi->cpixItemHeight/2;
  XWarpPointer(dpy, 0, pmd->w, 0, 0, 0, 0, x, y);
}

static
MenuItemInMenu *
PmiimFromPmdXY(DynamicMenu *pmd, int ARG_UNUSED(x), int y)
{
#define FUNC_NAME "PmiimFromPmdXY"
  int ipmiim;
  for (ipmiim = 0; ipmiim < pmd->cmiim; ipmiim++) {
    MenuItemInMenu *pmiim = pmd->rgpmiim[ipmiim];
    int item_y_offset = pmiim->pmidi->cpixOffsetY;
    if (y > item_y_offset && y <= item_y_offset + pmiim->pmidi->cpixItemHeight) {
      DBUG((DBG,FUNC_NAME,"pmiim->pmi->szLabel = %s @ %d,%d", pmiim->pmi->szLabel,x,y));
      return pmiim;
    }
  }
  return NULL;
}
#undef FUNC_NAME

static int
InPopupZone(MenuItemInMenu *pmiim, int cpixXoffset, int ARG_IGNORE(cpixYoffset))
{
  /* FIXJTL: this works well for narrow menus; *3/4 works better for larger
     one; maybe something more complicated is needed */
  return cpixXoffset > pmiim->pmd->cpixWidth/2;
}

/* px and py are in & out parameters; return the x,y location for the
   top left of a child popup when the mouse was clicked at x,y; This
   function doesn't have to concern itself with screen borders */
static void
GetChildPopupPosition(DynamicMenu * pmd, int *px, int *ARG_UNUSED(py))
{
  *px = pmd->x + pmd->cpixWidth - 5;
}

/* px and py are in & out parameters; return the x,y location for the
   top left of the popup when the mouse was clicked at x,y; This function
   doesn't have to concern itself with screen borders or with being
   popped up from another menu or decoration */
static void
GetPreferredPopupPosition(DynamicMenu * pmd, int *px, int *py)
{
  *px = *px - pmd->cpixWidth/2;
  *py = *py - pmd->rgpmiim[0]->pmidi->cpixItemHeight/2 - MENU_EDGE_VERT_SPACING;
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

/* ConstructDynamicMenu should try to do all the computations for the
   paint routine -- little should be done in the painting, as it'd be
   really hard to maintain the two routines in synch.  pmd->pmdi and
   pmd->rgpmiim should have all the information needed for drawing in
   response to expose events */
void
ConstructDynamicMenu(DynamicMenu *pmd)
{
#define FUNC_NAME "ConstructDynamicMenu"
  if (pmd->pmdi != NULL)
    return;

  /* remember how to paint this menu */
  pmd->pmdv = MENULOOK(drawmenu_menu_look)->mdvt;
  
  { /* scope */
    Menu *pmenu = pmd->pmenu;
    scwm_image *psimgSide = DYNAMIC_SAFE_IMAGE(pmenu->scmImgSide);
    scwm_image *psimgBackground = DYNAMIC_SAFE_IMAGE(pmenu->scmImgBackground);
    Pixel TextColorDefault = DYNAMIC_SAFE_COLOR(pmenu->scmTextColor);
    MenuItemInMenu **rgpmiim = pmd->rgpmiim;
    scwm_font *scfontDefault = DYNAMIC_SAFE_FONT(pmenu->scmFont);

    int cmiim = pmd->cmiim;
    int imiim = 0;
    int total_height = MENU_EDGE_VERT_SPACING;
    int max_text_width = 0;
    int max_extra_text_width = 0;
    int max_left_image_width = 0;
    int max_right_image_width = 0;
    int max_above_image_width = 0;
    int max_item_width = 0;

    MenuDrawingInfo *pmdi = pmd->pmdi =
      NEW(MenuDrawingInfo);

    pmdi->BGColor = DYNAMIC_SAFE_COLOR(pmenu->scmBGColor);
    pmdi->HLBGColor = DYNAMIC_SAFE_COLOR_USE_DEF(pmenu->scmHLBGColor,pmdi->BGColor);
    pmdi->HLTextColor = DYNAMIC_SAFE_COLOR_USE_DEF(pmenu->scmHLTextColor,TextColorDefault);
    pmdi->SideBGColor = DYNAMIC_SAFE_COLOR(pmenu->scmSideBGColor);
    pmdi->StippleColor = DYNAMIC_SAFE_COLOR(pmenu->scmStippleColor);
    
    pmdi->ccol = 1;

    for (imiim = 0; imiim < cmiim; imiim++) {
      MenuItemInMenu *pmiim = rgpmiim[imiim];
      MenuItemDrawingInfo *pmidi = pmiim->pmidi = NEW(MenuItemDrawingInfo);
      MenuItem *pmi = pmiim->pmi;
      scwm_image *psimgAbove = DYNAMIC_SAFE_IMAGE(pmi->scmImgAbove);
      scwm_image *psimgLeft = DYNAMIC_SAFE_IMAGE(pmi->scmImgLeft);
      scwm_font *scfont = pmidi->scfont = PscwmFontForMenuItem(scfontDefault, pmi->scmFont);

      int label_font_height = scfont->height;

      int text_width;
      int extra_text_width = 0;
      int item_height = MENU_ITEM_EXTRA_VERT_SPACE * 2;

      pmidi->cpixOffsetY = total_height;

      text_width = ComputeXTextWidth(XFONT_FONTTYPE(scfont), pmi->szLabel, pmi->cchLabel);

      DBUG((DBG,FUNC_NAME,"`%s' has width %d (%d chars)\n",
	   pmi->szLabel,text_width,pmi->cchLabel));

      /* need to set the bg color even for separators. */
      pmidi->BGColor = DYNAMIC_SAFE_COLOR_USE_DEF(pmi->scmBGColor,pmdi->BGColor);

      if (pmi->fIsSeparator) {
	item_height = MENU_HEIGHT_SEPARATOR;
      } else {
	/* szLabel we know is not null, but szExtra can be */
	if (pmi->szExtra) {
          extra_text_width = ComputeXTextWidth(XFONT_FONTTYPE(scfont), 
                                               pmi->szExtra, pmi->cchExtra);
	}
      
	/* These are easy when using only one column */
	pmidi->fOnTopEdge = (imiim == 0);
	pmidi->fOnBottomEdge = (imiim == (cmiim - 1));

        pmidi->TextColor = DYNAMIC_SAFE_COLOR_USE_DEF(pmi->scmFGColor,TextColorDefault);

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
      pmidi->cpixItemHeight = item_height;
      total_height += item_height;
    }

    /* now set global menu drawing properties */
    /* Handle the side image, if any */
    pmdi->cpixItemOffset = MENU_ITEM_RR_SPACE + MENU_EDGE_HORIZ_SPACING;

    pmdi->cpixSideImage = 0;
    if (psimgSide) {
      pmdi->cpixSideImage = psimgSide->width + MENU_SIDE_IMAGE_SPACING;
      pmdi->cpixItemOffset += pmdi->cpixSideImage;
    }

    total_height += MENU_EDGE_VERT_SPACING;

    pmdi->cpixLeftPicWidth = max_left_image_width;
    pmdi->cpixTextWidth = max_text_width + MENU_TEXT_SPACING;
    pmdi->cpixExtraTextWidth = max_extra_text_width + MENU_TEXT_SPACING;
    pmd->cpixHeight = total_height;

    /* use the width of the largest above image if it is greater than the sum of
       the widths of the other components */
    max_item_width = max(pmdi->cpixLeftPicWidth+
			 pmdi->cpixTextWidth+
			 pmdi->cpixExtraTextWidth+
			 max_right_image_width,
			 max_above_image_width);

    pmd->cpixWidth = pmdi->cpixItemOffset + max_item_width +  
      MENU_ITEM_RR_SPACE + MENU_EDGE_HORIZ_SPACING*2;

    DBUG((DBG,FUNC_NAME,"LeftPic = %d, Text = %d, ExtraText = %d, RightImage = %d; above = %d\n",
	 pmdi->cpixLeftPicWidth,pmdi->cpixTextWidth,pmdi->cpixExtraTextWidth,
	 max_right_image_width,max_above_image_width));

    /* Now create the window */
    { /* scope */
      unsigned long valuemask = (CWBackPixel | CWCursor | CWSaveUnder);
      XSetWindowAttributes attributes;
      attributes.background_pixel = pmdi->BGColor;
      attributes.cursor = XCursorByNumber(XC_sb_left_arrow);
      attributes.save_under = True;

      pmd->w = XCreateWindow(dpy, Scr.Root, 0, 0, pmd->cpixWidth,
			     pmd->cpixHeight, 0, CopyFromParent, InputOutput,
			     CopyFromParent, valuemask, &attributes);

      if (psimgBackground) {
	XSetWindowBackgroundPixmap(dpy, pmd->w, psimgBackground->image);
      }
    }
  }
}
#undef FUNC_NAME

#undef INCREASE_MAYBE

void
init_drawmenu()
{
  MenuDrawingVtable * pmdvt;
  
  pmdvt = NEW(MenuDrawingVtable);
  memset(pmdvt, 0, sizeof *pmdvt);
  
  pmdvt->fnConstructDynamicMenu = ConstructDynamicMenu;
  pmdvt->fnPaintDynamicMenu = PaintDynamicMenu;
  pmdvt->fnPaintMenuItem = PaintMenuItem;
  pmdvt->fnSetPopupMenuPositionFromMenuItem = SetPopupMenuPositionFromMenuItem;
  pmdvt->fnGetChildPopupPosition = GetChildPopupPosition;
  pmdvt->fnGetPreferredPopupPosition = GetPreferredPopupPosition;
  pmdvt->fnWarpPointerToPmiim = WarpPointerToPmiim;
  pmdvt->fnPmiimFromPmdXY = PmiimFromPmdXY;
  pmdvt->fnInPopupZone = InPopupZone;
  pmdvt->fnFreePmdi = FreePmdi;
  pmdvt->fnFreePmidi = FreePmidi;
  
  drawmenu_menu_look = make_menulook("scwm-menu-look", SCM_UNDEFINED, pmdvt);

  SCWM_VAR_READ_ONLY(NULL,"scwm-menu-look",drawmenu_menu_look);
  /** The default menu look for Scwm.
Used as an argument to `set-menu-look'. */
  
#ifndef SCM_MAGIC_SNARFER
#include "drawmenu.x"
#endif
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

