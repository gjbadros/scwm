/* $Id$
 * draw-xpm-menu.c
 * By Todd Larason -- Sep 30, 1998
 * Modified from the original menu drawing code
 *  implemented by Greg J. Badros -- Nov 22, 1997
 */

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include <X11/Xlib.h>
#include <X11/Xresource.h>
#include <X11/Xutil.h>
#include <X11/extensions/shape.h>

#include <guile/gh.h>
#include "guile-compat.h"

#include "scwm.h"
#include "menu.h"
#include "menulook.h"
#include "screen.h"
#include "font.h"
#include "xmisc.h"
#include "drawmenu.h"
#include "cursor.h"

#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif

static SCM xpm_shaped_menu_look = SCM_UNDEFINED;

extern SCM sym_top, sym_center, sym_bottom;

/* horizontal space for label or extra label, besides text */
#define MENU_TEXT_SPACING 4
/* vertical space for above and left images, besides image height */
#define MENU_ITEM_PICTURE_EXTRA_VERT_SPACE 4
/* vertical space to leave above and below item (FIXJTL: allocated, but not used? */
#define MENU_ITEM_EXTRA_VERT_SPACE 2
/* vertical space to leave for label or extra label, besides text */
#define MENU_ITEM_LABEL_EXTRA_VERT_SPACE 2
/* horizontal space to leave for left image */
#define MENU_ITEM_PICTURE_EXTRA_HORIZ_SPACE 4
/* horizontal space to leave for side image, besides image */
#define MENU_SIDE_IMAGE_SPACING 5
/* Relief rectangle width */
#define MENU_ITEM_RR_SPACE 2
#define MENU_POPUP_ARROW_WIDTH 8
#define MENU_HEIGHT_SEPARATOR 4

#define NUM_IMAGES 8

static GC MenuGC;
static GC MenuStippleGC;
static GC MenuReliefGC;
static GC MenuShadowGC;
static GC MaskGC;

struct MenuDrawingInfo_tag
{
  int ccol;			/* the number of columns in the menu */
  int cpixItemOffset;		/* how far from the left edge are items */
  int cpixItemWidth;
  int cpixLeftPicWidth;		/* how wide are the left images */
  int cpixTextWidth;		/* how wide are the text items */
  int cpixExtraTextWidth;	/* how wide are the text items */
  int cpixSideImage;		/* how wide is the side image */
  int cpixBorder;
  /* cpixItemOffset + ccol * cpixItemWidth == cpixWidth */
  Pixel HLBGColor;		/* the highlight background color */
  Pixel HLTextColor;		/* the highlight text color */
  Pixel BGColor;		/* the background color */
  Pixel SideBGColor;		/* the side image bg color */
  Pixel StippleColor;		/* the stipple color */
  scwm_image *rimg[NUM_IMAGES];	/* the pixmaps to draw with */
};

struct MenuItemDrawingInfo_tag
{
  int cpixOffsetY;		/* top y offset of the item */
  int cpixItemHeight;		/* height for item */
  Pixel BGColor;		/* the background color-- inherited from menu's if 0 */
  Pixel TextColor;		/* the text/fg color, set from menu's if not given */
                                /* above colors are overridden by HL* colors when selected */
  scwm_font *scfont;		/* set from menu's if not given */
};

#define INCREASE_MAYBE(var,val) do { if (val > var) { var = val; } } while (0)

void
InitGCs()
{
  XGCValues gcv;
  unsigned long gcm;
  Pixmap bitmap;
  static Bool GCs_initted;

  if (GCs_initted)
    return;
  
  GCs_initted = True;

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

  bitmap = XCreatePixmap(dpy, Scr.Root, 1, 1, 1);
  gcm = GCFunction | GCPlaneMask | GCGraphicsExposures | GCForeground | GCBackground | GCFillStyle;
  gcv.fill_style = FillSolid;
  gcv.plane_mask = 1;
  gcv.function = GXcopy;
  gcv.graphics_exposures = False;
  MaskGC = XCreateGC(dpy, bitmap, gcm, &gcv);
  XFreePixmap(dpy, bitmap);
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
  
  if (pmd->pmdi->BGColor != LastBGColor ||
      menu_highlight_factor_val != last_highlight_factor ||
      menu_shadow_factor_val != last_shadow_factor) {
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
PaintSideImage(DynamicMenu * pmd, Pixel bg, int cpixHeight, scwm_image *psimg,
	       SCM align)
{
#define FUNC_NAME "PaintSideImage"
  int cpixDstYoffset, cpixSrcYoffset;
  int height;
  
  if (!psimg) {
    scwm_msg(ERR,FUNC_NAME,"psimg is NULL");
    return;
  }
  SetGCFg(Scr.ScratchGC1,bg);
  XFillRectangle(dpy, pmd->w, Scr.ScratchGC1,
		 pmd->pmdi->cpixBorder, pmd->pmdi->cpixBorder,
		 psimg->width, cpixHeight - 2*pmd->pmdi->cpixBorder);

  height = psimg->height;
  if (height > cpixHeight - 2*pmd->pmdi->cpixBorder)
    height = cpixHeight - 2*pmd->pmdi->cpixBorder;
  
  if (align == sym_top) {
    cpixDstYoffset = pmd->pmdi->cpixBorder;
    cpixSrcYoffset = 0;
  } else if (align == sym_center) {
    if (psimg->height > height) {
      cpixDstYoffset = pmd->pmdi->cpixBorder;
      cpixSrcYoffset = (psimg->height - height)/2;
    } else {
      cpixDstYoffset = (cpixHeight - height)/2;
      cpixSrcYoffset = 0;
    }
  } else {
    if (psimg->height > height) {
      cpixDstYoffset = pmd->pmdi->cpixBorder;
      cpixSrcYoffset = psimg->height - height;
    } else {
      cpixDstYoffset = cpixHeight - height - pmd->pmdi->cpixBorder;
      cpixSrcYoffset = 0;
    }
  }
  
  DrawSubImage(pmd->w, psimg,
	       pmd->pmdi->cpixBorder, cpixDstYoffset,
	       0, cpixSrcYoffset,
	       psimg->width, height,
	       NULL);
}
#undef FUNC_NAME

static void
PaintCornerImage(DynamicMenu *pmd, scwm_image *psimg, Bool fRight,Bool fBottom)
{
  if (!psimg)
    return;

  DrawImage(pmd->w, psimg,
	    fRight ? pmd->cpixWidth - psimg->width : 0,
	    fBottom ? pmd->cpixHeight - psimg->height : 0,
	    NULL);
}

static void
TileHorizontal(DynamicMenu *pmd, scwm_image *psimg,
	       int cpixX1, int cpixX2, int cpixY)
{
  int x, y, w, h;

  h = psimg->height;
  x = cpixX1;
  y = cpixY;
  while (x < cpixX2) {
    w = cpixX2 - x;
    if (w > psimg->width)
      w = psimg->width;
    
    DrawSubImage(pmd->w, psimg, x, y, 0, 0, w, h, NULL);
    x += w;
  }
}

static void
TileVertical(DynamicMenu *pmd, scwm_image *psimg,
	     int cpixX, int cpixY1, int cpixY2)
{
  int x, y, w, h;

  w = psimg->width;
  x = cpixX;
  y = cpixY1;
  while (y < cpixY2) {
    h = cpixY2 - y;
    if (h > psimg->height)
      h = psimg->height;
    
    DrawSubImage(pmd->w, psimg, x, y, 0, 0, w, h, NULL);
    y += h;
  }
}

static
void
PaintMask(Drawable dest, scwm_image *psimg, int x, int y, int width, int height)
{
  if (psimg->mask) {
    XCopyPlane(dpy, psimg->mask, dest, MaskGC,
	       0, 0,
	       width, height,
	       x, y,
	       1 /* plane */);
  } else {
    XFillRectangle(dpy, dest, MaskGC, x, y, width, height);
  }
}

static
void
PaintCornerMask(DynamicMenu * pmd, Drawable dest, scwm_image *psimg, Bool fRight,Bool fBottom)
{
  if (!psimg)
    return;

  PaintMask(dest, psimg,
	    fRight ? pmd->cpixWidth - psimg->width : 0,
	    fBottom ? pmd->cpixHeight - psimg->height : 0,
	    psimg->width, psimg->height);
}

static void
TileHorizontalMask(Drawable dest, scwm_image *psimg,
		   int cpixX1, int cpixX2, int cpixY)
{
  int x, y, w, h;

  h = psimg->height;
  x = cpixX1;
  y = cpixY;
  while (x < cpixX2) {
    w = cpixX2 - x;
    if (w > psimg->width)
      w = psimg->width;
    
    PaintMask(dest, psimg, x, y, w, h);
    x += w;
  }
}

static void
TileVerticalMask(Drawable dest, scwm_image *psimg,
		 int cpixX, int cpixY1, int cpixY2)
{
  int x, y, w, h;

  w = psimg->width;
  x = cpixX;
  y = cpixY1;
  while (y < cpixY2) {
    h = cpixY2 - y;
    if (h > psimg->height)
      h = psimg->height;
    
    PaintMask(dest, psimg, x, y, w, h);
    y += h;
  }
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
  /*  Menu *pmenu = pmd->pmenu; */
  MenuDrawingInfo *pmdi = pmd->pmdi;
  MenuItem *pmi = pmiim->pmi;
  MenuItemDrawingInfo *pmidi = pmiim->pmidi;
  scwm_font *scfont = pmidi->scfont;
  int label_font_height = scfont->height;
  int y_offset = pmidi->cpixOffsetY;
  int x_offset = pmdi->cpixItemOffset;
  int item_width = pmd->pmdi->cpixItemWidth;
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
               x_offset-MENU_ITEM_RR_SPACE-1, y_offset,
               item_width+2*MENU_ITEM_RR_SPACE+2, item_height, False);
  } else {
    /* use the item-specific color, or the highlight color */
    Pixel bg = fSelected? pmdi->HLBGColor : pmidi->BGColor;
    SetGCFg(Scr.ScratchGC1,bg);
    XFillRectangle(dpy, w, Scr.ScratchGC1, 
                   x_offset-MENU_ITEM_RR_SPACE-1, y_offset, 
                   item_width+2*MENU_ITEM_RR_SPACE+2, item_height);
  }
  
  /* Draw the shadows for the absolute outside of the menus
     This stuff belongs in here, not in PaintMenu, since we only
     want to redraw it when we have too (i.e. on expose event) */

  /* Only highlight if the item has an action */
  if (fSelected) {
    if (pmd->pmenu->fHighlightRelief) {
      RelieveRectangle(w, x_offset-MENU_ITEM_RR_SPACE, y_offset,
                       item_width+2*MENU_ITEM_RR_SPACE+1, item_height,
                       ReliefGC,ShadowGC);
    }
  }

  if (pmi->fIsSeparator) {
    DrawSeparator(w,ShadowGC,ReliefGC,
		  x_offset-MENU_ITEM_RR_SPACE,
		  x_offset+item_width+MENU_ITEM_RR_SPACE,
		  y_offset-1+MENU_HEIGHT_SEPARATOR/2,0);
  } else {
    if (psimgAbove) {
      int x = (item_width - psimgAbove->width) / 2 + x_offset;
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
    if (mis == MIS_Grayed) {
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

    x_offset += MENU_TEXT_SPACING/2;
    
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
			    x_offset,
			    y_offset+d-1,
			    x_offset + 7,
			    y_offset+d+7);
      } else {
	DrawTrianglePattern(w, ReliefGC, ShadowGC, ReliefGC, /* ShadowGC, */
			    x_offset,
			    y_offset+d-1,
			    x_offset + 7,
			    y_offset+d+7);
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

  PaintCornerImage(pmd, pmdi->rimg[0], False, False);
  PaintCornerImage(pmd, pmdi->rimg[2], True, False);
  PaintCornerImage(pmd, pmdi->rimg[5], False, True);
  PaintCornerImage(pmd, pmdi->rimg[7], True, True);
  
  /* pmd, image, x1, x2, y */
  TileHorizontal(pmd, pmdi->rimg[1],
		 pmdi->rimg[0]->width,
		 pmd->cpixWidth - pmdi->rimg[2]->width,
		 pmdi->cpixBorder - pmdi->rimg[1]->height - 1);
  TileHorizontal(pmd, pmdi->rimg[6],
		 pmdi->rimg[5]->width,
		 pmd->cpixWidth - pmdi->rimg[7]->width,
		 pmd->cpixHeight - pmdi->cpixBorder + 1);
  /* pmd, image, x, y1, y2 */
  TileVertical(pmd, pmdi->rimg[3],
	       pmdi->cpixBorder - pmdi->rimg[3]->width - 1,
	       pmdi->rimg[0]->height, pmd->cpixHeight - pmdi->rimg[5]->height);
  TileVertical(pmd, pmdi->rimg[4],
	       pmd->cpixWidth - pmdi->cpixBorder + 1,
	       pmdi->rimg[2]->height, pmd->cpixHeight - pmdi->rimg[7]->height);
  XClearArea(dpy, pmd->w,
	     pmd->pmdi->cpixBorder - 1, pmd->pmdi->cpixBorder - 1,
	     pmd->cpixWidth - 2*pmd->pmdi->cpixBorder + 2,
	     pmd->cpixHeight - 2*pmd->pmdi->cpixBorder + 2, False);
  
  DBUG((DBG,FUNC_NAME,"Painting menu"));
  for (imiim = 0; imiim < cmiim; imiim++) {
    MenuItemInMenu *pmiim = rgpmiim[imiim];
    if (1 || /* FIXJTL: optimize all of this! */
	(pxe->xexpose.y < (pmiim->pmidi->cpixOffsetY +
                           pmiim->pmidi->cpixItemHeight) &&
         (pxe->xexpose.y + pxe->xexpose.height) > pmiim->pmidi->cpixOffsetY)) {
      DBUG((DBG,FUNC_NAME,"Painting menu item"));
      PaintMenuItem(w,pmd,pmiim);
    }
  }

  { /* scope */
    scwm_image *psimgSide = DYNAMIC_SAFE_IMAGE(pmd->pmenu->scmImgSide);
    if (psimgSide) {
      DBUG((DBG,FUNC_NAME,"Painting side image"));
      PaintSideImage(pmd,pmdi->SideBGColor,pmd->cpixHeight,psimgSide,
		     pmd->pmenu->scmSideAlign);
    }
  }
  XSync(dpy,0);
}
#undef FUNC_NAME

static
void
SetShape(DynamicMenu * pmd)
{
  Pixmap mask;
  MenuDrawingInfo * pmdi = pmd->pmdi;

  InitGCs();
  
  mask = XCreatePixmap(dpy, Scr.Root, pmd->cpixWidth, pmd->cpixHeight, 1);

  XSetForeground(dpy, MaskGC, 0);
  XFillRectangle(dpy, mask, MaskGC, 0, 0, pmd->cpixWidth, pmd->cpixHeight);

  XSetForeground(dpy, MaskGC, 1);

  PaintCornerMask(pmd, mask, pmdi->rimg[0], False, False);
  PaintCornerMask(pmd, mask, pmdi->rimg[2], True, False);
  PaintCornerMask(pmd, mask, pmdi->rimg[5], False, True);
  PaintCornerMask(pmd, mask, pmdi->rimg[7], True, True);

  /* mask, image, x1, x2, y */
  TileHorizontalMask(mask, pmdi->rimg[1],
		     pmdi->rimg[0]->width,
		     pmd->cpixWidth - pmdi->rimg[2]->width,
		     pmdi->cpixBorder - pmdi->rimg[1]->height - 1);
  TileHorizontalMask(mask, pmdi->rimg[6],
		     pmdi->rimg[5]->width,
		     pmd->cpixWidth - pmdi->rimg[7]->width,
		     pmd->cpixHeight - pmdi->cpixBorder + 1);
  /* mask, image, x, y1, y2 */
  TileVerticalMask(mask, pmdi->rimg[3],
		   pmdi->cpixBorder - pmdi->rimg[3]->width - 1,
		   pmdi->rimg[0]->height,
		   pmd->cpixHeight - pmdi->rimg[5]->height);
  TileVerticalMask(mask, pmdi->rimg[4],
		   pmd->cpixWidth - pmdi->cpixBorder + 1,
		   pmdi->rimg[2]->height,
		   pmd->cpixHeight - pmdi->rimg[7]->height);
  
  XFillRectangle(dpy, mask, MaskGC,
	     pmd->pmdi->cpixBorder - 1, pmd->pmdi->cpixBorder - 1,
	     pmd->cpixWidth - 2*pmd->pmdi->cpixBorder + 2,
	     pmd->cpixHeight - 2*pmd->pmdi->cpixBorder + 2);
  
  XShapeCombineMask(dpy, pmd->w, ShapeBounding, 0, 0, mask, ShapeSet);

  XFreePixmap(dpy, mask);
}

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
  /* MenuDrawingInfo * pmdiNew = pmdNew->pmdi; */
  int cpixWidthNewMenu = pmdNew->cpixWidth;

  if (cpixXmenu + cpixWidthMenu + cpixWidthNewMenu - pmdOld->pmdi->cpixBorder <= Scr.DisplayWidth) {
    pmdNew->x = cpixXmenu + cpixWidthMenu - pmdOld->pmdi->cpixBorder - 2;
  } else {
    /* pop to the left */
    pmdNew->x = cpixXmenu - cpixWidthNewMenu +
      pmdiOld->cpixSideImage + pmdiOld->cpixBorder;
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

  /* FIXGJB: make fraction of menu that pointer goes to configurable */
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
InPopupZone(MenuItemInMenu *pmiim, int cpixXoffset, int ARG_UNUSED(cpixYoffset))
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
  *px = pmd->x + pmd->cpixWidth - pmd->pmdi->cpixBorder;
}

/* px and py are in & out parameters; return the x,y location for the
   top left of the popup when the mouse was clicked at x,y; This function
   doesn't have to concern itself with screen borders or with being
   popped up from another menu or decoration */
static void
GetPreferredPopupPosition(DynamicMenu * pmd, int *px, int *py)
{
  *px = *px - pmd->cpixWidth/2;
  *py = *py - pmd->rgpmiim[0]->pmidi->cpixItemHeight/2 - pmd->pmdi->cpixBorder;
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
static void
ConstructDynamicXpmMenu(DynamicMenu *pmd)
{
#define FUNC_NAME "ConstructDynamicXpmMenu"
  SCM scmRest;
  SCM scmImage;
  int iImage;
  int cpixBorder = 4; /* KDE always uses at least 4 pixel borders */

  if (pmd->pmdi != NULL)
    return;

  /* remember how to paint this menu */
  pmd->pmdv = DYNAMIC_SAFE_MENULOOK(pmd->pmenu->scmMenuLook)->mdvt;

  pmd->pmdi = NEW(MenuDrawingInfo);
    
  scmRest = pmd->pmenu->scmExtraOptions;
  DEREF_IF_SYMBOL(scmRest);
  if (SCM_IMP(scmRest)) {
    scwm_menulook * pml = DYNAMIC_SAFE_MENULOOK(pmd->pmenu->scmMenuLook);
    if (pml)
      scmRest = pml->extra;
    DEREF_IF_SYMBOL(scmRest);
  }
  
  memset(pmd->pmdi->rimg, 0, sizeof pmd->pmdi->rimg);
  for (iImage = 0; iImage < NUM_IMAGES; iImage++) {
      if (SCM_IMP(scmRest))
	break;
      if (SCM_NULLP(scmRest))
	break;
      scmImage = gh_car(scmRest);
      pmd->pmdi->rimg[iImage] = DYNAMIC_SAFE_IMAGE(gh_car(scmRest));
      if (!pmd->pmdi->rimg[iImage])
	break;
      scmRest = gh_cdr(scmRest);
  }
  /* Is this a valid xpm menu? */
  if (iImage != NUM_IMAGES) {
    /* print an error message only if it looks like it was supposed to
       be; otherwise, just fall back to the standard method */
    if (iImage != 0)
      scwm_msg(ERR,FUNC_NAME,"psimg is NULL");
    FREE(pmd->pmdi);
    pmd->pmdi = NULL;
    ConstructDynamicMenu(pmd);
    return;
  }

#define IMAGE_WIDTH(X) ((X)?((X)->width):0)
#define IMAGE_HEIGHT(X) ((X)?((X)->height):0)
  
  INCREASE_MAYBE(cpixBorder, IMAGE_HEIGHT(pmd->pmdi->rimg[1]));
  INCREASE_MAYBE(cpixBorder, IMAGE_WIDTH(pmd->pmdi->rimg[3]));
  INCREASE_MAYBE(cpixBorder, IMAGE_WIDTH(pmd->pmdi->rimg[4]));
  INCREASE_MAYBE(cpixBorder, IMAGE_HEIGHT(pmd->pmdi->rimg[6]));

#undef IMAGE_HEIGHT
#undef IMAGE_WIDTH
  
  pmd->pmdi->cpixBorder = cpixBorder;
    
  { /* scope */
    Menu *pmenu = pmd->pmenu;
    scwm_image *psimgSide = DYNAMIC_SAFE_IMAGE(pmenu->scmImgSide);
    scwm_image *psimgBackground = DYNAMIC_SAFE_IMAGE(pmenu->scmImgBackground);
    Pixel TextColorDefault = DYNAMIC_SAFE_COLOR(pmenu->scmTextColor);
    MenuItemInMenu **rgpmiim = pmd->rgpmiim;
    MenuDrawingInfo *pmdi = pmd->pmdi;
    scwm_font *scfontDefault = DYNAMIC_SAFE_FONT(pmenu->scmFont);

    int cmiim = pmd->cmiim;
    int imiim = 0;
    int total_height = cpixBorder;
    int max_text_width = 0;
    int max_extra_text_width = 0;
    int max_left_image_width = 0;
    int max_right_image_width = 0;
    int max_above_image_width = 0;
    int max_item_width = 0;

    pmdi->HLBGColor = DYNAMIC_SAFE_COLOR_USE_DEF(pmenu->scmHLBGColor,pmdi->BGColor);
    pmdi->HLTextColor = DYNAMIC_SAFE_COLOR_USE_DEF(pmenu->scmHLTextColor,TextColorDefault);
    pmdi->BGColor = DYNAMIC_SAFE_COLOR(pmenu->scmBGColor);
    pmdi->SideBGColor = DYNAMIC_SAFE_COLOR(pmenu->scmSideBGColor);
    pmdi->StippleColor = DYNAMIC_SAFE_COLOR(pmenu->scmStippleColor);
    
    pmd->x = 0;		/* just init: gets set elsewhere */
    pmd->y = 0;		/* just init: gets set elsewhere */
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

      text_width = ComputeXTextWidth(XFONT_FONTTYPE(scfont),
				     pmi->szLabel, pmi->cchLabel);

      DBUG((DBG,FUNC_NAME,"`%s' has width %d (%d chars)\n",
	   pmi->szLabel,text_width,pmi->cchLabel));

      /* need to set the bg color even for separators. */
      pmidi->BGColor = DYNAMIC_SAFE_COLOR_USE_DEF(pmi->scmBGColor,pmdi->BGColor);

      if (pmi->fIsSeparator) {
	/* XXX should have separator pixmap */
	item_height = MENU_HEIGHT_SEPARATOR;
      } else {
	/* szLabel we know is not null, but szExtra can be */
	if (pmi->szExtra) {
          extra_text_width = ComputeXTextWidth(XFONT_FONTTYPE(scfont), 
                                               pmi->szExtra, pmi->cchExtra);
	}

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
    pmdi->cpixItemOffset = MENU_ITEM_RR_SPACE + cpixBorder;

    /* Handle the side image, if any */
    pmdi->cpixSideImage = 0;
    if (psimgSide) {
      pmdi->cpixSideImage = psimgSide->width + MENU_SIDE_IMAGE_SPACING;
      pmdi->cpixItemOffset += pmdi->cpixSideImage;
    }

    total_height += cpixBorder;

    pmdi->cpixLeftPicWidth = max_left_image_width;
    pmdi->cpixTextWidth = max_text_width + MENU_TEXT_SPACING;
    pmdi->cpixExtraTextWidth = max_extra_text_width + MENU_TEXT_SPACING;
    pmd->cpixHeight = total_height;

    /* use the width of the largest above image if it is greater than
       the sum of the widths of the other components */
    max_item_width = max(pmdi->cpixLeftPicWidth+
			 pmdi->cpixTextWidth+
			 pmdi->cpixExtraTextWidth+
			 max_right_image_width,
			 max_above_image_width);

    pmd->cpixWidth = pmdi->cpixItemOffset + max_item_width +  
      MENU_ITEM_RR_SPACE + cpixBorder;
    pmd->pmdi->cpixItemWidth = max_item_width;
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

      SetShape(pmd);
    }
  }
}
#undef FUNC_NAME
#undef INCREASE_MAYBE

static void
init_draw_xpm_menu()
{
  MenuDrawingVtable * pmdvt;
  
  pmdvt = NEW(MenuDrawingVtable);
  memset(pmdvt, 0, sizeof *pmdvt);
  
  pmdvt->fnConstructDynamicMenu = ConstructDynamicXpmMenu;
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
  
  xpm_shaped_menu_look = make_menulook("xpm-shaped-menu-look", SCM_BOOL_T, pmdvt);
  SCWM_VAR_READ_ONLY(NULL,"xpm-shaped-menu-look",xpm_shaped_menu_look);
  /** The shaped XPM menu look. */

  /* FIXJTL:
     xpm_menu_look = make_menulook("xpm-menu-look", SCM_BOOL_F, pmdvt);
     SCWM_VAR(xpm_menu_look,"xpm-menu-look");

     for non-shaped menus */
  
#ifndef SCM_MAGIC_SNARFER
#include "draw-xpm-menu.x"
#endif
}

void scm_init_app_scwm_xpm_menus_module()
{
  scm_register_module_xxx("app scwm xpm-menus", init_draw_xpm_menu);
}

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
