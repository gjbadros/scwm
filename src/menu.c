/* $Id$
 * scwmmenu.c
 * Copyright (C) 1997-1999 Greg J. Badros and Maciej Stachowiak
 */

/* #define SCWM_DEBUG_MSGS */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/time.h>
#include <X11/keysym.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include <guile/gh.h>

#define MENU_IMPLEMENTATION
#include "menu.h"

#include "scwm.h"
#include "font.h"
#include "events.h"
#include "menulook.h"
#include "drawmenu.h"
#include "colormaps.h"
#include "xmisc.h"
#include "screen.h"
#include "color.h"
#include "util.h"
#include "string_token.h"
#include "guile-compat.h"
#include "syscompat.h"
#include "callbacks.h"
#include "cursor.h"

#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif

extern SCM sym_top, sym_center, sym_bottom;

static Bool fMenuHotkeysActivateItems = True;


static DynamicMenu *NewDynamicMenu(Menu *pmenu, DynamicMenu *pmdPoppedFrom);
static void PopdownMenu(DynamicMenu *pmd);
static void FreeDynamicMenu(DynamicMenu *pmd);
SCM popup_menu(SCM menu, SCM warp_to_first, SCM x_pos, SCM y_pos, SCM left_side_p, SCM permit_alt_release_selection_p);

static
SCM
scwm_safe_call0_sym(SCM thunk)
{
  DEREF_IF_SYMBOL(thunk);
  return scwm_safe_call0(thunk);
}

#ifdef SCWM_DEBUG_MSGS
/* Give string name of first item in a dynamic menu; mostly used for debugging */
static const char *
SzFirstItemFromPmd(const DynamicMenu *pmd)
{
  SCM scmItem = pmd->pmenu->scmMenuTitle;
  MenuItem *pmi = SAFE_MENUITEM(scmItem);

  if (pmi)
    return pmi->szLabel;

  scmItem = gh_car(pmd->pmenu->scmMenuItems);
  pmi = SAFE_MENUITEM(scmItem);

  if (pmi)
    return pmi->szLabel;
  else
    return "NULL";
}
#endif

SCM 
mark_menu(SCM obj)
{
  Menu *pmenu;
  pmenu = MENU(obj);
  GC_MARK_SCM_IF_SET(pmenu->scmMenuTitle);
  GC_MARK_SCM_IF_SET(pmenu->scmMenuItems);
  GC_MARK_SCM_IF_SET(pmenu->scmImgSide);
  GC_MARK_SCM_IF_SET(pmenu->scmSideAlign);
  GC_MARK_SCM_IF_SET(pmenu->scmSideBGColor);
  GC_MARK_SCM_IF_SET(pmenu->scmBGColor);
  GC_MARK_SCM_IF_SET(pmenu->scmTextColor);
  GC_MARK_SCM_IF_SET(pmenu->scmHLBGColor);
  GC_MARK_SCM_IF_SET(pmenu->scmHLTextColor);
  GC_MARK_SCM_IF_SET(pmenu->scmStippleColor);
  GC_MARK_SCM_IF_SET(pmenu->scmImgBackground);
  GC_MARK_SCM_IF_SET(pmenu->scmFont);
  GC_MARK_SCM_IF_SET(pmenu->scmExtraOptions);
  GC_MARK_SCM_IF_SET(pmenu->scmMenuLook);
  return SCM_BOOL_F;
}

size_t 
free_menu(SCM obj)
{
  Menu *pmenu = MENU(obj);
  if (pmenu->pchUsedShortcutKeys) {
    FREE(pmenu->pchUsedShortcutKeys);
  }
  FREE(pmenu);
  return(0);
}

int 
print_menu(SCM obj, SCM port, scm_print_state *ARG_IGNORE(pstate))
{
  scm_puts("#<menu ", port);
  if (MENU_P(obj)) {
    Menu *pmenu = MENU(obj);
    if (MENUITEM_P(pmenu->scmMenuTitle)) {
      scm_write(pmenu->scmMenuTitle, port);
    } else if (SCM_NIMP(pmenu->scmMenuItems)) {
      scm_write(gh_car(pmenu->scmMenuItems), port);
    }
    if (pmenu->pchUsedShortcutKeys) {
      scm_puts(", hotkeys: ",port);
      scm_puts(pmenu->pchUsedShortcutKeys,port);
    }
  } else {
    scm_puts("(invalid)", port);
  }
  scm_putc('>', port);

  return 1;
}

SCWM_PROC(menu_p,"menu?", 1,0,0,
          (SCM obj),
"Return #t if and only if OBJ is a menu object.")
#define FUNC_NAME s_menu_p
{
  return SCM_BOOL_FromBool(MENU_P(obj));
}
#undef FUNC_NAME


/* NewPchKeysUsed:
 * Assign shortcut keys to the menu items in pmd->rgpmiim[],
 * and return a newly-allocated null-terminated array of characters
 * listing those that we assigned as short cuts
 */
char *
NewPchKeysUsed(DynamicMenu *pmd)
{
#define FUNC_NAME "NewPchKeysUsed"
  SCM list_of_menuitems = pmd->pmenu->scmMenuItems;
  MenuItemInMenu **rgpmiim = pmd->rgpmiim;
  int ipmiim = 0;
  int cItems = gh_length(list_of_menuitems);
  char *pch = NEWC(cItems+1, char);
  int ich = 0;
  SCM item;
  SCM rest = list_of_menuitems;
  MenuItem *pmi;

  while (True) {
    item = gh_car(rest);
    pmi = SAFE_MENUITEM(item);
    if (!pmi) {
      /* do not print a warning if a menu item is #f */
      if (item != SCM_BOOL_F)
	scwm_msg(WARN,FUNC_NAME,"Bad menu item %d",ipmiim);
    } else {
      if (pmi->pchHotkeyPreferences) {
	char *pchDesiredChars = pmi->pchHotkeyPreferences;
	char ch;
	while ((ch = *pchDesiredChars++) != '\0') {
	  if (!strchr(pch,ch)) {
	    /* Found the char to use */
	    rgpmiim[ipmiim]->chShortcut = ch;
	    pch[ich++] = tolower(ch);
	  rgpmiim[ipmiim]->ichShortcutOffset = IchIgnoreCaseInSz(pmi->szLabel,ch);
	  break;
	  }
	}
      }
      ipmiim++;
    }
    rest = gh_cdr(rest);
    if (SCM_NULLP(rest))
      break;
  }
  return pch;
}
#undef FUNC_NAME

/* GJB:FIXME:: better as an assoc list, probably: we'd have to invent
   a name for menu-items if we did */

SCWM_PROC(menu_properties, "menu-properties", 1, 0, 0,
          (SCM menu),
"Returns the a list of the menu properties of MENU, a menu object.
The properties returned are: 
'(menu-title menu-items side-image side-image-align side-bg-color bg-color
text-color stipple-color
image-bg font extra-options used-shortcut-keys popup-delay hover-delay)")
#define FUNC_NAME s_menu_properties
{
  Menu *pmenu;
  VALIDATE_ARG_MENU_COPY(1,menu,pmenu);
  return gh_list(pmenu->scmMenuTitle,
		 pmenu->scmMenuItems,
		 pmenu->scmImgSide,
		 pmenu->scmSideAlign,
		 pmenu->scmSideBGColor,
		 pmenu->scmBGColor,
		 pmenu->scmTextColor,
		 pmenu->scmStippleColor,
		 pmenu->scmImgBackground,
		 pmenu->scmFont,
		 pmenu->scmExtraOptions,
		 gh_str02scm(pmenu->pchUsedShortcutKeys),
                 gh_int2scm(pmenu->cmsPopupDelay),
                 gh_int2scm(pmenu->cmsHoverDelay),
                 SCM_UNDEFINED);
}
#undef FUNC_NAME


/* FIXJTL: guile has a limit of 10 arguments?  Either raise the limit
   somehow, or restructure hwo make-menu works.  Maybe it's time to move
   side picture stuff into extra_options?  Or maybe an alist or keyword list
   for all the args? */
/* Proposal: remove all optional arguments here, and add set-menu-...!
   functions for them; either rename the set-menu-...! functions in
   base.scm to set-default-menu-...! or remove them (does anybody
   use them directly? */
SCWM_PROC(make_menu, "make-menu", 5, 5, 0,
          (SCM list_of_menuitems,
           SCM bg_color, SCM text_color, SCM stipple_color, SCM font,
           SCM picture_side, SCM side_picture_align, SCM side_bg_color,
           SCM picture_bg, 
           SCM extra_options),
"Make and return a menu object from the given arguments.
LIST-OF-MENUITEMS is a non-empty scheme list of menu items -- see `make-menuitem';
BG-COLOR, TEXT-COLOR and STIPPLE-COLOR are color objects or symbols;
FONT is a font object or symbol;
PICTURE-SIDE is an image object to draw on the left side of the menu;
SIDE-PICTURE-ALIGN is one of 'top, 'center, or 'bottom;
SIDE-BG-COLOR is a color object or symbol;
PICTURE-BG is an image object;
EXTRA-OPTIONS can be anything understood by the menu-look")
#define FUNC_NAME s_make_menu
{
  Menu *pmenu = NEW(Menu);
  SCM answer;
  pmenu->cmsPopupDelay = 900;
  pmenu->cmsHoverDelay = 500;

  /* LIST-OF-MENUITEMS: Required */
  VALIDATE_ARG_LISTNONEMPTY(1,list_of_menuitems);
  pmenu->scmMenuItems = list_of_menuitems;

  pmenu->fHighlightRelief = True;

  /* BG-COLOR: Required */
  VALIDATE_ARG_COLOR_OR_SYM(2,bg_color);
  pmenu->scmBGColor = bg_color;
  pmenu->scmHLBGColor = SCM_BOOL_F; /* unchanged when highlighted */

  /* TEXT-COLOR: Required */
  VALIDATE_ARG_COLOR_OR_SYM(3,text_color);
  pmenu->scmTextColor = text_color;
  pmenu->scmHLTextColor = SCM_BOOL_F; /* unchanged when highlighted */

  /* STIPPLE-COLOR: Required */
  VALIDATE_ARG_COLOR_OR_SYM(4,stipple_color);
  pmenu->scmStippleColor = stipple_color;

  /* FONT: Required */
  VALIDATE_ARG_FONT_OR_SYM(5,font);
  pmenu->scmFont = font;

  /* PICTURE-SIDE: Optional */
  VALIDATE_ARG_IMAGE_OR_SYM_USE_F(6,picture_side);
  pmenu->scmImgSide = picture_side;

  /* SIDE-PICTURE-ALIGN: Optional */
  if (UNSET_SCM(side_picture_align)) {
    side_picture_align = sym_top;
  } else if (!gh_symbol_p(side_picture_align) ||
	     (side_picture_align != sym_top &&
	      side_picture_align != sym_center &&
	      side_picture_align != sym_bottom)) {
    scm_misc_error(FUNC_NAME,"SIDE-PICTURE-ALIGN must be 'top, 'center or 'bottom'",
		   SCM_EOL);
  }
  pmenu->scmSideAlign = side_picture_align;

  /* SIDE-BG-COLOR: Optional */
  VALIDATE_ARG_COLOR_OR_SYM_USE_WHITE(8,side_bg_color);
  pmenu->scmSideBGColor = side_bg_color;

  /* PICTURE-BG: Optional */
  VALIDATE_ARG_IMAGE_OR_SYM_USE_F(9,picture_bg);
  pmenu->scmImgBackground = picture_bg;

  /* EXTRA-OPTIONS: Optional */
  pmenu->scmExtraOptions = extra_options;

  pmenu->scmMenuTitle = SCM_BOOL_F;
  pmenu->scmMenuLook = SCM_BOOL_F;

  pmenu->pchUsedShortcutKeys = NULL;

  SCWM_NEWCELL_SMOB(answer,scm_tc16_scwm_menu, pmenu);
  pmenu->self = answer;
  return answer;
}
#undef FUNC_NAME

SCWM_PROC(set_menu_popup_delay_x, "set-menu-popup-delay!", 2, 0, 0,
          (SCM menu, SCM popup_delay),
"Set MENU's submenu popup delay to POPUP-DELAY.
POPUP-DELAY is the number of ms to wait before popping up submenus.")
#define FUNC_NAME s_set_menu_popup_delay_x
{
  VALIDATE_ARG_MENU(1,menu);
  VALIDATE_ARG_INT_COPY(2,popup_delay,MENU(menu)->cmsPopupDelay);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(menu_popup_delay, "menu-popup-delay", 1, 0, 0,
          (SCM menu),
"Return MENU's submenu popup delay.
See `set-menu-popup-delay!'.")
#define FUNC_NAME s_menu_popup_delay
{
  VALIDATE_ARG_MENU(1,menu);
  return gh_int2scm(MENU(menu)->cmsPopupDelay);
}
#undef FUNC_NAME



SCWM_PROC(set_menu_hover_delay_x, "set-menu-hover-delay!", 2, 0, 0,
          (SCM menu, SCM hover_delay),
"Set MENU's hover delay to HOVER-DELAY.
HOVER-DELAY is the number of ms to wait before invoking the hover action.")
#define FUNC_NAME s_set_menu_hover_delay_x
{
  VALIDATE_ARG_MENU(1,menu);
  VALIDATE_ARG_INT_COPY(2,hover_delay,MENU(menu)->cmsHoverDelay);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(menu_hover_delay, "menu-hover-delay", 1, 0, 0,
          (SCM menu),
"Return MENU's hover action delay.
See `set-menu-hover-delay!'.")
#define FUNC_NAME s_menu_hover_delay
{
  VALIDATE_ARG_MENU(1,menu);
  return gh_int2scm(MENU(menu)->cmsHoverDelay);
}
#undef FUNC_NAME


SCWM_PROC(set_menu_look_x, "set-menu-look!", 2, 0, 0,
          (SCM menu, SCM menu_look),
"Use MENU-LOOK as the menu-look for MENU.")
#define FUNC_NAME s_set_menu_look_x
{
  VALIDATE_ARG_MENU(1,menu);
  VALIDATE_ARG_MENULOOK_OR_SYM(2,menu_look);

  MENU(menu)->scmMenuLook = menu_look;
  
  return (SCM_UNSPECIFIED);
}
#undef FUNC_NAME

SCWM_PROC(set_menu_title_x, "set-menu-title!", 2, 0, 0,
          (SCM menu, SCM menu_title),
"Use MENU-TITLE as the title for MENU.")
#define FUNC_NAME s_set_menu_title_x
{
  VALIDATE_ARG_MENU(1,menu);
  VALIDATE_ARG_MENUITEM(2,menu_title);

  MENU(menu)->scmMenuTitle = menu_title;
  
  return (SCM_UNSPECIFIED);
}
#undef FUNC_NAME

SCWM_PROC(set_menu_colors_x, "set-menu-colors!", 3, 1, 0,
          (SCM menu, SCM text_color, SCM bg_color, SCM stipple_color),
"Use TEXT-COLOR and BG-COLOR as the colors for MENU.
STIPPLE-COLOR is optional, and if given will be used for the
stipple color for the MENU.")
#define FUNC_NAME s_set_menu_colors_x
{
  Menu *pm;
  VALIDATE_ARG_MENU_COPY(1,menu,pm);
  if (!UNSET_SCM(text_color)) {
    VALIDATE_ARG_COLOR_OR_SYM(2,text_color);
    pm->scmTextColor = text_color;
  }
  if (!UNSET_SCM(bg_color)) {
    VALIDATE_ARG_COLOR_OR_SYM(3,bg_color);
    pm->scmBGColor = bg_color;
  }
  if (!UNSET_SCM(stipple_color)) {
    VALIDATE_ARG_COLOR_OR_SYM(4,stipple_color);
    pm->scmStippleColor = stipple_color;
  }
  
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(set_menu_highlight_colors_x, "set-menu-highlight-colors!", 3, 0, 0,
          (SCM menu, SCM text_color, SCM bg_color),
"Use TEXT-COLOR and BG-COLOR as the highlight colors for MENU.
These colors will be used for the selected item.")
#define FUNC_NAME s_set_menu_highlight_colors_x
{
  Menu *pm;
  VALIDATE_ARG_MENU_COPY(1,menu,pm);
  if (!UNSET_SCM(text_color)) {
    VALIDATE_ARG_COLOR_OR_SYM(2,text_color);
    pm->scmHLTextColor = text_color;
  }
  if (!UNSET_SCM(bg_color)) {
    VALIDATE_ARG_COLOR_OR_SYM(3,bg_color);
    pm->scmHLBGColor = bg_color;
  }
  
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(set_menu_highlight_relief_x, "set-menu-highlight-relief!", 2, 0, 0,
          (SCM menu, SCM highlight_relief_p),
"If HIGHLIGHT-RELIEF? is #t, then draw a relief on selected items in MENU.
Otherwise, do not.  See also `set-menu-highlight-colors!'.")
#define FUNC_NAME s_set_menu_highlight_relief_x
{
  Menu *pm;
  VALIDATE_ARG_MENU_COPY(1,menu,pm);
  VALIDATE_ARG_BOOL_COPY(2,highlight_relief_p,pm->fHighlightRelief);
  
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(menu_highlight_colors, "menu-highlight-colors", 1, 0, 0,
          (SCM menu),
"Return list text-color, bg-color, the highlight colors for MENU.")
#define FUNC_NAME s_menu_highlight_colors
{
  Menu *pm;
  VALIDATE_ARG_MENU_COPY(1,menu,pm);
  return gh_list(pm->scmHLTextColor,pm->scmHLBGColor,SCM_UNDEFINED);
}
#undef FUNC_NAME

SCWM_PROC(menu_highlight_relief_p, "menu-highlight-relief?", 1, 0, 0,
          (SCM menu),
"Return #t if MENU's selected item is relieved, #f otherwise.")
#define FUNC_NAME s_menu_highlight_relief_p
{
  Menu *pm;
  VALIDATE_ARG_MENU_COPY(1,menu,pm);
  return SCM_BOOL_FromBool(pm->fHighlightRelief);
}
#undef FUNC_NAME


SCWM_PROC(set_menu_font_x, "set-menu-font!", 2, 0, 0,
          (SCM menu, SCM font),
"Use FONT as the font for MENU.")
#define FUNC_NAME s_set_menu_font_x
{
  Menu *pm;
  VALIDATE_ARG_MENU_COPY(1,menu,pm);
  VALIDATE_ARG_FONT_OR_SYM(2,font);

  pm->scmFont = font;

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(set_menu_side_picture_x, "set-menu-side-picture!", 2, 2, 0,
          (SCM menu, SCM picture, SCM align, SCM bg_color),
"Use PICTURE as the side picture for MENU.
Set its alignment to ALIGN, and its background
color to BG-COLOR.")
#define FUNC_NAME s_set_menu_side_picture_x
{
  Menu *pm;
  VALIDATE_ARG_MENU_COPY(1,menu,pm);

  VALIDATE_ARG_IMAGE_OR_SYM(2,picture);
  pm->scmImgSide = picture;

  if (!UNSET_SCM(align)) {
    if (!gh_symbol_p(align) ||
        (align != sym_top &&
         align != sym_center &&
         align != sym_bottom)) {
      scm_misc_error(FUNC_NAME,"ALIGN must be 'top, 'center or 'bottom'",
                     SCM_EOL);
    }
    pm->scmSideAlign = align;
  }

  if (!UNSET_SCM(bg_color)) {
    VALIDATE_ARG_COLOR_OR_SYM_USE_WHITE(4,bg_color);
    pm->scmSideBGColor = bg_color;
  }
  
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(set_menu_background_picture_x, "set-menu-background-picture!", 2, 0, 0,
          (SCM menu, SCM picture),
"Use PICTURE as the background image for MENU.")
#define FUNC_NAME s_set_menu_background_picture_x
{
  Menu *pm;
  VALIDATE_ARG_MENU_COPY(1,menu,pm);
  VALIDATE_ARG_IMAGE_OR_SYM(2,picture);
  
  pm->scmImgBackground  = picture;
  
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(set_menu_extra_options_x, "set-menu-extra-options!", 2, 0, 0,
          (SCM menu, SCM options),
"Set MENU's extra options to OPTIONS.")
#define FUNC_NAME s_set_menu_extra_options_x
{
  VALIDATE_ARG_MENU(1,menu);
  MENU(menu)->scmExtraOptions = options;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/*
 * GetPreferredPopupPosition
 * Given x,y and the menu to popup, return the coords
 * that should be used for the upper left
 */
static
void 
GetPreferredPopupPosition(DynamicMenu *pmd,
			  DynamicMenu *pmdPoppedFrom, int x, int y, 
			  int *pxReturn, int *pyReturn)
{
  if (pmdPoppedFrom) {
    *pxReturn = x;
    *pyReturn = y;
    pmdPoppedFrom->pmdv->fnGetChildPopupPosition(pmdPoppedFrom, pxReturn, pyReturn);
  } else {
    *pxReturn = x;
    *pyReturn = y;
    pmd->pmdv->fnGetPreferredPopupPosition(pmd, pxReturn, pyReturn);
  }
  if (*pyReturn + pmd->cpixHeight > Scr.DisplayHeight) {
    *pyReturn = Scr.DisplayHeight-pmd->cpixHeight;
  }
  if (*pxReturn + pmd->cpixWidth > Scr.DisplayWidth) {
    *pxReturn = Scr.DisplayWidth-pmd->cpixWidth;
  }
  if (*pxReturn < 0) *pxReturn = 0;
  if (*pyReturn < 0) *pyReturn = 0;
  return;
}

static
void
SetPopupMenuPosition(DynamicMenu *pmd, int x_pointer, int y_pointer)
{
  DynamicMenu *pmdPoppedFrom = pmd->pmdPrior;
  int x;
  int y;
  GetPreferredPopupPosition(pmd, pmdPoppedFrom, x_pointer, y_pointer,
			    &x, &y);
  pmd->x = x;
  pmd->y = y;
}

static
DynamicMenu *
PmdFromWindow(Display *dpy, Window w)
{
#define FUNC_NAME "PmdFromWindow"
  DynamicMenu *pmd = NULL;

  if (w == None)
    return NULL;

  if ((XFindContext(dpy, w, MenuContext,
		    (caddr_t *)&pmd) == XCNOENT)) {
    DBUG((DBG,FUNC_NAME,"XFindContext gave XCNOENT"));
    pmd = NULL;
  }
  return pmd;
}
#undef FUNC_NAME


#ifdef BUILD_UNUSED_FUNCTIONS /* GJB:FIXME:: UNUSED */
static
DynamicMenu *
PmdFromPointerLocation(Display *dpy)
{
  Window wChild = WXGetPointerChild( Scr.Root );
  return PmdFromWindow(dpy,wChild);
}
#endif

/* PmiimFromPointerLocation
 * Find the MenuItemInMenu that the pointer is at now
 * Return a pointer to the MenuItemInMenu, *px_offset, and *py_offset
 * the x and y offsets within the item -- pass px_offset = NULL
 * or py_offset = NULL to ignore those return values
 * Returns NULL if pointer not pointing at a menu item
 */
static
MenuItemInMenu *
PmiimFromPointerLocation(Display *dpy, int *px_offset, int *py_offset)
{
#define FUNC_NAME "PmiimFromPointerLocation"
  int root_x, root_y;
  int x,y;
  Window wChild;
  DynamicMenu *pmd = NULL;

  /* x_offset returns the x offset of the pointer in the found menu item */
  if (px_offset) *px_offset = 0;

  XQueryPointer( dpy, Scr.Root, &JunkRoot, &wChild,
		&root_x,&root_y, &x, &y, &JunkMask);
  if ((pmd = PmdFromWindow(dpy,wChild)) == NULL) {
    DBUG((DBG,FUNC_NAME,"No window"));
    return NULL;
  }

  DBUG((DBG,FUNC_NAME,"In window %s (%ld)",SzFirstItemFromPmd(pmd),wChild));
  DBUG((DBG,FUNC_NAME,"root = %d,%d",root_x,root_y));

  /* now get position in that child window */
  WXGetPointerOffsets( wChild, &root_x,&root_y, &x, &y);

  DBUG((DBG,FUNC_NAME,"Now root = %d,%d; window = %d, %d",root_x,root_y, x,y));

  /* set the return value for the offsets */
  if (px_offset) *px_offset = x;
  if (py_offset) *py_offset = y;
  
  /* look for the entry that the mouse is in */
  return pmd->pmdv->fnPmiimFromPmdXY(pmd,x,y);
}
#undef FUNC_NAME

#if 0
static
MenuItemInMenu *
PmiimFromPmdShortcutKeypress(DynamicMenu *pmd, char ch)
{
  int ipmiim;
  for (ipmiim = 0; ipmiim < pmd->cmiim; ipmiim++) {
    MenuItemInMenu *pmiim = pmd->rgpmiim[ipmiim];
    if (pmiim->chShortcut == ch) {
      return pmiim;
    }
  }
  return NULL;
}
#endif


static
void
RepaintMenuItem(MenuItemInMenu *pmiim)
{
/*  MenuItem *pmi = pmiim->pmi; */
  DynamicMenu *pmd = pmiim->pmd;
  Window w = pmd->w;
  pmd->pmdv->fnPaintMenuItem(w,pmd,pmiim);
}

MenuItemInMenu *
PmiimSelectedFromPmd(DynamicMenu *pmd)
{
#define FUNC_NAME "PmiimSelectedFromPmd"
  int ipmiimSelected;
  if (!pmd)
    return NULL;

  ipmiimSelected = pmd->ipmiimSelected;
  if (ipmiimSelected < 0)
    return NULL;
  if (ipmiimSelected >= pmd->cmiim) {
    scwm_msg(WARN,FUNC_NAME,"ipmiimSelected = %d > pmd->cmiim = %d",
	     ipmiimSelected, pmd->cmiim);
    return NULL;
  }
  return pmd->rgpmiim[ipmiimSelected];
}
#undef FUNC_NAME

static SCM InvokeUnhoverAction(DynamicMenu *pmd);

static
void
UnselectAndRepaintSelectionForPmd(DynamicMenu *pmd)
{
#define FUNC_NAME "UnselectAndRepaintSelectionForPmd"
  MenuItemInMenu *pmiim = PmiimSelectedFromPmd(pmd);

  if (pmd->fHoverActionInvoked) {
    InvokeUnhoverAction(pmd);
    pmd->fHoverActionInvoked = False;
  }

  if (!pmiim) {
    DBUG((DBG,FUNC_NAME,"pmiimSelected == NULL"));
    return;
  }

  if (pmiim->mis == MIS_Grayed) {
    DBUG((DBG,FUNC_NAME,"pmiimSelected is grayed"));
    return;
  }
  
  if (pmiim->mis != MIS_Selected) {
    scwm_msg(DBG,FUNC_NAME,"pmiim->mis != MIS_Selected");
  }
  pmd->ipmiimSelected = -1;
  pmiim->mis = MIS_Enabled;
  RepaintMenuItem(pmiim);
  if (pmd->pmdNext) {
    PopdownMenu(pmd->pmdNext);
    FreeDynamicMenu(pmd->pmdNext);
    pmd->pmdNext = NULL;
  }
}
#undef FUNC_NAME

static
void
SelectAndRepaintPmiim(MenuItemInMenu *pmiim)
{
#define FUNC_NAME "SelectAndRepaintPmiim"
  DynamicMenu *pmd = pmiim->pmd;
  if (pmiim->mis == MIS_Selected) {
    DBUG((DBG,FUNC_NAME,"Already selected"));
    return;
  }
  pmiim->mis = MIS_Selected;
  pmd->ipmiimSelected = pmiim->ipmiim;
  RepaintMenuItem(pmiim);
}
#undef FUNC_NAME

/* GJB:FIXME:: Need EnterWindowMask? */
static const long menu_event_mask = (ButtonPressMask | ButtonReleaseMask | 
				     ExposureMask | KeyPressMask | KeyReleaseMask |
				     VisibilityChangeMask | ButtonMotionMask |
				     PointerMotionMask );


static
SCM
InvokeUnhoverAction(DynamicMenu *pmd)
{
#define FUNC_NAME "InvokeUnhoverAction"
  MenuItemInMenu *pmiimSelected = PmiimSelectedFromPmd(pmd);
  /* invoke the un-hover action */
  if (pmiimSelected && pmiimSelected->pmi &&
      !UNSET_SCM(pmiimSelected->pmi->scmUnhover)) {
    return scwm_safe_call0_sym(pmiimSelected->pmi->scmUnhover);
  } else {
    DBUG((DBG,FUNC_NAME,"No unhover hook, %ld",pmiimSelected));
  }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


static
void
PopupMenu(DynamicMenu *pmd)
{
/*  DynamicMenu *pmdPoppedFrom = pmd->pmdPrior; */
  Window w = pmd->w;
  pmd->fHoverActionInvoked = False;

  InstallRootColormap();
  XMoveWindow(dpy, w, pmd->x, pmd->y);
  XMapRaised(dpy, w);
}

static
void
PopdownMenu(DynamicMenu *pmd)
{
  if (pmd->pmdNext) {
    PopdownMenu(pmd->pmdNext);
    FreeDynamicMenu(pmd->pmdNext);
    pmd->pmdNext = NULL;
  }
  if (pmd->fHoverActionInvoked) {
    InvokeUnhoverAction(pmd);
    pmd->fHoverActionInvoked = False;
  }
  XUnmapWindow(dpy, pmd->w);
  /* unconnect the window from the dynamic menu */
  XSaveContext(dpy, pmd->w,MenuContext,(caddr_t)NULL);
  UninstallRootColormap();
  XFlush(dpy);
}

static
Bool
FPmdInPmdPriorChain(DynamicMenu *pmdToFind, DynamicMenu *pmd)
{
  while (pmd) {
    if (pmd->pmdPrior == pmdToFind) {
      return True;
    }
    pmd = pmd->pmdPrior;
  }
  return False;
}

enum menu_status { 
  MENUSTATUS_ABORTED, 
  MENUSTATUS_ITEM_SELECTED, 
  MENUSTATUS_POPUP_AND_MOVE,
  MENUSTATUS_NOP,
  MENUSTATUS_NEWITEM,
  MENUSTATUS_NEWITEM_HOTKEY
};

#define CMIIM_CONTROL_KEY_MOVES 5

static
MenuItemInMenu *
PmiimStepItems(MenuItemInMenu *pmiim, int n, int direction)
{
  DynamicMenu *pmd = pmiim->pmd;
  MenuItemInMenu **rgpmiim = pmd->rgpmiim;
  int ipmiimLastEnabled = pmiim->ipmiim;
  int ipmiim = ipmiimLastEnabled;
  
  while (True) {
    if (ipmiim >= pmd->cmiim || ipmiim < 0) 
      break;
    if (!UNSET_SCM(rgpmiim[ipmiim]->pmi->scmAction)) {
      n--;
      ipmiimLastEnabled = ipmiim;
    }
    if (n < 0 || (n == 0 && ipmiimLastEnabled != pmiim->ipmiim))
      break;
    ipmiim+=direction;
  }
  return rgpmiim[ipmiimLastEnabled];
}

static
MenuItemInMenu *
PmiimMenuShortcuts(DynamicMenu *pmd, XEvent *Event, enum menu_status *pmenu_status, 
		   Bool *pfHotkeyUsed, Bool *pfPermitAltReleaseToSelect)
{
  Bool fControlKey = Event->xkey.state & ControlMask? True : False;
  Bool fShiftedKey = Event->xkey.state & ShiftMask? True: False;
  Bool fAltedKey = Event->xkey.state & (Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask)? True: False;
  Bool fNeedControl = False;
  Bool fWrapAround = False;
  KeySym keysym;
  int cch;
  char ch;
  MenuItemInMenu *pmiimSelected = PmiimSelectedFromPmd(pmd);
  MenuItemInMenu *pmiimNewItem = NULL;
  MenuItemInMenu **rgpmiim = pmd->rgpmiim;

  *pfHotkeyUsed = False;
  *pmenu_status = MENUSTATUS_NOP;

  cch = XLookupString(&Event->xkey,&ch,1,&keysym,NULL);

  /* Try to match hot keys */
  if (cch == 1 && isascii(ch) && isgraph(ch) && fControlKey == False) { 
    /* allow any printable character to be a keysym, but be sure control
       isn't pressed */
    int ipmiim = 0;
    ch = tolower(ch);
    /* Search menu for matching hotkey */
    for (; ipmiim < pmd->cmiim; ipmiim ++ ) {
      if (ch == tolower(rgpmiim[ipmiim]->chShortcut)) {
	*pmenu_status = MENUSTATUS_NEWITEM_HOTKEY;
	*pfHotkeyUsed = True;
	return rgpmiim[ipmiim];
      }
    }
  }
  /* Fell through here, so it didn't match a shortcut key */

  fNeedControl = True;
  fWrapAround = False;

#ifdef XK_ISO_Left_Tab
  /* let shift-tab be XK_ISO_Left_Tab
     and below it'll do the same thing as up-arrow */
  if (XK_Tab == keysym && fShiftedKey) {
    keysym = XK_ISO_Left_Tab;
  }
#endif

  switch(keysym)		/* Other special keyboard handling	*/
    {
    case XK_Escape:		/* Escape key pressed. Abort		*/
      *pmenu_status = MENUSTATUS_ABORTED;
      return NULL;
      break;

    case XK_Return:
    case XK_KP_Enter:
    case XK_space:
      *pmenu_status = MENUSTATUS_ITEM_SELECTED;
      return PmiimSelectedFromPmd(pmd);
      break;

    case XK_Left:
      fNeedControl = False;
      /* fall through */
    case XK_b: /* back */
    case XK_h: /* vi left */
      if (fNeedControl && !fControlKey) break;  /* require C-b, C-h */
      pmiimNewItem = pmd->pmdPrior? PmiimSelectedFromPmd(pmd->pmdPrior) : NULL;
      if (pmiimNewItem) {
	*pmenu_status = MENUSTATUS_NEWITEM;
      }
      return pmiimNewItem;
      break;
      
    case XK_Right:
      fNeedControl = False;
      /* fall through */
    case XK_f: /* forward */
    case XK_l: /* vi right */
      if (fNeedControl && !fControlKey) break;  /* require C-f, C-l */
      *pmenu_status = MENUSTATUS_POPUP_AND_MOVE;
      return pmiimSelected;
      break;

      /* GJB:FIXME:: Don't let keyboard movements go to
	 unenabled items */

#ifdef XK_ISO_Left_Tab
    case XK_ISO_Left_Tab:
      fWrapAround = True;
      if (fAltedKey) {
        *pfPermitAltReleaseToSelect = True;
        fShiftedKey = False;
      }
      /* fall through */
#endif
    case XK_Up:
      fNeedControl = False;
      /* fall through */
    case XK_k: /* vi up */
    case XK_p: /* prior */
      if (fNeedControl && !fControlKey) break;  /* require C-k, C-p */
      if (isascii(keysym) && isgraph(keysym))
	  fControlKey = False; /* don't use control modifier 
				  for k or p, since those might
				  be shortcuts too-- C-k, C-p will
				  always work to do a single up */
      if (fShiftedKey) {
	pmiimNewItem = PmiimStepItems(pmd->rgpmiim[0],0,+1);
      } else {
	int cmiimToMove = fControlKey?CMIIM_CONTROL_KEY_MOVES:1;
	if (pmiimSelected == NULL) {
	  pmiimSelected = pmd->rgpmiim[pmd->cmiim-1];
	  cmiimToMove--;
	}
        if (fWrapAround && pmiimSelected->ipmiim == 
            PmiimStepItems(pmd->rgpmiim[0],0,+1)->ipmiim) {
          pmiimNewItem = pmd->rgpmiim[pmd->cmiim-1];
        } else {
          pmiimNewItem = PmiimStepItems(pmiimSelected,cmiimToMove,-1);
        }
      }
      *pmenu_status = MENUSTATUS_NEWITEM;
      return pmiimNewItem;
      break;

    case XK_Tab:
      fWrapAround = True;
      if (fAltedKey)
        *pfPermitAltReleaseToSelect = True;
      /* fall through */
    case XK_Down:
      fNeedControl = False;
      /* fall through */
    case XK_j: /* vi down */
    case XK_n: /* next */
      if (fNeedControl && !fControlKey) break;  /* require C-j, C-n */
      if (isascii(keysym) && isgraph(keysym))
	  fControlKey = False; /* don't use control modifier
				  for j or n, since those might
				  be shortcuts too-- C-j, C-n will
				  always work to do a single down */
      if (fShiftedKey) {
	pmiimNewItem = PmiimStepItems(pmd->rgpmiim[pmd->cmiim-1],0,-1);
      } else {
	int cmiimToMove = fControlKey?CMIIM_CONTROL_KEY_MOVES:1;
	if (pmiimSelected == NULL) {
	  pmiimSelected = pmd->rgpmiim[0];
	  cmiimToMove--;
	}
        if (fWrapAround && pmiimSelected->ipmiim == pmd->cmiim - 1)
          pmiimNewItem = PmiimStepItems(pmd->rgpmiim[0],0,+1);
        else
          pmiimNewItem = PmiimStepItems(pmiimSelected,cmiimToMove,+1);
      }
      *pmenu_status = MENUSTATUS_NEWITEM;
      return pmiimNewItem;
      break;
      
      /* Nothing special --- Allow other shortcuts */
    default:
      break;
    }
  
  return NULL;
}

static
DynamicMenu *
PmdPrepopFromPmiim(MenuItemInMenu *pmiim) 
{
#define FUNC_NAME "PmdPrepopFromPmiim"
  DynamicMenu *pmd;
  DynamicMenu *pmdNew = NULL;
  if (pmiim) {
    Menu *pmenu = DYNAMIC_SAFE_MENU(pmiim->pmi->scmAction);
    if (!pmenu) {
      pmenu = DYNAMIC_SAFE_MENU(pmiim->pmi->scmHover);
    }
    if (!pmenu && pmiim->pmi->fIsForcedSubmenu) {
      SCM scmMenu = scwm_safe_call0(pmiim->pmi->scmAction);
      pmenu = DYNAMIC_SAFE_MENU(scmMenu);
    }
    if (pmenu) {
      pmd = pmiim->pmd;
      pmdNew = NewDynamicMenu(pmenu,pmd);
      if (pmd->pmdNext) {
	scwm_msg(WARN,FUNC_NAME,"pmdNext != NULL! Why?\n");
      }
      pmd->pmdNext = pmdNew;
      pmd->pmdv->fnSetPopupMenuPositionFromMenuItem(pmdNew,pmiim);
      PopupMenu(pmdNew);
    }
  }
  return pmdNew;
}
#undef FUNC_NAME

static
void
XPutBackKeystrokeEvent(Display *dpy, Window w, KeySym keysym)
{
#define FUNC_NAME "XPutBackKeystrokeEvent"
  XKeyEvent ev;
  DBUG((DBG,FUNC_NAME,"entered"));
  ev.type = KeyPress;
  ev.send_event = False;
  ev.display = dpy;
  ev.window = w;
  ev.root = Scr.Root;
  ev.state = 0;
  ev.keycode = XKeysymToKeycode(dpy, keysym);
  ev.same_screen = True;
  XPutBackEvent(dpy,(XEvent *) &ev);
}
#undef FUNC_NAME


static
MenuItem *
MenuInteraction(DynamicMenu *pmd, int warp_to, Bool fPermitAltReleaseToSelect)
{
#define FUNC_NAME "MenuInteraction"
  int c10ms_delays = 0;
  MenuItem *pmiSelected = NULL; /* we return this selected menu item */
  MenuItemInMenu *pmiim = NULL;
  int cpixXoffsetInMenu = 0;
  int cpixYoffsetInMenu = 0;
  Bool fGotMouseMove = False;
  Bool fHotkeyUsed = False;

  /* Warp the pointer to a specific menu item
     (perhaps selected if the keyboard was used to pop up this menu) */
  if (warp_to >= 1)
    pmd->pmdv->fnWarpPointerToPmiim(PmiimStepItems(pmd->rgpmiim[0],0,+warp_to));
  else if (warp_to < 0)
    pmd->pmdv->fnWarpPointerToPmiim(PmiimStepItems(pmd->rgpmiim[pmd->cmiim-1],0,warp_to+1));

  /* Don't assume all menu types pop up with the pointer on the first
     item; pie menus for instance will pop up with nothing selected */
  UnselectAndRepaintSelectionForPmd(pmd);

  /* get the item the pointer is at */
  pmiim = PmiimFromPointerLocation(dpy,&cpixXoffsetInMenu,&cpixYoffsetInMenu);

  Event.type = MotionNotify;
  /* GJB:FIXME:: This is ugly, but this whole crap needs rewriting anyway */
  goto START_MENU_INTERACTION_LOOP;

  while (True) {
    while (XCheckMaskEvent(dpy, menu_event_mask, &Event) == False) {
#ifndef NOT_MORE_RESPONSIVE
      NoEventsScwmUpdate(True);
#endif
      /* check using equality so we only invoke the operation once */
      if (c10ms_delays == pmd->pmenu->cmsPopupDelay/10) {
	MenuItemInMenu *pmiimSelected = PmiimSelectedFromPmd(pmd);
	if (pmd->pmdNext == NULL) {
	  PmdPrepopFromPmiim(pmiimSelected);
	}
      }

      if (c10ms_delays == pmd->pmenu->cmsHoverDelay/10) {
	MenuItemInMenu *pmiimSelected = PmiimSelectedFromPmd(pmd);
	if (pmiimSelected) {
	  SCM scmHover = pmiimSelected->pmi->scmHover;
	  pmd->fHoverActionInvoked = True;
	  /* invoke the hover action */
	  if (DYNAMIC_PROCEDURE_P(scmHover)) {
	    scwm_safe_call0_sym(scmHover);
	  }
	}
      }

      ms_sleep(10);
      ++c10ms_delays;
    }
    if (Event.type == MotionNotify) {
      /* discard any extra motion events before a release */
      while((XCheckMaskEvent(dpy,ButtonMotionMask|ButtonReleaseMask,
			     &Event))&&(Event.type != ButtonRelease));
    }
    
    /* get the item the pointer is at */
    pmiim = PmiimFromPointerLocation(dpy,&cpixXoffsetInMenu,&cpixYoffsetInMenu);

    if (Event.type == MotionNotify) {
      fGotMouseMove = True;
    }

  START_MENU_INTERACTION_LOOP:
    switch(Event.type) {
    case ButtonRelease:
    { /* scope */
      if (pmiim) {
	MenuItemInMenu *pmiimSelected = PmiimSelectedFromPmd(pmd);
	if (pmiim != pmiimSelected) {
	  /* GJB:FIXME:: this spews a lot if you pop up a menu, don't move
	     the mouse, and release. Commenting out for now. */
	  DBUG((DBG,FUNC_NAME,"Pointer not in selected item -- weird!"));
	} else {
          if (pmiim && pmiim->pmi)
            pmiSelected = pmiim->pmi;
	}
      }
      goto MENU_INTERACTION_RETURN;
      break;
    }

    case VisibilityNotify:
    case ButtonPress:
      continue;

    case KeyRelease:
      if (fPermitAltReleaseToSelect) {
        KeySym keysym;
        char ch;
        /* int cch = */ XLookupString(&Event.xkey,&ch,1,&keysym,NULL);
        if (keysym == XK_Meta_L || keysym == XK_Meta_R ||
            keysym == XK_Alt_L || keysym == XK_Alt_R ||
            keysym == XK_Hyper_L || keysym == XK_Hyper_R) {
          if (pmiim && pmiim->pmi)
            pmiSelected = pmiim->pmi;
          goto MENU_INTERACTION_RETURN;
        }
      }
      break;
      
    case KeyPress:
    {
      enum menu_status ms = MENUSTATUS_NOP;
      /* Handle a key press events to allow mouseless operation */
      pmiim = PmiimMenuShortcuts(pmd,&Event,&ms,&fHotkeyUsed,&fPermitAltReleaseToSelect);
      if (ms == MENUSTATUS_ABORTED) {
	goto MENU_INTERACTION_RETURN;
      } else if (ms == MENUSTATUS_ITEM_SELECTED ||
                 (fMenuHotkeysActivateItems && ms == MENUSTATUS_NEWITEM_HOTKEY )) {
        if (pmd->fHoverActionInvoked) {
          InvokeUnhoverAction(pmd);
          pmd->fHoverActionInvoked = False;
        }
	if (pmiim) {
          MenuItemInMenu *pmiimSelected;
          if (fMenuHotkeysActivateItems && (ms == MENUSTATUS_NEWITEM ||
                                            ms == MENUSTATUS_NEWITEM_HOTKEY)) {
            pmd->ipmiimSelected = pmiim->ipmiim;
          }
	  /* GJB:FIXME:: duplicated above */
	  pmiimSelected = PmiimSelectedFromPmd(pmd);
	  if (pmiim != pmiimSelected) {
	    scwm_msg(WARN,FUNC_NAME,"Pointer not in selected item -- weird!");
	  } else {
            if (pmiim && pmiim->pmi)
              pmiSelected = pmiim->pmi;
	    goto MENU_INTERACTION_RETURN;
	  }
	}
      } else if (ms == MENUSTATUS_POPUP_AND_MOVE) {
	if (pmiim && pmiim->pmd) {
	  DynamicMenu *pmdNew = NULL;
	  if (pmiim->pmd->pmdNext == NULL) {
	    pmdNew = PmdPrepopFromPmiim(pmiim);
	  } else {
	    pmdNew = pmiim->pmd->pmdNext;
	  }
	  if (pmdNew) {
	    pmiim = PmiimStepItems(pmdNew->rgpmiim[0],0,+1);
	  } else {
	    /* couldn't prepop, so we're done -- don't change menu item */
	    break;
	  }
	} else {
          if (!pmiim) {
            scwm_msg(WARN,FUNC_NAME,"MENUSTATUS_POPUP_AND_MOVE: pmiim == NULL");
          } else if (!pmiim->pmd) {
            scwm_msg(WARN,FUNC_NAME,"MENUSTATUS_POPUP_AND_MOVE: pmiim->pmd == NULL");
          }
	  break;
	}
      } else if (ms == MENUSTATUS_NOP) {
	break;
      }
      if (pmiim)
        pmiim->pmd->pmdv->fnWarpPointerToPmiim(pmiim);
      /* no break -- fall through to MotionNotify */
    }
      
    case MotionNotify:
      /* BEWARE: fall through case above */
      /* GJB:FIXME:: update selected item, mark mouse_moved boolean if
	 it's moved enough, reset action hook timer, etc. */
    { /* scope */
      if (pmiim == NULL) {
	/* not on an item now */
        DBUG((DBG,FUNC_NAME,"Not on menu item, %d", c10ms_delays));
	/* FIXJTL: maybe something more delicate than just removing this;
	   don't unselect if between two menus of a popup pair, somehow */
	/* FIXJTL: bleah: this is needed for proper pie menus, for
	   moving back into the inactive zone. */
	/* UnselectAndRepaintSelectionForPmd(pmd); */
      } else {
	/* we're on a menu item */
	if (pmiim->pmd != pmd) {
	  /* it's for a different menu than we were on */
	  if (pmiim->pmd == pmd->pmdNext) {
            DBUG((DBG,FUNC_NAME,"Moved into pre-popped menu %s, %d",
                 SzFirstItemFromPmd(pmiim->pmd),c10ms_delays));
	    /* we've moved to the pre-popped menu */
	    pmd = pmd->pmdNext;
	  } else if (FPmdInPmdPriorChain(pmiim->pmd,pmd)) {
	    /* we've moved to a prior menu in the chain */
	    pmd = pmiim->pmd;
	    if (pmiim != PmiimSelectedFromPmd(pmd)) {
	      /* it's not the one that we had selected before,
		 so we need to pop down everything */
	      DBUG((DBG,FUNC_NAME,"Moved back to different item of %s, %d", 
		    SzFirstItemFromPmd(pmiim->pmd),c10ms_delays));
	      /* FIXJTL: this requires scanning for selecte pmiid again */
	      UnselectAndRepaintSelectionForPmd(pmd);
	    } else {
              DBUG((DBG,FUNC_NAME,"Moving back within chain of dynamic menus to %s, unselect %s, %d", 
                   SzFirstItemFromPmd(pmiim->pmd),SzFirstItemFromPmd(pmd->pmdNext),c10ms_delays));
	      UnselectAndRepaintSelectionForPmd(pmd->pmdNext);
	    }
	  } else {
	    /* we're on an unrelated menu */
	    DBUG((DBG,FUNC_NAME,"Moved to unrelated menu %s, %d", 
                 SzFirstItemFromPmd(pmiim->pmd),c10ms_delays));
	    UnselectAndRepaintSelectionForPmd(pmd);
	    pmiim = NULL;
	  }
	} else {
	  /* same menu as we were on */
	  if (pmiim != PmiimSelectedFromPmd(pmd)) {
#ifdef SCWM_DEBUG_MSGS
            MenuItemInMenu *pmiimSelected = PmiimSelectedFromPmd(pmd);
	    /* and it's not the one we've already got selected */
            scwm_msg(DBG,FUNC_NAME,
                     "Moved off old selection from %s onto %s vs. %s, %d", 
                     SzFirstItemFromPmd(pmd), pmiim->pmi->szLabel, 
                     pmiimSelected?pmiimSelected->pmi->szLabel: "NULL",
                     c10ms_delays);
#endif
	    UnselectAndRepaintSelectionForPmd(pmd);
	  } else {
            DBUG((DBG,FUNC_NAME,"Same item, %d", c10ms_delays));
          }
	}
        if (!pmiim)
          continue;
	if (pmiim->mis != MIS_Selected && pmiim->mis != MIS_Grayed) {
	  DBUG((DBG,FUNC_NAME,"New selection, %d", c10ms_delays));
	  SelectAndRepaintPmiim(pmiim);
	  c10ms_delays = 0;
	}
	if (fHotkeyUsed ||
	    pmd->pmdv->fnInPopupZone(pmiim, cpixXoffsetInMenu, cpixYoffsetInMenu)) {
	  /* we're at the right edge of the menu so be sure we popup
	     the cascade menu if any */
	  if (pmd->pmdNext == NULL) {
            DBUG((DBG,FUNC_NAME,"Prepopping, %d", c10ms_delays));
            PmdPrepopFromPmiim(pmiim);
	  }
	}
      }
      break;
    }
      
    case Expose:
    { /* scope */
      DynamicMenu *pmdNeedsPainting = NULL;
      DBUG((DBG,FUNC_NAME,"Got expose event for menu"));
      /* grab our expose events, let the rest go through */
      pmdNeedsPainting = PmdFromWindow(dpy,Event.xany.window);
      if (pmdNeedsPainting) {
	DBUG((DBG,FUNC_NAME,"Trying to paint menu"));
	pmdNeedsPainting->pmdv->fnPaintDynamicMenu(pmdNeedsPainting,&Event);
      }
    }
    continue;
    
    default:
      /* do other event handling */
      DispatchEvent();
      break;
    } /* switch */
    
    /* GJB:FIXME:: Now handle newly selected menu items, whether it is from a keypress or
	 a pointer motion event */
    XFlush(dpy);

    /* GJB:FIXME:: this doesn't work -- we'd like to be able to jump to
       the first item of the next menu if a shortcut key was used to popup a new menu */
    if (fHotkeyUsed)
      XPutBackKeystrokeEvent(dpy,pmiim->pmd->w,XK_Right);
  } /* while true */
 MENU_INTERACTION_RETURN:
  return pmiSelected;
}
#undef FUNC_NAME

static
void
FreeDynamicMenu(DynamicMenu *pmd)
{
  int ipmiim = 0;
  int cmiim = pmd->cmiim;
  for ( ; ipmiim < cmiim; ipmiim++) {
    MenuItemInMenu *pmiim = pmd->rgpmiim[ipmiim];
    pmd->pmdv->fnFreePmidi(pmiim->pmidi);
    /* permit the menu item to be freed again */
    scm_unprotect_object(pmiim->pmi->self);
    FREE(pmiim);
    pmd->rgpmiim[ipmiim] = NULL;
  }
  FREEC(pmd->rgpmiim);
  pmd->pmdv->fnFreePmdi(pmd->pmdi);
  /* now let the menu get gc-d if appropriate */
  scm_unprotect_object(pmd->pmenu->self);

  FREE(pmd);
}  
  
  
static
MenuItemInMenu *
InitializeMenuItemInMenu(SCM item, int ipmiim, DynamicMenu * pmd)
{
  MenuItem * pmi;
  MenuItemInMenu * pmiim;

  /* GJB:FIXME:: strip #f-s in make-menu!
     allow #f-s to be embed and just skip them */
  if (item == SCM_BOOL_F) {
    return NULL;
  }

  pmi = SAFE_MENUITEM(item);
  if (!pmi) {
    return NULL;
  }
  
  pmiim = NEW(MenuItemInMenu);

  /* save some back pointers so we can find a dynamic menu
     just from the menu item */
  pmiim->pmi = pmi;
  pmiim->pmi->self = item;
  scm_protect_object(item); /* be sure the item is not gc-d */
  pmiim->pmd = pmd;
  pmiim->ipmiim = ipmiim;
  pmiim->chShortcut = '\0';
  pmiim->ichShortcutOffset = -1;

  pmiim->fShowPopupArrow = 
    (DYNAMIC_MENU_P(pmiim->pmi->scmAction)) ||
    (DYNAMIC_MENU_P(pmiim->pmi->scmHover)) ||
    pmiim->pmi->fIsForcedSubmenu;
  
  if (pmiim->pmi->scmAction == SCM_BOOL_F)
    pmiim->mis = MIS_Grayed;
  else
    pmiim->mis = MIS_Enabled;	/* GJB:FIXME:: set using hook info? */
  return pmiim;
}

static
void
InitializeDynamicMenu(DynamicMenu *pmd)
{
#define FUNC_NAME "InitializeDynamicMenu"
  Menu *pmenu = pmd->pmenu;
  int cmiim = gh_length(pmenu->scmMenuItems);
  int ipmiim = 0;
  MenuItemInMenu **rgpmiim = pmd->rgpmiim = NEWC(cmiim, MenuItemInMenu *);
  SCM rest = pmd->pmenu->scmMenuItems;

  /* be sure the menu does not get gc-d */
  scm_protect_object(pmenu->self);

  /* Initialize the list of dynamic menu items;
     only the drawing-independent code here */
  while (True) {
    SCM item = gh_car(rest);

    rgpmiim[ipmiim] = InitializeMenuItemInMenu(item, ipmiim, pmd);
    if (rgpmiim[ipmiim]) {
      ipmiim++;
    } else {
      /* permit #f menuitems without giving warning message */
      if (item != SCM_BOOL_F) {
        scwm_msg(WARN,FUNC_NAME,"Bad menu item number %d",ipmiim);
      }
    }
    
    rest = gh_cdr(rest);
    if (SCM_NULLP(rest))
      break;
  }
  /* save the array size in the struct */
  pmd->cmiim = ipmiim;
  pmd->ipmiimSelected = -1;

  pmd->pmiimTitle =
    InitializeMenuItemInMenu(pmd->pmenu->scmMenuTitle, -2 /* FIXJTL: ugly */, pmd);
  
  if (pmd->pmenu->pchUsedShortcutKeys) {
    FREE(pmd->pmenu->pchUsedShortcutKeys); 
  }
  pmd->pmenu->pchUsedShortcutKeys = NewPchKeysUsed(pmd);
}
#undef FUNC_NAME

static
DynamicMenu *
NewDynamicMenu(Menu *pmenu, DynamicMenu *pmdPoppedFrom) 
{
  DynamicMenu *pmd = NEW(DynamicMenu);
  scwm_menulook * pml;
  pmd->pmenu = pmenu;
  pmd->pmdNext = NULL;
  pmd->pmdPrior = pmdPoppedFrom;
  pmd->pmdi = NULL;
  pmd->fPinned = False;

  InitializeDynamicMenu(pmd);	/* add drawing independent fields */

  pml = DYNAMIC_SAFE_MENULOOK(pmenu->scmMenuLook);
  if (pml) {
    pml->mdvt->fnConstructDynamicMenu(pmd);
  } else {
    ConstructDynamicMenu(pmd);
  }
  
  { /* scope */
    /* Get the right events -- don't trust the drawing code to do this */
    XSetWindowAttributes attributes;
    attributes.event_mask = menu_event_mask;
    XChangeWindowAttributes(dpy,pmd->w,CWEventMask, &attributes);
  }
  
  /* Connect the window to the dynamic menu, pmd */
  XSaveContext(dpy,pmd->w,MenuContext,(caddr_t)pmd);

  return pmd;
}

static
void
PopdownAllPriorMenus(DynamicMenu *pmd)
{
  DynamicMenu *pmdPrior = pmd->pmdPrior;
  while (True) {
    pmd = pmdPrior;
    if (!pmd)
      break;
    pmdPrior=pmd->pmdPrior;
    PopdownMenu(pmd);
    FreeDynamicMenu(pmd);
  }
}

/* x,y are the outermost position of the decoration's nearest edge
   corner is 0 for NW, 1 for NE, 2 for SE, 3 for SW  (clockwise) */
static
void
SetPopupMenuPositionFromDecoration(DynamicMenu *pmd, int x, int y, int corner)
{
  switch (corner) {
  case 0: /* NW */
    pmd->x = x;
    pmd->y = y;
    break;
  case 1: /* NE */
    pmd->x = x - pmd->cpixWidth;
    pmd->y = y;
    break;
  case 2: /* SE */
    pmd->x = x - pmd->cpixWidth;
    pmd->y = y - pmd->cpixHeight;
    break;
  case 3: /* SW */
    pmd->x = x;
    pmd->y = y - pmd->cpixHeight;
    break;
  }
  /* Be sure the menu stays on screen */
  if (pmd->y + pmd->cpixHeight > Scr.DisplayHeight) {
    pmd->y = Scr.DisplayHeight-pmd->cpixHeight;
  }
  if (pmd->x + pmd->cpixWidth > Scr.DisplayWidth) {
    pmd->x = Scr.DisplayWidth-pmd->cpixWidth;
  }
  if (pmd->x < 0) pmd->x = 0;
  if (pmd->y < 0) pmd->y = 0;
  return;
}

/* x,y are the outermost position of the decoration's nearest edge
   corner is 0 for NW, 1 for NE, 2 for SE, 3 for SW  (clockwise) */
static 
SCM
PopupGrabMenu(Menu *pmenu, DynamicMenu *pmdPoppedFrom, 
              int warp_to, Bool fPermitAltReleaseToSelect,
              int x, int y, int corner)
{
  DynamicMenu *pmd = NewDynamicMenu(pmenu,pmdPoppedFrom);
  MenuItem *pmi = NULL;
  int cpixX_startpointer;
  int cpixY_startpointer;
  SCM scmAction = SCM_UNDEFINED;

  if (x < 0 || y < 0 || corner < 0) {
    WXGetPointerWindowOffsets(Scr.Root,&cpixX_startpointer,&cpixY_startpointer);
    SetPopupMenuPosition(pmd, cpixX_startpointer, cpixY_startpointer);
  } else {
    SetPopupMenuPositionFromDecoration(pmd, x, y, corner);
  }

  PopupMenu(pmd);
  GrabEm(XCURSOR_MENU);
  pmi = MenuInteraction(pmd, warp_to, fPermitAltReleaseToSelect);
  UngrabEm();
  PopdownMenu(pmd);
  PopdownAllPriorMenus(pmd);
  FreeDynamicMenu(pmd);
  if (!pmi)
    return SCM_BOOL_F;

  scmAction = pmi->scmAction;
  DEREF_IF_SYMBOL(scmAction);
  if (pmi->fIsForcedSubmenu && DYNAMIC_PROCEDURE_P(scmAction)) {
    scmAction = scwm_safe_call0(scmAction);
  }
  if (DYNAMIC_PROCEDURE_P(scmAction)) {
    return scwm_safe_call0(scmAction);
  } else if (DYNAMIC_MENU_P(scmAction)) {
    /* this recurses indirectly back into PopupGrabMenu */
    return popup_menu(scmAction, SCM_BOOL_F,
                      SCM_BOOL_F, SCM_BOOL_F, SCM_BOOL_F, gh_bool2scm(fPermitAltReleaseToSelect));
  }
  return SCM_BOOL_F;
}

SCWM_PROC(set_menu_hotkeys_activate_item_x,"set-menu-hotkeys-activate-item!", 1, 0, 0,
          (SCM activate_p),
"If ACTIVATE? is #t, let menu hotkeys invoke the item.
If #f, a menuitem hotkey just makes that item selected and still requires
a Return or Space keypress to activate the item.")
#define FUNC_NAME s_set_menu_hotkeys_activate_item_x
{
  VALIDATE_ARG_BOOL_COPY_USE_T(1,activate_p,fMenuHotkeysActivateItems);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(menu_hotkeys_activate_item_p,"menu-hotkeys-activate-item?", 0, 0, 0,
          (),
"Return #t if hotkeys invoke item, #f if they just select the item.")
#define FUNC_NAME s_menu_hotkeys_activate_item_p
{
  return SCM_BOOL_FromBool(fMenuHotkeysActivateItems);
}
#undef FUNC_NAME


SCWM_PROC(popup_menu,"popup-menu", 1,5,0,
          (SCM menu, SCM warp_to_index, SCM x_pos, SCM y_pos, SCM left_side_p, SCM permit_alt_release_selection_p),
"Popup MENU, a menu object, and warp to the item WARP-TO-INDEX if it is a number.
X-POS, Y-POS specify a desired position for the menu, and LEFT-SIDE? should be
#t if the menu should be left justified against X-POS, or #f if it should be
right justified against X-POS. If PERMIT-ALT-RELEASE-SELECTION? is #t, then releasing
the Alt/Meta modifier select a menu item.")
#define FUNC_NAME s_popup_menu
{
  int warp_to;
  Bool fLeftSide = True;
  Bool fPermitAltReleaseToSelect;
  int x = -1, y = -1;
  /* permit 'menu to be used, and look up dynamically */
  DEREF_IF_SYMBOL(menu);
  VALIDATE_ARG_MENU(1,menu);
  if (SCM_BOOL_T == warp_to_index) warp_to_index = gh_int2scm(1);
  VALIDATE_ARG_INT_COPY_USE_DEF(2,warp_to_index,warp_to,0);
  VALIDATE_ARG_INT_COPY_USE_DEF(3,x_pos,x,-1);
  VALIDATE_ARG_INT_COPY_USE_DEF(4,y_pos,y,-1);
  VALIDATE_ARG_BOOL_COPY_USE_T(5,left_side_p,fLeftSide);
  VALIDATE_ARG_BOOL_COPY_USE_F(6,permit_alt_release_selection_p,fPermitAltReleaseToSelect);

  return PopupGrabMenu(MENU(menu),NULL,warp_to,fPermitAltReleaseToSelect,
                       x,y, fLeftSide?0:1);
}
#undef FUNC_NAME

MAKE_SMOBFUNS(menu);

void
init_menu()
{
  REGISTER_SCWMSMOBFUNS(menu);

#ifndef SCM_MAGIC_SNARFER
# include "menu.x"
#endif
}

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

