/* $Id$
 * scwmmenu.c
 * By Greg J. Badros -- Nov. 14, 1997
 * (C) 1998, 1997 Greg J. Badros and Maciej Stachowiak
 */


#define MENU_IMPLEMENTATION

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <X11/keysym.h>
#include <sys/types.h>
#include <sys/time.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include <guile/gh.h>
#include "menu.h"
#include "scwm.h"
#include "font.h"
#include "events.h"
#include "drawmenu.h"
#include "colormaps.h"
#include "xmisc.h"
#include "screen.h"
#include "color.h"
#include "util.h"
#include "string_token.h"
#include "guile-compat.h"
#include "syscompat.h"
#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif

static DynamicMenu *NewDynamicMenu(Menu *pmenu, DynamicMenu *pmdPoppedFrom);
static void PopdownMenu(DynamicMenu *pmd);
static void FreeDynamicMenu(DynamicMenu *pmd);

#ifndef NDEBUG
/* Give string name of first item in a dynamic menu; mostly used for debugging */
static const char *
SzFirstItemFromPmd(const DynamicMenu *pmd)
{
  SCM scmFirstMenuItem = gh_car(pmd->pmenu->scmMenuItems);
  MenuItem *pmi = SAFE_MENUITEM(scmFirstMenuItem);
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
  SCM_SETGC8MARK(obj);

  GC_MARK_SCM_IF_SET(pmenu->scmMenuItems);
  GC_MARK_SCM_IF_SET(pmenu->scmImgSide);
  GC_MARK_SCM_IF_SET(pmenu->scmSideBGColor);
  GC_MARK_SCM_IF_SET(pmenu->scmBGColor);
  GC_MARK_SCM_IF_SET(pmenu->scmTextColor);
  GC_MARK_SCM_IF_SET(pmenu->scmImgBackground);
  GC_MARK_SCM_IF_SET(pmenu->scmFont);
  GC_MARK_SCM_IF_SET(pmenu->scmExtraOptions);

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
print_menu(SCM obj, SCM port, scm_print_state * pstate)
{
  scm_puts("#<menu ", port);
  if (MENU_P(obj)) {
    Menu *pmenu = MENU(obj);
    scm_write(gh_car(pmenu->scmMenuItems), port);
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
          (SCM obj))
/** Return #t if and only if OBJ is a menu object. */
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
	scwm_msg(WARN,__FUNCTION__,"Bad menu item %d",ipmiim);
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

SCWM_PROC(menu_properties, "menu-properties", 1, 0, 0,
          (SCM menu))
/** Returns the a list of the menu properties of MENU, a menu object.
The properties returned are: 
'(menu-items side-image side-bg-color bg-color text-color image-bg
font extra-options used-shortcut-keys) */
#define FUNC_NAME s_menu_properties
{
  Menu *pmenu = SAFE_MENU(menu);
  if (!pmenu) {
    scm_wrong_type_arg(FUNC_NAME,1,menu);
  }
  return gh_list(pmenu->scmMenuItems,
		 pmenu->scmImgSide,
		 pmenu->scmSideBGColor,
		 pmenu->scmBGColor,
		 pmenu->scmTextColor,
		 pmenu->scmImgBackground,
		 pmenu->scmFont,
		 pmenu->scmExtraOptions,
		 gh_str02scm(pmenu->pchUsedShortcutKeys),
                 SCM_UNDEFINED);
}
#undef FUNC_NAME


SCWM_PROC(make_menu, "make-menu", 1, 7, 0,
          (SCM list_of_menuitems,
           SCM picture_side, SCM side_bg_color,
           SCM bg_color, SCM text_color,
           SCM picture_bg, SCM font, SCM extra_options))
/** Make and return a menu object from the given arguments.
LIST-OF-MENUITEMS is a scheme list of menu items -- see `make-menuitem';
PICTURE-SIDE is an image object;
SIDE-BG-COLOR, BG-COLOR, TEXT-COLOR, PICTURE-BG are color objects;
FONT is a font object;
EXTRA-OPTIONS can be anything understood by the menu-specific
drawing code (not used currently).
*/
#define FUNC_NAME s_make_menu
{
  Menu *pmenu = NEW(Menu);
  SCM answer;
  int iarg = 1;

  if (!gh_list_p(list_of_menuitems)) {
    scm_wrong_type_arg(FUNC_NAME,iarg,list_of_menuitems);
  }
  pmenu->scmMenuItems = list_of_menuitems;

  iarg++;
  if (UNSET_SCM(picture_side)) {
    picture_side = SCM_BOOL_F;
  } else if (!IMAGE_P(picture_side)) {
    scm_wrong_type_arg(FUNC_NAME,iarg,picture_side);
  } 
  pmenu->scmImgSide = picture_side;

  iarg++;
  if (UNSET_SCM(side_bg_color)) {
    side_bg_color = WHITE_COLOR;
  } else if (!COLOR_OR_SYMBOL_P(side_bg_color)) {
    scm_wrong_type_arg(FUNC_NAME,iarg,side_bg_color);
  }
  pmenu->scmSideBGColor = side_bg_color;

  iarg++;
  if (UNSET_SCM(bg_color)) {
    bg_color = Scr.MenuColors.bg; 
  } else if (!COLOR_OR_SYMBOL_P(bg_color)) {
    scm_wrong_type_arg(FUNC_NAME,iarg,bg_color);
  }
  pmenu->scmBGColor = bg_color;

  iarg++;
  if (UNSET_SCM(text_color)) {
    text_color =  Scr.MenuColors.fg;
  } else if (!COLOR_OR_SYMBOL_P(text_color)) {
    scm_wrong_type_arg(FUNC_NAME,iarg,text_color);
  }
  pmenu->scmTextColor = text_color;

  iarg++;
  if (UNSET_SCM(picture_bg)) {
    picture_bg = SCM_BOOL_F;
  } else if (!IMAGE_P(picture_bg)) {
    scm_wrong_type_arg(FUNC_NAME,iarg,picture_bg);
  } 
  pmenu->scmImgBackground = picture_bg;

  iarg++;
  /* FIXGJB: order dependency on menu_font being set before making
     the menu -- is there a better default -- maybe we should just
     always have some font object for "fixed" */
  if (UNSET_SCM(font) && Scr.menu_font != SCM_UNDEFINED) {
    pmenu->scmFont = Scr.menu_font;
  } else if (!FONT_OR_SYMBOL_P(font)) {
    scm_wrong_type_arg(FUNC_NAME,iarg,font);
  }
  pmenu->scmFont = font;

  pmenu->scmExtraOptions = extra_options;

  pmenu->pchUsedShortcutKeys = NULL;

#ifdef FIXGJB_SHOULD_WE_TEST_ITEMS_HERE_OR_DEFER_TO_LATER
  rest = pmenu->scmMenuItems;

  while (True) {
    item = gh_car(rest);
    pmi = SAFE_MENUITEM(item);
    if (!pmi) {
      scwm_msg(WARN,FUNC_NAME,"Bad menu item %d",ipmiim);
    }
    if (pmi && pmi->pchHotkeyPreferences) {
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
    rest = gh_cdr(rest);
    if (SCM_NULLP(rest))
      break;
  }
#endif

  SCM_NEWCELL(answer);
  SCM_SETCAR(answer, scm_tc16_scwm_menu);
  SCM_SETCDR(answer, (SCM) pmenu);
  return answer;
}
#undef FUNC_NAME


/* return the appropriate x offset from the prior menu to
   use as the location of a popup menu */
/* FIXGJB: this should be a callback function, perhaps */
static
int 
PopupPositionOffset(DynamicMenu *pmd)
{
  return pmd->pmdi->cpixWidth - 5;
}

/*
 * GetPreferredPopupPosition
 * Given x,y and the menu to popup, return the coords
 * that should be used for the upper left
 */
/* FIXGJB: this should be a callback function, perhaps */
static
void 
GetPreferredPopupPosition(DynamicMenu *pmd,
			  DynamicMenu *pmdPoppedFrom, int x, int y, 
			  int *pxReturn, int *pyReturn)
{
  if (pmdPoppedFrom) {
    *pxReturn = pmdPoppedFrom->pmdi->x + PopupPositionOffset(pmdPoppedFrom);
    *pyReturn = y;
  } else {
    *pxReturn = x - pmd->pmdi->cpixWidth/2;
    *pyReturn = y - pmd->rgpmiim[0]->cpixItemHeight/2;
  }
  if (*pyReturn + pmd->pmdi->cpixHeight > Scr.DisplayHeight) {
    *pyReturn = Scr.DisplayHeight-pmd->pmdi->cpixHeight;
  }
  if (*pxReturn + pmd->pmdi->cpixWidth > Scr.DisplayWidth) {
    *pxReturn = Scr.DisplayWidth-pmd->pmdi->cpixWidth;
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
  pmd->pmdi->x = x;
  pmd->pmdi->y = y;
}

/* FIXGJB: this could be a callback */
static
void
SetPopupMenuPositionFromMenuItem(DynamicMenu *pmd, 
				 MenuItemInMenu *pmiimSelected)
{
  MenuDrawingInfo *pmdi = pmiimSelected->pmd->pmdi;
  int cpixXmenu = pmdi->x;
  int cpixYmenu = pmdi->y;
  int cpixWidthMenu = pmdi->cpixWidth;
  MenuDrawingInfo *pmdiNew = pmd->pmdi;
  int cpixWidthNewMenu = pmdiNew->cpixWidth;

  if (cpixXmenu + cpixWidthMenu + pmdiNew->cpixWidth <= Scr.DisplayWidth) {
    pmd->pmdi->x = cpixXmenu + cpixWidthMenu - 2;
  } else {
    /* pop to the left */
    pmd->pmdi->x = cpixXmenu - cpixWidthNewMenu + pmdi->cpixSideImage;
  }
  pmd->pmdi->y = cpixYmenu + pmiimSelected->cpixOffsetY - 2;
  if (pmd->pmdi->y + pmdiNew->cpixHeight > Scr.DisplayHeight) {
    /* would go off the bottom edge of the screen;
       force it up from the bottom of the screen */
    pmd->pmdi->y = Scr.DisplayHeight-pmdiNew->cpixHeight;
  }
}


static
DynamicMenu *
PmdFromWindow(Display *dpy, Window w)
{
  DynamicMenu *pmd = NULL;

  if (w == None)
    return NULL;

  if ((XFindContext(dpy, w, MenuContext,
		    (caddr_t *)&pmd) == XCNOENT)) {
    DBUG(__FUNCTION__,"XFindContext gave XCNOENT");
    pmd = NULL;
  }
  return pmd;
}

/* FIXGJB this may need to be dynamically loadable for more
   flexible menu types (like pie menus) */
static
MenuItemInMenu *
PmiimFromPmdXY(DynamicMenu *pmd, int x, int y)
{
  int ipmiim;
  for (ipmiim = 0; ipmiim < pmd->cmiim; ipmiim++) {
    MenuItemInMenu *pmiim = pmd->rgpmiim[ipmiim];
    int item_y_offset = pmiim->cpixOffsetY;
    if (y > item_y_offset && y <= item_y_offset + pmiim->cpixItemHeight) {
      DBUG(__FUNCTION__,"pmiim->pmi->szLabel = %s @ %d,%d", pmiim->pmi->szLabel,x,y);
      return pmiim;
    }
  }
  return NULL;
}

#ifdef FIXGJB_UNUSED
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
 * Return a pointer to the MenuItemInMenu, and *px_offset,
 * the x offset within the item -- pass px_offset = NULL
 * to ignore that return value
 * Returns NULL if pointer not pointing at a menu item
 */
static
MenuItemInMenu *
PmiimFromPointerLocation(Display *dpy, int *px_offset)
{
  int root_x, root_y;
  int x,y;
  Window wChild;
  DynamicMenu *pmd = NULL;

  /* x_offset returns the x offset of the pointer in the found menu item */
  if (px_offset) *px_offset = 0;

  XQueryPointer( dpy, Scr.Root, &JunkRoot, &wChild,
		&root_x,&root_y, &x, &y, &JunkMask);
  if ((pmd = PmdFromWindow(dpy,wChild)) == NULL) {
    DBUG(__FUNCTION__,"No window");
    return NULL;
  }

  DBUG(__FUNCTION__,"In window %s (%ld)",SzFirstItemFromPmd(pmd),wChild);
  DBUG(__FUNCTION__,"root = %d,%d",root_x,root_y);

  /* now get position in that child window */
  WXGetPointerOffsets( wChild, &root_x,&root_y, &x, &y);

  DBUG(__FUNCTION__,"Now root = %d,%d; window = %d, %d (%d)",root_x,root_y, x,y, (int) f);

  /* set the return value for the x_offset */
  if (px_offset) *px_offset = x;

  /* look for the entry that the mouse is in */
  return PmiimFromPmdXY(pmd,x,y);
}

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


static
void
RepaintMenuItem(MenuItemInMenu *pmiim)
{
/*  MenuItem *pmi = pmiim->pmi; */
  DynamicMenu *pmd = pmiim->pmd;
  Window w = pmd->pmdi->w;
  pmd->fnPaintMenuItem(w,pmd,pmiim);
}

MenuItemInMenu *
PmiimSelectedFromPmd(DynamicMenu *pmd)
{
  int ipmiimSelected = pmd->ipmiimSelected;
  if (ipmiimSelected < 0)
    return NULL;
  if (ipmiimSelected >= pmd->cmiim) {
    scwm_msg(WARN,__FUNCTION__,"ipmiimSelected = %d > pmd->cmiim = %d",
	     ipmiimSelected, pmd->cmiim);
    return NULL;
  }
  return pmd->rgpmiim[ipmiimSelected];
}

static
void
UnselectAndRepaintSelectionForPmd(DynamicMenu *pmd)
{
  MenuItemInMenu *pmiim = PmiimSelectedFromPmd(pmd);
  if (!pmiim) {
    DBUG(__FUNCTION__,"pmiimSelected == NULL");
    return;
  }

  if (pmiim->mis != MIS_Selected) {
    scwm_msg(DBG,__FUNCTION__,"pmiim->mis != MIS_Selected");
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

static
void
SelectAndRepaintPmiim(MenuItemInMenu *pmiim)
{
  DynamicMenu *pmd = pmiim->pmd;
  if (pmiim->mis == MIS_Selected) {
    DBUG(__FUNCTION__,"Already selected");
    return;
  }
  pmiim->mis = MIS_Selected;
  pmd->ipmiimSelected = pmiim->ipmiim;
  RepaintMenuItem(pmiim);
}

/* FIXGJB : Need EnterWindowMask? */
static const long menu_event_mask = (ButtonPressMask | ButtonReleaseMask | 
				     ExposureMask | KeyPressMask | 
				     VisibilityChangeMask | ButtonMotionMask |
				     PointerMotionMask );

static int HOVER_DELAY_MS = 500;  /* FIXGJB: make configurable */
static int MENU_POPUP_DELAY_MS = 900;  /* FIXGJB: make configurable */

static
SCM
InvokeUnhoverAction(DynamicMenu *pmd)
{
  MenuItemInMenu *pmiimSelected = PmiimSelectedFromPmd(pmd);
  /* invoke the un-hover action */
  if (pmiimSelected && !UNSET_SCM(pmiimSelected->pmi->scmUnhover)) {
    return call_thunk_with_message_handler(pmiimSelected->pmi->scmUnhover);
  } else {
    DBUG(__FUNCTION__,"No unhover hook, %ld",pmiimSelected);
  }
  return SCM_UNDEFINED;
}


static
void
PopupMenu(DynamicMenu *pmd)
{
  MenuDrawingInfo *pmdi = pmd->pmdi;
/*  DynamicMenu *pmdPoppedFrom = pmd->pmdPrior; */
  Window w = pmdi->w;
  pmd->fHoverActionInvoked = False;

  InstallRootColormap();
  XMoveWindow(dpy, w, pmdi->x, pmdi->y);
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
  InvokeUnhoverAction(pmd);
  pmd->fHoverActionInvoked = False;
  XUnmapWindow(dpy, pmd->pmdi->w);
  /* unconnect the window from the dynamic menu */
  XSaveContext(dpy, pmd->pmdi->w,MenuContext,(caddr_t)NULL);
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
  MENUSTATUS_NEWITEM
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
		   Bool *pfHotkeyUsed)
{
  int fControlKey = Event->xkey.state & ControlMask? True : False;
  int fShiftedKey = Event->xkey.state & ShiftMask? True: False;
  KeySym keysym = XLookupKeysym(&Event->xkey,0);
  MenuItemInMenu *pmiimSelected = PmiimSelectedFromPmd(pmd);
  MenuItemInMenu *pmiimNewItem = NULL;
  MenuItemInMenu **rgpmiim = pmd->rgpmiim;

  *pfHotkeyUsed = False;
  *pmenu_status = MENUSTATUS_NOP;
  /* Is it okay to treat keysym-s as Ascii? */

  /* Try to match hot keys */
  if (isascii(keysym) && isgraph(keysym) && fControlKey == False) { 
    /* allow any printable character to be a keysym, but be sure control
       isn't pressed */
    int ipmiim = 0;
    keysym = tolower(keysym);
    /* Search menu for matching hotkey */
    for (; ipmiim < pmd->cmiim; ipmiim ++ ) {
      if (keysym == tolower(rgpmiim[ipmiim]->chShortcut)) {
	*pmenu_status = MENUSTATUS_NEWITEM;
	*pfHotkeyUsed = True;
	return rgpmiim[ipmiim];
      }
    }
  }
  /* Fell through here, so it didn't match a shortcut key */

  switch(keysym)		/* Other special keyboard handling	*/
    {
    case XK_Escape:		/* Escape key pressed. Abort		*/
      *pmenu_status = MENUSTATUS_ABORTED;
      return NULL;
      break;

    case XK_Return:
      *pmenu_status = MENUSTATUS_ITEM_SELECTED;
      return PmiimSelectedFromPmd(pmd);
      break;

    case XK_Left:
    case XK_b: /* back */
    case XK_h: /* vi left */
      pmiimNewItem = pmd->pmdPrior? PmiimSelectedFromPmd(pmd->pmdPrior) : NULL;
      if (pmiimNewItem) {
	*pmenu_status = MENUSTATUS_NEWITEM;
      }
      return pmiimNewItem;
      break;
      
    case XK_Right:
    case XK_f: /* forward */
    case XK_l: /* vi right */
      *pmenu_status = MENUSTATUS_POPUP_AND_MOVE;
      return pmiimSelected;
      break;

      /* FIXGJB: Don't let keyboard movements go to
	 unenabled items */
      
    case XK_Up:
    case XK_k: /* vi up */
    case XK_p: /* prior */
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
	pmiimNewItem = PmiimStepItems(pmiimSelected,cmiimToMove,-1);
      }
      *pmenu_status = MENUSTATUS_NEWITEM;
      return pmiimNewItem;
      break;

    case XK_Down:
    case XK_j: /* vi down */
    case XK_n: /* next */
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
  DynamicMenu *pmd;
  DynamicMenu *pmdNew = NULL;
  if (pmiim) {
    SCM scmAction = pmiim->pmi->scmAction;
    Menu *pmenu = DYNAMIC_SAFE_MENU(scmAction);
    if (pmenu) {
      pmd = pmiim->pmd;
      pmdNew = NewDynamicMenu(pmenu,pmd);
      if (pmd->pmdNext) {
	scwm_msg(WARN,__FUNCTION__,"pmdNext != NULL! Why?\n");
      }
      pmd->pmdNext = pmdNew;
      SetPopupMenuPositionFromMenuItem(pmdNew,pmiim);
      PopupMenu(pmdNew);
    }
  }
  return pmdNew;
}

static
void
XPutBackKeystrokeEvent(Display *dpy, Window w, KeySym keysym)
{
  XKeyEvent ev;
  DBUG(__FUNCTION__,"entered");
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
  x = 2*(pmdi->cpixWidth - pmdi->cpixItemOffset)/3;
  y = pmiim->cpixOffsetY + pmiim->cpixItemHeight/2;
  XWarpPointer(dpy, 0, pmdi->w, 0, 0, 0, 0, x, y);
}

static
SCM
MenuInteraction(DynamicMenu *pmd, Bool fWarpToFirst)
{
  int c10ms_delays = 0;
  SCM scmAction = SCM_UNDEFINED;
  MenuItemInMenu *pmiim = NULL;
  int cpixXoffsetInMenu = 0;
  Bool fGotMouseMove = False;
  Bool fHotkeyUsed = False;

  /* Warp the pointer to the first menu item
     (perhaps selected if the keyboard was used to pop up this menu) */
  if (fWarpToFirst)
    WarpPointerToPmiim(PmiimStepItems(pmd->rgpmiim[0],0,+1));

  /* FIXGJB: need to make initial item selection */
  while (True) {
    while (XCheckMaskEvent(dpy, menu_event_mask, &Event) == False) {
      usleep(10);

      if (c10ms_delays++ == MENU_POPUP_DELAY_MS/10) {
	MenuItemInMenu *pmiimSelected = PmiimSelectedFromPmd(pmd);
	if (pmd->pmdNext == NULL) {
	  PmdPrepopFromPmiim(pmiimSelected);
	}
      }

      if (c10ms_delays++ == HOVER_DELAY_MS/10 ) {
	MenuItemInMenu *pmiimSelected = PmiimSelectedFromPmd(pmd);
	if (pmiimSelected) {
	  SCM scmHover = pmiimSelected->pmi->scmHover;
	  pmd->fHoverActionInvoked = True;
	  /* invoke the hover action */
	  if (DYNAMIC_PROCEDURE_P(scmHover)) {
	    call_thunk_with_message_handler(scmHover);
	  }
	}
	/* block until there is an interesting event */
	XMaskEvent(dpy, menu_event_mask, &Event);
	break; /* skip out of the while loop since we just blocked 
		  for an event and got one */
      }
    }
    if (Event.type == MotionNotify) {
      /* discard any extra motion events before a release */
      while((XCheckMaskEvent(dpy,ButtonMotionMask|ButtonReleaseMask,
			     &Event))&&(Event.type != ButtonRelease));
    }
    
    /* get the item the pointer is at */
    pmiim = PmiimFromPointerLocation(dpy,&cpixXoffsetInMenu);

    if (Event.type == MotionNotify) {
      fGotMouseMove = True;
    }

    switch(Event.type) {
    case ButtonRelease:
    { /* scope */
      if (pmiim) {
	MenuItemInMenu *pmiimSelected = PmiimSelectedFromPmd(pmd);
	if (pmiim != pmiimSelected) {
	  /* FIXGJB: this spews a lot if you pop up a menu, don't move
	     the mouse, and release. Commenting out for now. */
	  DBUG(__FUNCTION__,"Pointer not in selected item -- weird!");
	} else {
	  scmAction = pmiim->pmi->scmAction;
	}
      }
      goto MENU_INTERACTION_RETURN;
      break;
    }

    case VisibilityNotify:
    case ButtonPress:
      continue;
      
    case KeyPress:
    {
      enum menu_status ms = MENUSTATUS_NOP;
      /* Handle a key press events to allow mouseless operation */
      pmiim = PmiimMenuShortcuts(pmd,&Event,&ms,&fHotkeyUsed);
      if (ms == MENUSTATUS_ABORTED) {
	goto MENU_INTERACTION_RETURN;
      } else if (ms == MENUSTATUS_ITEM_SELECTED) {
	if (pmiim) {
	  /* FIXGJB: duplicated above */
	  MenuItemInMenu *pmiimSelected = PmiimSelectedFromPmd(pmd);
	  if (pmiim != pmiimSelected) {
	    scwm_msg(WARN,__FUNCTION__,"Pointer not in selected item -- wierd!");
	  } else {
	    scmAction = pmiim->pmi->scmAction;
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
	  scwm_msg(WARN,__FUNCTION__,"pmiim or pmiim->pmd == NULL");
	  break;
	}
      } else if (ms == MENUSTATUS_NOP) {
	break;
      }
      WarpPointerToPmiim(pmiim);
      /* no break -- fall through to MotionNotify */
    }
      
    case MotionNotify:
      /* BEWARE: fall through case above */
      /* FIXGJB: update selected item, mark mouse_moved boolean if
	 it's moved enough, reset action hook timer, etc. */
    { /* scope */
      if (pmiim == NULL) {
	/* not on an item now */
        DBUG(__FUNCTION__,"Not on menu item, %d", c10ms_delays);
	if (pmd->fHoverActionInvoked) {
	  InvokeUnhoverAction(pmd);
	  pmd->fHoverActionInvoked = False;
	}
	UnselectAndRepaintSelectionForPmd(pmd);
      } else {
	/* we're on a menu item */
	if (pmiim->pmd != pmd) {
	  /* it's for a different menu than we were on */
	  if (pmiim->pmd == pmd->pmdNext) {
            DBUG(__FUNCTION__,"Moved into pre-popped menu %s, %d",
                 SzFirstItemFromPmd(pmiim->pmd),c10ms_delays);
	    /* we've moved to the pre-popped menu */
	    pmd = pmd->pmdNext;
	  } else if (FPmdInPmdPriorChain(pmiim->pmd,pmd)) {
	    /* we've moved to a prior menu in the chain */
	    pmd = pmiim->pmd;
	    if (pmiim != PmiimSelectedFromPmd(pmd)) {
	      /* it's not the one that we had selected before,
		 so we need to pop down everything */
	      DBUG(__FUNCTION__,"Moved back to different item of %s, %d", 
                   SzFirstItemFromPmd(pmiim->pmd),c10ms_delays);
	      UnselectAndRepaintSelectionForPmd(pmd);
	    } else {
              DBUG(__FUNCTION__,"Moving back within chain of dynamic menus to %s, unselect %s, %d", 
                   SzFirstItemFromPmd(pmiim->pmd),SzFirstItemFromPmd(pmd->pmdNext),c10ms_delays);
	      UnselectAndRepaintSelectionForPmd(pmd->pmdNext);
	    }
	  } else {
	    /* we're on an unrelated menu */
	    DBUG(__FUNCTION__,"Moved to unrelated menu %s, %d", 
                 SzFirstItemFromPmd(pmiim->pmd),c10ms_delays);
	    UnselectAndRepaintSelectionForPmd(pmd);
	    pmiim = NULL;
	  }
	} else {
	  /* same menu as we were on */
	  if (pmiim != PmiimSelectedFromPmd(pmd)) {
            MenuItemInMenu *pmiimSelected = PmiimSelectedFromPmd(pmd);
	    /* and it's not the one we've already got selected */
            DBUG(__FUNCTION__,"Moved off old selection from %s onto %s vs. %s, %d", 
                 SzFirstItemFromPmd(pmd), pmiim->pmi->szLabel, pmiimSelected?pmiimSelected->pmi->szLabel: "NULL",
                 c10ms_delays);
	    UnselectAndRepaintSelectionForPmd(pmd);
	  } else {
            DBUG(__FUNCTION__,"Same item, %d", c10ms_delays);
          }
	}
	if (pmiim && pmiim->mis != MIS_Selected) {
	  DBUG(__FUNCTION__,"New selection, %d", c10ms_delays);
	  SelectAndRepaintPmiim(pmiim);
	  c10ms_delays = 0;
	}
	if (cpixXoffsetInMenu > pmd->pmdi->cpixWidth*3/4 || fHotkeyUsed) {
	  /* we're at the right edge of the menu so be sure we popup
	     the cascade menu if any */
	  if (pmd->pmdNext == NULL) {
            DBUG(__FUNCTION__,"Prepopping, %d", c10ms_delays);
            PmdPrepopFromPmiim(pmiim);
	  }
	}
      }
      break;
    }
      
    case Expose:
    { /* scope */
      DynamicMenu *pmdNeedsPainting = NULL;
      DBUG(__FUNCTION__,"Got expose event for menu");
      /* grab our expose events, let the rest go through */
      pmdNeedsPainting = PmdFromWindow(dpy,Event.xany.window);
      if (pmdNeedsPainting) {
	DBUG(__FUNCTION__,"Trying to paint menu");
	pmdNeedsPainting->fnPaintDynamicMenu(pmdNeedsPainting,&Event);
      }
    }
    continue;
    
    default:
      /* FIXGJB: do other event handling */
      DispatchEvent();
      break;
    } /* switch */
    
    /* FIXGJB: Now handle newly selected menu items, whether it is from a keypress or
	 a pointer motion event */
    XFlush(dpy);

    /* FIXGJB this doesn't work -- we'd like to be able to jump to
       the first item of the next menu if a shortcut key was used to popup a new menu */
#if 0    
    if (fHotkeyUsed)
      XPutBackKeystrokeEvent(dpy,pmiim->pmd->pmdi->w,XK_Right);
#endif    
  } /* while true */
 MENU_INTERACTION_RETURN:
  return scmAction;
}

static
void
FreeDynamicMenu(DynamicMenu *pmd)
{
  int ipmiim = 0;
  int cmiim = pmd->cmiim;
  for ( ; ipmiim < cmiim; ipmiim++) {
    FREE(pmd->rgpmiim[ipmiim]);
  }
  FREEC(pmd->rgpmiim);
  FREE(pmd->pmdi);
}  
  
  

static
void
InitializeDynamicMenu(DynamicMenu *pmd)
{
  Menu *pmenu = pmd->pmenu;
  int cmiim = gh_length(pmenu->scmMenuItems);
  int ipmiim = 0;
  MenuItemInMenu **rgpmiim = pmd->rgpmiim = NEWC(cmiim, MenuItemInMenu *);
  SCM rest = pmd->pmenu->scmMenuItems;

  /* Initialize the list of dynamic menu items;
     only the drawing-independent code here */
  while (True) {
    SCM item = gh_car(rest);
    MenuItem *pmi;
    MenuItemInMenu *pmiim;

    /* FIXGJB: strip #f-s in make-menu!
       allow #f-s to be embed and just skip them */
    if (item == SCM_BOOL_F) {
      goto NEXT_MENU_ITEM;
    }
    pmi = SAFE_MENUITEM(item);
    if (!pmi) {
      scwm_msg(WARN,__FUNCTION__,"Bad menu item number %d",ipmiim);
      goto NEXT_MENU_ITEM;
    }
    pmiim = NEW(MenuItemInMenu);
    rgpmiim[ipmiim] = pmiim;

    /* save some back pointers so we can find a dynamic menu
       just from the menu item */
    pmiim->pmi = pmi;
    pmiim->pmd = pmd;
    pmiim->ipmiim = ipmiim;
    pmiim->chShortcut = '\0';
    pmiim->ichShortcutOffset = -1;

    pmiim->cpixItemHeight = -1;	/* just init: gets set in drawing code */
    pmiim->cpixOffsetY = -1;	/* just init: gets set in drawing code */
    pmiim->fOnTopEdge = False;	/* just init: gets set in drawing code */
    pmiim->fOnBottomEdge = False; /* just init: gets set in drawing code */

    pmiim->fShowPopupArrow = (DYNAMIC_MENU_P(pmiim->pmi->scmAction));

    pmiim->mis = MIS_Enabled;	/* FIXGJB: set using hook info? */
    ipmiim++;
  NEXT_MENU_ITEM:
    rest = gh_cdr(rest);
    if (SCM_NULLP(rest))
      break;
  }
  /* save the array size in the struct */
  pmd->cmiim = ipmiim;
  pmd->ipmiimSelected = -1;

  /*
  if (!pmd->pmenu->pchUsedShortcutKeys) {
  */
  /* we choose to not use this optimization for now */
  FREE(pmd->pmenu->pchUsedShortcutKeys); 
  pmd->pmenu->pchUsedShortcutKeys = NewPchKeysUsed(pmd);
}

PfnConstructDynamicMenu fnConstructDynamicMenuCurrent;
static SCM *pscm_construct_menu_primitive;

static
DynamicMenu *
NewDynamicMenu(Menu *pmenu, DynamicMenu *pmdPoppedFrom) 
{
  DynamicMenu *pmd = NEW(DynamicMenu);
  pmd->pmenu = pmenu;
  pmd->pmdNext = NULL;
  pmd->pmdPrior = pmdPoppedFrom;
  pmd->pmdi = NULL;
  pmd->fPinned = False;

  InitializeDynamicMenu(pmd);	/* add drawing independent fields */
  /* FIXGJB: this ConstructDynamicMenu needs to possibly call a different
     ConstructDynamicMenu == should be through a function pointer */

  if (*pscm_construct_menu_primitive != SCM_BOOL_F)
    {
    fnConstructDynamicMenuCurrent = (PfnConstructDynamicMenu) 
      scm_num2ulong (*pscm_construct_menu_primitive, (char *)SCM_ARG1, "NewDynamicMenu" );
    }
  else
    {
    fnConstructDynamicMenuCurrent = ConstructDynamicMenu;
    }

  fnConstructDynamicMenuCurrent(pmd);	/* update/create pmd->pmdi */

  { /* scope */
    /* Get the right events -- don't trust the drawing code to do this */
    XSetWindowAttributes attributes;
    attributes.event_mask = menu_event_mask;
    XChangeWindowAttributes(dpy,pmd->pmdi->w,CWEventMask, &attributes);
  }

  /* Connect the window to the dynamic menu, pmd */
  XSaveContext(dpy,pmd->pmdi->w,MenuContext,(caddr_t)pmd);

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

static 
SCM
PopupGrabMenu(Menu *pmenu, DynamicMenu *pmdPoppedFrom, Bool fWarpToFirst)
{
  DynamicMenu *pmd = NewDynamicMenu(pmenu,pmdPoppedFrom);
  int cpixX_startpointer;
  int cpixY_startpointer;
  SCM scmAction = SCM_UNDEFINED;

  WXGetPointerWindowOffsets(Scr.Root,&cpixX_startpointer,&cpixY_startpointer);
  
  SetPopupMenuPosition(pmd, cpixX_startpointer, cpixY_startpointer);
  
  PopupMenu(pmd);
  GrabEm(CURSOR_MENU);
  scmAction = MenuInteraction(pmd, fWarpToFirst);
  UngrabEm();
  PopdownMenu(pmd);
  PopdownAllPriorMenus(pmd);
  FreeDynamicMenu(pmd);
  DEREF_IF_SYMBOL(scmAction);
  if (DYNAMIC_PROCEDURE_P(scmAction)) {
    return call_thunk_with_message_handler(scmAction);
  } else if (DYNAMIC_MENU_P(scmAction)) {
    /* FIXGJB: is this recursion  bad? */
    return popup_menu(scmAction, SCM_BOOL_FromBool(fWarpToFirst));
  }
}

SCWM_PROC(popup_menu,"popup-menu", 1,1,0,
          (SCM menu, SCM warp_to_first_p))
/** Popup MENU, a menu object, and warp to the first item if WARP-TO-FIRST? is #t. */
#define FUNC_NAME s_popup_menu
{
  Bool fWarpToFirst = False;
  /* permit 'menu to be used, and look up dynamically */
  DEREF_IF_SYMBOL(menu);
  if (!MENU_P(menu)) {
    scm_wrong_type_arg(FUNC_NAME, 1, menu);
  }
  if (warp_to_first_p == SCM_BOOL_T)
    fWarpToFirst = True;
  /* FIXGJB: how can we tell if keybd was used to invoke this command? */
  return PopupGrabMenu(MENU(menu),NULL,fWarpToFirst);
  /*  return SCM_UNSPECIFIED; */
}
#undef FUNC_NAME

void 
menu_init_gcs()
{
  XGCValues gcv;
  unsigned long gcm;

  gcm = GCFunction | GCPlaneMask | GCGraphicsExposures | GCLineWidth | GCFillStyle;
  gcv.fill_style = FillSolid;
  gcv.plane_mask = AllPlanes;
  gcv.function = GXcopy;
  gcv.graphics_exposures = False;
  gcv.line_width = 0;
  Scr.MenuReliefGC = XCreateGC(dpy, Scr.Root, gcm, &gcv);
  Scr.MenuShadowGC = XCreateGC(dpy, Scr.Root, gcm, &gcv);
  Scr.MenuGC = XCreateGC(dpy, Scr.Root, gcm, &gcv);
  Scr.MenuStippleGC = XCreateGC(dpy, Scr.Root, gcm, &gcv);
}


MAKE_SMOBFUNS(menu);

void
init_menu()
{
  REGISTER_SCWMSMOBFUNS(menu);

  pscm_construct_menu_primitive = SCM_CDRLOC
    (scm_sysintern("construct-menu-primitive", SCM_BOOL_F));

#ifndef SCM_MAGIC_SNARFER
# include "menu.x"
#endif
}
