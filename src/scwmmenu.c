/* $Id$
 * menuitem.c
 * By Greg J. Badros -- Nov. 14, 1997
 *
 */


#define SCWMMENU_IMPLEMENTATION

#include <config.h>
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
#include "scwm.h"
#include "font.h"
#include "system.h"
#include "scwmmenu.h"
#include "drawmenu.h"
#include "colormaps.h"
#include "xmisc.h"
#include "screen.h"
#include "color.h"
#include "util.h"
#include "misc.h"

#define scmBLACK load_color(gh_str02scm("black"))
#define scmWHITE load_color(gh_str02scm("white"))

SCM 
mark_scwmmenu(SCM obj)
{
  Scwm_Menu *mi = SCWM_SCWMMENU(obj);

  SCM_SETGC8MARK(obj);
  GC_MARK_SCM_IF_SET(mi->scmMenuItems);
  GC_MARK_SCM_IF_SET(mi->scmSideBGColor);
  GC_MARK_SCM_IF_SET(mi->scmBGColor);
  GC_MARK_SCM_IF_SET(mi->scmTextColor);
  GC_MARK_SCM_IF_SET(mi->scmFont);
  GC_MARK_SCM_IF_SET(mi->scmImgSide);
  GC_MARK_SCM_IF_SET(mi->scmImgBackground);

  return SCM_BOOL_F;
}

size_t 
free_scwmmenu(SCM obj)
{
  Scwm_Menu *menu = SCWM_SCWMMENU(obj);
  if (menu->pchUsedShortcutKeys) {
    free(menu->pchUsedShortcutKeys);
  }
  free(menu);
  return(0);
}

int 
print_scwmmenu(SCM obj, SCM port, scm_print_state * pstate)
{
  scm_puts("#<scwmmenu ", port);
  if (SCWM_MENU_P(obj)) {
    Scwm_Menu *menu = SCWM_SCWMMENU(obj);
    scm_write(gh_car(menu->scmMenuItems), port);
    if (menu->pchUsedShortcutKeys) {
      scm_puts(", hotkeys: ",port);
      scm_puts(menu->pchUsedShortcutKeys,port);
    }
  } else {
    scm_puts("(invalid)", port);
  }
  scm_putc('>', port);

  return 1;
}

SCM 
scwmmenu_p(SCM obj)
{
  return (SCWM_MENU_P(obj) ? SCM_BOOL_T : SCM_BOOL_F);
}


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
  int imiim = 0;
  int cItems = gh_length(list_of_menuitems);
  char *pch = safemalloc(sizeof(char) * (cItems + 1));
  int ich = 0;
  SCM item;
  SCM rest = list_of_menuitems;
  Scwm_MenuItem *pmi;

  memset(pch,0,cItems+1);
  while (True) {
    item = gh_car(rest);
    pmi = SCWM_MENUITEM(item);
    if (pmi->pchHotkeyPreferences) {
      char *pchDesiredChars = pmi->pchHotkeyPreferences;
      char ch;
      while ((ch = *pchDesiredChars++) != '\0') {
	if (!strchr(pch,ch)) {
	  /* Found the char to use */
	  rgpmiim[imiim]->chShortcut = ch;
	  pch[ich++] = ch;
	  rgpmiim[imiim]->ichShortcutOffset = IchIgnoreCaseInSz(pmi->szLabel,ch);
	  break;
	}
      }
    }
    rest = gh_cdr(rest);
    imiim++;
    if (SCM_NULLP(rest))
      break;
  }
  return pch;
}

SCM_PROC(s_menu_properties, "menu-properties", 1, 0, 0, menu_properties);

SCM
menu_properties(SCM scmMenu)
{
  Scwm_Menu *pmenu = SAFE_SCWMMENU(scmMenu);
  if (!pmenu) {
    scm_wrong_type_arg(s_menu_properties,1,scmMenu);
  }
  return gh_list(pmenu->scmMenuItems,
		 pmenu->scmImgSide,
		 pmenu->scmSideBGColor,
		 pmenu->scmBGColor,
		 pmenu->scmTextColor,
		 pmenu->scmImgBackground,
		 pmenu->scmFont,
		 gh_str02scm(pmenu->pchUsedShortcutKeys));
}


SCM 
make_scwmmenu(SCM list_of_menuitems,
	      SCM picture_side, SCM side_bg_color,
	      SCM bg_color, SCM text_color,
	      SCM picture_bg, SCM font)
{
  Scwm_Menu *menu = safemalloc(sizeof(Scwm_Menu));
  SCM answer;
  int iarg = 1;

  if (!gh_list_p(list_of_menuitems)) {
    scm_wrong_type_arg(__FUNCTION__,iarg,list_of_menuitems);
  }
  menu->scmMenuItems = list_of_menuitems;

  iarg++;
  if (UNSET_SCM(picture_side)) {
    picture_side = SCM_BOOL_F;
  } else if (!IMAGE_P(picture_side)) {
    scm_wrong_type_arg(__FUNCTION__,iarg,picture_side);
  } 
  menu->scmImgSide = picture_side;

  iarg++;
  if (UNSET_SCM(side_bg_color)) {
    side_bg_color = scmWHITE;
  } else if (!COLORP(side_bg_color)) {
    scm_wrong_type_arg(__FUNCTION__,iarg,side_bg_color);
  }
  menu->scmSideBGColor = side_bg_color;

  iarg++;
  if (UNSET_SCM(bg_color)) {
    bg_color = scmWHITE; /* FIXGJB: Scr.MenuColors.back; */
  } else if (!COLORP(bg_color)) {
    scm_wrong_type_arg(__FUNCTION__,iarg,bg_color);
  }
  menu->scmBGColor = bg_color;

  iarg++;
  if (UNSET_SCM(text_color)) {
    text_color =  scmBLACK; /* FIXGJB: Scr.MenuColors.fore ; */
  } else if (!COLORP(text_color)) {
    scm_wrong_type_arg(__FUNCTION__,iarg,text_color);
  }
  menu->scmTextColor = text_color;

  iarg++;
  if (UNSET_SCM(picture_bg)) {
    picture_bg = SCM_BOOL_F;
  } else if (!IMAGE_P(picture_bg)) {
    scm_wrong_type_arg(__FUNCTION__,iarg,picture_bg);
  } 
  menu->scmImgBackground = picture_bg;

  iarg++;
  /* FIXGJB: order dependency on menu_font being set before making
     the menu -- is there a better default -- maybe we should just
     always have some font object for "fixed" */
  if (UNSET_SCM(font) && menu_font != SCM_UNDEFINED) {
    menu->scmFont = menu_font;
  } else if (!FONTP(font)) {
    scm_wrong_type_arg(__FUNCTION__,iarg,font);
  }
  menu->scmFont = font;

  menu->pchUsedShortcutKeys = NULL;

  SCM_NEWCELL(answer);
  SCM_SETCAR(answer, scm_tc16_scwm_scwmmenu);
  SCM_SETCDR(answer, (SCM) menu);
  return answer;
}


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

  /* May want to select a different object initially if we popped up
     somewhere near an edge/bottom, depending on what behaviour
     we choose there */
  pmd->imiimSelected = 0;

  /* mark menu item as selected */
  if (pmd->imiimSelected >= 0) {
    pmd->rgpmiim[pmd->imiimSelected]->mis = MIS_Selected;
  }
}

static
void
PopupMenu(DynamicMenu *pmd)
{
  MenuDrawingInfo *pmdi = pmd->pmdi;
/*  DynamicMenu *pmdPoppedFrom = pmd->pmdPrior; */
  Window w = pmdi->w;
  InstallRootColormap();
  XMoveWindow(dpy, w, pmdi->x, pmdi->y);
  XMapRaised(dpy, w);
}

static
void
PopdownMenu(DynamicMenu *pmd)
{
  XUnmapWindow(dpy, pmd->pmdi->w);
  UninstallRootColormap();
  XFlush(dpy);
}


static
DynamicMenu *
PmdFromWindow(Display *dpy, Window w)
{
  DynamicMenu *pmd = NULL;
  if ((XFindContext(dpy, Event.xany.window,ScwmMenuContext,
		    (caddr_t *)&pmd) == XCNOENT)) {
    pmd = NULL;
  }
  return pmd;
}

static
MenuItemInMenu *
PmiimFromPmdXY(DynamicMenu *pmd, int x, int y)
{
  int imiim;
  for (imiim = 0; imiim < pmd->cmiim; imiim++) {
    MenuItemInMenu *pmiim = pmd->rgpmiim[imiim];
    int item_y_offset = pmiim->cpixOffsetY;
    if (y > item_y_offset && y < item_y_offset + pmiim->cpixItemHeight) {
      return pmiim;
    }
  }
  return NULL;
}


static
DynamicMenu *
PmdFromPointerLocation(Display *dpy)
{
  Window wChild;
  
  XQueryPointer( dpy, Scr.Root, &JunkRoot, &wChild,
		&JunkX,&JunkY, &JunkX, &JunkY, &JunkMask);
  return PmdFromWindow(dpy,wChild);
}

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
		&root_x,&root_y, &JunkX, &JunkY, &JunkMask);
  if ((pmd = PmdFromWindow(dpy,wChild)) == NULL) {
    return NULL;
  }

  /* now get position in that child window */
  XQueryPointer( dpy, wChild, &JunkRoot, &JunkChild,
		&root_x,&root_y, &x, &y, &JunkMask);

  /* set the return value for the x_offset */
  if (px_offset) *px_offset = x;

  /* look for the entry that the mouse is in */
  return PmiimFromPmdXY(pmd,x,y);
}

static
MenuItemInMenu *
PmiimFromPmdShortcutKeypress(DynamicMenu *pmd, char ch)
{
  int imiim;
  for (imiim = 0; imiim < pmd->cmiim; imiim++) {
    MenuItemInMenu *pmiim = pmd->rgpmiim[imiim];
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
/*  Scwm_MenuItem *pmi = pmiim->pmi; */
  DynamicMenu *pmd = pmiim->pmd;
  Window w = pmd->pmdi->w;
  PaintMenuItem(w,pmd,pmiim);
}

inline
MenuItemInMenu *
PmiimSelectedFromPmd(DynamicMenu *pmd)
{
  int imiimSelected = pmd->imiimSelected;
  if (imiimSelected < 0)
    return NULL;
  return pmd->rgpmiim[imiimSelected];
}

static
void
UnselectAndRepaintSelectionForPmd(DynamicMenu *pmd)
{
  MenuItemInMenu *pmiim = PmiimSelectedFromPmd(pmd);
  if (!pmiim)
    return;

  if (pmiim->mis != MIS_Selected) {
    scwm_msg(DBG,__FUNCTION__,"pmiim->mis != MIS_Selected");
  }
  pmd->imiimSelected = -1;
  pmiim->mis = MIS_Enabled;
  RepaintMenuItem(pmiim);
}

static
void
SelectAndRepaintPmiim(MenuItemInMenu *pmiim)
{
  DynamicMenu *pmd = pmiim->pmd;
  if (pmiim->mis == MIS_Selected) {
    return;
  }
  pmiim->mis = MIS_Selected;
  pmd->imiimSelected = pmiim->imiim;
  RepaintMenuItem(pmiim);
}

/* FIXGJB : Need EnterWindowMask? */
static const long menu_event_mask = (ButtonPressMask | ButtonReleaseMask | 
				     ExposureMask | KeyPressMask | 
				     VisibilityChangeMask | ButtonMotionMask |
				     PointerMotionMask );

static int HOVER_DELAY_MS = 300;  /* FIXGJB: make configurable */

static
SCM
MenuInteraction(DynamicMenu *pmd)
{
  int c10ms_delays = 0;
  SCM scmAction = SCM_UNDEFINED;
  Bool fHoverActionInvoked = False;
  while (True) {
    while (XCheckMaskEvent(dpy, menu_event_mask, &Event) == FALSE) {
      sleep_ms(10);

      if (c10ms_delays++ == HOVER_DELAY_MS/10 ) {
	MenuItemInMenu *pmiimSelected = PmiimSelectedFromPmd(pmd);
	if (pmiimSelected) {
	  fHoverActionInvoked = True;
	  /* invoke the hover action */
	  if (gh_procedure_p(pmiimSelected->pmi->scmHover)) {
	    call_thunk_with_message_handler(pmiimSelected->pmi->scmHover);
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
    
    scwm_msg(DBG,__FUNCTION__,"Got an event");
    switch(Event.type) {
    case ButtonRelease:
    { /* scope */
      MenuItemInMenu *pmiim = PmiimFromPointerLocation(dpy,NULL);
      if (pmiim) {
	MenuItemInMenu *pmiimSelected = PmiimSelectedFromPmd(pmd);
	if (pmiim != pmiimSelected) {
	  scwm_msg(WARN,__FUNCTION__,"Pointer not in selected item -- wierd!");
	}
	if (gh_procedure_p(pmiim->pmi->scmAction)) {
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
      KeySym keysym = XLookupKeysym(&Event.xkey,0);
      scwm_msg(DBG,__FUNCTION__,"Got a keypress");
      /* Handle a key press events to allow mouseless operation */
      /* FIXGJB: check shortcuts and other keybindings in the menu event map */
      if (keysym == XK_Escape)
	goto MENU_INTERACTION_RETURN;
      break;
    }
      
    case MotionNotify:
      /* FIXGJB: update selected item, mark mouse_moved boolean if
	 it's moved enough, reset action hook timer, etc. */
    { 
      MenuItemInMenu *pmiim = PmiimFromPointerLocation(dpy,NULL);
      scwm_msg(DBG,__FUNCTION__,"MotionNotify event %ld", (unsigned long) pmiim);
      if (!pmiim || (pmiim && pmiim->mis != MIS_Selected)) {
	if (fHoverActionInvoked) {
	  MenuItemInMenu *pmiimSelected = PmiimSelectedFromPmd(pmd);
	  /* invoke the un-hover action */
	  if (pmiimSelected && !UNSET_SCM(pmiimSelected->pmi->scmUnhover)) {
	    call_thunk_with_message_handler(pmiimSelected->pmi->scmUnhover);
	  } else {
	    scwm_msg(DBG,__FUNCTION__,"No unhover hook, %ld",pmiimSelected);
	  }
	  fHoverActionInvoked = False;
	}
	UnselectAndRepaintSelectionForPmd(pmd);
	c10ms_delays = 0;
      }
      if (pmiim && pmiim->mis != MIS_Selected) {
	SelectAndRepaintPmiim(pmiim);
      }
      break;
    }
      
    case Expose:
    { /* scope */
      DynamicMenu *pmdNeedsPainting = NULL;
      scwm_msg(DBG,__FUNCTION__,"Got expose event for menu");
      /* grab our expose events, let the rest go through */
      pmdNeedsPainting = PmdFromWindow(dpy,Event.xany.window);
      if (pmdNeedsPainting) {
	   scwm_msg(DBG,__FUNCTION__,"Trying to paint menu");
	PaintDynamicMenu(pmdNeedsPainting,&Event);
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
  } /* while true */
 MENU_INTERACTION_RETURN:
  return scmAction;
}

static
void
InitializeDynamicMenu(DynamicMenu *pmd)
{
  Scwm_Menu *pmenu = pmd->pmenu;
  int cmiim = gh_length(pmenu->scmMenuItems);
  int imiim = 0;
  MenuItemInMenu **rgpmiim = pmd->rgpmiim =
    safemalloc(cmiim * sizeof(MenuDrawingInfo));
  SCM rest = pmd->pmenu->scmMenuItems;

  /* save the array size in the struct */
  pmd->cmiim = cmiim;

  /* Initialize the list of dynamic menu items;
     only the drawing-independent code here */
  while (True) {
    SCM item = gh_car(rest);
    Scwm_MenuItem *pmi = SCWM_MENUITEM(item);
    MenuItemInMenu *pmiim = safemalloc(sizeof(MenuItemInMenu));
    rgpmiim[imiim] = pmiim;

    /* save some back pointers so we can find a dynamic menu
       just from the menu item */
    pmiim->pmi = pmi;
    pmiim->pmd = pmd;
    pmiim->imiim = imiim;
    pmiim->chShortcut = '\0';
    pmiim->ichShortcutOffset = -1;

    pmiim->cpixItemHeight = -1;	/* just init: gets set in drawing code */
    pmiim->cpixOffsetY = -1;	/* just init: gets set in drawing code */
    pmiim->fOnTopEdge = False;	/* just init: gets set in drawing code */
    pmiim->fOnBottomEdge = False; /* just init: gets set in drawing code */

    /* Not sure what rule should determine the show popup arrow flag...
       could be the setting of a Hover action, but that's not quite right....
       maybe the scheme code should just specify it wants a popup arrow... 
       --11/23/97 gjb */
    pmiim->fShowPopupArrow = (!UNSET_SCM(pmiim->pmi->scmHover));

    pmiim->mis = MIS_Enabled;	/* FIXGJB: set using hook info? */
    rest = gh_cdr(rest);
    if (SCM_NULLP(rest))
      break;
    imiim++;
  }
  pmd->pmenu->pchUsedShortcutKeys = NewPchKeysUsed(pmd);

}

static 
void
PopupGrabMenu(Scwm_Menu *psm, DynamicMenu *pmdPoppedFrom)
{
  DynamicMenu *pmd = safemalloc(sizeof(DynamicMenu));
  pmd->pmenu = psm;
  pmd->pmdNext = NULL;
  pmd->pmdPrior = pmdPoppedFrom;
  pmd->pmdi = NULL;
  pmd->fPinned = False;

  InitializeDynamicMenu(pmd);	/* add drawing independent fields */
  ConstructDynamicMenu(pmd);	/* update/create pmd->pmdi */


  { /* scope */
    /* Get the right events -- don't trust the drawing code to do this */
    XSetWindowAttributes attributes;
    attributes.event_mask = menu_event_mask;
    XChangeWindowAttributes(dpy,pmd->pmdi->w,CWEventMask, &attributes);
  }

  /* Connect the window to the dynamic menu, pmd */
  XSaveContext(dpy,pmd->pmdi->w,ScwmMenuContext,(caddr_t)pmd);

  { /* scope */
    int cpixX_startpointer;
    int cpixY_startpointer;
    SCM scmAction = SCM_UNDEFINED;
    XGetPointerWindowOffsets(Scr.Root,&cpixX_startpointer,&cpixY_startpointer);

    SetPopupMenuPosition(pmd, cpixX_startpointer, cpixY_startpointer);

    PopupMenu(pmd);
    GrabEm(MENU);
    scmAction = MenuInteraction(pmd);
    UngrabEm();
    PopdownMenu(pmd);
    if (gh_procedure_p(scmAction)) {
      call_thunk_with_message_handler(scmAction);
    }
  }
}

SCM 
popup_menu(SCM menu)
{
  if (!SCWM_MENU_P(menu)) {
    scm_wrong_type_arg("popup-menu", 1, menu);
  }
  PopupGrabMenu(SCWM_SCWMMENU(menu),NULL);
  return SCM_UNSPECIFIED;
}

void 
init_scwm_menu()
{
# include "scwmmenu.x"
}
