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

#define scmBLACK load_color(gh_str02scm("black"))
#define scmWHITE load_color(gh_str02scm("white"))


SCM 
mark_scwmmenu(SCM obj)
{
  Scwm_Menu *mi = SCWM_SCWMMENU(obj);

  SCM_SETGC8MARK(obj);
  if (mi->scmMenuItems != SCM_UNDEFINED) {
    scm_gc_mark(mi->scmMenuItems);
  }
  if (mi->scmSideBGColor != SCM_UNDEFINED) {
    scm_gc_mark(mi->scmSideBGColor);
  }
  if (mi->scmBGColor != SCM_UNDEFINED) {
    scm_gc_mark(mi->scmBGColor);
  }
  if (mi->scmTextColor != SCM_UNDEFINED) {
    scm_gc_mark(mi->scmTextColor);
  }
  if (mi->scmFont != SCM_UNDEFINED) {
    scm_gc_mark(mi->scmFont);
  }
  return SCM_BOOL_F;
}

size_t 
free_scwmmenu(SCM obj)
{
  Scwm_Menu *menu = SCWM_SCWMMENU(obj);
  if (menu->picSide) {
    DestroyPicture(dpy,menu->picSide);
  }
  if (menu->picBackground) {
    DestroyPicture(dpy,menu->picBackground);
  }
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
    scm_puts(", hotkeys: ",port);
    scm_puts(menu->pchUsedShortcutKeys,port);
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


char *
NewPchKeysUsed(SCM list_of_menuitems)
{
  int cItems = gh_length(list_of_menuitems);
  char *pch = safemalloc(sizeof(char) * (cItems + 1));
  int ich = 0;
  SCM item;
  SCM rest = list_of_menuitems;
  Scwm_MenuItem *mi;

  memset(pch,0,cItems+1);
  while (True) {
    item = gh_car(rest);
    mi = SCWM_MENUITEM(item);
    if (mi->pchHotkeyPreferences) {
      char *pchDesiredChars = mi->pchHotkeyPreferences;
      char ch;
      while ((ch = *pchDesiredChars++) != '\0') {
	if (!strchr(pch,ch)) {
	  pch[ich++] = ch;
	  break;
	}
      }
    }
    rest = gh_cdr(rest);
    if (SCM_NULLP(rest))
      break;
  }
  return pch;
}

SCM make_scwmmenu(SCM list_of_menuitems,
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
  if (picture_side == SCM_UNDEFINED) {
    menu->picSide = NULL;
  } else if (!PICTURE_P(picture_side)) {
    scm_wrong_type_arg(__FUNCTION__,iarg,picture_side);
  } else {
    menu->picSide = PICTURE(picture_side)->pic;
  }

  iarg++;
  if (side_bg_color == SCM_UNDEFINED) {
    side_bg_color = scmWHITE;
  } else if (!COLORP(side_bg_color)) {
    scm_wrong_type_arg(__FUNCTION__,iarg,side_bg_color);
  }
  menu->scmSideBGColor = side_bg_color;

  iarg++;
  if (bg_color == SCM_UNDEFINED) {
    bg_color = scmWHITE;
  } else if (!COLORP(bg_color)) {
    scm_wrong_type_arg(__FUNCTION__,iarg,bg_color);
  }
  menu->scmBGColor = bg_color;

  iarg++;
  if (text_color == SCM_UNDEFINED) {
    text_color = scmBLACK;
  } else if (!COLORP(text_color)) {
    scm_wrong_type_arg(__FUNCTION__,iarg,text_color);
  }
  menu->scmTextColor = text_color;

  iarg++;
  if (picture_bg == SCM_UNDEFINED) {
    menu->picBackground = NULL;
  } else if (!PICTURE_P(picture_bg)) {
    scm_wrong_type_arg(__FUNCTION__,iarg,picture_bg);
  } else {
    menu->picBackground = PICTURE(picture_bg)->pic;
  }

  iarg++;
  /* FIXGJB: order dependency on menu_font being set before making
     the menu -- is there a better default -- maybe we should just
     always have some font object for "fixed" */
  if (font == SCM_UNDEFINED && menu_font != SCM_UNDEFINED) {
    menu->scmFont = menu_font;
  } else if (!FONTP(font)) {
    scm_wrong_type_arg(__FUNCTION__,iarg,font);
  }
  menu->scmFont = font;

  menu->pchUsedShortcutKeys = NewPchKeysUsed(menu->scmMenuItems);

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
}

static
void
PopupMenu(DynamicMenu *pmd)
{
  MenuDrawingInfo *pmdi = pmd->pmdi;
  DynamicMenu *pmdPoppedFrom = pmd->pmdPrior;
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

static int HOVER_DELAY_MS = 100;

static
void
MenuInteraction(DynamicMenu *pmd)
{
  while (True) {
    int c10ms_delays = 0;
    while (XCheckMaskEvent(dpy, 
			   ButtonPressMask|ButtonReleaseMask|ExposureMask | 
			   KeyPressMask|VisibilityChangeMask|ButtonMotionMask, 
			   &Event) == FALSE) {
      sleep_ms(10);
      if (c10ms_delays++ >= HOVER_DELAY_MS/10 ) {
	/* FIXGJB: invoke the hover action */
      } else {  /* block until there is an interesting event */
	XMaskEvent(dpy, 
		   ButtonPressMask|ButtonReleaseMask|ExposureMask | 
		   KeyPressMask|VisibilityChangeMask|ButtonMotionMask, 
		   &Event);
      }
      if (Event.type == MotionNotify) {
	/* discard any extra motion events before a release */
	while((XCheckMaskEvent(dpy,ButtonMotionMask|ButtonReleaseMask,
			       &Event))&&(Event.type != ButtonRelease));
      }

      switch(Event.type) 
	{
	case ButtonRelease:
	  /* FIXGJB: handle release */
	  goto MENU_INTERACTION_RETURN;
	  break;

	case VisibilityNotify:
	case ButtonPress:
	  continue;

	case KeyPress:
	  /* Handle a key press events to allow mouseless operation */
	  /* FIXGJB: check shortcuts and other keybindings in the menu event map */
	  goto MENU_INTERACTION_RETURN;
	  break;
	  
	case MotionNotify:
	  /* FIXGJB: update selected item, mark mouse_moved boolean if
	     it's moved enough, reset action hook timer, etc. */
	  break;

	case Expose:
	{ /* scope */
	  DynamicMenu *pmdNeedsPainting = NULL;
	  /* grab our expose events, let the rest go through */
	  if((XFindContext(dpy, Event.xany.window,ScwmMenuContext,
			   (caddr_t *)&pmdNeedsPainting) !=XCNOENT)) {
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
    } /* while next event */
  } /* while true */
 MENU_INTERACTION_RETURN:
  return;
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
    pmiim->pmi = pmi;

    pmiim->cpixItemHeight = -1;	/* just init: gets set in drawing code */
    pmiim->cpixOffsetY = -1;	/* just init: gets set in drawing code */
    pmiim->fOnTopEdge = False;	/* just init: gets set in drawing code */
    pmiim->fOnBottomEdge = False; /* just init: gets set in drawing code */

    /* Not sure what rule should determine the show popup arrow flag...
       could be the setting of a Hover action, but that's not quite right....
       maybe the scheme code should just specify it wants a popup arrow... 
       --11/23/97 gjb */
    pmiim->fShowPopupArrow = False;

    pmiim->mis = MIS_Enabled;	/* FIXGJB: set using hook info? */
    rest = gh_cdr(rest);
    if (SCM_NULLP(rest))
      break;
    imiim++;
  }
}

static 
void
PopupGrabMenu(Scwm_Menu *psm, DynamicMenu *pmdPoppedFrom)
{
  DynamicMenu *pmd = safemalloc(sizeof(Scwm_Menu));
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
    attributes.event_mask = (ExposureMask | EnterWindowMask);
    XChangeWindowAttributes(dpy,pmd->pmdi->w,CWEventMask,&attributes);
  }

  /* Connect the window to the dynamic menu, pmd */
  XSaveContext(dpy,pmd->pmdi->w,ScwmMenuContext,(caddr_t)pmd);

  { /* scope */
    int cpixX_startpointer;
    int cpixY_startpointer;
    XGetPointerWindowOffsets(Scr.Root,&cpixX_startpointer,&cpixY_startpointer);

    SetPopupMenuPosition(pmd, cpixX_startpointer, cpixY_startpointer);

    PopupMenu(pmd);
    MenuInteraction(pmd);
    PopdownMenu(pmd);
  }
}

SCM 
popup_menu(SCM menu)
{
  if (SCWM_MENU_P(menu)) {
    scm_wrong_type_arg("popup-menu", 1, menu);
  }
  PopupGrabMenu(SCWM_SCWMMENU(menu),NULL);
  return SCM_UNSPECIFIED;
}

void 
init_scwm_menu()
{
  /* empty */
}
