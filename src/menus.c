

/****************************************************************************
 * This module is all original code 
 * by Rob Nation 
 * Copyright 1993, Robert Nation
 *     You may use this code for any purpose, as long as the original
 *     copyright remains in the source code and all documentation
 ****************************************************************************/


/***********************************************************************
 *
 * scwm menu code
 *
 ***********************************************************************/
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
#include <assert.h>

#include "scwm.h"
#include "menus.h"
#include "misc.h"
#include "move.h"
#include "parse.h"
#include "screen.h"
#include "menu.h"
#include "util.h"
#include "string_token.h"
#include "colormaps.h"
#include "paths.h"


int menu_on = 0;

MenuRoot *ActiveMenu = NULL;	/* the active menu */
MenuItem *ActiveItem = NULL;	/* the active menu item */

int menuFromFrameOrWindowOrTitlebar = FALSE;

extern int Context, Button;
extern ScwmWindow *ButtonWindow, *swCurrent;
extern XEvent Event;
int Stashed_X, Stashed_Y, MenuY = 0;

void DrawTrianglePattern(Window, GC, GC, GC, int, int, int, int);
void DrawSeparator(Window, GC, GC, int, int, int, int, int);
void DrawUnderline(Window w, GC gc, int x, int y, char *txt, int off);
int UpdateMenu(int sticks);
int mouse_moved = 0;
int menu_aborted = 0;

static int PrevActiveMenuX = -1;

extern XContext MenuContext;

/****************************************************************************
 *
 * Initiates a menu pop-up
 *
 * Style = 1 = sticky menu, stays up on initial button release.
 * Style = 0 = transient menu, drops on initial release.
 ***************************************************************************/
int 
do_menu(MenuRoot * menu, int style)
{
  int prevStashedX = 0, prevStashedY = 0;
  MenuRoot *PrevActiveMenu = 0;
  MenuItem *PrevActiveItem = 0;
  int retval = MENU_NOP;
  int x, y;
  Time t0 = 0;
  extern Time lastTimestamp;

  int PrevMenuX = PrevActiveMenuX;

  /* this condition could get ugly */
  if (menu->in_use)
    return MENU_ERROR;

  /* In case we wind up with a move from a menu which is
   * from a window border, we'll return to here to start
   * the move */
  XQueryPointer(dpy, Scr.Root, &JunkRoot, &JunkChild,
		&x, &y, &JunkX, &JunkY, &JunkMask);

  if (menu_on) {
    prevStashedX = Stashed_X;
    prevStashedY = Stashed_Y;

    PrevActiveMenu = ActiveMenu;
    PrevActiveItem = ActiveItem;
    if (ActiveMenu)
      if (Scr.flags & MWMMenus)
	x = Stashed_X + (ActiveMenu->width >> 1) + (menu->width >> 1) - 3;
    if (ActiveItem)
      y = ActiveItem->y_offset + MenuY + (Scr.EntryHeight >> 1);
  } else {
    mouse_moved = 0;
    t0 = lastTimestamp;
    if (!GrabEm(MENU)) {

      XBell(dpy, Scr.screen);
      return MENU_DONE;
    }
    if (Scr.flags & MWMMenus)
      x += (menu->width >> 1) - 3;
  }
  if (PopUpMenu(menu, x, y)) {
    retval = UpdateMenu(style);
  } else {
    XBell(dpy, Scr.screen);
  }

  ActiveMenu = PrevActiveMenu;
  ActiveItem = PrevActiveItem;
  if ((ActiveItem) && (menu_on))
    ActiveItem->state = 1;
  Stashed_X = prevStashedX;
  Stashed_Y = prevStashedY;


  if (!menu_on) {
    UngrabEm();
    WaitForButtonsUp();
  }
  if (((lastTimestamp - t0) < 3 * Scr.ClickTime) && (mouse_moved == 0))
    menu_aborted = 1;
  else
    menu_aborted = 0;
  PrevActiveMenuX = PrevMenuX;
  return retval;
}

/***********************************************************************
 *
 *  Procedure:
 *	RelieveRectangle - add relief lines to a rectangular window
 *
 ***********************************************************************/
void 
RelieveRectangle(Window win, int x, int y, int w, int h, GC Hilite, GC Shadow)
{
  XDrawLine(dpy, win, Hilite, x, y, w + x - 1, y);
  XDrawLine(dpy, win, Hilite, x, y, x, h + y - 1);

  XDrawLine(dpy, win, Shadow, x, h + y - 1, w + x - 1, h + y - 1);
  XDrawLine(dpy, win, Shadow, w + x - 1, y, w + x - 1, h + y - 1);
}

/***********************************************************************
 *
 *  Procedure:
 *	RelieveHalfRectangle - add relief lines to the sides only of a
 *      rectangular window
 *
 ***********************************************************************/
void 
RelieveHalfRectangle(Window win, int x, int y, int w, int h,
		     GC Hilite, GC Shadow)
{
  XDrawLine(dpy, win, Hilite, x, y - 1, x, h + y);
  XDrawLine(dpy, win, Hilite, x + 1, y, x + 1, h + y - 1);

  XDrawLine(dpy, win, Shadow, w + x - 1, y - 1, w + x - 1, h + y);
  XDrawLine(dpy, win, Shadow, w + x - 2, y, w + x - 2, h + y - 1);
}


/***********************************************************************
 *
 *  Procedure:
 *      PaintEntry - draws a single entry in a poped up menu
 *
 ***********************************************************************/
void 
PaintEntry(MenuRoot * mr, MenuItem * mi)
{
  int y_offset, text_y, d, y_height, y, x;
  GC ShadowGC, ReliefGC, currentGC;

  y_offset = mi->y_offset;
  y_height = mi->y_height;
  text_y = y_offset + Scr.StdFont.y;
  if (mi->picture)
    text_y += mi->picture->height;
  if (mi->lpicture) {
    y = mi->lpicture->height - Scr.StdFont.height;
    if (y > 1)
      text_y += y >> 1;
  }
  ShadowGC = Scr.MenuShadowGC;
  if (Scr.d_depth < 2)
    ReliefGC = Scr.MenuShadowGC;
  else
    ReliefGC = Scr.MenuReliefGC;


  if (Scr.flags & MWMMenus) {
    if ((!mi->prev) || (!mi->prev->state))
      XClearArea(dpy, mr->w, 0, y_offset - 1, mr->width, y_height + 2, 0);
    else
      XClearArea(dpy, mr->w, 0, y_offset + 1, mr->width, y_height - 1, 0);
    if ((mi->state) && (mi->func_type != F_TITLE) &&
	(mi->func_type != F_NOP) && (((*mi->item) != 0) || mi->picture)) {
      RelieveRectangle(mr->w, 3, y_offset, mr->width - 5, mi->y_height,
		       ReliefGC, ShadowGC);
      RelieveRectangle(mr->w, 2, y_offset - 1, mr->width - 3, mi->y_height + 2,
		       ReliefGC, ShadowGC);
    }
    RelieveHalfRectangle(mr->w, 0, y_offset - 1, mr->width,
			 y_height + 2, ReliefGC, ShadowGC);
  } else {
    XClearArea(dpy, mr->w, 0, y_offset, mr->width, y_height, 0);
    if ((mi->state) && (mi->func_type != F_TITLE) &&
	(mi->func_type != F_NOP) && (((*mi->item) != 0) ||
				     mi->picture || mi->lpicture))
      RelieveRectangle(mr->w, 2, y_offset, mr->width - 4, mi->y_height,
		       ReliefGC, ShadowGC);
    RelieveHalfRectangle(mr->w, 0, y_offset, mr->width,
			 y_height, ReliefGC, ShadowGC);
  }
  if (mi->func_type == F_TITLE) {
    if (Scr.flags & MWMMenus) {
      text_y += HEIGHT_EXTRA >> 1;
      XDrawLine(dpy, mr->w, ShadowGC, 2, y_offset + y_height - 2,
		mr->width - 3, y_offset + y_height - 2);
      XDrawLine(dpy, mr->w, ShadowGC, 2, y_offset + y_height - 4,
		mr->width - 3, y_offset + y_height - 4);
    } else {
      if (mi->next != NULL) {
	DrawSeparator(mr->w, ShadowGC, ReliefGC, 5, y_offset + y_height - 3,
		      mr->width - 6, y_offset + y_height - 3, 1);
      }
      if (mi != mr->first) {
	text_y += HEIGHT_EXTRA_TITLE >> 1;
	DrawSeparator(mr->w, ShadowGC, ReliefGC, 5, y_offset + 1,
		      mr->width - 6, y_offset + 1, 1);
      }
    }
  } else
    text_y += HEIGHT_EXTRA >> 1;
  if (mi->func_type == F_NOP && *mi->item == 0) {
    if (Scr.flags & MWMMenus)
      DrawSeparator(mr->w, ShadowGC, ReliefGC, 2, y_offset - 1 + HEIGHT_SEPARATOR / 2,
		    mr->width - 3, y_offset - 1 + HEIGHT_SEPARATOR / 2, 0);
    else
      DrawSeparator(mr->w, ShadowGC, ReliefGC, 5, y_offset - 1 + HEIGHT_SEPARATOR / 2,
		    mr->width - 6, y_offset - 1 + HEIGHT_SEPARATOR / 2, 1);
  }
  if (mi->next == NULL)
    DrawSeparator(mr->w, ShadowGC, ShadowGC, 1, mr->height - 2,
		  mr->width - 2, mr->height - 2, 1);
  if (mi == mr->first)
    DrawSeparator(mr->w, ReliefGC, ReliefGC, 0, 0, mr->width - 1, 0, -1);

  if (check_allowed_function(mi))
    currentGC = Scr.MenuGC;
  else
    /* should be a shaded out word, no just re-colored. */
    currentGC = Scr.MenuStippleGC;

  if (*mi->item)
    XDrawString(dpy, mr->w, currentGC, mi->x, text_y, mi->item, mi->strlen);
  if (mi->strlen2 > 0)
    XDrawString(dpy, mr->w, currentGC, mi->x2, text_y, mi->item2, mi->strlen2);

  /* pete@tecc.co.uk: If the item has a hot key, underline it */
  if (mi->hotkey > 0)
    DrawUnderline(mr->w, currentGC, mi->x, text_y, mi->item, mi->hotkey - 1);
  if (mi->hotkey < 0)
    DrawUnderline(mr->w, currentGC, mi->x2, text_y, mi->item2, -1 - mi->hotkey);

  d = (Scr.EntryHeight - 7) / 2;
  if (mi->func_type == F_POPUP)
    if (mi->state)
      DrawTrianglePattern(mr->w, ShadowGC, ReliefGC, ShadowGC, mr->width - d - 8,
		     y_offset + d - 1, mr->width - d - 1, y_offset + d + 7);
    else
      DrawTrianglePattern(mr->w, ReliefGC, ShadowGC, ReliefGC, mr->width - d - 8,
		     y_offset + d - 1, mr->width - d - 1, y_offset + d + 7);

  if (mi->picture) {
    x = (mr->width - mi->picture->width) / 2;
    if (mi->lpicture && x < mr->width0 + 5)
      x = mr->width0 + 5;

    if (mi->picture->depth > 0) {	/* pixmap? */
      Globalgcm = GCClipMask | GCClipXOrigin | GCClipYOrigin;
      Globalgcv.clip_mask = mi->picture->mask;
      Globalgcv.clip_x_origin = x;
      Globalgcv.clip_y_origin = y_offset + 1;
      XChangeGC(dpy, ReliefGC, Globalgcm, &Globalgcv);
      XCopyArea(dpy, mi->picture->picture, mr->w, ReliefGC, 0, 0,
		mi->picture->width, mi->picture->height,
		x, y_offset + 1);
      Globalgcm = GCClipMask;
      Globalgcv.clip_mask = None;
      XChangeGC(dpy, ReliefGC, Globalgcm, &Globalgcv);
    } else {
      XCopyPlane(dpy, mi->picture->picture, mr->w,
		 currentGC, 0, 0, mi->picture->width, mi->picture->height,
		 x, y_offset + 1, 1);
    }
  }
  if (mi->lpicture) {
    if (mi->picture && *mi->item != 0)
      y = y_offset + mi->y_height - mi->lpicture->height - 1;
    else
      y = y_offset + mi->y_height / 2 - mi->lpicture->height / 2;
    if (mi->lpicture->depth > 0) {	/* pixmap? */
      Globalgcm = GCClipMask | GCClipXOrigin | GCClipYOrigin;
      Globalgcv.clip_mask = mi->lpicture->mask;
      Globalgcv.clip_x_origin = 5;
      Globalgcv.clip_y_origin = y;

      XChangeGC(dpy, ReliefGC, Globalgcm, &Globalgcv);
      XCopyArea(dpy, mi->lpicture->picture, mr->w, ReliefGC, 0, 0,
		mi->lpicture->width, mi->lpicture->height, 5, y);
      Globalgcm = GCClipMask;
      Globalgcv.clip_mask = None;
      XChangeGC(dpy, ReliefGC, Globalgcm, &Globalgcv);
    } else {
      XCopyPlane(dpy, mi->lpicture->picture, mr->w,
		 currentGC, 0, 0, mi->lpicture->width, mi->lpicture->height,
		 5, y, 1);
    }
  }
  return;
}

/****************************************************************************
 * Procedure:
 *	DrawUnderline() - Underline a character in a string (pete@tecc.co.uk)
 *
 * Calculate the pixel offsets to the start of the character position we
 * want to underline and to the next character in the string.  Shrink by
 * one pixel from each end and the draw a line that long two pixels below
 * the character...
 *
 ****************************************************************************/
void 
DrawUnderline(Window w, GC gc, int x, int y, char *txt, int posn)
{
  int off1 = XTextWidth(Scr.StdFont.font, txt, posn);
  int off2 = XTextWidth(Scr.StdFont.font, txt, posn + 1) - 1;

  XDrawLine(dpy, w, gc, x + off1, y + 2, x + off2, y + 2);
}

/****************************************************************************
 *
 *  Draws two horizontal lines to form a separator
 *
 ****************************************************************************/
void 
DrawSeparator(Window w, GC TopGC, GC BottomGC, int x1, int y1, int x2, int y2,
	      int extra_off)
{
  XDrawLine(dpy, w, TopGC, x1, y1, x2, y2);
  XDrawLine(dpy, w, BottomGC, x1 - extra_off, y1 + 1, x2 + extra_off, y2 + 1);
}

/****************************************************************************
 *
 *  Draws a little Triangle pattern within a window
 *
 ****************************************************************************/
void 
DrawTrianglePattern(Window w, GC GC1, GC GC2, GC GC3, int l, int u, int r, int b)
{
  int m;

  m = (u + b) / 2;

  XDrawLine(dpy, w, GC1, l, u, l, b);

  XDrawLine(dpy, w, GC2, l, b, r, m);
  XDrawLine(dpy, w, GC3, r, m, l, u);
}

/***********************************************************************
 *
 *  Procedure:
 *	PaintMenu - draws the entire menu
 *
 ***********************************************************************/
void 
PaintMenu(MenuRoot * mr, XEvent * e)
{
  MenuItem *mi;

  for (mi = mr->first; mi != NULL; mi = mi->next) {
    /* be smart about handling the expose, redraw only the entries
     * that we need to
     */
    if (e->xexpose.y < (mi->y_offset + mi->y_height) &&
	(e->xexpose.y + e->xexpose.height) > mi->y_offset) {
      PaintEntry(mr, mi);
    }
  }
  XSync(dpy, 0);
  return;
}

MenuRoot *PrevMenu = NULL;
MenuItem *PrevItem = NULL;
int PrevY = 0;

/***********************************************************************
 *
 *  Procedure:
 *	Updates menu display to reflect the highlighted item
 *
 ***********************************************************************/
int 
FindEntry(void)
{
  MenuItem *mi;
  MenuRoot *actual_mr;
  int retval = MENU_NOP;
  MenuRoot *PrevPrevMenu;
  MenuItem *PrevPrevItem;
  int PrevPrevY;
  int x, y, ChildY;
  Window Child;

  XQueryPointer(dpy, Scr.Root, &JunkRoot, &Child,
		&JunkX, &ChildY, &x, &y, &JunkMask);
  XQueryPointer(dpy, ActiveMenu->w, &JunkRoot, &JunkChild,
		&JunkX, &ChildY, &x, &y, &JunkMask);

  /* look for the entry that the mouse is in */
  for (mi = ActiveMenu->first; mi; mi = mi->next)
    if (y >= mi->y_offset && y < mi->y_offset + mi->y_height)
      break;
  if (x < 0 || x > ActiveMenu->width)
    mi = NULL;


  /* if we weren't on the active entry, let's turn the old active one off */
  if ((ActiveItem) && (mi != ActiveItem)) {
    ActiveItem->state = 0;
    PaintEntry(ActiveMenu, ActiveItem);
  }
  /* if we weren't on the active item, change the active item and turn it on */
  if ((mi != ActiveItem) && (mi != NULL)) {
    mi->state = 1;
    PaintEntry(ActiveMenu, mi);
  }
  ActiveItem = mi;

  if (ActiveItem) {
    MenuRoot *menu;

    /* create a new sub-menu */
    if ((ActiveItem->func_type == F_POPUP) &&
	((Scr.flags & MWMMenus) || (x > (3 * ActiveMenu->width >> 2)))) {
      PrevPrevMenu = PrevMenu;
      PrevPrevItem = PrevItem;
      PrevPrevY = PrevY;
      PrevY = MenuY;
      PrevMenu = ActiveMenu;
      PrevItem = ActiveItem;
/*******************************************/
      if (STREQ(ActiveItem->action, "SchemeMenu")) {
	menu = MENUROOT(ActiveItem->thunk);
      } else {
        assert(0);
      }
      if (menu != NULL) {
	retval = do_menu(menu, 0);
	/* Unfortunately, this is needed (why?) for multi-screen 
	   operation */
	/* flush_expose(ActiveMenu->w); */
	for (mi = ActiveMenu->first; mi != NULL; mi = mi->next) {
	  PaintEntry(ActiveMenu, mi);
	}
	XSync(dpy, 0);
	MenuY = PrevY;
	PrevMenu = PrevPrevMenu;
	PrevItem = PrevPrevItem;
	PrevY = PrevPrevY;
      }
    }
  }
  /* end a sub-menu */
  if (XFindContext(dpy, Child, MenuContext, (caddr_t *) & actual_mr) == XCNOENT) {
    return retval;
  }
  if (actual_mr != ActiveMenu) {
    if (actual_mr == PrevMenu) {
      if ((PrevItem->y_offset + PrevY > ChildY) ||
	  ((PrevItem->y_offset + PrevItem->y_height + PrevY) < ChildY)) {
	return SUBMENU_DONE;
      }
    } else
      return SUBMENU_DONE;
  }
  return retval;
}

/***********************************************************************
 * Procedure
 * 	menuShortcuts() - Menu keyboard processing (pete@tecc.co.uk)
 *
 * Function called from UpdateMenu instead of Keyboard_Shortcuts()
 * when a KeyPress event is received.  If the key is alphanumeric,
 * then the menu is scanned for a matching hot key.  Otherwise if
 * it was the escape key then the menu processing is aborted.
 * If none of these conditions are true, then the default processing
 * routine is called.
 ***********************************************************************/
void 
menuShortcuts(XEvent * ev)
{
  MenuItem *mi;
  KeySym keysym = XLookupKeysym(&ev->xkey, 0);

  /* Try to match hot keys */
  if (((keysym >= XK_a) && (keysym <= XK_z)) ||		/* Only consider alphabetic */
      ((keysym >= XK_0) && (keysym <= XK_9))) {		/* ...or numeric keys     */
    /* Search menu for matching hotkey */
    for (mi = ActiveMenu->first; mi; mi = mi->next) {
      char key;

      if (mi->hotkey == 0)
	continue;		/* Item has no hotkey   */
      key = (mi->hotkey > 0) ?	/* Extract hot character */
	mi->item[mi->hotkey - 1] : mi->item2[-1 - mi->hotkey];

      /* Convert to lower case to match the keysym */
      if (isupper(key))
	key = tolower(key);

      if (keysym == key) {	/* Are they equal?              */
	ActiveItem = mi;	/* Yes: Make this the active item */
	ev->type = ButtonRelease;	/* Force a menu exit            */
	return;
      }
    }
  }
  switch (keysym) {		/* Other special keyboard handling        */
  case XK_Escape:		/* Escape key pressed. Abort            */
    ActiveItem = NULL;		/* No selection                         */
    ev->type = ButtonRelease;	/* Make the menu exit                   */
    break;

    /* Nothing special --- Allow other shortcuts (cursor movement)    */
  default:
    Keyboard_shortcuts(ev, ButtonRelease);
    break;
  }
}

/***********************************************************************
 *
 *  Procedure:
 *	Updates menu display to reflect the highlighted item
 * 
 *  Input
 *      sticks = 0, transient style menu, drops on button release
 *      sticks = 1, sticky style, stays up if initial release is close to initial press.
 *  Returns:
 *      0 on error condition
 *      1 on return from submenu to parent menu
 *      2 on button release return
 *
 ***********************************************************************/
int 
UpdateMenu(int sticks)
{
  int done;
  int retval;
  MenuItem *InitialMI;
  MenuRoot *actual_mr;
  int x_init, y_init, x, y;

  XQueryPointer(dpy, Scr.Root, &JunkRoot, &JunkChild,
		&JunkX, &JunkY, &x_init, &y_init, &JunkMask);

  FindEntry();
  InitialMI = ActiveItem;

  while (TRUE) {
    /* block until there is an event */
    XMaskEvent(dpy, ButtonPressMask | ButtonReleaseMask | ExposureMask |
	    KeyPressMask | VisibilityChangeMask | ButtonMotionMask, &Event);
    StashEventTime(&Event);
    done = 0;
    if (Event.type == MotionNotify) {
      /* discard any extra motion events before a release */
      while ((XCheckMaskEvent(dpy, ButtonMotionMask | ButtonReleaseMask,
			      &Event)) && (Event.type != ButtonRelease)) ;
    }
    /* Handle a limited number of key press events to allow mouseless
     * operation */
    if (Event.type == KeyPress)
      menuShortcuts(&Event);

    switch (Event.type) {
    case ButtonRelease:
      /* The following lines holds the menu when the button is released */
      if (sticks) {
	sticks = 0;
	break;
      }
      if ((Scr.flags & MWMMenus) && !ActiveItem && (menu_on > 1)) {
	int x, y;

	XQueryPointer(dpy, Scr.Root, &JunkRoot, &JunkChild,
		      &JunkX, &JunkY, &x, &y, &JunkMask);
	if ((XFindContext(dpy, JunkChild, MenuContext,
			  (caddr_t *) & actual_mr) != XCNOENT) &&
	    (actual_mr != ActiveMenu)) {
	  done = 1;
	  break;
	}
      }
      PopDownMenu();
      if (ActiveItem) {
	done = 1;
	if (STREQ(ActiveItem->action, "Scheme")) {
	  call_thunk_with_message_handler(ActiveItem->thunk);
	} else if (STREQ(ActiveItem->action, "SchemeMenu")) {
	  popup(ActiveItem->thunk, SCM_UNDEFINED);
	} else {
	}
      }
      ActiveItem = NULL;
      ActiveMenu = NULL;
      menuFromFrameOrWindowOrTitlebar = FALSE;
      return MENU_DONE;

    case KeyPress:
    case VisibilityNotify:
    case ButtonPress:
      done = 1;
      break;

    case MotionNotify:
      XQueryPointer(dpy, Scr.Root, &JunkRoot, &JunkChild,
		    &JunkX, &JunkY, &x, &y, &JunkMask);
      if (((x - x_init > 3) || (x_init - x > 3)) &&
	  ((y - y_init > 3) || (y_init - y > 3)))
	mouse_moved = 1;
      done = 1;

      retval = FindEntry();
      if (ActiveItem != InitialMI)
	sticks = 0;
      if ((retval == MENU_DONE) || (retval == SUBMENU_DONE)) {
	PopDownMenu();
	ActiveItem = NULL;
	ActiveMenu = NULL;
	menuFromFrameOrWindowOrTitlebar = FALSE;
      }
      if (retval == MENU_DONE)
	return MENU_DONE;
      else if (retval == SUBMENU_DONE)
	return MENU_NOP;

      break;

    case Expose:
      /* grab our expose events, let the rest go through */
      if ((XFindContext(dpy, Event.xany.window, MenuContext,
			(caddr_t *) & actual_mr) != XCNOENT)) {
	PaintMenu(actual_mr, &Event);
	done = 1;
      }
      break;

    default:
      break;
    }

    if (!done)
      DispatchEvent();
    XFlush(dpy);
  }
}


/***********************************************************************
 *
 *  Procedure:
 *	PopUpMenu - pop up a pull down menu
 *
 *  Inputs:
 *	menu	- the root pointer of the menu to pop up
 *	x, y	- location of upper left of menu
 *      center	- whether or not to center horizontally over position
 *
 ***********************************************************************/
Bool 
PopUpMenu(MenuRoot * menu, int x, int y)
{
  if (menu->w == None)
    puts("No w.");
  if ((!menu) || (menu->w == None) || (menu->items == 0) || (menu->in_use))
    return False;

  menu_on++;
  InstallRootColormap();

  Stashed_X = x;
  Stashed_Y = y;

  /* pop up the menu */
  ActiveMenu = menu;
  ActiveItem = NULL;

  x -= (menu->width >> 1);
  y -= (Scr.EntryHeight >> 1);

  if ((swCurrent) && (menu_on == 1) && (Context & C_LALL)) {
    y = swCurrent->frame_y + swCurrent->boundary_width + swCurrent->title_height + 1;
    x = swCurrent->frame_x + swCurrent->boundary_width +
      ButtonPosition(Context, swCurrent) * swCurrent->title_height + 1;
  }
  if ((swCurrent) && (menu_on == 1) && (Context & C_RALL)) {
    y = swCurrent->frame_y + swCurrent->boundary_width + swCurrent->title_height + 1;
    x = swCurrent->frame_x + swCurrent->frame_width - swCurrent->boundary_width -
      ButtonPosition(Context, swCurrent) * swCurrent->title_height - menu->width + 1;
  }
  if ((swCurrent) && (menu_on == 1) && (Context & C_TITLE)) {
    y = swCurrent->frame_y + swCurrent->boundary_width + swCurrent->title_height + 1;
    if (x < swCurrent->frame_x + swCurrent->title_x)
      x = swCurrent->frame_x + swCurrent->title_x;
    if ((x + menu->width) >
	(swCurrent->frame_x + swCurrent->title_x + swCurrent->title_width))
      x = swCurrent->frame_x + swCurrent->title_x + swCurrent->title_width -
	menu->width + 1;
  }
  if (PrevActiveMenuX != -1)
    if (x + menu->width > Scr.MyDisplayWidth - 2) {
      x = PrevActiveMenuX - menu->width + 2;
    }
  /* clip to screen */
  if (x + menu->width > Scr.MyDisplayWidth - 2)
    x = Scr.MyDisplayWidth - menu->width - 2;
  if (x < 0)
    x = 0;

  if (y + menu->height > Scr.MyDisplayHeight - 2) {
    y = Scr.MyDisplayHeight - menu->height - 2;
    /* Warp pointer to middle of top line */
    /* Not with MWMMenus! */
    if ((!(Scr.flags & MWMMenus)) || (menu_on < 2))
      XWarpPointer(dpy, Scr.Root, Scr.Root, 0, 0, Scr.MyDisplayWidth,
		   Scr.MyDisplayHeight,
		   x + (menu->width >> 1), (y + (Scr.EntryHeight >> 1)));
  }
  if (y < 0)
    y = 0;

  PrevActiveMenuX = x;
  MenuY = y;
  XMoveWindow(dpy, menu->w, x, y);
  XMapRaised(dpy, menu->w);
  menu->in_use = True;
  return True;
}


/***********************************************************************
 *
 *  Procedure:
 *	PopDownMenu - unhighlight the current menu selection and
 *		take down the menus
 *
 ***********************************************************************/
void 
PopDownMenu()
{
  if (ActiveMenu == NULL)
    return;

  menu_on--;

  if (menu_on == 0) {		/* last menu down? */
    PrevActiveMenuX = -1;	/* Return to no prev item state */
  }				/* end last menu down */
  if (ActiveItem)
    ActiveItem->state = 0;

  XUnmapWindow(dpy, ActiveMenu->w);

  UninstallRootColormap();
  XFlush(dpy);
  if (Context & (C_WINDOW | C_FRAME | C_TITLE | C_SIDEBAR))
    menuFromFrameOrWindowOrTitlebar = TRUE;
  else
    menuFromFrameOrWindowOrTitlebar = FALSE;
  ActiveMenu->in_use = FALSE;
}

/***************************************************************************
 * 
 * Wait for all mouse buttons to be released 
 * This can ease some confusion on the part of the user sometimes 
 * 
 * Discard superflous button events during this wait period.
 *
 ***************************************************************************/
void 
WaitForButtonsUp()
{
  Bool AllUp = False;
  XEvent JunkEvent;
  unsigned int mask;

  while (!AllUp) {
    XAllowEvents(dpy, ReplayPointer, CurrentTime);
    XQueryPointer(dpy, Scr.Root, &JunkRoot, &JunkChild,
		  &JunkX, &JunkY, &JunkX, &JunkY, &mask);

    if ((mask &
	 (Button1Mask | Button2Mask | Button3Mask | Button4Mask | Button5Mask)) == 0)
      AllUp = True;
  }
  XSync(dpy, 0);
  while (XCheckMaskEvent(dpy,
		     ButtonPressMask | ButtonReleaseMask | ButtonMotionMask,
			 &JunkEvent)) {
    StashEventTime(&JunkEvent);
    XAllowEvents(dpy, ReplayPointer, CurrentTime);
  }

}




/****************************************************************************
 * 
 * Generates the windows for all menus
 *
 ****************************************************************************/
void 
MakeMenus(void)
{
  MenuRoot *mr;

  mr = Scr.AllMenus;
  while (mr != NULL) {
    MakeMenu(mr);
    mr = mr->next;
  }
  mr = Scr.SchemeMenus;
  while (mr != NULL) {
    MakeMenu(mr);
    mr = mr->next;
  }
}

/****************************************************************************
 * 
 * Generates the window for a menu
 *
 ****************************************************************************/
void 
MakeMenu(MenuRoot * mr)
{
  MenuItem *cur;
  unsigned long valuemask;
  XSetWindowAttributes attributes;
  int y, width;


  if ((mr->func != F_POPUP) || (!Scr.flags & WindowsCaptured))
    return;

  /* allow two pixels for top border */
  mr->width = 0;
  mr->width2 = 0;
  mr->width0 = 0;
  for (cur = mr->first; cur != NULL; cur = cur->next) {
    width = XTextWidth(Scr.StdFont.font, cur->item, cur->strlen);
    if (cur->picture && width < cur->picture->width)
      width = cur->picture->width;
    if (cur->func_type == F_POPUP)
      width += 15;
    if (width <= 0)
      width = 1;
    if (width > mr->width)
      mr->width = width;

    width = XTextWidth(Scr.StdFont.font, cur->item2, cur->strlen2);
    if (width < 0)
      width = 0;
    if (width > mr->width2)
      mr->width2 = width;
    if ((width == 0) && (cur->strlen2 > 0))
      mr->width2 = 1;

    if (cur->lpicture)
      if (mr->width0 < (cur->lpicture->width + 3))
	mr->width0 = cur->lpicture->width + 3;
  }

  /* lets first size the window accordingly */
  mr->width += 10;
  if (mr->width2 > 0)
    mr->width += 5;


  for (y = 2, cur = mr->first; cur != NULL; cur = cur->next) {
    cur->y_offset = y;
    cur->x = 5 + mr->width0;
    if (cur->func_type == F_TITLE) {
      /* Title */
      if (cur->strlen2 == 0)
	cur->x = (mr->width + mr->width2 + mr->width0
		  - XTextWidth(Scr.StdFont.font, cur->item,
			       cur->strlen)) >> 1;

      if ((cur->strlen > 0) || (cur->strlen2 > 0)) {
	if (Scr.flags & MWMMenus)
	  cur->y_height = Scr.EntryHeight + HEIGHT_EXTRA_TITLE;
	else {
	  if ((cur == mr->first) || (cur->next == NULL))
	    cur->y_height = Scr.EntryHeight - HEIGHT_EXTRA + 1 +
	      (HEIGHT_EXTRA_TITLE >> 1);
	  else
	    cur->y_height = Scr.EntryHeight - HEIGHT_EXTRA + 1 +
	      HEIGHT_EXTRA_TITLE;
	}
      } else
	cur->y_height = HEIGHT_SEPARATOR;
    } else if ((cur->strlen == 0) && (cur->strlen2 == 0))
      /* Separator */
      cur->y_height = HEIGHT_SEPARATOR;
    else
      /* Normal text entry */
      cur->y_height = Scr.EntryHeight;
    if (cur->picture)
      cur->y_height += cur->picture->height;
    if (cur->lpicture && cur->y_height < cur->lpicture->height + 4)
      cur->y_height = cur->lpicture->height + 4;
    y += cur->y_height;
    if (mr->width2 == 0) {
      cur->x2 = cur->x;
    } else {
      cur->x2 = mr->width - 5 + mr->width0;
    }
  }
  mr->in_use = 0;
  mr->height = y + 2;


#ifndef NO_SAVEUNDERS
  valuemask = (CWBackPixel | CWEventMask | CWCursor | CWSaveUnder);
#else
  valuemask = (CWBackPixel | CWEventMask | CWCursor);
#endif
  attributes.background_pixel = Scr.MenuColors.back;
  attributes.event_mask = (ExposureMask | EnterWindowMask);
  attributes.cursor = Scr.ScwmCursors[MENU];
#ifndef NO_SAVEUNDERS
  attributes.save_under = TRUE;
#endif
  if (mr->w != None)
    XDestroyWindow(dpy, mr->w);

  mr->width = mr->width0 + mr->width + mr->width2;
  mr->w = XCreateWindow(dpy, Scr.Root, 0, 0, (unsigned int) (mr->width),
			(unsigned int) mr->height, (unsigned int) 0,
			CopyFromParent, (unsigned int) InputOutput,
			(Visual *) CopyFromParent,
			valuemask, &attributes);
  XSaveContext(dpy, mr->w, MenuContext, (caddr_t) mr);
  return;
}

/***********************************************************************
 * Procedure:
 *	scanForHotkeys - Look for hotkey markers in a MenuItem
 * 							(pete@tecc.co.uk)
 * 
 * Inputs:
 *	it	- MenuItem to scan
 * 	which 	- +1 to look in it->item1 and -1 to look in it->item2.
 *
 ***********************************************************************/
void 
scanForHotkeys(MenuItem * it, int which)
{
  char *start, *txt;

  start = (which > 0) ? it->item : it->item2;	/* Get start of string  */
  for (txt = start; *txt != '\0'; txt++) {
    /* Scan whole string      */
    if (*txt == '&') {		/* A hotkey marker?                     */
      if (txt[1] == '&') {	/* Just an escaped &                    */
	char *tmp;		/* Copy the string down over it */

	for (tmp = txt; *tmp != '\0'; tmp++)
	  tmp[0] = tmp[1];
	continue;		/* ...And skip to the key char          */
      }
      /* It's a hot key marker - work out the offset value          */
      it->hotkey = (1 + (txt - start)) * which;
      for (; *txt != '\0'; txt++)
	txt[0] = txt[1];	/* Copy down..  */
      return;			/* Only one hotkey per item...  */
    }
  }
  it->hotkey = 0;		/* No hotkey found.  Set offset to zero */
}



void 
scanForPixmap(char *instring, Picture ** p, char identifier)
{
  char *tstart, *txt, *save_instring;
  int i;
  Picture *pp;
  char name[100];

  /* save instring in case can't find pixmap */
  save_instring = (char *) safemalloc(strlen(instring) + 1);
  strcpy(save_instring, instring);

  /* Scan whole string  */
  for (txt = instring; *txt != '\0'; txt++) {
    /* A hotkey marker? */
    if (*txt == identifier) {
      /* Just an escaped &  */
      if (txt[1] == identifier) {
	char *tmp;		/* Copy the string down over it */

	for (tmp = txt; *tmp != '\0'; tmp++)
	  tmp[0] = tmp[1];
	continue;		/* ...And skip to the key char          */
      }
      /* It's a hot key marker - work out the offset value          */
      tstart = txt;
      txt++;
      i = 0;
      while ((*txt != identifier) && (*txt != '\0') && (i < 99)) {
	name[i] = *txt;
	txt++;
	i++;
      }
      name[i] = 0;

      /* Next, check for a color pixmap */
      pp = CachePicture(dpy, Scr.Root, szImagePath, name);
      if (*txt != '\0')
	txt++;
      while (*txt != '\0') {
	*tstart++ = *txt++;
      }
      *tstart = 0;
      if (!pp)
	strcpy(instring, save_instring);
      else
	*p = pp;
      free(save_instring);
      return;
    }
  }
}



/***********************************************************************
 *
 *  Procedure:
 *	AddToMenu - add an item to a root menu
 *
 *  Returned Value:
 *	(MenuItem *)
 *
 *  Inputs:
 *	menu	- pointer to the root menu to add the item
 *	item	- the text to appear in the menu
 *	action	- the string to possibly execute
 *	func	- the numeric function
 *
 * ckh - need to add boolean to say whether or not to expand for pixmaps,
 *       so built in window list can handle windows w/ * and % in title.
 *
 ***********************************************************************/
void 
AddToMenu(MenuRoot * menu, char *item, char *action)
{
  MenuItem *tmp;
  char *start, *end;

  if (item == NULL)
    return;

  tmp = (MenuItem *) safemalloc(sizeof(MenuItem));
  if (menu->first == NULL) {
    menu->first = tmp;
    tmp->prev = NULL;
  } else {
    menu->last->next = tmp;
    tmp->prev = menu->last;
  }
  menu->last = tmp;
  tmp->picture = NULL;
  tmp->lpicture = NULL;

  /* skip leading spaces */
  /*while(isspace(*item)&&(item != NULL))
     item++; */
  /* up to first tab goes in "item" field */
  start = item;
  end = item;
  while ((*end != '\t') && (*end != 0))
    end++;
  tmp->item = safemalloc(end - start + 1);
  strncpy(tmp->item, start, end - start);
  tmp->item[end - start] = 0;
  tmp->item2 = NULL;
  if (*end == '\t') {
    start = end + 1;
    while (*end != 0)
      end++;
    if (end - start != 0) {
      tmp->item2 = safemalloc(end - start + 1);
      strncpy(tmp->item2, start, end - start);
      tmp->item2[end - start] = 0;
    }
  }
  if (item != (char *) 0) {
    scanForPixmap(tmp->item, &tmp->picture, '*');
    scanForPixmap(tmp->item, &tmp->lpicture, '%');
    scanForHotkeys(tmp, 1);	/* pete@tecc.co.uk */
    tmp->strlen = strlen(tmp->item);
  } else
    tmp->strlen = 0;

  if (tmp->item2 != (char *) 0) {
    if (!tmp->picture)
      scanForPixmap(tmp->item2, &tmp->picture, '*');
    if (!tmp->lpicture)
      scanForPixmap(tmp->item2, &tmp->lpicture, '%');
    if (tmp->hotkey == 0)
      scanForHotkeys(tmp, -1);	/* pete@tecc.co.uk */
    tmp->strlen2 = strlen(tmp->item2);
  } else
    tmp->strlen2 = 0;

  tmp->action = stripcpy(action);
  tmp->next = NULL;
  tmp->state = 0;
#ifdef GJB_FIXME_COMMENT
  tmp->func_type = find_func_type(tmp->action);
#endif
  tmp->func_type = F_BEEP; /*FIXGJB: just cannot be F_NOP */
  if (STREQ(tmp->action, "SchemeMenu")) {
    tmp->func_type = F_POPUP;
  }
  if (STREQ(tmp->action, "Nop")) {
    tmp->func_type = F_NOP;
  }
  tmp->item_num = menu->items++;
}

/***********************************************************************
 *
 *  Procedure:
 *	NewMenuRoot - create a new menu root
 *
 *  Returned Value:
 *	(MenuRoot *)
 *
 *  Inputs:
 *	name	- the name of the menu root
 *
 ***********************************************************************/
MenuRoot *
NewMenuRoot(char *name, int junk)
{
  MenuRoot *tmp;

  tmp = (MenuRoot *) safemalloc(sizeof(MenuRoot));
  if (junk == 0)
    tmp->func = F_POPUP;
  else
    tmp->func = F_FUNCTION;

  tmp->name = stripcpy(name);
  tmp->first = NULL;
  tmp->last = NULL;
  tmp->items = 0;
  tmp->width = 0;
  tmp->width2 = 0;
  tmp->w = None;
  tmp->next = Scr.AllMenus;
  Scr.AllMenus = tmp;
  return (tmp);
}



/***********************************************************************
 * change by KitS@bartley.demon.co.uk to correct popups off title buttons
 *
 *  Procedure:
 *ButtonPosition - find the actual position of the button
 *                 since some buttons may be disabled
 *
 *  Returned Value:
 *The button count from left or right taking in to account
 *that some buttons may not be enabled for this window
 *
 *  Inputs:
 *      context - context as per the global Context
 *      t       - the window (ScwmWindow) to test against
 *
 ***********************************************************************/
int 
ButtonPosition(int context, ScwmWindow * t)
{
  int i;
  int buttons = -1;

  if (context & C_RALL) {
    for (i = 0; i < Scr.nr_right_buttons; i++) {
      if (t->right_w[i]) {
	buttons++;
      }
      /* is this the button ? */
      if (((1 << i) * C_R1) & context)
	return (buttons);
    }
  } else {
    for (i = 0; i < Scr.nr_left_buttons; i++) {
      if (t->left_w[i]) {
	buttons++;
      }
      /* is this the button ? */
      if (((1 << i) * C_L1) & context)
	return (buttons);
    }
  }
  /* you never know... */
  return 0;
}

void 
init_menus(void)
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

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
