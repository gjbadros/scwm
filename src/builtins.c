

/****************************************************************************
 * This module is all original code 
 * by Rob Nation 
 * Copyright 1993, Robert Nation
 *     You may use this code for any purpose, as long as the original
 *     copyright remains in the source code and all documentation
 ****************************************************************************/

#include <config.h>

#include <stdio.h>
#include <signal.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <errno.h>

#include "scwm.h"
#include "menus.h"
#include "misc.h"
#include "parse.h"
#include "screen.h"
#include "module.h"
#include "string_token.h"


#ifdef GJB_DELETION_COMMENT
/* button state strings must match the enumerated states */
static char *button_states[MaxButtonState] =
{
  "ActiveUp",
  "ActiveDown",
  "Inactive",
};
#endif

/* These should be preserved here, everything else can die
   FlipFocus
   GlobalOpts
   Nop
   None
   Quit
   QuitScreen
   Title
   Menu
   Popup
 */


/***********************************************************************
 *
 *  Procedure:
 *	DeferExecution - defer the execution of a function to the
 *	    next button press if the context is C_ROOT
 *
 *  Inputs:
 *      eventp  - pointer to XEvent to patch up
 *      w       - pointer to Window to patch up
 *      tmp_win - pointer to ScwmWindow Structure to patch up
 *	context	- the context in which the mouse button was pressed
 *	func	- the function to defer
 *	cursor	- the cursor to display while waiting
 *      finishEvent - ButtonRelease or ButtonPress; tells what kind of event to
 *                    terminate on.
 *
 ***********************************************************************/
int 
DeferExecution(XEvent * eventp, Window * w, ScwmWindow ** tmp_win,
	       unsigned long *context, int cursor, int FinishEvent)
{
  Bool fDone = False;
  Bool fFinished = False;
  Window dummy;
  Window original_w;

  original_w = *w;

  if ((*context != C_ROOT) && (*context != C_NO_CONTEXT)) {
    if ((FinishEvent == ButtonPress) || ((FinishEvent == ButtonRelease) &&
					 (eventp->type != ButtonPress))) {
      return False;
    }
  }

  if (!GrabEm(cursor)) {
    /* FIXGJB: call a scheme hook, not XBell */
    XBell(dpy, Scr.screen);
    return True;
  }

  while (!fFinished) {
    fDone = False;
    /* block until there is an event */
/* FIXGJB: hard to know why this was looking for so many different
   events; I think we just need those in the new call below --10/25/97 gjb
    XMaskEvent(dpy, ButtonPressMask | ButtonReleaseMask |
	       ExposureMask | KeyPressMask | VisibilityChangeMask |
	       ButtonMotionMask | PointerMotionMask, eventp); */
    XMaskEvent(dpy, ButtonPressMask | ButtonReleaseMask |
	       ExposureMask | KeyPressMask | VisibilityChangeMask, eventp);
    StashEventTime(eventp);

    if (eventp->type == KeyPress)
      Keyboard_shortcuts(eventp, FinishEvent);
    if (eventp->type == FinishEvent)
      fFinished = True;
    if (eventp->type == ButtonPress) {
      XMaskEvent(dpy, ButtonPressMask | ButtonReleaseMask |
		 ExposureMask | KeyPressMask | VisibilityChangeMask |
		 ButtonMotionMask | PointerMotionMask, eventp);
      XAllowEvents(dpy, ReplayPointer, CurrentTime);
      fDone = True;
    }
    if (eventp->type == ButtonRelease || eventp->type == KeyPress) {
      fDone = True;
    }
    if (!fDone) {
      DispatchEvent();
    }
  }


  *w = eventp->xany.window;
  if (((*w == Scr.Root) || (*w == Scr.NoFocusWin))
      && (eventp->xbutton.subwindow != (Window) 0)) {
    *w = eventp->xbutton.subwindow;
    eventp->xany.window = *w;
  }
  if (*w == Scr.Root) {
    *context = C_ROOT;
    XBell(dpy, Scr.screen);
    UngrabEm();
    return TRUE;
  }
  if (XFindContext(dpy, *w, ScwmContext, (caddr_t *) tmp_win) == XCNOENT) {
    *tmp_win = NULL;
    XBell(dpy, Scr.screen);
    UngrabEm();
    return (TRUE);
  }
  if (*w == (*tmp_win)->Parent)
    *w = (*tmp_win)->w;

  if (original_w == (*tmp_win)->Parent)
    original_w = (*tmp_win)->w;

  /* this ugly mess attempts to ensure that the release and press
   * are in the same window. */
  if ((*w != original_w) && (original_w != Scr.Root) &&
      (original_w != None) && (original_w != Scr.NoFocusWin))
    if (!((*w == (*tmp_win)->frame) &&
	  (original_w == (*tmp_win)->w))) {
      *context = C_ROOT;
      XBell(dpy, Scr.screen);
      UngrabEm();
      return TRUE;
    }
  *context = GetContext(*tmp_win, eventp, &dummy);

  UngrabEm();
  return FALSE;
}




/**************************************************************************
 *
 * Moves focus to specified window 
 *
 *************************************************************************/
void 
FocusOn(ScwmWindow * t, int DeIconifyOnly)
{
#ifndef NON_VIRTUAL
  int dx, dy;
  int cx, cy;

#endif
  int x, y;

  if (t == (ScwmWindow *) 0)
    return;

  if (t->Desk != Scr.CurrentDesk) {
    changeDesks(0, t->Desk);
  }
#ifndef NON_VIRTUAL
  if (t->flags & ICONIFIED) {
    cx = t->icon_xl_loc + t->icon_w_width / 2;
    cy = t->icon_y_loc + t->icon_p_height + ICON_HEIGHT / 2;
  } else {
    cx = t->frame_x + t->frame_width / 2;
    cy = t->frame_y + t->frame_height / 2;
  }

  dx = (cx + Scr.Vx) / Scr.MyDisplayWidth * Scr.MyDisplayWidth;
  dy = (cy + Scr.Vy) / Scr.MyDisplayHeight * Scr.MyDisplayHeight;

  MoveViewport(dx, dy, True);
#endif

  if (t->flags & ICONIFIED) {
    x = t->icon_xl_loc + t->icon_w_width / 2;
    y = t->icon_y_loc + t->icon_p_height + ICON_HEIGHT / 2;
  } else {
    x = t->frame_x;
    y = t->frame_y;
  }
  RaiseWindow(t);
  KeepOnTop();

  /* If the window is still not visible, make it visible! */
  if (((t->frame_x + t->frame_height) < 0) || (t->frame_y + t->frame_width < 0) ||
  (t->frame_x > Scr.MyDisplayWidth) || (t->frame_y > Scr.MyDisplayHeight)) {
    SetupFrame(t, 0, 0, t->frame_width, t->frame_height, False);
    if (!(t->flags & ClickToFocus))
      XWarpPointer(dpy, None, Scr.Root, 0, 0, 0, 0, 2, 2);
  }
  UngrabEm();
  SetFocus(t->w, t, 0);
}



/**************************************************************************
 *
 * Moves pointer to specified window 
 *
 *************************************************************************/
void 
WarpOn(ScwmWindow * t, int warp_x, int x_unit, int warp_y, int y_unit)
{
#ifndef NON_VIRTUAL
  int dx, dy;
  int cx, cy;

#endif
  int x, y;

  if (t == (ScwmWindow *) 0 || (t->flags & ICONIFIED && t->icon_w == None))
    return;

  if (t->Desk != Scr.CurrentDesk) {
    changeDesks(0, t->Desk);
  }
#ifndef NON_VIRTUAL
  if (t->flags & ICONIFIED) {
    cx = t->icon_xl_loc + t->icon_w_width / 2;
    cy = t->icon_y_loc + t->icon_p_height + ICON_HEIGHT / 2;
  } else {
    cx = t->frame_x + t->frame_width / 2;
    cy = t->frame_y + t->frame_height / 2;
  }

  dx = (cx + Scr.Vx) / Scr.MyDisplayWidth * Scr.MyDisplayWidth;
  dy = (cy + Scr.Vy) / Scr.MyDisplayHeight * Scr.MyDisplayHeight;

  MoveViewport(dx, dy, True);
#endif

  if (t->flags & ICONIFIED) {
    x = t->icon_xl_loc + t->icon_w_width / 2 + 2;
    y = t->icon_y_loc + t->icon_p_height + ICON_HEIGHT / 2 + 2;
  } else {
    if (x_unit != Scr.MyDisplayWidth)
      x = t->frame_x + 2 + warp_x;
    else
      x = t->frame_x + 2 + (t->frame_width - 4) * warp_x / 100;
    if (y_unit != Scr.MyDisplayHeight)
      y = t->frame_y + 2 + warp_y;
    else
      y = t->frame_y + 2 + (t->frame_height - 4) * warp_y / 100;
  }
  if (warp_x >= 0 && warp_y >= 0) {
    XWarpPointer(dpy, None, Scr.Root, 0, 0, 0, 0, x, y);
  }
  RaiseWindow(t);
  KeepOnTop();

  /* If the window is still not visible, make it visible! */
  if (((t->frame_x + t->frame_height) < 0) || (t->frame_y + t->frame_width < 0) ||
  (t->frame_x > Scr.MyDisplayWidth) || (t->frame_y > Scr.MyDisplayHeight)) {
    SetupFrame(t, 0, 0, t->frame_width, t->frame_height, False);
    XWarpPointer(dpy, None, Scr.Root, 0, 0, 0, 0, 2, 2);
  }
  UngrabEm();
}




/* For Ultrix 4.2 */
#include <sys/types.h>
#include <sys/time.h>


MenuRoot *
FindPopup(char *action)
{
  char *tmp;
  MenuRoot *mr;

  GetNextToken(action, &tmp);

  if (tmp == NULL)
    return NULL;

  mr = Scr.AllMenus;
  while (mr != NULL) {
    if (mr->name != NULL)
      if (strcasecmp(tmp, mr->name) == 0) {
	free(tmp);
	return mr;
      }
    mr = mr->next;
  }
  free(tmp);
  return NULL;

}



ScwmDecor *last_decor = NULL, *cur_decor = NULL;

char *last_menu = NULL;






void 
Nop_func(XEvent * eventp, Window w, ScwmWindow * tmp_win, unsigned long context,
	 char *action, int *Module)
{

}

void 
flip_focus_func(XEvent * eventp, Window w, ScwmWindow * tmp_win,
		unsigned long context, char *action, int *Module)
{

  if (DeferExecution(eventp, &w, &tmp_win, &context, SELECT, ButtonRelease))
    return;

  /* Reorder the window list */
  if (Scr.Focus) {
    if (Scr.Focus->next)
      Scr.Focus->next->prev = Scr.Focus->prev;
    if (Scr.Focus->prev)
      Scr.Focus->prev->next = Scr.Focus->next;
    Scr.Focus->next = Scr.ScwmRoot.next;
    Scr.Focus->prev = &Scr.ScwmRoot;
    if (Scr.ScwmRoot.next)
      Scr.ScwmRoot.next->prev = Scr.Focus;
    Scr.ScwmRoot.next = Scr.Focus;
  }
  if (tmp_win != Scr.Focus) {
    if (tmp_win->next)
      tmp_win->next->prev = tmp_win->prev;
    if (tmp_win->prev)
      tmp_win->prev->next = tmp_win->next;
    tmp_win->next = Scr.ScwmRoot.next;
    tmp_win->prev = &Scr.ScwmRoot;
    if (Scr.ScwmRoot.next)
      Scr.ScwmRoot.next->prev = tmp_win;
    Scr.ScwmRoot.next = tmp_win;
  }
  FocusOn(tmp_win, 0);

}



void 
popup_func(XEvent * eventp, Window w, ScwmWindow * tmp_win,
	   unsigned long context, char *action, int *Module)
{
  MenuRoot *menu;
  extern int menuFromFrameOrWindowOrTitlebar;

  menu = FindPopup(action);
  if (menu == NULL) {
    scwm_msg(ERR, "popup_func", "No such menu %s", action);
    return;
  }
  ActiveItem = NULL;
  ActiveMenu = NULL;
  menuFromFrameOrWindowOrTitlebar = FALSE;
  do_menu(menu, 0);
}

#ifdef GJB_DELETION_COMMENT
void 
staysup_func(XEvent * eventp, Window w, ScwmWindow * tmp_win,
	     unsigned long context, char *action, int *Module)
{
  MenuRoot *menu;
  extern int menuFromFrameOrWindowOrTitlebar;
  char *default_action = NULL, *menu_name = NULL;
  extern int menu_aborted;

  action = GetNextToken(action, &menu_name);
  GetNextToken(action, &default_action);
  menu = FindPopup(menu_name);
  if (menu == NULL) {
    if (menu_name != NULL) {
      scwm_msg(ERR, "staysup_func", "No such menu %s", menu_name);
      free(menu_name);
    }
    if (default_action != NULL)
      free(default_action);
    return;
  }
  ActiveItem = NULL;
  ActiveMenu = NULL;
  menuFromFrameOrWindowOrTitlebar = FALSE;

  /* See bottom of windows.c for rationale behind this */
  if (eventp->type == ButtonPress)
    do_menu(menu, 1);
  else
    do_menu(menu, 0);

  if (menu_name != NULL)
    free(menu_name);
  if ((menu_aborted) && (default_action != NULL))
    ExecuteFunction(default_action, tmp_win, eventp, context, *Module);
  if (default_action != NULL)
    free(default_action);
}
#endif


void 
quit_func(XEvent * eventp, Window w, ScwmWindow * tmp_win,
	  unsigned long context, char *action, int *Module)
{
  if (master_pid != getpid())
    kill(master_pid, SIGTERM);
  Done(0, NULL);
}

void 
quit_screen_func(XEvent * eventp, Window w, ScwmWindow * tmp_win,
		 unsigned long context, char *action, int *Module)
{
  Done(0, NULL);
}






extern char *PixmapPath;



extern char *IconPath;





#ifdef GJB_DELETION_COMMENT
void 
SetGlobalOptions(XEvent * eventp, Window junk, ScwmWindow * tmp_win,
		 unsigned long context, char *action, int *Module)
{
  char *opt, *opts;

  if (!action || !action[0])
    return;
  else
    opts = strdup(action);

  /* scwm_msg(DBG,"SetGlobalOptions","init action == '%s'\n",action); */

  while ((opt = GetToken(&opts))) {
    /* scwm_msg(DBG,"SetGlobalOptions"," opt == '%s'\n",opt); */
    /* scwm_msg(DBG,"SetGlobalOptions"," remaining == '%s'\n",opts?opts:"(NULL)"); */
    if (StrEquals(opt, "SMARTPLACEMENTISREALLYSMART")) {
      Scr.SmartPlacementIsClever = True;
    } else if (StrEquals(opt, "SMARTPLACEMENTISNORMAL")) {
      Scr.SmartPlacementIsClever = False;
    } else if (StrEquals(opt, "CLICKTOFOCUSDOESNTPASSCLICK")) {
      Scr.ClickToFocusPassesClick = False;
    } else if (StrEquals(opt, "CLICKTOFOCUSPASSESCLICK")) {
      Scr.ClickToFocusPassesClick = True;
    } else if (StrEquals(opt, "CLICKTOFOCUSDOESNTRAISE")) {
      Scr.ClickToFocusRaises = False;
    } else if (StrEquals(opt, "CLICKTOFOCUSRAISES")) {
      Scr.ClickToFocusRaises = True;
    } else if (StrEquals(opt, "MOUSEFOCUSCLICKDOESNTRAISE")) {
      Scr.MouseFocusClickRaises = False;
    } else if (StrEquals(opt, "MOUSEFOCUSCLICKRAISES")) {
      Scr.MouseFocusClickRaises = True;
    } else
      scwm_msg(ERR, "GlobalOpts", "Unknown Global Option '%s'", opt);

    if (opt)			/* should never be null, but checking anyways... */
      free(opt);
  }
  if (opts)			/* should be empty at this point... */
    free(opts);
}

#endif


