/* $Id$ */

#ifdef GJB_DELETION_COMMENT
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


extern char *PixmapPath;
extern char *IconPath;
ScwmDecor *last_decor = NULL, *cur_decor = NULL;
char *last_menu = NULL;

/* button state strings must match the enumerated states */
static char *button_states[MaxButtonState] =
{
  "ActiveUp",
  "ActiveDown",
  "Inactive",
};

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


/* For Ultrix 4.2 */
#include <sys/types.h>
#include <sys/time.h>

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


