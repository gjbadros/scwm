/****************************************************************************
 * This module is all original code 
 * by Rob Nation 
 * Copyright 1993, Robert Nation
 *     You may use this code for any purpose, as long as the original
 *     copyright remains in the source code and all documentation
 ****************************************************************************/

/***********************************************************************
 *
 * scwm built-in functions
 *
 ***********************************************************************/

#include <config.h>

#include <stdio.h>
#include <signal.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>

#include "scwm.h"
#include "menus.h"
#include "misc.h"
#include "parse.h"
#include "screen.h"
#include "module.h"

#undef MS_DELETION_COMMENT

extern XEvent Event;
extern ScwmWindow *Tmp_win;
extern int menuFromFrameOrWindowOrTitlebar;
Bool desperate;

struct functions func_config[] =
{
#ifdef MULTISTYLE
  {"AddButtonStyle",AddButtonStyle,  F_ADD_BUTTON_STYLE,    FUNC_NO_WINDOW},
#ifdef EXTENDED_TITLESTYLE
  {"AddTitleStyle",AddTitleStyle,    F_ADD_TITLE_STYLE,     FUNC_NO_WINDOW},
#endif
#endif
#ifdef USEDECOR
  {"AddToDecor",   add_item_to_decor,F_ADD_DECOR,	    FUNC_NO_WINDOW},
#endif

#if MS_DELETION_COMMENT
  {"AddToMenu",    add_item_to_menu, F_ADDMENU,             FUNC_NO_WINDOW},
  {"AddToFunc",    add_item_to_func, F_ADDFUNC,             FUNC_NO_WINDOW},
  {"AddModuleConfig",  AddModConfig, F_ADD_MOD,             FUNC_NO_WINDOW},
  {"Beep",         Bell,             F_BEEP,                FUNC_NO_WINDOW},
#endif /* MS_DELETION_COMMENT */

#ifdef BORDERSTYLE
  {"BorderStyle",  SetBorderStyle,   F_BORDERSTYLE,         FUNC_NO_WINDOW},
#endif
  {"ButtonStyle",  ButtonStyle,      F_BUTTON_STYLE,        FUNC_NO_WINDOW},
#ifdef USEDECOR
  {"ChangeDecor",  ChangeDecor,      F_CHANGE_DECOR,        FUNC_NEEDS_WINDOW},
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT
  {"ClickTime",    SetClick,         F_CLICK,               FUNC_NO_WINDOW},
  {"Close",        close_function,   F_CLOSE,               FUNC_NEEDS_WINDOW},
  {"ColormapFocus",SetColormapFocus, F_COLORMAP_FOCUS,      FUNC_NO_WINDOW},
  {"CursorMove",   movecursor,       F_MOVECURSOR,          FUNC_NO_WINDOW},
  {"Delete",       delete_function,  F_DELETE,              FUNC_NEEDS_WINDOW},
  {"DesktopSize",  SetDeskSize,       F_SETDESK,            FUNC_NO_WINDOW},
  {"Desk",         changeDesks_func, F_DESK,                FUNC_NO_WINDOW},
  {"Destroy",      destroy_function, F_DESTROY,             FUNC_NEEDS_WINDOW},
#endif /* MS_DELETION_COMMENT */

#ifdef USEDECOR
  {"DestroyDecor", DestroyDecor,     F_DESTROY_DECOR,	    FUNC_NO_WINDOW},
#endif

#if MS_DELETION_COMMENT
  {"DestroyFunc",  destroy_menu,     F_DESTROY_MENU,        FUNC_NO_WINDOW},
  {"DestroyMenu",  destroy_menu,     F_DESTROY_MENU,        FUNC_NO_WINDOW},
  {"DestroyModuleConfig", DestroyModConfig, F_DESTROY_MOD,  FUNC_NO_WINDOW},
  {"Echo",         echo_func,        F_ECHO,                FUNC_NO_WINDOW},
  {"EdgeScroll",   SetEdgeScroll,    F_EDGE_SCROLL,         FUNC_NO_WINDOW},
  {"EdgeResistance",SetEdgeResistance,F_EDGE_RES,           FUNC_NO_WINDOW},
  {"Exec",         exec_function,    F_EXEC,                FUNC_NO_WINDOW},
  {"ExecUseSHELL", exec_setup,       F_EXEC_SETUP,          FUNC_NO_WINDOW},
#endif /* MS_DELETION_COMMENT */

  {"FlipFocus",    flip_focus_func,  F_FLIP_FOCUS,          FUNC_NEEDS_WINDOW},

#if MS_DELETION_COMMENT
  {"Focus",        focus_func,       F_FOCUS,               FUNC_NEEDS_WINDOW},
  {"Function",     ComplexFunction,  F_FUNCTION,            FUNC_NO_WINDOW},
#endif /* MS_DELETION_COMMENT */

  {"GlobalOpts",   SetGlobalOptions, F_GLOBAL_OPTS,         FUNC_NO_WINDOW},

#if MS_DELETION_COMMENT
  {"GotoPage",     goto_page_func,   F_GOTO_PAGE,           FUNC_NO_WINDOW},
  {"HilightColor", SetHiColor,       F_HICOLOR,             FUNC_NO_WINDOW},
  {"Iconify",      iconify_function, F_ICONIFY,             FUNC_NEEDS_WINDOW},
  {"IconFont",     LoadIconFont,     F_ICONFONT,            FUNC_NO_WINDOW},
  {"IconPath",     setIconPath,      F_ICON_PATH,           FUNC_NO_WINDOW},
  {"Key",          ParseKeyEntry,    F_KEY,                 FUNC_NO_WINDOW},
  {"Lower",        lower_function,   F_LOWER,               FUNC_NEEDS_WINDOW},
  {"Maximize",     Maximize,         F_MAXIMIZE,            FUNC_NEEDS_WINDOW},
  {"Menustyle",    SetMenuStyle,     F_MENUSTYLE,           FUNC_NO_WINDOW},
#endif /* MS_DELETION_COMMENT */

  {"Menu",         staysup_func,     F_STAYSUP,             FUNC_POPUP},

#if MS_DELETION_COMMENT
  {"ModulePath",   setModulePath,    F_MODULE_PATH,         FUNC_NO_WINDOW},
  {"Module",       executeModule,    F_MODULE,              FUNC_NO_WINDOW},
  {"Mouse",        ParseMouseEntry,  F_MOUSE,               FUNC_NO_WINDOW},
  {"Move",         move_window,      F_MOVE,                FUNC_NEEDS_WINDOW},
#endif /* MS_DELETION_COMMENT */

  {"Nop",          Nop_func,         F_NOP,                 FUNC_NOP},

#if MS_DELETION_COMMENT
  {"Next",         NextFunc,         F_NEXT,                FUNC_NO_WINDOW},
  {"None",         NoneFunc,         F_NONE,                FUNC_NO_WINDOW},
  {"OpaqueMoveSize", SetOpaque,      F_OPAQUE,              FUNC_NO_WINDOW},
  {"PipeRead",     PipeRead,         F_READ,                FUNC_NO_WINDOW},
#ifdef XPM
  {"PixmapPath",   setPixmapPath,    F_PIXMAP_PATH,         FUNC_NO_WINDOW},
#endif
#endif /* MS_DELETION_COMMENT */

  {"PopUp",        popup_func,       F_POPUP,               FUNC_POPUP},

#if MS_DELETION_COMMENT
  {"Prev",         PrevFunc,         F_PREV,                FUNC_NO_WINDOW},
#endif /* MS_DELETION_COMMENT */

  {"Quit",         quit_func,        F_QUIT,                FUNC_NO_WINDOW},
  {"QuitScreen",   quit_screen_func, F_QUIT_SCREEN,         FUNC_NO_WINDOW},

#if MS_DELETION_COMMENT
  {"Raise_it",     raise_it_func,    F_RAISE_IT,            FUNC_NO_WINDOW},
  {"RaiseLower",   raiselower_func,  F_RAISELOWER,          FUNC_NEEDS_WINDOW},
  {"Raise",        raise_function,   F_RAISE,               FUNC_NEEDS_WINDOW},
  {"Read",         ReadFile,         F_READ,                FUNC_NO_WINDOW},
  {"Recapture",    Recapture,        F_RECAPTURE,           FUNC_NO_WINDOW},
  {"Refresh",      refresh_function, F_REFRESH,             FUNC_NO_WINDOW},
  {"RefreshWindow",refresh_win_function, F_REFRESH,         FUNC_NEEDS_WINDOW},
  {"Resize",       resize_window,    F_RESIZE,              FUNC_NEEDS_WINDOW},
  {"Restart",      restart_function, F_RESTART,             FUNC_NO_WINDOW},
  {"KillModule",   module_zapper,    F_ZAP,                 FUNC_NO_WINDOW},
  {"SendToModule", SendStrToModule,  F_SEND_STRING,         FUNC_NO_WINDOW},
  {"Scroll",       scroll,           F_SCROLL,              FUNC_NO_WINDOW},
  {"Stick",        stick_function,   F_STICK,               FUNC_NEEDS_WINDOW},
  {"Send_WindowList",send_list_func, F_SEND_WINDOW_LIST,    FUNC_NO_WINDOW},
  {"Send_ConfigInfo",SendDataToModule, F_CONFIG_LIST,       FUNC_NO_WINDOW},
  {"set_mask",     set_mask_function,F_SET_MASK,            FUNC_NO_WINDOW}, 
  {"Style",        ProcessNewStyle,  F_STYLE,               FUNC_NO_WINDOW},
#endif /* MS_DELETION_COMMENT */

  {"Title",        Nop_func,         F_TITLE,               FUNC_TITLE},
  {"Titlestyle",   SetTitleStyle,    F_TITLESTYLE,          FUNC_NO_WINDOW},
  {"UpdateDecor",  UpdateDecor,      F_UPDATE_DECOR,        FUNC_NO_WINDOW},

#if MS_DELETION_COMMENT
  {"Wait",         wait_func,        F_WAIT,                FUNC_NO_WINDOW},
  {"WarpToWindow", warp_func,        F_WARP,                FUNC_NEEDS_WINDOW},
  {"WindowFont",   LoadWindowFont,   F_WINDOWFONT,          FUNC_NO_WINDOW},
  {"WindowId",     WindowIdFunc,     F_WINDOWID,            FUNC_NO_WINDOW},
  {"WindowList",   do_windowList,    F_WINDOWLIST,          FUNC_NO_WINDOW},
#ifdef WINDOWSHADE
  {"WindowShade",  WindowShade,      F_WINDOW_SHADE,        FUNC_NEEDS_WINDOW},
#endif
  {"WindowsDesk",  changeWindowsDesk,F_CHANGE_WINDOWS_DESK, FUNC_NEEDS_WINDOW},
  {"XORValue",     SetXOR,           F_XOR,                 FUNC_NO_WINDOW},
  {"+",            add_another_item, F_ADDMENU2,            FUNC_NO_WINDOW},
#endif /* MS_DELETION_COMMENT */

  {"",                    0,0}
};
  

#if MS_DELETION_COMMENT
/***********************************************************************
 *
 *  Procedure:
 *	ExecuteFunction - execute a scwm built in function
 *
 *  Inputs:
 *	func	- the function to execute
 *	action	- the menu action to execute 
 *	w	- the window to execute this function on
 *	tmp_win	- the scwm window structure
 *	event	- the event that caused the function
 *	context - the context in which the button was pressed
 *      val1,val2 - the distances to move in a scroll operation 
 *
 ***********************************************************************/
void ExecuteFunction(char *Action, ScwmWindow *tmp_win, XEvent *eventp,
		     unsigned long context, int Module)
{
  Window w;
  int matched,j;
  char *function;
  char *action, *taction;
  char *arguments[10];

  for(j=0;j<10;j++)
    arguments[j] = NULL;

  if(tmp_win == NULL)
    w = Scr.Root;
  else
    w = tmp_win->w;

  if((tmp_win) &&(eventp))
    w = eventp->xany.window;
  if((tmp_win)&&(eventp->xbutton.subwindow != None)&&
     (eventp->xany.window != tmp_win->w))
    w = eventp->xbutton.subwindow;

  taction = expand(Action,arguments,tmp_win);
  action = GetNextToken(taction,&function);
  j=0;
  matched = FALSE;

  while((!matched)&&(strlen(func_config[j].keyword)>0))
    {
      if(mystrcasecmp(function,func_config[j].keyword)==0)
	{
	  matched=TRUE;
	  /* found key word */
	  func_config[j].action(eventp,w,tmp_win,context,action, &Module);
	}
      else
	j++;
    }

  if(!matched)
    {
      desperate = 1;
      ComplexFunction(eventp,w,tmp_win,context,taction, &Module);
      if(desperate)
	executeModule(eventp,w,tmp_win,context,taction, &Module);
      desperate = 0;
    }
  
  /* Only wait for an all-buttons-up condition after calls from
   * regular built-ins, not from complex-functions or modules. */
  if(Module == -1)
    WaitForButtonsUp();

  free(function);
  if(taction != NULL)
    free(taction);
  return;
}

#endif /* MS_DELETION_COMMENT */


int find_func_type(char *action)
{
  int j, len = 0;
  char *endtok = action;
  Bool matched;
  while (*endtok&&!isspace(*endtok))++endtok;
  len = endtok - action;
  j=0;
  matched = FALSE;
  while((!matched)&&(strlen(func_config[j].keyword)>0))
    {
      int mlen = strlen(func_config[j].keyword);
      if((mlen == len)
	 && (mystrncasecmp(action,func_config[j].keyword,mlen)==0))
      {
	  matched=TRUE;
	  /* found key word */
	  return (int)func_config[j].code;
	}
      else
	j++;
    }
  /* No clue what the function is. Just return "BEEP" */
  return F_BEEP;
}
