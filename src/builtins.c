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

#undef MS_DELETION_COMMENT /* Undefine explicitly just in case. */

static char *exec_shell_name="/bin/sh";
/* button state strings must match the enumerated states */
static char  *button_states[MaxButtonState]={
    "ActiveUp",
#ifdef ACTIVEDOWN_BTNS
    "ActiveDown",
#endif
#ifdef INACTIVE_BTNS
    "Inactive",
#endif
};

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
int DeferExecution(XEvent *eventp, Window *w,ScwmWindow **tmp_win,
		   unsigned long *context, int cursor, int FinishEvent)

{
  int done;
  int finished = 0;
  Window dummy;
  Window original_w;

  original_w = *w;

  if((*context != C_ROOT)&&(*context != C_NO_CONTEXT))
  {
    if((FinishEvent == ButtonPress)||((FinishEvent == ButtonRelease) &&
                                      (eventp->type != ButtonPress)))
    {
      return FALSE;
    }
  }
  if(!GrabEm(cursor))
  {
    XBell(dpy,Scr.screen);
    return True;
  }
  
  while (!finished)
  {
    done = 0;
    /* block until there is an event */
    XMaskEvent(dpy, ButtonPressMask | ButtonReleaseMask |
               ExposureMask |KeyPressMask | VisibilityChangeMask |
               ButtonMotionMask| PointerMotionMask/* | EnterWindowMask | 
                                                     LeaveWindowMask*/, eventp);
    StashEventTime(eventp);

    if(eventp->type == KeyPress)
      Keyboard_shortcuts(eventp,FinishEvent);	
    if(eventp->type == FinishEvent)
      finished = 1;
    if(eventp->type == ButtonPress)
    {
      XAllowEvents(dpy,ReplayPointer,CurrentTime);
      done = 1;
    }
    if(eventp->type == ButtonRelease)
      done = 1;
    if(eventp->type == KeyPress)
      done = 1;
      
    if(!done)
    {
      DispatchEvent();
    }

  }

  
  *w = eventp->xany.window;
  if(((*w == Scr.Root)||(*w == Scr.NoFocusWin))
     && (eventp->xbutton.subwindow != (Window)0))
  {
    *w = eventp->xbutton.subwindow;
    eventp->xany.window = *w;
  }
  if (*w == Scr.Root)
  {
    *context = C_ROOT;
    XBell(dpy,Scr.screen);
    UngrabEm();
    return TRUE;
  }
  if (XFindContext (dpy, *w, ScwmContext, (caddr_t *)tmp_win) == XCNOENT)
  {
    *tmp_win = NULL;
    XBell(dpy,Scr.screen);
    UngrabEm();
    return (TRUE);
  }

  if(*w == (*tmp_win)->Parent)
    *w = (*tmp_win)->w;

  if(original_w == (*tmp_win)->Parent)
    original_w = (*tmp_win)->w;
  
  /* this ugly mess attempts to ensure that the release and press
   * are in the same window. */
  if((*w != original_w)&&(original_w != Scr.Root)&&
     (original_w != None)&&(original_w != Scr.NoFocusWin))
    if(!((*w == (*tmp_win)->frame)&&
         (original_w == (*tmp_win)->w)))
    {
      *context = C_ROOT;
      XBell(dpy,Scr.screen);
      UngrabEm();
      return TRUE;
    }
  
  *context = GetContext(*tmp_win,eventp,&dummy);
  
  UngrabEm();
  return FALSE;
}




/**************************************************************************
 *
 * Moves focus to specified window 
 *
 *************************************************************************/
void FocusOn(ScwmWindow *t,int DeIconifyOnly)
{
#ifndef NON_VIRTUAL
  int dx,dy;
  int cx,cy;

#endif
  int x,y;

  if(t == (ScwmWindow *)0)
    return;

  if(t->Desk != Scr.CurrentDesk)
  {
    changeDesks(0,t->Desk);
  }

#ifndef NON_VIRTUAL
  if(t->flags & ICONIFIED)
  {
    cx = t->icon_xl_loc + t->icon_w_width/2;
    cy = t->icon_y_loc + t->icon_p_height + ICON_HEIGHT/2;
  }
  else
  {
    cx = t->frame_x + t->frame_width/2;
    cy = t->frame_y + t->frame_height/2;
  }

  dx = (cx + Scr.Vx)/Scr.MyDisplayWidth*Scr.MyDisplayWidth;
  dy = (cy +Scr.Vy)/Scr.MyDisplayHeight*Scr.MyDisplayHeight;

  MoveViewport(dx,dy,True);
#endif

  if(t->flags & ICONIFIED)
  {
    x = t->icon_xl_loc + t->icon_w_width/2;
    y = t->icon_y_loc + t->icon_p_height + ICON_HEIGHT/2;
  }
  else
  {
    x = t->frame_x;
    y = t->frame_y;
  }
#if 0 /* don't want to warp the pointer by default anymore */
  if(!(t->flags & ClickToFocus))
    XWarpPointer(dpy, None, Scr.Root, 0, 0, 0, 0, x+2,y+2);
#endif /* 0 */
  RaiseWindow(t);
  KeepOnTop();

  /* If the window is still not visible, make it visible! */
  if(((t->frame_x + t->frame_height)< 0)||(t->frame_y + t->frame_width < 0)||
     (t->frame_x >Scr.MyDisplayWidth)||(t->frame_y>Scr.MyDisplayHeight))
  {
    SetupFrame(t,0,0,t->frame_width, t->frame_height,False);
    if(!(t->flags & ClickToFocus))
      XWarpPointer(dpy, None, Scr.Root, 0, 0, 0, 0, 2,2);
  }
  UngrabEm();
  SetFocus(t->w,t,0);
}


   
/**************************************************************************
 *
 * Moves pointer to specified window 
 *
 *************************************************************************/
void WarpOn(ScwmWindow *t,int warp_x, int x_unit, int warp_y, int y_unit)
{
#ifndef NON_VIRTUAL
  int dx,dy;
  int cx,cy;
#endif
  int x,y;

  if(t == (ScwmWindow *)0 || (t->flags & ICONIFIED && t->icon_w == None))
    return;

  if(t->Desk != Scr.CurrentDesk)
  {
    changeDesks(0,t->Desk);
  }

#ifndef NON_VIRTUAL
  if(t->flags & ICONIFIED)
  {
    cx = t->icon_xl_loc + t->icon_w_width/2;
    cy = t->icon_y_loc + t->icon_p_height + ICON_HEIGHT/2;
  }
  else
  {
    cx = t->frame_x + t->frame_width/2;
    cy = t->frame_y + t->frame_height/2;
  }

  dx = (cx + Scr.Vx)/Scr.MyDisplayWidth*Scr.MyDisplayWidth;
  dy = (cy +Scr.Vy)/Scr.MyDisplayHeight*Scr.MyDisplayHeight;

  MoveViewport(dx,dy,True);
#endif

  if(t->flags & ICONIFIED)
  {
    x = t->icon_xl_loc + t->icon_w_width/2 + 2;
    y = t->icon_y_loc + t->icon_p_height + ICON_HEIGHT/2 + 2;
  }
  else
  {
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
  if(((t->frame_x + t->frame_height)< 0)||(t->frame_y + t->frame_width < 0)||
     (t->frame_x >Scr.MyDisplayWidth)||(t->frame_y>Scr.MyDisplayHeight))
  {
    SetupFrame(t,0,0,t->frame_width, t->frame_height,False);
    XWarpPointer(dpy, None, Scr.Root, 0, 0, 0, 0, 2,2);
  }
  UngrabEm();
}


#if MS_DELETION_COMMENT   
/***********************************************************************
 *
 *  Procedure:
 *	(Un)Maximize a window.
 *
 ***********************************************************************/
void Maximize(XEvent *eventp,Window w,ScwmWindow *tmp_win,
	      unsigned long context, char *action, int *Module)
{
  int new_width, new_height,new_x,new_y;
  int val1, val2, val1_unit,val2_unit,n;

  if (DeferExecution(eventp,&w,&tmp_win,&context, SELECT,ButtonRelease))
    return;

  if(tmp_win == NULL)
    return;
  
  /* XXX - our scheme maximize does not do this check */
  if(check_allowed_function2(F_MAXIMIZE,tmp_win) == 0
#ifdef WINDOWSHADE
     || (tmp_win->buttons & WSHADE)
#endif
     )
  {
    XBell(dpy, Scr.screen);
    return;
  }
  n = GetTwoArguments(action, &val1, &val2, &val1_unit, &val2_unit);
  if(n != 2)
  {
    val1 = 100;
    val2 = 100;
    val1_unit = Scr.MyDisplayWidth;
    val2_unit = Scr.MyDisplayHeight;
  }

  if (tmp_win->flags & MAXIMIZED)
  {
    tmp_win->flags &= ~MAXIMIZED;
    SetupFrame(tmp_win, tmp_win->orig_x, tmp_win->orig_y, tmp_win->orig_wd,
               tmp_win->orig_ht,TRUE);
    SetBorder(tmp_win,True,True,True,None);
  }
  else
  {
    new_width = tmp_win->frame_width;      
    new_height = tmp_win->frame_height;
    new_x = tmp_win->frame_x;
    new_y = tmp_win->frame_y;
    if(val1 >0)
    {
      new_width = val1*val1_unit/100-2;
      new_x = 0;
    }
    if(val2 >0)
    {
      new_height = val2*val2_unit/100-2;
      new_y = 0;
    }
    if((val1==0)&&(val2==0))
    {
      new_x = 0;
      new_y = 0;
      new_height = Scr.MyDisplayHeight-2;
      new_width = Scr.MyDisplayWidth-2;
    }
    tmp_win->flags |= MAXIMIZED;
    ConstrainSize (tmp_win, &new_width, &new_height);
    SetupFrame(tmp_win,new_x,new_y,new_width,new_height,TRUE);
    SetBorder(tmp_win,Scr.Hilite == tmp_win,True,True,None);
  }
}
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT
#ifdef WINDOWSHADE
/***********************************************************************
 *
 *  WindowShade -- shades or unshades a window (veliaa@rpi.edu)
 *
 *  Args: 1 -- force shade, 2 -- force unshade  No Arg: toggle
 * 
 ***********************************************************************/
void WindowShade(XEvent *eventp,Window w,ScwmWindow *tmp_win,
		 unsigned long context, char *action, int *Module)
{
    int n = 0;

    if (DeferExecution(eventp,&w,&tmp_win,&context, SELECT,ButtonRelease))
	return;

    if (!(tmp_win->flags & TITLE) || (tmp_win->flags & MAXIMIZED)) {
	XBell(dpy, Scr.screen);
	return;
    }
    while (isspace(*action))++action;
    if (isdigit(*action))
	sscanf(action,"%d",&n);

    if (((tmp_win->buttons & WSHADE)||(n==2))&&(n!=1))
    {
	tmp_win->buttons &= ~WSHADE;
	SetupFrame(tmp_win,
		   tmp_win->frame_x, 
		   tmp_win->frame_y, 
		   tmp_win->orig_wd,
		   tmp_win->orig_ht,
		   True);
        Broadcast(M_DEWINDOWSHADE, 1, tmp_win->w, 0, 0, 0, 0, 0, 0);
    }
    else
    {
	tmp_win->buttons |= WSHADE;
	SetupFrame(tmp_win,
		   tmp_win->frame_x,
		   tmp_win->frame_y,
		   tmp_win->frame_width,
		   tmp_win->title_height + tmp_win->boundary_width,
		   False);
        Broadcast(M_WINDOWSHADE, 1, tmp_win->w, 0, 0, 0, 0, 0, 0);
    }
}
#endif /* WINDOWSHADE */
#endif /* MS_DELETION_COMMENT */

/* For Ultrix 4.2 */
#include <sys/types.h>
#include <sys/time.h>


MenuRoot *FindPopup(char *action)
{
  char *tmp;
  MenuRoot *mr;

  GetNextToken(action,&tmp);
  
  if(tmp == NULL)
    return NULL;

  mr = Scr.AllMenus;
  while(mr != NULL)
  {
    if(mr->name != NULL)
      if(mystrcasecmp(tmp,mr->name)== 0)
      {
        free(tmp);
        return mr;
      }
    mr = mr->next;
  }
  free(tmp);
  return NULL;
    
}

      
#if MS_DELETION_COMMENT
void Bell(XEvent *eventp,Window w,ScwmWindow *tmp_win,unsigned long context,
	  char *action, int *Module)
{
  XBell(dpy, Scr.screen);
}
#endif /* MS_DELETION_COMMENT */

#ifdef USEDECOR
ScwmDecor *last_decor = NULL, *cur_decor = NULL;
#endif
char *last_menu = NULL;

#if MS_DELETION_COMMENT
void add_item_to_menu(XEvent *eventp,Window w,ScwmWindow *tmp_win,
		      unsigned long context,
		      char *action, int *Module)
{
  MenuRoot *mr;

  char *token, *rest,*item;

#ifdef USEDECOR
  last_decor = NULL;
#endif

  rest = GetNextToken(action,&token);
  mr = FindPopup(token);
  if(mr == NULL)
    mr = NewMenuRoot(token, 0);
  if(last_menu != NULL)
    free(last_menu);
  last_menu = token;
  rest = GetNextToken(rest,&item);

  AddToMenu(mr, item,rest);
  free(item);
  
  MakeMenu(mr);
  return;
}
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT
#ifdef USEDECOR
void add_another_item(XEvent *eventp,Window w,ScwmWindow *tmp_win,
		      unsigned long context,
		      char *action, int *Module)
{
  extern void AddToDecor(ScwmDecor *, char *);
  MenuRoot *mr;

  char *rest,*item;

  if((last_menu == NULL) && (last_decor == NULL))
      return;

  if (last_menu != NULL) {
      
      mr = FindPopup(last_menu);
      if(mr == NULL)
	  return;
      rest = GetNextToken(action,&item);
      
      AddToMenu(mr, item,rest);
      free(item);
      
      MakeMenu(mr);
  }
  else if (last_decor != NULL) {
      ScwmDecor *tmp = &Scr.DefaultDecor;
      for (; tmp; tmp = tmp->next)
	  if (tmp == last_decor)
	      break;
      if (!tmp)
	  return;
      AddToDecor(tmp, action);
  }
}
#else /* ! USEDECOR */
void add_another_item(XEvent *eventp,Window w,ScwmWindow *tmp_win,
		      unsigned long context,
		      char *action, int *Module)
{
  MenuRoot *mr;

  char *rest,*item;

  if(last_menu == NULL)
    return;

  mr = FindPopup(last_menu);
  if(mr == NULL)
    return;
  rest = GetNextToken(action,&item);

  AddToMenu(mr, item,rest);
  free(item);
  
  MakeMenu(mr);
  return;
}
#endif /* USEDECOR */
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT
void destroy_menu(XEvent *eventp,Window w,ScwmWindow *tmp_win,
                  unsigned long context,
                  char *action, int *Module)
{
  MenuRoot *mr;

  char *token, *rest;

  rest = GetNextToken(action,&token);
  mr = FindPopup(token);
  if(mr == NULL)
    return;
  DestroyMenu(mr);
  return;

}
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT
void add_item_to_func(XEvent *eventp,Window w,ScwmWindow *tmp_win,
		      unsigned long context,
		      char *action, int *Module)
{
  MenuRoot *mr;

  char *token, *rest,*item;

  rest = GetNextToken(action,&token);
  mr = FindPopup(token);
  if(mr == NULL)
    mr = NewMenuRoot(token, 1);
  if(last_menu != NULL)
    free(last_menu);
  last_menu = token;
  rest = GetNextToken(rest,&item);

  AddToMenu(mr, item,rest);
  free(item);
  
  return;
}
#endif /* MS_DELETION_COMMENT */


void Nop_func(XEvent *eventp,Window w,ScwmWindow *tmp_win,unsigned long context,
              char *action, int *Module)
{

}


#if MS_DELETION_COMMENT
void movecursor(XEvent *eventp,Window w,ScwmWindow *tmp_win,unsigned long context,
		char *action, int *Module)
{
#ifndef NON_VIRTUAL
  int x,y,delta_x,delta_y,warp_x,warp_y;
  int val1, val2, val1_unit,val2_unit,n;

  n = GetTwoArguments(action, &val1, &val2, &val1_unit, &val2_unit);

  XQueryPointer( dpy, Scr.Root, &JunkRoot, &JunkChild,
                 &x,&y,&JunkX, &JunkY, &JunkMask);
  delta_x = 0;
  delta_y = 0;
  warp_x = 0;
  warp_y = 0;
  if(x >= Scr.MyDisplayWidth -2)
  {
    delta_x = Scr.EdgeScrollX;
    warp_x = Scr.EdgeScrollX - 4;
  }
  if(y>= Scr.MyDisplayHeight -2)
  {
    delta_y = Scr.EdgeScrollY;
    warp_y = Scr.EdgeScrollY - 4;      
  }
  if(x < 2)
  {
    delta_x = -Scr.EdgeScrollX;
    warp_x =  -Scr.EdgeScrollX + 4;
  }
  if(y < 2)
  {
    delta_y = -Scr.EdgeScrollY;
    warp_y =  -Scr.EdgeScrollY + 4;
  }
  if(Scr.Vx + delta_x < 0)
    delta_x = -Scr.Vx;
  if(Scr.Vy + delta_y < 0)
    delta_y = -Scr.Vy;
  if(Scr.Vx + delta_x > Scr.VxMax)
    delta_x = Scr.VxMax - Scr.Vx;
  if(Scr.Vy + delta_y > Scr.VyMax)
    delta_y = Scr.VyMax - Scr.Vy;
  if((delta_x!=0)||(delta_y!=0))
  {
    MoveViewport(Scr.Vx + delta_x,Scr.Vy+delta_y,True);
    XWarpPointer(dpy, Scr.Root, Scr.Root, 0, 0, Scr.MyDisplayWidth, 
                 Scr.MyDisplayHeight, 
                 x - warp_x,
                 y - warp_y);
  }
#endif
  XWarpPointer(dpy, Scr.Root, Scr.Root, 0, 0, Scr.MyDisplayWidth, 
	       Scr.MyDisplayHeight, x + val1*val1_unit/100-warp_x,
	       y+val2*val2_unit/100 - warp_y);
}
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT
void iconify_function(XEvent *eventp,Window w,ScwmWindow *tmp_win,
		      unsigned long context,char *action, int *Module)

{
  long val1;
  int val1_unit,n;

  if (DeferExecution(eventp,&w,&tmp_win,&context, SELECT, 
		     ButtonRelease))
    return;

  n = GetOneArgument(action, &val1, &val1_unit);

  if (tmp_win->flags & ICONIFIED)
  {
    if(val1 <=0)
      DeIconify(tmp_win);
  }
  else
  {
    if(check_allowed_function2(F_ICONIFY,tmp_win) == 0)
    {
      XBell(dpy, Scr.screen);
      return;
    }
    if(val1 >=0)
      Iconify(tmp_win,eventp->xbutton.x_root-5,eventp->xbutton.y_root-5);
  }
}
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT
void raise_function(XEvent *eventp,Window w,ScwmWindow *tmp_win,
		    unsigned long context, char *action, int *Module)
{
  char *junk, *junkC;
  unsigned long junkN;
  int junkD, method, BoxJunk[4];

  if (DeferExecution(eventp,&w,&tmp_win,&context, SELECT,ButtonRelease))
    return;
      
  if(tmp_win)
    RaiseWindow(tmp_win);

  if (LookInList(Scr.TheList,tmp_win->name, &tmp_win->class, &junk,
#ifdef MINI_ICONS
                 &junk,
#endif
#ifdef USEDECOR
		 &junkC,
#endif
		 &junkD, &junkD, &junkD, &junkC, &junkC, &junkN,
		 BoxJunk,  &method)& STAYSONTOP_FLAG)
    tmp_win->flags |= ONTOP;
  KeepOnTop();
}
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT
void lower_function(XEvent *eventp,Window w,ScwmWindow *tmp_win,
		    unsigned long context,char *action, int *Module)
{
  if (DeferExecution(eventp,&w,&tmp_win,&context, SELECT, ButtonRelease))
    return;

  LowerWindow(tmp_win);
  
  tmp_win->flags &= ~ONTOP;
}
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT
void destroy_function(XEvent *eventp,Window w,ScwmWindow *tmp_win,
		      unsigned long context, char *action, int *Module)
{
  if (DeferExecution(eventp,&w,&tmp_win,&context, DESTROY, ButtonRelease))
    return;

  if(check_allowed_function2(F_DESTROY,tmp_win) == 0)
  {
    XBell(dpy, Scr.screen);
    return;
  }
  
  if (XGetGeometry(dpy, tmp_win->w, &JunkRoot, &JunkX, &JunkY,
		   &JunkWidth, &JunkHeight, &JunkBW, &JunkDepth) == 0)
    Destroy(tmp_win);
  else
    XKillClient(dpy, tmp_win->w);
  XSync(dpy,0);
}
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT
void delete_function(XEvent *eventp,Window w,ScwmWindow *tmp_win,
		     unsigned long context,char *action, int *Module)
{
  if (DeferExecution(eventp,&w,&tmp_win,&context, DESTROY,ButtonRelease))
    return;

  if(check_allowed_function2(F_DELETE,tmp_win) == 0)
  {
    XBell(dpy, Scr.screen);
    return;
  }
  
  if (tmp_win->flags & DoesWmDeleteWindow)
  {
    send_clientmessage (dpy, tmp_win->w, _XA_WM_DELETE_WINDOW, CurrentTime);
    return;
  }
  else
    XBell (dpy, Scr.screen);
  XSync(dpy,0);
}
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT
void close_function(XEvent *eventp,Window w,ScwmWindow *tmp_win,
		    unsigned long context,char *action, int *Module)
{
  if (DeferExecution(eventp,&w,&tmp_win,&context, DESTROY,ButtonRelease))
    return;

  if(check_allowed_function2(F_CLOSE,tmp_win) == 0)
  {
    XBell(dpy, Scr.screen);
    return;
  }
  
  if (tmp_win->flags & DoesWmDeleteWindow)
  {
    send_clientmessage (dpy, tmp_win->w, _XA_WM_DELETE_WINDOW, CurrentTime);
    return;
  }
  else if (XGetGeometry(dpy, tmp_win->w, &JunkRoot, &JunkX, &JunkY,
			&JunkWidth, &JunkHeight, &JunkBW, &JunkDepth) == 0)
    Destroy(tmp_win);
  else
    XKillClient(dpy, tmp_win->w);
  XSync(dpy,0);
}      
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT
void restart_function(XEvent *eventp,Window w,ScwmWindow *tmp_win,
		      unsigned long context, char *action, int *Module)
{
  Done(1, action);
}
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT 
void exec_setup(XEvent *eventp,Window w,ScwmWindow *tmp_win,
                unsigned long context,char *action, int *Module)
{
  char *arg=NULL;

  action = GetNextToken(action,&arg);

  if (arg && (strcmp(arg,"")!=0)) /* specific shell was specified */
  {
    exec_shell_name = strdup(arg);
  }
  else /* no arg, so use $SHELL -- not working??? */
  {
    if (getenv("SHELL"))
      exec_shell_name = strdup(getenv("SHELL"));
    else
      exec_shell_name = strdup("/bin/sh"); /* if $SHELL not set, use default */
  }
}
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT
#if !defined(HAVE_STRERROR) || HAVE_STRERROR == 0
char *strerror(int num)
{
  extern int sys_nerr;
  extern char *sys_errlist[];

  if (num >= 0 && num < sys_nerr)
    return(sys_errlist[num]);
  else
    return "Unknown error number";
}
#endif
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT
void exec_function(XEvent *eventp,Window w,ScwmWindow *tmp_win,
		   unsigned long context,char *action, int *Module)
{
  char *cmd=NULL;

  /* if it doesn't already have an 'exec' as the first word, add that
   * to keep down number of procs started */
  /* need to parse string better to do this right though, so not doing this
     for now... */
  if (0 && mystrncasecmp(action,"exec",4)!=0)
  {
    cmd = (char *)safemalloc(strlen(action)+6);
    strcpy(cmd,"exec ");
    strcat(cmd,action);
  }
  else
  {
    cmd = strdup(action);
  }
  /* Use to grab the pointer here, but the fork guarantees that
   * we wont be held up waiting for the function to finish,
   * so the pointer-gram just caused needless delay and flashing
   * on the screen */
  /* Thought I'd try vfork and _exit() instead of regular fork().
   * The man page says that its better. */
  /* Not everyone has vfork! */
  if (!(fork())) /* child process */
  {
    if (execl(exec_shell_name, exec_shell_name, "-c", cmd, NULL)==-1)
    {
      scwm_msg(ERR,"exec_function","execl failed (%s)",strerror(errno));
      exit(100);
    }
  }
  free(cmd);
  return;
}
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT
void refresh_function(XEvent *eventp,Window w,ScwmWindow *tmp_win,
		      unsigned long context, char *action, int *Module)
{
  XSetWindowAttributes attributes;
  unsigned long valuemask;

#if 0
  valuemask = (CWBackPixel);
  attributes.background_pixel = 0;
#else /* CKH - i'd like to try this a little differently (clear window)*/
  valuemask = CWOverrideRedirect | CWBackingStore | CWSaveUnder | CWBackPixmap;
  attributes.override_redirect = True;
  attributes.save_under = False;
  attributes.background_pixmap = None;
#endif
  attributes.backing_store = NotUseful;
  w = XCreateWindow (dpy, Scr.Root, 0, 0,
		     (unsigned int) Scr.MyDisplayWidth,
		     (unsigned int) Scr.MyDisplayHeight,
		     (unsigned int) 0,
		     CopyFromParent, (unsigned int) CopyFromParent,
		     (Visual *) CopyFromParent, valuemask,
		     &attributes);
  XMapWindow (dpy, w);
  XDestroyWindow (dpy, w);
  XFlush (dpy);
}
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT
void refresh_win_function(XEvent *eventp,Window w,ScwmWindow *tmp_win,
                          unsigned long context, char *action, int *Module)
{
  XSetWindowAttributes attributes;
  unsigned long valuemask;

  if (DeferExecution(eventp,&w,&tmp_win,&context,SELECT,ButtonRelease))
    return;

  valuemask = CWOverrideRedirect | CWBackingStore | CWSaveUnder | CWBackPixmap;
  attributes.override_redirect = True;
  attributes.save_under = False;
  attributes.background_pixmap = None;
  attributes.backing_store = NotUseful;
  w = XCreateWindow (dpy,
                     (context == C_ICON)?(tmp_win->icon_w):(tmp_win->frame),
                     0, 0,
		     (unsigned int) Scr.MyDisplayWidth,
		     (unsigned int) Scr.MyDisplayHeight,
		     (unsigned int) 0,
		     CopyFromParent, (unsigned int) CopyFromParent,
		     (Visual *) CopyFromParent, valuemask,
		     &attributes);
  XMapWindow (dpy, w);
  XDestroyWindow (dpy, w);
  XFlush (dpy);
}
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT
void stick_function(XEvent *eventp,Window w,ScwmWindow *tmp_win,
		    unsigned long context, char *action, int *Module)
{
  if (DeferExecution(eventp,&w,&tmp_win,&context,SELECT,ButtonRelease))
    return;

  if(tmp_win->flags & STICKY)
  {
    tmp_win->flags &= ~STICKY;
  }
  else
  {
    tmp_win->flags |=STICKY;
  }
  BroadcastConfig(M_CONFIGURE_WINDOW,tmp_win);
  SetTitleBar(tmp_win,(Scr.Hilite==tmp_win),True);
}
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT
void wait_func(XEvent *eventp,Window w,ScwmWindow *tmp_win,
	       unsigned long context,char *action, int *Module)
{
  Bool done = False;
  extern ScwmWindow *Tmp_win;

  while(!done)
  {
    if(My_XNextEvent(dpy, &Event))
    {
      DispatchEvent ();
      if(Event.type == MapNotify)
      {
        if((Tmp_win)&&(matchWildcards(action,Tmp_win->name)==True))
          done = True;
        if((Tmp_win)&&(Tmp_win->class.res_class)&&
           (matchWildcards(action,Tmp_win->class.res_class)==True))
          done = True;
        if((Tmp_win)&&(Tmp_win->class.res_name)&&
           (matchWildcards(action,Tmp_win->class.res_name)==True))
          done = True;
      }
    }
  }
}
#endif /* MS_DELETION_COMMENT */

void raise_it_func(XEvent *eventp,Window w,ScwmWindow *tmp_win,
		   unsigned long context, char *action, int *Module)
{
  int val1_unit,n;
  long val1; 
  n = GetOneArgument(action, &val1, &val1_unit);

  if(val1 != 0)
  {
    FocusOn((ScwmWindow *)val1,0);
    if (((ScwmWindow *)(val1))->flags & ICONIFIED)
    {
      DeIconify((ScwmWindow *)val1);
      FocusOn((ScwmWindow *)val1,0);
    }
  }
}


      
void flip_focus_func(XEvent *eventp,Window w,ScwmWindow *tmp_win,
		unsigned long context, char *action, int *Module)
{

  ScwmWindow *scratch;

  if (DeferExecution(eventp,&w,&tmp_win,&context,SELECT,ButtonRelease))
    return;

  /* Reorder the window list */
  if( Scr.Focus ){
    if( Scr.Focus->next ) Scr.Focus->next->prev = Scr.Focus->prev;
    if( Scr.Focus->prev ) Scr.Focus->prev->next = Scr.Focus->next;
    Scr.Focus->next = Scr.ScwmRoot.next;
    Scr.Focus->prev = &Scr.ScwmRoot;
    if(Scr.ScwmRoot.next)Scr.ScwmRoot.next->prev = Scr.Focus;
    Scr.ScwmRoot.next = Scr.Focus;
  }
  if( tmp_win != Scr.Focus ){
    if( tmp_win->next ) tmp_win->next->prev = tmp_win->prev;
    if( tmp_win->prev ) tmp_win->prev->next = tmp_win->next;
    tmp_win->next = Scr.ScwmRoot.next;
    tmp_win->prev = &Scr.ScwmRoot;
    if(Scr.ScwmRoot.next)Scr.ScwmRoot.next->prev = tmp_win;
    Scr.ScwmRoot.next = tmp_win;
  }

  FocusOn(tmp_win,0);

}

#if MS_DELETION_COMMENT
void focus_func(XEvent *eventp,Window w,ScwmWindow *tmp_win,
		unsigned long context, char *action, int *Module)
{
  if (DeferExecution(eventp,&w,&tmp_win,&context,SELECT,ButtonRelease))
    return;

  FocusOn(tmp_win,0);
}
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT
void warp_func(XEvent *eventp,Window w,ScwmWindow *tmp_win,
               unsigned long context, char *action, int *Module)
{
   int val1_unit, val2_unit, n;
   int val1, val2;

  if (DeferExecution(eventp,&w,&tmp_win,&context,SELECT,ButtonRelease))
    return;

   n = GetTwoArguments (action, &val1, &val2, &val1_unit, &val2_unit);

   if (n == 2)
     WarpOn (tmp_win, val1, val1_unit, val2, val2_unit);
   else
     WarpOn (tmp_win, 0, 0, 0, 0);
}
#endif /* MS_DELETION_COMMENT */

void popup_func(XEvent *eventp,Window w,ScwmWindow *tmp_win,
		unsigned long context, char *action,int *Module)
{
  MenuRoot *menu;
  extern int menuFromFrameOrWindowOrTitlebar;

  menu = FindPopup(action);
  if(menu == NULL)
  {
    scwm_msg(ERR,"popup_func","No such menu %s",action);
    return;
  }
  ActiveItem = NULL;
  ActiveMenu = NULL;
  menuFromFrameOrWindowOrTitlebar = FALSE;
  do_menu(menu,0);
}

void staysup_func(XEvent *eventp,Window w,ScwmWindow *tmp_win,
                  unsigned long context, char *action,int *Module)
{
#if 0
  MenuRoot *menu;
  extern int menuFromFrameOrWindowOrTitlebar;
  char *default_action = NULL, *menu_name = NULL;
  extern int menu_aborted;

  action = GetNextToken(action,&menu_name);
  GetNextToken(action,&default_action);
  menu = FindPopup(menu_name);
  if(menu == NULL)
  {
    if(menu_name != NULL)
    {
      scwm_msg(ERR,"staysup_func","No such menu %s",menu_name);
      free(menu_name);
    }
    if(default_action != NULL)
      free(default_action);
    return;
  }
  ActiveItem = NULL;
  ActiveMenu = NULL;
  menuFromFrameOrWindowOrTitlebar = FALSE;

  /* See bottom of windows.c for rationale behind this */
  if (eventp->type == ButtonPress)
    do_menu(menu,1);
  else
    do_menu(menu,0);

  if(menu_name != NULL)
    free(menu_name);
  if((menu_aborted)&&(default_action != NULL))
    ExecuteFunction(default_action,tmp_win,eventp,context,*Module);
  if(default_action != NULL)
    free(default_action);
#endif
}


void quit_func(XEvent *eventp,Window w,ScwmWindow *tmp_win,
	       unsigned long context, char *action,int *Module)
{
  if (master_pid != getpid())
    kill(master_pid, SIGTERM);
  Done(0,NULL);
}

void quit_screen_func(XEvent *eventp,Window w,ScwmWindow *tmp_win,
                      unsigned long context, char *action,int *Module)
{
  Done(0,NULL);
}

#if MS_DELETION_COMMENT
void echo_func(XEvent *eventp,Window w,ScwmWindow *tmp_win,
               unsigned long context, char *action,int *Module)
{
  if (action && *action)
  {
    scwm_msg(INFO,"Echo",action);
  }
}
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT
void raiselower_func(XEvent *eventp,Window w,ScwmWindow *tmp_win,
		     unsigned long context, char *action,int *Module)
{
  char *junk, *junkC;
  unsigned long junkN;
  int junkD,method, BoxJunk[4];

  if (DeferExecution(eventp,&w,&tmp_win,&context, SELECT,ButtonRelease))
    return;
  if(tmp_win == NULL)
    return;
  
  if((tmp_win == Scr.LastWindowRaised)||
     (tmp_win->flags & VISIBLE))
  {
    LowerWindow(tmp_win);
    tmp_win->flags &= ~ONTOP;
  }
  else
  {
    RaiseWindow(tmp_win);
    if (LookInList(Scr.TheList,tmp_win->name, &tmp_win->class,&junk,
#ifdef MINI_ICONS
                   &junk,
#endif
#ifdef USEDECOR
		   &junkC,
#endif
                   &junkD,&junkD, &junkD, &junkC,&junkC,&junkN,BoxJunk,
                   &method)&STAYSONTOP_FLAG)
      tmp_win->flags |= ONTOP;	    
    KeepOnTop();
  }
}
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT
void SetEdgeScroll(XEvent *eventp,Window w,ScwmWindow *tmp_win,
                   unsigned long context, char *action,int* Module)
{
  int val1, val2, val1_unit,val2_unit,n;

  n = GetTwoArguments(action, &val1, &val2, &val1_unit, &val2_unit);
  if(n != 2)
  {
    scwm_msg(ERR,"SetEdgeScroll","EdgeScroll requires two arguments");
    return;
  }

  /*
  ** if edgescroll >1000 and < 100000m
  ** wrap at edges of desktop (a "spherical" desktop)
  */
  if (val1 >= 1000) 
  {
    val1 /= 1000;
    Scr.flags |= EdgeWrapX;
  }
  else
  {
    Scr.flags &= ~EdgeWrapX;
  }
  if (val2 >= 1000) 
  {
    val2 /= 1000;
    Scr.flags |= EdgeWrapY;
  }
  else
  {
    Scr.flags &= ~EdgeWrapY;
  }

  Scr.EdgeScrollX = val1*val1_unit/100;
  Scr.EdgeScrollY = val2*val2_unit/100;

  checkPanFrames();
}
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT
void SetEdgeResistance(XEvent *eventp,Window w,ScwmWindow *tmp_win,
                       unsigned long context, char *action,int* Module)
{
  int val1, val2, val1_unit,val2_unit,n;
  
  n = GetTwoArguments(action, &val1, &val2, &val1_unit, &val2_unit);
  if(n != 2)
  {
    scwm_msg(ERR,"SetEdgeResistance","EdgeResistance requires two arguments");
    return;
  }

  Scr.ScrollResistance = val1;
  Scr.MoveResistance = val2;
}
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT
void SetColormapFocus(XEvent *eventp,Window w,ScwmWindow *tmp_win,
                      unsigned long context, char *action,int* Module)
{
  if (mystrncasecmp(action,"FollowsFocus",12)==0)
  {
    Scr.ColormapFocus = COLORMAP_FOLLOWS_FOCUS;
  }
  else if (mystrncasecmp(action,"FollowsMouse",12)==0)
  {
    Scr.ColormapFocus = COLORMAP_FOLLOWS_MOUSE;
  }
  else
  {
    scwm_msg(ERR,"SetColormapFocus",
             "ColormapFocus requires 1 arg: FollowsFocus or FollowsMouse");
    return;
  }
}
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT
void SetClick(XEvent *eventp,Window w,ScwmWindow *tmp_win,
              unsigned long context, char *action,int* Module)
{
  long val1;
  int val1_unit,n;

  n = GetOneArgument(action, &val1, &val1_unit);
  if(n != 1)
  {
    scwm_msg(ERR,"SetClick","ClickTime requires 1 argument");
    return;
  }

  Scr.ClickTime = val1;
}
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT
void SetXOR(XEvent *eventp,Window w,ScwmWindow *tmp_win,
            unsigned long context, char *action,int* Module)
{
  long val1;
  int val1_unit,n;
  XGCValues gcv;
  unsigned long gcm;

  n = GetOneArgument(action, &val1, &val1_unit);
  if(n != 1)
  {
    scwm_msg(ERR,"SetXOR","XORValue requires 1 argument");
    return;
  }

  gcm = GCFunction|GCLineWidth|GCForeground|GCSubwindowMode; 
  gcv.function = GXxor;
  gcv.line_width = 0;
  gcv.foreground = val1;
  gcv.subwindow_mode = IncludeInferiors;
  Scr.DrawGC = XCreateGC(dpy, Scr.Root, gcm, &gcv);
}
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT
void SetOpaque(XEvent *eventp,Window w,ScwmWindow *tmp_win,
	       unsigned long context, char *action,int* Module)
{
  long val1;
  int val1_unit,n;

  n = GetOneArgument(action, &val1, &val1_unit);
  if(n != 1)
  {
    scwm_msg(ERR,"SetOpaque","OpaqueMoveSize requires 1 argument");
    return;
  }

  Scr.OpaqueSize = val1;
}
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT
void SetDeskSize(XEvent *eventp,Window w,ScwmWindow *tmp_win,
                 unsigned long context, char *action,int* Module)
{
  int val1, val2, val1_unit,val2_unit,n;

  n = GetTwoArguments(action, &val1, &val2, &val1_unit, &val2_unit);
  if(n != 2)
  {
    scwm_msg(ERR,"SetDeskSize","DesktopSize requires two arguments");
    return;
  }
  if((val1_unit != Scr.MyDisplayWidth)||
     (val2_unit != Scr.MyDisplayHeight))
  {
    scwm_msg(ERR,"SetDeskSize","DeskTopSize arguments should be unitless");
  }

  Scr.VxMax = val1;
  Scr.VyMax = val2;
  Scr.VxMax = Scr.VxMax*Scr.MyDisplayWidth - Scr.MyDisplayWidth;
  Scr.VyMax = Scr.VyMax*Scr.MyDisplayHeight - Scr.MyDisplayHeight;
  if(Scr.VxMax <0)
    Scr.VxMax = 0;
  if(Scr.VyMax <0)
    Scr.VyMax = 0;
  Broadcast(M_NEW_PAGE,5,Scr.Vx,Scr.Vy,Scr.CurrentDesk,Scr.VxMax,Scr.VyMax,0,0);

  checkPanFrames();
}
#endif /* MS_DELETION_COMMENT */

#ifdef XPM
extern char *PixmapPath;

#if MS_DELETION_COMMENT
void setPixmapPath(XEvent *eventp,Window w,ScwmWindow *tmp_win,
                   unsigned long context, char *action,int* Module)
{
  static char *ptemp = NULL;
  char *tmp;

  if(ptemp == NULL)
    ptemp = PixmapPath;

  if((PixmapPath != ptemp)&&(PixmapPath != NULL))
    free(PixmapPath);
  tmp = stripcpy(action);
  PixmapPath = envDupExpand(tmp, 0);
  free(tmp);
}
#endif /* MS_DELETION_COMMENT */

#endif /* XPM */

extern char *IconPath;

#if MS_DELETION_COMMENT
void setIconPath(XEvent *eventp,Window w,ScwmWindow *tmp_win,
                 unsigned long context, char *action,int* Module)
{
  static char *ptemp = NULL;
  char *tmp;

  if(ptemp == NULL)
    ptemp = IconPath;

  if((IconPath != ptemp)&&(IconPath != NULL))
    free(IconPath);
  tmp = stripcpy(action);
  IconPath = envDupExpand(tmp, 0);
  free(tmp);
}
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT 
#ifdef SCWM_MODULEDIR
char *ModulePath = SCWM_MODULEDIR;
#else
char *ModulePath = SCWMDIR;
#endif
void setModulePath(XEvent *eventp,Window w,ScwmWindow *tmp_win,
                   unsigned long context, char *action,int* Module)
{
  static char *ptemp = NULL;
  char *tmp;

  if(ptemp == NULL)
    ptemp = ModulePath;

  if((ModulePath != ptemp)&&(ModulePath != NULL))
    free(ModulePath);
  tmp = stripcpy(action);
  ModulePath = envDupExpand(tmp, 0);
  free(tmp);
}
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT
void SetHiColor(XEvent *eventp,Window w,ScwmWindow *tmp_win,
		unsigned long context, char *action,int* Module)
{
  XGCValues gcv;
  unsigned long gcm;
  char *hifore=NULL, *hiback=NULL;
  extern char *white,*black;
  ScwmWindow *hilight;
#ifdef USEDECOR
  ScwmDecor *fl = cur_decor ? cur_decor : &Scr.DefaultDecor;
#else
  ScwmDecor *fl = &Scr.DefaultDecor;
#endif
  
  action = GetNextToken(action,&hifore);
  GetNextToken(action,&hiback);
  if(Scr.d_depth > 2)
  {
    if(hifore != NULL)
    {
	fl->HiColors.fore = GetColor(hifore);
    }
    if(hiback != NULL)
    {
	fl->HiColors.back = GetColor(hiback);
    }
    fl->HiRelief.back  = GetShadow(fl->HiColors.back);
    fl->HiRelief.fore  = GetHilite(fl->HiColors.back);
  }
  else
  {
    fl->HiColors.back  = GetColor(white);
    fl->HiColors.fore  = GetColor(black); 
    fl->HiRelief.back  = GetColor(black);
    fl->HiRelief.fore  = GetColor(white);
  }
  if (hifore) free(hifore); 
  if (hiback) free(hiback);
  gcm = GCFunction|GCPlaneMask|GCGraphicsExposures|GCLineWidth|GCForeground|
    GCBackground;
  gcv.foreground = fl->HiRelief.fore;
  gcv.background = fl->HiRelief.back;
  gcv.fill_style = FillSolid;
  gcv.plane_mask = AllPlanes;
  gcv.function = GXcopy;
  gcv.graphics_exposures = False;
  gcv.line_width = 0;
  if(fl->HiReliefGC != NULL)
  {
    XFreeGC(dpy,fl->HiReliefGC);
  }
  fl->HiReliefGC = XCreateGC(dpy, Scr.Root, gcm, &gcv);  

  gcv.foreground = fl->HiRelief.back;
  gcv.background = fl->HiRelief.fore;
  if(fl->HiShadowGC != NULL)
  {
    XFreeGC(dpy,fl->HiShadowGC);
  }
  fl->HiShadowGC = XCreateGC(dpy, Scr.Root, gcm, &gcv);  

  if((Scr.flags & WindowsCaptured)&&(Scr.Hilite != NULL))
  {
    hilight = Scr.Hilite;
    SetBorder(Scr.Hilite,False,True,True,None);
    SetBorder(hilight,True,True,True,None);
  }
}
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT
void SetMenuStyle(XEvent *eventp,Window w,ScwmWindow *tmp_win,
                  unsigned long context, char *action,int* Module)
{
  XGCValues gcv;
  unsigned long gcm;
  char *fore=NULL, *back=NULL, *stipple = NULL, *font= NULL, *style = 0;
  extern char *white,*black;
  int wid,hei;

  action = GetNextToken(action,&fore);
  action = GetNextToken(action,&back);
  action = GetNextToken(action,&stipple);
  action = GetNextToken(action,&font);
  action = GetNextToken(action,&style);

  if((style != NULL)&&(mystrncasecmp(style,"MWM",3)==0))
  {
    Scr.flags |= MWMMenus;
  }
  else
    Scr.flags &= ~MWMMenus;

  if(Scr.d_depth > 2)
  {
    if(fore != NULL)
    {
      Scr.MenuColors.fore = GetColor(fore);
    }
    if(back != NULL)
    {
      Scr.MenuColors.back = GetColor(back);
    }
    Scr.MenuRelief.back  = GetShadow(Scr.MenuColors.back);
    Scr.MenuRelief.fore  = GetHilite(Scr.MenuColors.back);
    Scr.MenuStippleColors.back = Scr.MenuColors.back;
    Scr.MenuStippleColors.fore = GetColor(stipple); 
  }
  else
  {
    Scr.MenuColors.back  = GetColor(white);
    Scr.MenuColors.fore  = GetColor(black); 
    Scr.MenuRelief.back  = GetColor(black);
    Scr.MenuRelief.fore  = GetColor(white);
    Scr.MenuStippleColors.back = GetColor(white);
    Scr.MenuStippleColors.fore = GetColor(black);
  }

  if ((font == NULL)||
      (Scr.StdFont.font = GetFontOrFixed(dpy, font)) == NULL)
  {
    scwm_msg(ERR,"SetMenuStyle","Couldn't load font '%s' or 'fixed'\n",
            (font==NULL)?("NULL"):(font));
    exit(1);
  }
  Scr.StdFont.height = Scr.StdFont.font->ascent + Scr.StdFont.font->descent;
  Scr.StdFont.y = Scr.StdFont.font->ascent;
  Scr.EntryHeight = Scr.StdFont.height + HEIGHT_EXTRA;

  gcm = GCFunction|GCPlaneMask|GCFont|GCGraphicsExposures|
    GCLineWidth|GCForeground|GCBackground;
  gcv.foreground = Scr.MenuRelief.fore;
  gcv.background = Scr.MenuRelief.back;
  gcv.fill_style = FillSolid;
  gcv.font = Scr.StdFont.font->fid;
  gcv.plane_mask = AllPlanes;
  gcv.function = GXcopy;
  gcv.graphics_exposures = False;
  gcv.line_width = 0;
  if(Scr.MenuReliefGC != NULL)
  {
    XFreeGC(dpy,Scr.MenuReliefGC);
  }
  Scr.MenuReliefGC = XCreateGC(dpy, Scr.Root, gcm, &gcv);  

  gcv.foreground = Scr.MenuRelief.back;
  gcv.background = Scr.MenuRelief.fore;
  if(Scr.MenuShadowGC != NULL)
  {
    XFreeGC(dpy,Scr.MenuShadowGC);
  }
  Scr.MenuShadowGC = XCreateGC(dpy, Scr.Root, gcm, &gcv);  
  gcv.foreground = Scr.MenuColors.fore;
  gcv.background = Scr.MenuColors.back;
  if(Scr.MenuGC != NULL)
  {
    XFreeGC(dpy,Scr.MenuGC);
  }
  Scr.MenuGC = XCreateGC(dpy, Scr.Root, gcm, &gcv);
  if(Scr.d_depth < 2)
  {
    gcv.fill_style = FillStippled;
    gcv.stipple = Scr.gray_bitmap;
    gcm=GCFunction|GCPlaneMask|GCGraphicsExposures|GCLineWidth|GCForeground|
      GCBackground|GCFont|GCStipple|GCFillStyle;
    Scr.MenuStippleGC = XCreateGC(dpy, Scr.Root, gcm, &gcv);
      
    gcm=GCFunction|GCPlaneMask|GCGraphicsExposures|GCLineWidth|GCForeground|
      GCBackground|GCFont;
    gcv.fill_style = FillSolid;
  }
  else
  {
    gcv.foreground = Scr.MenuStippleColors.fore;
    gcv.background = Scr.MenuStippleColors.back;
    Scr.MenuStippleGC = XCreateGC(dpy, Scr.Root, gcm, &gcv);
  }
  if(Scr.SizeWindow != None)
  {
    Scr.SizeStringWidth = XTextWidth (Scr.StdFont.font,
                                      " +8888 x +8888 ", 15);
    wid = Scr.SizeStringWidth+SIZE_HINDENT*2;
    hei = Scr.StdFont.height+SIZE_VINDENT*2;
    if(Scr.flags & MWMMenus)
    {
      XMoveResizeWindow(dpy,Scr.SizeWindow,
                        Scr.MyDisplayWidth/2 -wid/2,
                        Scr.MyDisplayHeight/2 - hei/2,
                        wid,hei);
    }
    else
    {
      XMoveResizeWindow(dpy,Scr.SizeWindow,0, 0, wid,hei);
    }
  }
  

  if(Scr.SizeWindow != None)
  {
    XSetWindowBackground(dpy,Scr.SizeWindow,Scr.MenuColors.back);
  }
  MakeMenus();
  if(fore != NULL)
    free(fore);
  if(back != NULL)
    free(back);
  if(stipple != NULL)
    free(stipple);
  if(font != NULL)
    free(font);
  if(style != NULL)
    free(style);

}
#endif /* MS_DELETION_COMMENT */

Boolean ReadButtonFace(char *s, ButtonFace *bf, int button, int verbose);
void FreeButtonFace(Display *dpy, ButtonFace *bf);

#ifdef BORDERSTYLE
/****************************************************************************
 *
 *  Sets the border style (veliaa@rpi.edu)
 *
 ****************************************************************************/
void SetBorderStyle(XEvent *eventp,Window w,ScwmWindow *tmp_win,
		    unsigned long context, char *action,int* Module)
{
    char *parm = NULL, *prev = action;
#ifdef USEDECOR
    ScwmDecor *fl = cur_decor ? cur_decor : &Scr.DefaultDecor;
#else
    ScwmDecor *fl = &Scr.DefaultDecor;
#endif

    action = GetNextToken(action, &parm);
    while (parm && parm[0])
    {
	if (mystrncasecmp(parm,"active",6)==0
	    || mystrncasecmp(parm,"inactive",8)==0)
	{
	    int len;
	    char *end, *tmp;
	    ButtonFace tmpbf, *bf;
	    tmpbf.style = SimpleButton;
#ifdef MULTISTYLE
	    tmpbf.next = NULL;
#endif
	    if (mystrncasecmp(parm,"active",6)==0)
		bf = &fl->BorderStyle.active;
	    else
		bf = &fl->BorderStyle.inactive;
	    while (isspace(*action)) ++action;
	    if ('(' != *action) {
		if (!*action) {
		    scwm_msg(ERR,"SetBorderStyle", 
			     "error in %s border specification", parm);
		    free(parm);
		    return;
		}
		free(parm);
		if (ReadButtonFace(action, &tmpbf,-1,True)) {
		    FreeButtonFace(dpy, bf);
		    *bf = tmpbf;
		}
		break;
	    }
	    end = strchr(++action, ')');
	    if (!end) {
		scwm_msg(ERR,"SetBorderStyle", 
			 "error in %s border specification", parm);
		free(parm);
		return;
	    }
	    len = end - action + 1;
	    tmp = safemalloc(len);
	    strncpy(tmp, action, len - 1);
	    tmp[len - 1] = 0;
	    ReadButtonFace(tmp, bf,-1,True);
	    free(tmp);
	    action = end + 1;
	}
	else if (strcmp(parm,"--")==0) {
	    if (ReadButtonFace(prev, &fl->BorderStyle.active,-1,True)) 
		ReadButtonFace(prev, &fl->BorderStyle.inactive,-1,False);
	    free(parm);
	    break;
	} else {
	    ButtonFace tmpbf;
	    tmpbf.style = SimpleButton;
#ifdef MULTISTYLE
	    tmpbf.next = NULL;
#endif
	    if (ReadButtonFace(prev, &tmpbf,-1,True)) {
		FreeButtonFace(dpy,&fl->BorderStyle.active);
		fl->BorderStyle.active = tmpbf;
		ReadButtonFace(prev, &fl->BorderStyle.inactive,-1,False);
	    }
	    free(parm);
	    break;
	}
	free(parm);
	prev = action;
	action = GetNextToken(action,&parm);	
    }
}
#endif

char *ReadTitleButton(char *s, TitleButton *tb, Boolean append, int button);

#if defined(MULTISTYLE) && defined(EXTENDED_TITLESTYLE)
/*****************************************************************************
 * 
 * Appends a titlestyle (veliaa@rpi.edu)
 *
 ****************************************************************************/
void AddTitleStyle(XEvent *eventp,Window w,ScwmWindow *tmp_win,
                   unsigned long context, char *action,int* Module)
{
#ifdef USEDECOR
    ScwmDecor *fl = cur_decor ? cur_decor : &Scr.DefaultDecor;
#else
    ScwmDecor *fl = &Scr.DefaultDecor;
#endif
    char *parm=NULL, *prev;
    prev = action;
    action = GetNextToken(action,&parm);
    while(parm && parm[0]!='\0')
    {
	if (!(action = ReadTitleButton(prev, &fl->titlebar, True, -1))) {
	    free(parm);
	    break;
	}
	prev = action;
	action = GetNextToken(action,&parm);
    }
}
#endif /* MULTISTYLE && EXTENDED_TITLESTYLE */

void SetTitleStyle(XEvent *eventp,Window w,ScwmWindow *tmp_win,
                   unsigned long context, char *action,int* Module)
{
  char *parm=NULL, *prev = action;
#ifdef USEDECOR
  ScwmDecor *fl = cur_decor ? cur_decor : &Scr.DefaultDecor;
#else
  ScwmDecor *fl = &Scr.DefaultDecor;
#endif

  action = GetNextToken(action,&parm);
  while(parm && parm[0]!='\0')
  {
    if (mystrncasecmp(parm,"centered",8)==0)
    {
      fl->titlebar.flags &= ~HOffCenter;
    }
    else if (mystrncasecmp(parm,"leftjustified",13)==0)
    {
      fl->titlebar.flags |= HOffCenter;
      fl->titlebar.flags &= ~HRight;
    }
    else if (mystrncasecmp(parm,"rightjustified",14)==0)
    {
      fl->titlebar.flags |= HOffCenter | HRight;
    }
#ifdef EXTENDED_TITLESTYLE
    else if (mystrncasecmp(parm,"height",6)==0)
    {
	int height, next;
	if ( sscanf(action, "%d%n", &height, &next) > 0
	     && height > 4
	     && height <= 256)
	{
	    int x,y,w,h,extra_height;
	    ScwmWindow *tmp = Scr.ScwmRoot.next, *hi = Scr.Hilite;

	    extra_height = fl->TitleHeight;
	    fl->TitleHeight = height;
	    extra_height -= fl->TitleHeight;
	    
	    fl->WindowFont.y = fl->WindowFont.font->ascent 
		+ (height - (fl->WindowFont.font->ascent 
			     + fl->WindowFont.font->descent + 3)) / 2;
	    if (fl->WindowFont.y < fl->WindowFont.font->ascent)
		fl->WindowFont.y = fl->WindowFont.font->ascent;

	    tmp = Scr.ScwmRoot.next;
	    hi = Scr.Hilite;
	    while(tmp != NULL)
	    {
		if (!(tmp->flags & TITLE)
#ifdef USEDECOR
		    || (tmp->fl != fl)
#endif
		    ) {
		    tmp = tmp->next;
		    continue;
		}
		x = tmp->frame_x;
		y = tmp->frame_y;
		w = tmp->frame_width;
		h = tmp->frame_height-extra_height;
		tmp->frame_x = 0;
		tmp->frame_y = 0;
		tmp->frame_height = 0;
		tmp->frame_width = 0;
		SetupFrame(tmp,x,y,w,h,True);
		SetTitleBar(tmp,True,True);
		SetTitleBar(tmp,False,True);
		tmp = tmp->next;
	    }
	    SetTitleBar(hi,True,True);
	}
	else
	    scwm_msg(ERR,"SetTitleStyle",
		     "bad height argument (height must be from 5 to 256)");
	action += next;
    }
    else
    {
	if (!(action = ReadTitleButton(prev, &fl->titlebar, False, -1))) {
	    free(parm);
	    break;
	}
    }
#else /* ! EXTENDED_TITLESTYLE */
    else if (strcmp(parm,"--")==0) {
	if (!(action = ReadTitleButton(prev, &fl->titlebar, False, -1))) {
	    free(parm);
	    break;
	}
    }
#endif /* EXTENDED_TITLESTYLE */
    free(parm);
    prev = action;
    action = GetNextToken(action,&parm);
  }
} /* SetTitleStyle */

#if MS_DELETION_COMMENT
void LoadIconFont(XEvent *eventp,Window w,ScwmWindow *tmp_win,
                  unsigned long context, char *action,int* Module)
{
  char *font;
  ScwmWindow *tmp;

  action = GetNextToken(action,&font);

  if ((Scr.IconFont.font = GetFontOrFixed(dpy, font))==NULL)
  {
    scwm_msg(ERR,"LoadIconFont","Couldn't load font '%s' or 'fixed'\n",
            font);
    free(font);
    return;
  }
  Scr.IconFont.height=
    Scr.IconFont.font->ascent+Scr.IconFont.font->descent;
  Scr.IconFont.y = Scr.IconFont.font->ascent;

  free(font);
  tmp = Scr.ScwmRoot.next;
  while(tmp != NULL)
  {
    RedoIconName(tmp);

    if(tmp->flags& ICONIFIED)
    {
      DrawIconWindow(tmp);
    }
    tmp = tmp->next;
  }
}
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT
void LoadWindowFont(XEvent *eventp,Window win,ScwmWindow *tmp_win,
                    unsigned long context, char *action,int* Module)
{
  char *font;
  ScwmWindow *tmp,*hi;
  int x,y,w,h,extra_height;
  XFontStruct *newfont;
#ifdef USEDECOR
  ScwmDecor *fl = cur_decor ? cur_decor : &Scr.DefaultDecor;
#else
  ScwmDecor *fl = &Scr.DefaultDecor;
#endif

  action = GetNextToken(action,&font);

  if ((newfont = GetFontOrFixed(dpy, font))!=NULL)
  {
    fl->WindowFont.font = newfont;
    fl->WindowFont.height=
      fl->WindowFont.font->ascent+fl->WindowFont.font->descent;
    fl->WindowFont.y = fl->WindowFont.font->ascent;
    extra_height = fl->TitleHeight;
    fl->TitleHeight=fl->WindowFont.font->ascent+fl->WindowFont.font->descent+3;
    extra_height -= fl->TitleHeight;
    tmp = Scr.ScwmRoot.next;
    hi = Scr.Hilite;
    while(tmp != NULL)
    {
      if (!(tmp->flags & TITLE)
#ifdef USEDECOR
	  || (tmp->fl != fl)
#endif
	  ) {
	  tmp = tmp->next;
	  continue;
      }
      x = tmp->frame_x;
      y = tmp->frame_y;
      w = tmp->frame_width;
      h = tmp->frame_height-extra_height;
      tmp->frame_x = 0;
      tmp->frame_y = 0;
      tmp->frame_height = 0;
      tmp->frame_width = 0;
      SetupFrame(tmp,x,y,w,h,True);
      SetTitleBar(tmp,True,True);
      SetTitleBar(tmp,False,True);
      tmp = tmp->next;
    }
    SetTitleBar(hi,True,True);
    
  }
  else
  {
    scwm_msg(ERR,"LoadWindowFont","Couldn't load font '%s' or 'fixed'\n",
            font);
  }

  free(font);
}
#endif /* MS_DELETION_COMMENT */

void FreeButtonFace(Display *dpy, ButtonFace *bf)
{
    switch (bf->style)
    {
#ifdef GRADIENT_BUTTONS
    case HGradButton:
    case VGradButton:
	/* - should we check visual is not TrueColor before doing this? 
	   
	   XFreeColors(dpy, Scr.ScwmRoot.attr.colormap, 
		    bf->u.grad.pixels, bf->u.grad.npixels,
		    AllPlanes); */
	free(bf->u.grad.pixels);
	bf->u.grad.pixels = NULL;
	break;
#endif
	
#ifdef PIXMAP_BUTTONS
    case PixmapButton:
    case TiledPixmapButton:
	if (bf->u.p)
	    DestroyPicture(dpy, bf->u.p);
	bf->u.p = NULL;
	break;
#endif
    default:
	break;
    }
#ifdef MULTISTYLE
    /* delete any compound styles */
    if (bf->next) {
	FreeButtonFace(dpy, bf->next);
	free(bf->next);
    }
    bf->next = NULL;
#endif
    bf->style = SimpleButton;
}

/*****************************************************************************
 * 
 * Reads a button face line into a structure (veliaa@rpi.edu)
 *
 ****************************************************************************/
Boolean ReadButtonFace(char *s, ButtonFace *bf, int button, int verbose)
{
    int offset;
    char style[256], *file;
    char *action = s;

    if (sscanf(s, "%s%n", style, &offset) < 1) {
	if(verbose)scwm_msg(ERR, "ReadButtonFace", "error in face: %s", action);
	return False;
    }

    if (mystrncasecmp(style, "--", 2) != 0) {
	s += offset;

	FreeButtonFace(dpy, bf);
	
	/* determine button style */
	if (mystrncasecmp(style,"Simple",6)==0)
	{
	    bf->style = SimpleButton;
	}
	else if (mystrncasecmp(style,"Default",7)==0) {
	    int b = -1, n = sscanf(s, "%d%n", &b, &offset);
	    	    
	    if (n < 1) {
		if (button == -1) {
		    if(verbose)scwm_msg(ERR,"ReadButtonFace",
					"need default button number to load");
		    return False;
		}
		b = button;
	    }
	    s += offset;
	    if ((b > 0) && (b <= 10))
		LoadDefaultButton(bf, b);
	    else {
		if(verbose)scwm_msg(ERR,"ReadButtonFace",
				    "button number out of range: %d", b);
		return False;
	    }
	}
#ifdef VECTOR_BUTTONS
	else if (mystrncasecmp(style,"Vector",6)==0 || 
		 (strlen(style)<=2 && isdigit(*style)))
	{    
	    /* normal coordinate list button style */	    
	    int i, num_coords, num;
	    struct vector_coords *vc = &bf->vector;

	    /* get number of points */
	    if (mystrncasecmp(style,"Vector",6)==0) {
		num = sscanf(s,"%d%n",&num_coords,&offset);
		s += offset;
	    } else
		num = sscanf(style,"%d",&num_coords);

	    if((num != 1)||(num_coords>20)||(num_coords<2))
	    {
		if(verbose)scwm_msg(ERR,"ReadButtonFace",
				    "Bad button style (2) in line: %s",action);
		return False;
	    }
  
	    vc->num = num_coords;

	    /* get the points */
	    for(i = 0; i < vc->num; ++i)
	    {
		/* X x Y @ line_style */
		num = sscanf(s,"%dx%d@%d%n",&vc->x[i],&vc->y[i],&vc->line_style[i],
			     &offset);
		if(num != 3)
		{
		    if(verbose)scwm_msg(ERR,"ReadButtonFace",
					"Bad button style (3) in line %s",action);
		    return False;
		}
		s += offset;
	    }
	    bf->style = VectorButton;
	}
#endif
	else if (mystrncasecmp(style,"Solid",5)==0)
	{
	    s = GetNextToken(s, &file);
	    if (file && *file) {
		bf->style = SolidButton;
		bf->u.back = GetColor(file);
	    } else {
		if(verbose)scwm_msg(ERR,"ReadButtonFace",
				    "no color given for Solid face type: %s", 
				    action);
		return False;
	    }
	    free(file);
	}
#ifdef GRADIENT_BUTTONS
	else if (mystrncasecmp(style,"HGradient",9)==0
		 || mystrncasecmp(style,"VGradient",9)==0)
	{
	    char *item, **s_colors;
	    int npixels, nsegs, i, sum, *perc;
	    Pixel *pixels;

	    if (!(s = GetNextToken(s, &item)) || (item == NULL)) {
		if(verbose)scwm_msg(ERR,"ReadButtonFace",
				    "expected number of colors to allocate in gradient");
		return False;
	    }
	    npixels = atoi(item); free(item);

	    if (!(s = GetNextToken(s, &item)) || (item == NULL)) {
		if(verbose)scwm_msg(ERR,"ReadButtonFace",
				    "incomplete gradient style");
		return False;
	    }

	    if (!(isdigit(*item))) {
		s_colors = (char **)safemalloc(sizeof(char *) * 2);
		perc = (int *)safemalloc(sizeof(int));
		nsegs = 1;
		s_colors[0] = item;
		s = GetNextToken(s, &s_colors[1]);
		perc[0] = 100;
	    } else {
		nsegs = atoi(item); free(item);
		if (nsegs < 1) nsegs = 1;
		if (nsegs > 128) nsegs = 128;
		s_colors = (char **)safemalloc(sizeof(char *) * (nsegs + 1));
		perc = (int *)safemalloc(sizeof(int) * nsegs);
		for (i = 0; i <= nsegs; ++i) {
		    s =GetNextToken(s, &s_colors[i]);		
		    if (i < nsegs) {
			s = GetNextToken(s, &item);
			if (item)
			    perc[i] = atoi(item);
			else 
			    perc[i] = 0;
			free(item);
		    }
		} 
	    }

	    for (i = 0, sum = 0; i < nsegs; ++i)
		sum += perc[i];

	    if (sum != 100) {
		if(verbose)scwm_msg(ERR,"ReadButtonFace",
				    "multi gradient lenghts must sum to 100");
		for (i = 0; i <= nsegs; ++i)
		    free(s_colors[i]);
		free(s_colors);
		return False;
	    }
		
	    if (npixels < 2) npixels = 2;
	    if (npixels > 128) npixels = 128;

	    pixels = AllocNonlinearGradient(s_colors, perc, nsegs, npixels);
	    for (i = 0; i <= nsegs; ++i)
		free(s_colors[i]);
	    free(s_colors);
	    
	    if (!pixels) {
		if(verbose)scwm_msg(ERR,"ReadButtonFace",
				    "couldn't create gradient");
		return False;
	    }

	    bf->u.grad.pixels = pixels;
	    bf->u.grad.npixels = npixels;
		
	    if (mystrncasecmp(style,"H",1)==0)
		bf->style = HGradButton;
	    else
		bf->style = VGradButton;
	}
#endif /* GRADIENT_BUTTONS */
#ifdef PIXMAP_BUTTONS
	else if (mystrncasecmp(style,"Pixmap",6)==0
		 || mystrncasecmp(style,"TiledPixmap",11)==0)
	{
	    s = GetNextToken(s, &file);
	    bf->u.p = CachePicture(dpy, Scr.Root,
				   IconPath, 
#ifdef XPM
				   PixmapPath,
#else
				   NULL,
#endif
				   file);
	    if (bf->u.p == NULL)
	    {
		if(verbose)scwm_msg(ERR,"ReadButtonFace",
				    "couldn't load pixmap %s",
				    file);
		free(file);
		return False;
	    }
	    free(file); file = NULL;
	
	    if (mystrncasecmp(style,"Tiled",5)==0)
		bf->style = TiledPixmapButton;
	    else
		bf->style = PixmapButton;
	}
#ifdef MINI_ICONS
	else if (mystrncasecmp (style, "MiniIcon", 8) == 0) {
	    bf->style = MiniIconButton;
	    bf->u.p = NULL; /* pixmap read in when the window is created */
  	}
#endif
#endif /* PIXMAP_BUTTONS */
	else {
	    if(verbose)scwm_msg(ERR,"ReadButtonFace",
				"unknown style %s: %s", style, action);
	    return False;
	}
    }
    
    /* Process button flags ("--" signals start of flags,
       it is also checked for above) */
    s = GetNextToken(s, &file);
    if (file && (strcmp(file,"--")==0)) {
	char *tok;
	s = GetNextToken(s, &tok);
	while (tok && tok[0])
	{
	    int set = 1;

	    if (*tok == '!') { /* flag negate */
		set = 0;
		++tok;
	    }
	    if (mystrncasecmp(tok,"Clear",5)==0) {
		if (set)
		    bf->style &= ButtonFaceTypeMask;
		else
		    bf->style |= ~ButtonFaceTypeMask; /* ? */
	    }
	    else if (mystrncasecmp(tok,"Left",4)==0)
	    {
		if (set) {
		    bf->style |= HOffCenter;
		    bf->style &= ~HRight;
		} else
		    bf->style |= HOffCenter | HRight;
	    } 
	    else if (mystrncasecmp(tok,"Right",5)==0)
	    {
		if (set)
		    bf->style |= HOffCenter | HRight;
		else {
		    bf->style |= HOffCenter;
		    bf->style &= ~HRight;		  
		}
	    } 
	    else if (mystrncasecmp(tok,"Centered",8)==0) {
		bf->style &= ~HOffCenter;
		bf->style &= ~VOffCenter;
	    }
	    else if (mystrncasecmp(tok,"Top",3)==0)
	    {
		if (set) {
		    bf->style |= VOffCenter;
		    bf->style &= ~VBottom;
		} else
		    bf->style |= VOffCenter | VBottom;
		  
	    } 
	    else if (mystrncasecmp(tok,"Bottom",6)==0)
	    {
		if (set)
		    bf->style |= VOffCenter | VBottom;
		else {
		    bf->style |= VOffCenter;
		    bf->style &= ~VBottom;
		}
	    }
	    else if (mystrncasecmp(tok,"Flat",4)==0)
	    {
		if (set) {
		    bf->style &= ~SunkButton;
		    bf->style |= FlatButton;
		} else
		    bf->style &= ~FlatButton;
	    } 
	    else if (mystrncasecmp(tok,"Sunk",4)==0)
	    {
		if (set) {
		    bf->style &= ~FlatButton;
		    bf->style |= SunkButton;
		} else
		    bf->style &= ~SunkButton;
	    } 
	    else if (mystrncasecmp(tok,"Raised",6)==0)
	    {
		if (set) {
		    bf->style &= ~FlatButton;
		    bf->style &= ~SunkButton;
		} else {
		    bf->style |= SunkButton;		  
		    bf->style &= ~FlatButton;
		}
	    } 
#ifdef EXTENDED_TITLESTYLE
	    else if (mystrncasecmp(tok,"UseTitleStyle",13)==0)
	    {
		if (set) {
		    bf->style |= UseTitleStyle;
#ifdef BORDERSTYLE
		    bf->style &= ~UseBorderStyle;
#endif
		} else
		    bf->style &= ~UseTitleStyle;
	    }
#endif
#ifdef BORDERSTYLE
	    else if (mystrncasecmp(tok,"HiddenHandles",13)==0)
	    {
		if (set)
		    bf->style |= HiddenHandles;
		else
		    bf->style &= ~HiddenHandles;
	    } 
	    else if (mystrncasecmp(tok,"NoInset",7)==0)
	    {
		if (set)
		    bf->style |= NoInset;
		else
		    bf->style &= ~NoInset;
	    }
	    else if (mystrncasecmp(tok,"UseBorderStyle",14)==0)
	    {
		if (set) {
		    bf->style |= UseBorderStyle;
#ifdef EXTENDED_TITLESTYLE
		    bf->style &= ~UseTitleStyle;
#endif
		} else
		    bf->style &= ~UseBorderStyle;
	    }
#endif
	    else
		if(verbose)scwm_msg(ERR,"ReadButtonFace",
				    "unknown button face flag %s -- line: %s", tok, action);
	    if (set) free(tok);
	    else free(tok - 1);
	    s = GetNextToken(s, &tok);
	}
    }
    return True;
}


/*****************************************************************************
 * 
 * Reads a title button description (veliaa@rpi.edu)
 *
 ****************************************************************************/
char *ReadTitleButton(char *s, TitleButton *tb, Boolean append, int button)
{
    char *end = NULL, *spec;
    ButtonFace tmpbf;
    enum ButtonState bs = MaxButtonState;
    int i = 0, all = 0, pstyle = 0;

    while(isspace(*s))++s;
    for (; i < MaxButtonState; ++i)
	if (mystrncasecmp(button_states[i],s,
			  strlen(button_states[i]))==0) {
	    bs = i;
	    break;
	}
    if (bs != MaxButtonState)
	s += strlen(button_states[bs]);
    else 
	all = 1;
    while(isspace(*s))++s;
    if ('(' == *s) {
	int len;
	pstyle = 1;
	if (!(end = strchr(++s, ')'))) {
	    scwm_msg(ERR,"ReadTitleButton",
		     "missing parenthesis: %s", s);
	    return NULL;
	}
	len = end - s + 1;
	spec = safemalloc(len);
	strncpy(spec, s, len - 1);
	spec[len - 1] = 0;
    } else
	spec = s;

    while(isspace(*spec))++spec;
    /* setup temporary in case button read fails */
    tmpbf.style = SimpleButton;
#ifdef MULTISTYLE
    tmpbf.next = NULL;
#endif

    if (strncmp(spec, "--",2)==0) {
	/* only change flags */
	if (ReadButtonFace(spec, &tb->state[all ? 0 : bs],button,True) && all) {
	    for (i = 0; i < MaxButtonState; ++i)
		ReadButtonFace(spec, &tb->state[i],-1,False);
	}
    }
    else if (ReadButtonFace(spec, &tmpbf,button,True)) {
	int b = all ? 0 : bs;
#ifdef MULTISTYLE
	if (append) {
	    ButtonFace *tail = &tb->state[b];
	    while (tail->next) tail = tail->next;
	    tail->next = (ButtonFace *)safemalloc(sizeof(ButtonFace));
	    *tail->next = tmpbf;
	    if (all) 
		for (i = 1; i < MaxButtonState; ++i) {
		    tail = &tb->state[i];
		    while (tail->next) tail = tail->next;
		    tail->next = (ButtonFace *)safemalloc(sizeof(ButtonFace));
		    tail->next->style = SimpleButton;
		    tail->next->next = NULL;
		    ReadButtonFace(spec, tail->next, button, False);
		}
	} 
	else {
#endif
	    FreeButtonFace(dpy, &tb->state[b]);
	    tb->state[b] = tmpbf;
	    if (all)
		for (i = 1; i < MaxButtonState; ++i)
		    ReadButtonFace(spec, &tb->state[i],button,False);
#ifdef MULTISTYLE
	}
#endif
	
    }
    if (pstyle) {
	free(spec);
	++end;
	while(isspace(*end))++end;
    }
    return end;
}


#ifdef USEDECOR
/*****************************************************************************
 * 
 * Diverts a style definition to an ScwmDecor structure (veliaa@rpi.edu)
 *
 ****************************************************************************/
void AddToDecor(ScwmDecor *fl, char *s)
{
    if (!s) return;
    while (*s&&isspace(*s))++s;
    if (!*s) return;
    cur_decor = fl;
#if 0
    ExecuteFunction(s,NULL,&Event,C_ROOT,-1);
#endif
    cur_decor = NULL;
}

/*****************************************************************************
 * 
 * Changes the window's ScwmDecor pointer (veliaa@rpi.edu)
 *
 ****************************************************************************/
void ChangeDecor(XEvent *eventp,Window w,ScwmWindow *tmp_win,
		unsigned long context, char *action,int *Module)
{
    char *item;
    int x,y,width,height,old_height,extra_height;
    ScwmDecor *fl = &Scr.DefaultDecor, *found = NULL;
    if (DeferExecution(eventp,&w,&tmp_win,&context, SELECT,ButtonRelease))
	return;
    action = GetNextToken(action, &item);
    if (!(action && item && item[0]))
	return;
    /* search for tag */
    for (; fl; fl = fl->next)
	if (fl->tag)
	    if (mystrcasecmp(item, fl->tag)==0) {
		found = fl;
		break;
	    }
    free(item);
    if (!found) {
	XBell(dpy,Scr.screen);
	return;
    }
    old_height = tmp_win->fl->TitleHeight;
    tmp_win->fl = found;
    extra_height = (tmp_win->flags & TITLE) ?
      (old_height - tmp_win->fl->TitleHeight) : 0;
    x = tmp_win->frame_x;
    y = tmp_win->frame_y;
    width = tmp_win->frame_width;
    height = tmp_win->frame_height - extra_height;
    tmp_win->frame_x = 0;
    tmp_win->frame_y = 0;
    tmp_win->frame_height = 0;
    tmp_win->frame_width = 0;
    SetupFrame(tmp_win,x,y,width,height,True);
    SetBorder(tmp_win,Scr.Hilite == tmp_win,True,True,None);
}

/*****************************************************************************
 * 
 * Destroys an ScwmDecor (veliaa@rpi.edu)
 *
 ****************************************************************************/
void DestroyDecor(XEvent *eventp,Window junk,ScwmWindow *tmp_win,
		 unsigned long context, char *action,int* Module)
{
    char *item;
    ScwmDecor *fl = Scr.DefaultDecor.next;
    ScwmDecor *prev = &Scr.DefaultDecor, *found = NULL;

    action = GetNextToken(action, &item);
    if (!(action && item && item[0]))
	return;

    /* search for tag */
    for (; fl; fl = fl->next) {
	if (fl->tag)
	    if (mystrcasecmp(item, fl->tag)==0) {
		found = fl;
		break;
	    }
	prev = fl;
    }
    
    if (found && (found != &Scr.DefaultDecor)) {
	ScwmWindow *fw = Scr.ScwmRoot.next;
	while(fw != NULL)
	{
#if 0
	    if (fw->fl == found)
		ExecuteFunction("ChangeDecor Default",fw,eventp,C_WINDOW,*Module);
#endif
	    fw = fw->next;
	}
	prev->next = found->next;
	DestroyScwmDecor(found);
	free(found);
    }
}

/*****************************************************************************
 * 
 * Initiates an AddToDecor (veliaa@rpi.edu)
 *
 ****************************************************************************/
void add_item_to_decor(XEvent *eventp,Window junk,ScwmWindow *tmp_win,
		      unsigned long context, char *action,int* Module)
{
    ScwmDecor *fl = &Scr.DefaultDecor, *found = NULL;
    char *item = NULL, *s = action;

    last_menu = NULL;

    s = GetNextToken(s, &item);

    if (!(s && item && item[0]))
	return;

    /* search for tag */
    for (; fl; fl = fl->next)
	if (fl->tag)
	    if (mystrcasecmp(item, fl->tag)==0) {
		found = fl;
		break;
	    }
    if (!found) { /* then make a new one */
	found = (ScwmDecor *)safemalloc(sizeof( ScwmDecor ));
	InitScwmDecor(found);
	found->tag = item; /* tag it */
	/* add it to list */
	for (fl = &Scr.DefaultDecor; fl->next; fl = fl->next);
	fl->next = found;
    } else
	free(item);
    if (found) {
	AddToDecor(found, s);
	last_decor = found;
    }
}
#endif /* USEDECOR */


/*****************************************************************************
 * 
 * Updates window decoration styles (veliaa@rpi.edu)
 *
 ****************************************************************************/
void UpdateDecor(XEvent *eventp,Window junk,ScwmWindow *tmp_win,
		 unsigned long context, char *action,int* Module)
{
    ScwmWindow *fw = Scr.ScwmRoot.next;
#ifdef USEDECOR
    ScwmDecor *fl = &Scr.DefaultDecor, *found = NULL;
    char *item = NULL;
    action = GetNextToken(action, &item);
    if (item) {
	/* search for tag */
	for (; fl; fl = fl->next)
	    if (fl->tag)
		if (mystrcasecmp(item, fl->tag)==0) {
		    found = fl;
		    break;
		}
	free(item);
    }
#endif
    
    for (; fw != NULL; fw = fw->next)
    {
#ifdef USEDECOR
	/* update specific decor, or all */
	if (found) {
	    if (fw->fl == found) {
		SetBorder(fw,True,True,True,None);
		SetBorder(fw,False,True,True,None);
	    }
	}
	else
#endif
	{
	    SetBorder(fw,True,True,True,None);
	    SetBorder(fw,False,True,True,None);
	}
    }
    SetBorder(Scr.Hilite,True,True,True,None);
}


/*****************************************************************************
 * 
 * Changes a button decoration style (changes by veliaa@rpi.edu)
 *
 ****************************************************************************/
void ButtonStyle(XEvent *eventp,Window junk,ScwmWindow *tmp_win,
                 unsigned long context, char *action,int* Module)
{
    int button = 0,n;
    int multi = 0;
    char *text = action, *prev;
    char *parm = NULL;
    TitleButton *tb = NULL;
#ifdef USEDECOR
    ScwmDecor *fl = cur_decor ? cur_decor : &Scr.DefaultDecor;
#else
    ScwmDecor *fl = &Scr.DefaultDecor;
#endif
    
    text = GetNextToken(text, &parm);
    if (parm && isdigit(*parm))
	button = atoi(parm);
    
    if ((parm == NULL) || (button > 10) || (button < 0)) {
	scwm_msg(ERR,"ButtonStyle","Bad button style (1) in line %s",action);
	free(parm);
	return;
    }

    if (!isdigit(*parm)) {
	if (mystrcasecmp(parm,"left")==0)
	    multi = 1; /* affect all left buttons */
	else if (mystrcasecmp(parm,"right")==0)
	    multi = 2; /* affect all right buttons */
	else if (mystrcasecmp(parm,"all")==0)
	    multi = 3; /* affect all buttons */
	else {
	    /* we're either resetting buttons or
	       an invalid button set was specified */
	    if (mystrcasecmp(parm,"reset")==0)
		ResetAllButtons(fl);
	    else
		scwm_msg(ERR,"ButtonStyle","Bad button style (2) in line %s",action);
	    free(parm);
	    return;
	}
    } 
    free(parm);
    if (multi == 0) {
	/* a single button was specified */
	if (button==10) button=0;
	/* which arrays to use? */
	n=button/2;
	if((n*2) == button)
	{
	    /* right */
	    n = n - 1;
	    if(n<0)n=4;
	    tb = &fl->right_buttons[n];
	}
	else {
	    /* left */
	    tb = &fl->left_buttons[n];
	}
    }
	
    prev = text;
    text = GetNextToken(text,&parm);
    while(parm && parm[0]!='\0')
    {
	if (strcmp(parm,"-")==0) {		
	    char *tok;
	    text = GetNextToken(text, &tok);
	    while (tok && tok[0])
	    {
		int set = 1;
		
		if (*tok == '!') { /* flag negate */
		    set = 0;
		    ++tok;
		}
		if (mystrncasecmp(tok,"Clear",5)==0) {
		    int i;
		    if (multi) {
			if (multi&1)
			    for (i=0;i<5;++i)
				if (set)
				    fl->left_buttons[i].flags = 0;
				else
				    fl->left_buttons[i].flags = ~0;
			if (multi&2)
			    for (i=0;i<5;++i)
				if (set)
				    fl->right_buttons[i].flags = 0;
				else
				    fl->right_buttons[i].flags = ~0;
		    } else
			if (set)
			    tb->flags = 0;
			else
			    tb->flags = ~0;
		}
		else if (mystrncasecmp(tok,"MWMButton",9)==0)
		{
		    if (!multi)
			if (set)
			    tb->flags |= MWMButton;
			else
			    tb->flags &= ~MWMButton;
		}
		else
		    scwm_msg(ERR,"ButtonStyle",
			     "unknown title button flag %s -- line: %s", tok, text);
		if (set) free(tok);
		else free(tok - 1);
		text = GetNextToken(text, &tok);
	    }
	    free(parm);
	    break;
	}
	else {
	    if (multi) {
		int i;
		if (multi&1)
		    for (i=0;i<5;++i)
			text = ReadTitleButton(prev, &fl->left_buttons[i], False, i*2+1);
		if (multi&2)
		    for (i=0;i<5;++i)
			text = ReadTitleButton(prev, &fl->right_buttons[i], False, i*2);
	    }
	    else if (!(text = ReadTitleButton(prev, tb, False, button))) {
		free(parm);
		break;
	    }
	}
	free(parm);
	prev = text;
	text = GetNextToken(text,&parm);
    }
}

#ifdef MULTISTYLE
/*****************************************************************************
 * 
 * Appends a button decoration style (veliaa@rpi.edu)
 *
 ****************************************************************************/
void AddButtonStyle(XEvent *eventp,Window junk,ScwmWindow *tmp_win,
		    unsigned long context, char *action,int* Module)
{
    int button = 0,n;
    int multi = 0;
    char *text = action, *prev;
    char *parm = NULL;
    TitleButton *tb = NULL;
#ifdef USEDECOR
    ScwmDecor *fl = cur_decor ? cur_decor : &Scr.DefaultDecor;
#else
    ScwmDecor *fl = &Scr.DefaultDecor;
#endif
    
    text = GetNextToken(text, &parm);
    if (parm && isdigit(*parm))
	button = atoi(parm);
    
    if ((parm == NULL) || (button > 10) || (button < 0)) {
	scwm_msg(ERR,"ButtonStyle","Bad button style (1) in line %s",action);
	free(parm);
	return;
    }

    if (!isdigit(*parm)) {
	if (mystrcasecmp(parm,"left")==0)
	    multi = 1; /* affect all left buttons */
	else if (mystrcasecmp(parm,"right")==0)
	    multi = 2; /* affect all right buttons */
	else if (mystrcasecmp(parm,"all")==0)
	    multi = 3; /* affect all buttons */
	else {
	    /* we're either resetting buttons or
	       an invalid button set was specified */
	    if (mystrcasecmp(parm,"reset")==0)
		ResetAllButtons(fl);
	    else
		scwm_msg(ERR,"ButtonStyle","Bad button style (2) in line %s",action);
	    free(parm);
	    return;
	}
    } 
    free(parm);
    if (multi == 0) {
	/* a single button was specified */
	if (button==10) button=0;
	/* which arrays to use? */
	n=button/2;
	if((n*2) == button)
	{
	    /* right */
	    n = n - 1;
	    if(n<0)n=4;
	    tb = &fl->right_buttons[n];
	}
	else {
	    /* left */
	    tb = &fl->left_buttons[n];
	}
    }

    prev = text;
    text = GetNextToken(text,&parm);
    while(parm && parm[0]!='\0')
    {
	if (multi) {
	    int i;
	    if (multi&1)
		for (i=0;i<5;++i)
		    text = ReadTitleButton(prev, &fl->left_buttons[i], True, i*2+1);
	    if (multi&2)
		for (i=0;i<5;++i)
		    text = ReadTitleButton(prev, &fl->right_buttons[i], True, i*2);
	}
	else if (!(text = ReadTitleButton(prev, tb, True, button))) {
	    free(parm);
	    break;
	}
	free(parm);
	prev = text;
	text = GetNextToken(text,&parm);
    }
}
#endif /* MULTISTYLE */

#if MS_DELETION_COMMENT
/**************************************************************************
 *
 * Direction = 1 ==> "Next" operation
 * Direction = -1 ==> "Previous" operation 
 *
 **************************************************************************/
ScwmWindow *Circulate(char *action, int Direction, char **restofline)
{
  int l,pass = 0;
  ScwmWindow *fw, *found = NULL;
  char *t,*tstart,*name = NULL, *expression, *condition, *prev_condition=NULL;
  char *orig_expr;
  Bool needsIconic = 0;
  Bool needsNormal = 0;
  Bool needsCurrentDesk = 0;
  Bool needsCurrentPage = 0;
  Bool needsVisible = 0;
  Bool needsInvisible = 0;
  char *AnyWindow = "*";
  Bool useCirculateHit = 0;
  Bool useCirculateHitIcon = 0;

  l=0;

  if(action == NULL)
    return;

  t = action;
  while(isspace(*t)&&(*t!= 0))
    t++;
  if(*t == '[')
  {
    t++;
    tstart = t;

    while((*t !=0)&&(*t != ']'))
    {
      t++;
      l++;
    }
    if(*t == 0)
    {
      scwm_msg(ERR,"Circulate","Conditionals require closing brace");
      return NULL;
    }
      
    *restofline = t+1;
      
    orig_expr = expression = safemalloc(l+1);
    strncpy(expression,tstart,l);
    expression[l] = 0;
    expression = GetNextToken(expression,&condition);
    while((condition != NULL)&&(strlen(condition) > 0))
    {
      if(mystrcasecmp(condition,"iconic")==0)
        needsIconic = 1;
      else if(mystrcasecmp(condition,"!iconic")==0)
        needsNormal = 1;
      else if(mystrcasecmp(condition,"CurrentDesk")==0)
        needsCurrentDesk = 1;
      else if(mystrcasecmp(condition,"CurrentPage")==0)
      {
        needsCurrentDesk = 1;
        needsCurrentPage = 1;
      }
      else if(mystrcasecmp(condition,"Visible")==0)
        needsVisible = 1;
      else if(mystrcasecmp(condition,"!Visible")==0)
        needsInvisible = 1;
      else if(mystrcasecmp(condition,"CurrentPageAnyDesk")==0 ||
              mystrcasecmp(condition,"CurrentScreen")==0)
        needsCurrentPage = 1;
      else if(mystrcasecmp(condition,"CirculateHit")==0)
        useCirculateHit = 1;
      else if(mystrcasecmp(condition,"CirculateHitIcon")==0)
        useCirculateHitIcon = 1;
      else
      {
        name = condition;
        condition = NULL;
      }
      if(prev_condition)free(prev_condition);
      prev_condition = condition;
      expression = GetNextToken(expression,&condition);
    }
    if(prev_condition != NULL)
      free(prev_condition);
    if(orig_expr != NULL)
      free(orig_expr);
  }
  else
    *restofline = t;

  if(name == NULL)
    name = AnyWindow;

  if(Scr.Focus != NULL)
  {
    if(Direction == 1)
      fw = Scr.Focus->prev;
    else
      fw = Scr.Focus->next;
  }
  else
    fw = Scr.ScwmRoot.prev;  

  while((pass < 3)&&(found == NULL))
  {
    while((fw != NULL)&&(found==NULL)&&(fw != &Scr.ScwmRoot))
    {
      /* Make CirculateUp and CirculateDown take args. by Y.NOMURA */
      if (((matchWildcards(name, fw->name)) ||
           (matchWildcards(name, fw->icon_name))||
           (fw->class.res_class &&
            matchWildcards(name, fw->class.res_class))||
           (fw->class.res_name &&
            matchWildcards(name, fw->class.res_name)))&&
          ((useCirculateHit)||!(fw->flags & CirculateSkip))&&
          (((useCirculateHitIcon)&&(fw->flags & ICONIFIED))||
           !((fw->flags & CirculateSkipIcon)&&(fw->flags & ICONIFIED)))&&
          (((!needsIconic)||(fw->flags & ICONIFIED))&&
           ((!needsNormal)||(!(fw->flags & ICONIFIED)))&&
           ((!needsCurrentDesk)||(fw->Desk == Scr.CurrentDesk))&&
           ((!needsVisible)||(fw->flags & VISIBLE))&&
           ((!needsInvisible)||(!(fw->flags & VISIBLE)))&&
           ((!needsCurrentPage)||((fw->frame_x < Scr.MyDisplayWidth)&&
                                  (fw->frame_y < Scr.MyDisplayHeight)&&
                                  (fw->frame_x+fw->frame_width > 0)&&
                                  (fw->frame_y+fw->frame_height > 0)))))
        found = fw;
      else
      {
        if(Direction == 1)
          fw = fw->prev;
        else
          fw = fw->next;
      }
    }
    if((fw == NULL)||(fw == &Scr.ScwmRoot))
    {
      if(Direction == 1)
      {
        /* Go to end of list */
        fw = &Scr.ScwmRoot;
        while((fw) && (fw->next != NULL))
        {
          fw = fw->next;
        }
      }
      else
      {
        /* GO to top of list */
        fw = Scr.ScwmRoot.next;
      }
    }
    pass++;
  }
  if((name != NULL)&&(name != AnyWindow))
    free(name);
  return found;

}
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT
void PrevFunc(XEvent *eventp,Window junk,ScwmWindow *tmp_win,
              unsigned long context, char *action,int* Module)
{
  ScwmWindow *found;
  char *restofline;

  found = Circulate(action, -1, &restofline);
  if(found != NULL)
  {
#if 0
    ExecuteFunction(restofline,found,eventp,C_WINDOW,*Module);
#endif
  }

}
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT
void NextFunc(XEvent *eventp,Window junk,ScwmWindow *tmp_win,
              unsigned long context, char *action,int* Module)
{
  ScwmWindow *found;
  char *restofline;

  found = Circulate(action, 1, &restofline);
  if(found != NULL)
  {
#if 0
    ExecuteFunction(restofline,found,eventp,C_WINDOW,*Module);
#endif
  }

}
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT
void NoneFunc(XEvent *eventp,Window junk,ScwmWindow *tmp_win,
              unsigned long context, char *action,int* Module)
{
  ScwmWindow *found;
  char *restofline;

  found = Circulate(action, 1, &restofline);
  if(found == NULL)
  {
#if 0
    ExecuteFunction(restofline,NULL,eventp,C_ROOT,*Module);
#endif
  }
}
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT
void WindowIdFunc(XEvent *eventp,Window junk,ScwmWindow *tmp_win,
                  unsigned long context, char *action,int* Module)
{
  ScwmWindow *found=NULL,*t;
  char *restofline,*num;
  unsigned long win;

  restofline = strdup(action);
  num = GetToken(&restofline);
  win = (unsigned long)strtol(num,NULL,0); /* SunOS doesn't have strtoul */
  for (t = Scr.ScwmRoot.next; t != NULL; t = t->next)
  {
    if (t->w == win)
    {
      found = t;
      break;
    }
  }
  if(found)
  {
#if 0
    ExecuteFunction(restofline,found,eventp,C_WINDOW,*Module);
#endif
  }
  if (restofline)
    free(restofline);
}
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT
void module_zapper(XEvent *eventp,Window junk,ScwmWindow *tmp_win,
                   unsigned long context, char *action,int* Module)
{
  char *condition;

  GetNextToken(action,&condition);
  KillModuleByName(condition);
  free(condition);
}
#endif /* MS_DELETION_COMMENT */

#if MS_DELETION_COMMENT
/***********************************************************************
 *
 *  Procedure:
 *	Reborder - Removes scwm border windows
 *
 ************************************************************************/
void Recapture(XEvent *eventp,Window junk,ScwmWindow *tmp_win,
               unsigned long context, char *action,int* Module)
{
  BlackoutScreen(); /* if they want to hide the recapture */
  CaptureAllWindows();
  UnBlackoutScreen();
}
#endif /* MS_DELETION_COMMENT */

void SetGlobalOptions(XEvent *eventp,Window junk,ScwmWindow *tmp_win,
                      unsigned long context, char *action,int* Module)
{
  char *opt,*opts;

  if (!action || !action[0])
    return;
  else
    opts = strdup(action);

  /* scwm_msg(DBG,"SetGlobalOptions","init action == '%s'\n",action); */

  while ((opt = GetToken(&opts)))
  {
    /* scwm_msg(DBG,"SetGlobalOptions"," opt == '%s'\n",opt); */
    /* scwm_msg(DBG,"SetGlobalOptions"," remaining == '%s'\n",opts?opts:"(NULL)"); */
    if (StrEquals(opt,"SMARTPLACEMENTISREALLYSMART"))
    {
      Scr.SmartPlacementIsClever = True;
    }
    else if (StrEquals(opt,"SMARTPLACEMENTISNORMAL"))
    {
      Scr.SmartPlacementIsClever = False;
    }
    else if (StrEquals(opt,"CLICKTOFOCUSDOESNTPASSCLICK"))
    {
      Scr.ClickToFocusPassesClick = False;
    }
    else if (StrEquals(opt,"CLICKTOFOCUSPASSESCLICK"))
    {
      Scr.ClickToFocusPassesClick = True;
    }
    else if (StrEquals(opt,"CLICKTOFOCUSDOESNTRAISE"))
    {
      Scr.ClickToFocusRaises = False;
    }
    else if (StrEquals(opt,"CLICKTOFOCUSRAISES"))
    {
      Scr.ClickToFocusRaises = True;
    }
    else if (StrEquals(opt,"MOUSEFOCUSCLICKDOESNTRAISE"))
    {
      Scr.MouseFocusClickRaises = False;
    }
    else if (StrEquals(opt,"MOUSEFOCUSCLICKRAISES"))
    {
      Scr.MouseFocusClickRaises = True;
    }
    else
      scwm_msg(ERR,"GlobalOpts","Unknown Global Option '%s'",opt);

    if (opt) /* should never be null, but checking anyways... */
      free(opt);
  }
  if (opts) /* should be empty at this point... */
    free(opts);
}






