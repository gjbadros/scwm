/****************************************************************************
 * This module has been significantly modified by Maciej Stachowiak.
 * It may be used under the terms of the fvwm copyright (see COPYING.FVWM).
 * Changes Copyright 1997, Maciej stachowiak
 ****************************************************************************/
#include "../configure.h"
#include "scwm.h"
#include "screen.h"
#include "misc.h"
#include "parse.h"
#include "module.h"
#include <stdio.h>
#include <guile/gh.h>
#include <X11/keysym.h>
#include "window.h"
#include "util.h"

long scm_tc16_scwm_window;

SCM window_context = SCM_UNDEFINED;

size_t free_window (SCM obj) 
{
  free(WINDOW(obj));
  return(0);
}

int print_window (SCM obj, SCM port, scm_print_state *pstate) {
  scm_gen_puts(scm_regular_port, "#<window ", port);
  if (VALIDWINP(obj)) {
    scm_write(gh_ulong2scm((unsigned long)(SCWMWINDOW(obj)->w)), port);
  } else {
    scm_gen_puts(scm_regular_port, "(invalid)", port);
  }
  scm_gen_putc('>', port);
  return 1;
}


SCM make_window(ScwmWindow *win)
{
  scwm_window *schwin;
  SCM answer;
  gh_defer_ints();
  schwin=(scwm_window *)malloc(sizeof(scwm_window));
  if (schwin==NULL) {
    gh_allow_ints();
    scm_memory_error("make_window");
  }
  SCM_NEWCELL (answer);
  SCM_SETCAR (answer, scm_tc16_scwm_window);
  SCM_SETCDR (answer, (SCM)schwin);
  SCWMWINDOW(answer)=win;
  VALIDWINP(answer)=1;
  scm_protect_object(answer);
  gh_allow_ints();
  return answer;
}

void invalidate_window(SCM schwin) 
{
  VALIDWINP(schwin)=0;
  SCWMWINDOW(schwin)=NULL;
  scm_unprotect_object(schwin);
}


SCM ensure_valid(SCM win, int n, char *subr, SCM kill_p) {
  if (win==SCM_UNDEFINED) {
    win=get_window(kill_p);
    if (win==SCM_BOOL_F) {
      return SCM_BOOL_F;
    }
  }
  if (!WINDOWP(win)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg(subr,n,win);
  }
  if(!VALIDWINP(win)) {
    SCM_ALLOW_INTS;
    scwm_error(subr,6);
    /* maybe should just return SCM_BOOL_F; */
  }
  return(win);
}

#define VALIDATE(win,subr)  if(((win=ensure_valid(win,1,subr,SCM_BOOL_F)))==SCM_BOOL_F) return SCM_BOOL_F

#define VALIDATEKILL(win,subr)  if(((win=ensure_valid(win,1,subr,SCM_BOOL_T)))==SCM_BOOL_F) return SCM_BOOL_F

#define VALIDATEN(win,n,subr)  if(((win=ensure_valid(win,n,subr,SCM_BOOL_F)))==SCM_BOOL_F) return SCM_BOOL_F



SCM window_p(SCM obj) {
  return  ((SCM_NIMP(obj) && WINDOWP(obj)) ? SCM_BOOL_T : SCM_BOOL_F);
}

SCM get_window(SCM kill_p) 
{
  if (window_context==SCM_UNDEFINED) {
    return select_window(kill_p);
  }
  return window_context;
}


SCM select_window(SCM kill_p) 
{
  XEvent ev;
  Window w;
  ScwmWindow *tmp_win;
  unsigned long context;
  
  SCM_REDEFER_INTS;
  w=Scr.Root;
  context=C_ROOT;

  tmp_win=&Scr.ScwmRoot;

  if (kill_p==SCM_UNDEFINED) {
    kill_p=SCM_BOOL_F;
  } else if (!gh_boolean_p(kill_p)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("select-window",1,kill_p);
  }
  if(DeferExecution(&ev,
		    &w,
		    &tmp_win,
		    &context,
		    (kill_p != SCM_BOOL_F ? DESTROY : SELECT)
		    ,ButtonRelease)) {
    puts("reeturned TRUE");
  }
  /* XXX - this needs to done right. */
  if (tmp_win->schwin!=NULL) {
    SCM_REALLOW_INTS;
    return (tmp_win->schwin);
  } else {
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }
}


SCM delete_window(SCM win)
{
  ScwmWindow *tmp_win;
  SCM_REDEFER_INTS;

  VALIDATEKILL(win,"delete-window");

  tmp_win=SCWMWINDOW(win);
  if(check_allowed_function2(F_DELETE,tmp_win) == 0) {
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }
  if (tmp_win->flags & DoesWmDeleteWindow) {
    send_clientmessage (dpy, tmp_win->w, _XA_WM_DELETE_WINDOW, CurrentTime);
    SCM_REALLOW_INTS;
    return SCM_BOOL_T;
  }
  SCM_REALLOW_INTS;
  return SCM_BOOL_F;
}

SCM destroy_window(SCM win)
{
  ScwmWindow *tmp_win;
  SCM_REDEFER_INTS;
  VALIDATEKILL(win,"destroy-window");
  tmp_win=SCWMWINDOW(win);
  if(check_allowed_function2(F_DESTROY,tmp_win) == 0) {
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }
  if (XGetGeometry(dpy, tmp_win->w, &JunkRoot, &JunkX, &JunkY,
		   &JunkWidth, &JunkHeight, &JunkBW, &JunkDepth) == 0) {
    Destroy(tmp_win);
  } else {
    XKillClient(dpy, tmp_win->w);
  }
  XSync(dpy,0);
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}

SCM window_deletable_p(SCM win)
{
  VALIDATEKILL(win,"window-deletable?");
  return (SCWMWINDOW(win)->flags & DoesWmDeleteWindow) ? 
    SCM_BOOL_T : SCM_BOOL_F;
}

void FocusOn(ScwmWindow *t,int DeIconifyOnly);

SCM focus(SCM win)
{
  SCM_REDEFER_INTS;
  VALIDATE(win,"focus");
  FocusOn(SCWMWINDOW(win),0);
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}

void WarpOn(ScwmWindow *t,int warp_x, int x_unit, int warp_y, int y_unit);

SCM warp_to_window(SCM win)
{
  SCM_REDEFER_INTS;
  VALIDATE(win,"warp-to-window");
  WarpOn (SCWMWINDOW(win), 0, 0, 0, 0);
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}



SCM raise_window(SCM win)
{
  ScwmWindow *tmp_win;
  char *junk, *junkC;
  unsigned long junkN;
  int junkD, method, BoxJunk[4];

  SCM_REDEFER_INTS;  
  VALIDATE(win,"raise-window");
  
  tmp_win= SCWMWINDOW(win);
  
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
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}


SCM lower_window(SCM win)
{
  SCM_REDEFER_INTS;  
  VALIDATE(win,"lower-window");
  LowerWindow(SCWMWINDOW(win));
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}


SCM raised_p(SCM win)
{
  ScwmWindow *tmp_win;
  VALIDATE(win,"raised?");
  tmp_win= SCWMWINDOW(win);
  return ((tmp_win == Scr.LastWindowRaised)||
	  (tmp_win->flags & VISIBLE) ? SCM_BOOL_T : SCM_BOOL_F);
}



SCM iconify(SCM win)
{
  ScwmWindow *tmp_win;

  SCM_REDEFER_INTS;
  VALIDATE(win,"iconify");
  tmp_win=SCWMWINDOW(win);
  
  if(check_allowed_function2(F_ICONIFY,tmp_win) == 0)
    {

      return SCM_BOOL_F;
    }
  Iconify(tmp_win,0,0);

  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}

SCM deiconify(SCM win)
{
  SCM_REDEFER_INTS;
  VALIDATE(win,"deiconify");
  DeIconify(SCWMWINDOW(win));
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}

SCM iconified_p(SCM win)
{
  VALIDATE(win,"iconified?");
  return ((SCWMWINDOW(win)->flags & ICONIFIED) ? 
	  SCM_BOOL_T : SCM_BOOL_F);
}


SCM stick(SCM win)
{
  ScwmWindow *tmp_win;
  SCM_REDEFER_INTS;
  VALIDATE(win,"stick");
  tmp_win=SCWMWINDOW(win);
  tmp_win->flags |=STICKY;
  BroadcastConfig(M_CONFIGURE_WINDOW,tmp_win);
  SetTitleBar(tmp_win,(Scr.Hilite==tmp_win),True);
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}

SCM unstick(SCM win)
{
  ScwmWindow *tmp_win;
  SCM_REDEFER_INTS;
  VALIDATE(win,"unstick");
  tmp_win=SCWMWINDOW(win);
  tmp_win->flags &= ~STICKY;
  BroadcastConfig(M_CONFIGURE_WINDOW,tmp_win);
  SetTitleBar(tmp_win,(Scr.Hilite==tmp_win),True);
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}

SCM sticky_p(SCM win)
{
  VALIDATE(win,"sticky?");
  return (SCWMWINDOW(win)->flags & STICKY) ? SCM_BOOL_T : SCM_BOOL_F;
}




#ifdef WINDOWSHADE
/***********************************************************************
 *
 *  WindowShade -- shades or unshades a window (veliaa@rpi.edu)
 ***********************************************************************/

/* Modified for scwm by mstachow@mit.edu */

SCM window_shade(SCM win)
{
  ScwmWindow *tmp_win;
  SCM_REDEFER_INTS;
  VALIDATE(win,"window-shade");
  tmp_win=SCWMWINDOW(win);

  if (!(tmp_win->flags & TITLE) || (tmp_win->flags & MAXIMIZED)) {
    return SCM_BOOL_F;
  }
  tmp_win->buttons |= WSHADE;
  SetupFrame(tmp_win,
	     tmp_win->frame_x,
	     tmp_win->frame_y,
	     tmp_win->frame_width,
	     tmp_win->title_height + tmp_win->boundary_width,
	     False);
  Broadcast(M_WINDOWSHADE, 1, tmp_win->w, 0, 0, 0, 0, 0, 0);
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}

SCM un_window_shade(SCM win) 
{
  ScwmWindow *tmp_win;
  SCM_REDEFER_INTS;
  VALIDATE(win,"un-window-shade");
  tmp_win=SCWMWINDOW(win);

  tmp_win->buttons &= ~WSHADE;
  SetupFrame(tmp_win,
	     tmp_win->frame_x, 
	     tmp_win->frame_y, 
	     tmp_win->orig_wd,
	     tmp_win->orig_ht,
	     True);
  Broadcast(M_DEWINDOWSHADE, 1, tmp_win->w, 0, 0, 0, 0, 0, 0);
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}

SCM window_shaded_p(SCM win)
{
  VALIDATE(win,"window-shaded?");
  return ((SCWMWINDOW(win)->buttons & WSHADE) ? SCM_BOOL_T : SCM_BOOL_F);
}

#endif /* WINDOWSHADE */


void move_finalize(Window w, ScwmWindow *tmp_win, int x, int y) {
  if (w == tmp_win->frame)
    {
      SetupFrame (tmp_win, x, y,
		  tmp_win->frame_width, tmp_win->frame_height,FALSE);
    }
  else /* icon window */
    {
      tmp_win->flags |= ICON_MOVED;
      tmp_win->icon_x_loc = x ;
      tmp_win->icon_xl_loc = y -
	(tmp_win->icon_w_width - tmp_win->icon_p_width)/2;
      tmp_win->icon_y_loc = y; 
      Broadcast(M_ICON_LOCATION,7,tmp_win->w,tmp_win->frame,
		(unsigned long)tmp_win,
		tmp_win->icon_x_loc,tmp_win->icon_y_loc,
		tmp_win->icon_w_width, tmp_win->icon_w_height
		+tmp_win->icon_p_height);
      XMoveWindow(dpy,tmp_win->icon_w, 
		  tmp_win->icon_xl_loc, y+tmp_win->icon_p_height);
      if(tmp_win->icon_pixmap_w != None)
	{
	  XMapWindow(dpy,tmp_win->icon_w);
	  XMoveWindow(dpy, tmp_win->icon_pixmap_w, tmp_win->icon_x_loc,y);
	  XMapWindow(dpy,w);
	}
    }
}

SCM move_to(SCM x, SCM y, SCM win)
{
  ScwmWindow *tmp_win;
  Window w;

  SCM_REDEFER_INTS;
  VALIDATEN(win,3,"move-to");
  if (!gh_number_p(x)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("move-to",1,x);
  }
  if (!gh_number_p(y)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("move-to",2,y);
  }

  tmp_win=SCWMWINDOW(win);
  w = tmp_win->frame;
  if(tmp_win->flags & ICONIFIED)
    {
      if(tmp_win->icon_pixmap_w != None)
	{
	  XUnmapWindow(dpy,tmp_win->icon_w);
	  w = tmp_win->icon_pixmap_w;
	}
      else
	w = tmp_win->icon_w;
    }

  move_finalize(w,tmp_win,gh_scm2int(x),gh_scm2int(y));
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}

SCM interactive_move(SCM win)
{
  ScwmWindow *tmp_win;
  Window w;
  int x,y;
  SCM_REDEFER_INTS;
  VALIDATE(win,"interactive-move");
  tmp_win=SCWMWINDOW(win);
  w = tmp_win->frame;
  if(tmp_win->flags & ICONIFIED) {
    if(tmp_win->icon_pixmap_w != None) {
      XUnmapWindow(dpy,tmp_win->icon_w);
      w = tmp_win->icon_pixmap_w;
    } else {
      w = tmp_win->icon_w;
    }
  }
  InteractiveMove(&w,tmp_win,&x,&y,NULL);
  move_finalize(w,tmp_win,x,y);
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}

SCM resize_to(SCM w, SCM h, SCM win)
{
  int width,height;
  ScwmWindow *tmp_win;
  
  SCM_REDEFER_INTS;
  VALIDATEN(win,3,"resize-to");
  tmp_win=SCWMWINDOW(win);

  if(check_allowed_function2(F_RESIZE,tmp_win) == 0
#ifdef WINDOWSHADE
     || (tmp_win->buttons & WSHADE)
#endif
     ) {
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }

  tmp_win->flags &= ~MAXIMIZED;
  
  /* can't resize icons */
  if(tmp_win->flags & ICONIFIED) {
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }
  width = gh_scm2int(w);
  height = gh_scm2int(h);
  width += (2*tmp_win->boundary_width);
  height += (tmp_win->title_height + 2*tmp_win->boundary_width);
  
  ConstrainSize (tmp_win, &width, &height);
  SetupFrame (tmp_win, tmp_win->frame_x, 
	      tmp_win->frame_y , width, height,FALSE);
      
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}



extern int dragx;       /* all these variables are used */
extern int dragy;       /* in resize operations */
extern int dragWidth;
extern int dragHeight;

extern int origx;
extern int origy;
extern int origWidth;
extern int origHeight;

extern int ymotion, xmotion;
extern int last_width,last_height;
extern int menuFromFrameOrWindowOrTitlebar;
extern Window PressedW;

SCM interactive_resize(SCM win)
{
  ScwmWindow *tmp_win;
  Bool finished = FALSE, done = FALSE, abort = FALSE;
  int x,y,delta_x,delta_y;
  Window ResizeWindow;
  extern int Stashed_X, Stashed_Y;
  Bool flags;

  VALIDATE(win,"interactive-resize");
  tmp_win=SCWMWINDOW(win);

  
  if(check_allowed_function2(F_RESIZE,tmp_win) == 0
#ifdef WINDOWSHADE
     || (tmp_win->buttons & WSHADE)
#endif
     ) {
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }
  

  tmp_win->flags &= ~MAXIMIZED;

  if(tmp_win->flags & ICONIFIED) {
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }
  ResizeWindow = tmp_win->frame;


  InstallRootColormap();
  if (menuFromFrameOrWindowOrTitlebar) 
    {
      /* warp the pointer to the cursor position from before menu appeared*/
      XWarpPointer(dpy, None, Scr.Root, 0, 0, 0, 0, Stashed_X,Stashed_Y);
      XFlush(dpy);
    }

  if(!GrabEm(MOVE))
    {
      XBell(dpy,Scr.screen);
      SCM_REALLOW_INTS;
      return SCM_BOOL_F;
    }

  MyXGrabServer(dpy);

  /* handle problems with edge-wrapping while resizing */
  flags = Scr.flags;
  Scr.flags &= ~(EdgeWrapX|EdgeWrapY);

  XGetGeometry(dpy, (Drawable) ResizeWindow, &JunkRoot,
	       &dragx, &dragy, (unsigned int *)&dragWidth, 
	       (unsigned int *)&dragHeight, &JunkBW,&JunkDepth);

  dragx += tmp_win->bw;
  dragy += tmp_win->bw;
  origx = dragx;
  origy = dragy;
  origWidth = dragWidth;
  origHeight = dragHeight;
  ymotion=xmotion=0;

  /* pop up a resize dimensions window */
  XMapRaised(dpy, Scr.SizeWindow);
  last_width = 0;
  last_height = 0;
  DisplaySize(tmp_win, origWidth, origHeight,True);

  /* Get the current position to determine which border to resize */
  if((PressedW != Scr.Root)&&(PressedW != None))
    {
      if(PressedW == tmp_win->sides[0])   /* top */
	ymotion = 1;
      if(PressedW == tmp_win->sides[1])  /* right */
	xmotion = -1;
      if(PressedW == tmp_win->sides[2])  /* bottom */
	ymotion = -1;
      if(PressedW == tmp_win->sides[3])  /* left */
	xmotion = 1;
      if(PressedW == tmp_win->corners[0])  /* upper-left */
	{
	  ymotion = 1;
	  xmotion = 1;
	}
      if(PressedW == tmp_win->corners[1])  /* upper-right */
	{
	  xmotion = -1;
	  ymotion = 1;
	}
      if(PressedW == tmp_win->corners[2]) /* lower left */
	{
	  ymotion = -1;
	  xmotion = 1;
	}
      if(PressedW == tmp_win->corners[3])  /* lower right */
	{
	  ymotion = -1;
	  xmotion = -1;
	}
    }
  /* draw the rubber-band window */
  MoveOutline (Scr.Root, dragx - tmp_win->bw, dragy - tmp_win->bw, 
	       dragWidth + 2 * tmp_win->bw,
	       dragHeight + 2 * tmp_win->bw);

  /* loop to resize */
  while(!finished)
    {
      XMaskEvent(dpy, ButtonPressMask | ButtonReleaseMask | KeyPressMask |
		 ButtonMotionMask | PointerMotionMask | ExposureMask,  &Event);
      StashEventTime(&Event);      

      if (Event.type == MotionNotify) 
	/* discard any extra motion events before a release */
	while(XCheckMaskEvent(dpy, ButtonMotionMask |	ButtonReleaseMask |
			      PointerMotionMask,&Event))
	  {
	    StashEventTime(&Event);      
	    if (Event.type == ButtonRelease) break;
	  }


      done = FALSE;
      /* Handle a limited number of key press events to allow mouseless
       * operation */
      if(Event.type == KeyPress)
	Keyboard_shortcuts(&Event,ButtonRelease);
      switch(Event.type)
	{
	case ButtonPress:
	  XAllowEvents(dpy,ReplayPointer,CurrentTime);
	case KeyPress:
	  /* simple code to bag out of move - CKH */
	  if (XLookupKeysym(&(Event.xkey),0) == XK_Escape)
	    {
	      abort = TRUE;
	      finished = TRUE;
	    }
	  done = TRUE;
	  break;

	case ButtonRelease:
	  finished = TRUE;
	  done = TRUE;
	  break;

	case MotionNotify:
	  x = Event.xmotion.x_root;
	  y = Event.xmotion.y_root;
	  /* resize before paging request to prevent resize from lagging mouse - mab */
	  DoResize(x, y, tmp_win);  
	  /* need to move the viewport */
	  HandlePaging(Scr.EdgeScrollX,Scr.EdgeScrollY,&x,&y,
		       &delta_x,&delta_y,False);
	/* redraw outline if we paged - mab */
	if ( (delta_x != 0) || (delta_y != 0) )
	  {
	  origx -= delta_x;
	  origy -= delta_y;
	  dragx -= delta_x;
	  dragy -= delta_y;

	  DoResize(x, y, tmp_win);
	  }
	  done = TRUE;
	default:
	  break;
	}
      if(!done)
	{
	  MoveOutline(Scr.Root,0,0,0,0);

	  DispatchEvent();

	  MoveOutline(Scr.Root, dragx - tmp_win->bw, dragy - tmp_win->bw,
		      dragWidth + 2 * tmp_win->bw, dragHeight + 2 * tmp_win->bw);

	}
    } 

  /* erase the rubber-band */
  MoveOutline(Scr.Root, 0, 0, 0, 0);

  /* pop down the size window */
  XUnmapWindow(dpy, Scr.SizeWindow);

  if(!abort)
    {
      ConstrainSize (tmp_win, &dragWidth, &dragHeight);
      SetupFrame (tmp_win, dragx - tmp_win->bw, 
		  dragy - tmp_win->bw, dragWidth, dragHeight,FALSE);
    }
  UninstallRootColormap();
  ResizeWindow = None; 
  MyXUngrabServer(dpy);
  UngrabEm();
  xmotion = 0;
  ymotion = 0;

  Scr.flags |= flags & (EdgeWrapX|EdgeWrapY);
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}

SCM refresh_window(SCM win) 
{
  ScwmWindow *tmp_win;
  SCM_REDEFER_INTS;
  VALIDATE(win,"refresh-window");
  tmp_win=SCWMWINDOW(win);

  refresh_common((tmp_win->flags & ICONIFIED)?
		 (tmp_win->icon_w):(tmp_win->frame));

  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}


SCM move_window_to_desk(SCM which, SCM win)
{
  ScwmWindow *t;
  int val1;

  SCM_REDEFER_INTS;
  if (!gh_number_p(which)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("move-window-to-desk",1,which);
  }

  VALIDATEN(win,2,"move-window-to-desk");

  t=SCWMWINDOW(win);

  val1=gh_scm2int(which);

  /* Mapping window on its new Desk,
     unmapping it from the old Desk */
  /* Only change mapping for non-sticky windows */
  if(!((t->flags & ICONIFIED)&&(t->flags & StickyIcon)) &&
     (!(t->flags & STICKY))&&(!(t->flags & ICON_UNMAPPED)))
    {
      if(t->Desk == Scr.CurrentDesk) {
	t->Desk = val1;
	if (val1 != Scr.CurrentDesk) { 
	  UnmapIt(t);
	}
      } else if(val1 == Scr.CurrentDesk) {
	t->Desk = val1;
	/* If its an icon, auto-place it */
	if(t->flags & ICONIFIED)
	  AutoPlace(t);
	MapIt(t);
      } else {
	t->Desk = val1;
      }
    }
  BroadcastConfig(M_CONFIGURE_WINDOW,t);
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;;
}


SCM window_position(SCM win) {
  ScwmWindow *tmp_win;

  VALIDATE(win,"window-position");
  tmp_win=SCWMWINDOW(win);

  return scm_listify(SCM_MAKINUM(tmp_win->frame_x),
		     SCM_MAKINUM(tmp_win->frame_y),
		     SCM_UNDEFINED);
}

SCM window_size(SCM win) {
  ScwmWindow *tmp_win;

  VALIDATE(win,"window-size");
  tmp_win=SCWMWINDOW(win);

  return scm_listify(SCM_MAKINUM(tmp_win->frame_width),
		     SCM_MAKINUM(tmp_win->frame_height),
		     SCM_UNDEFINED);
}

SCM window_id(SCM win) {
  ScwmWindow *tmp_win;

  VALIDATE(win,"window-id");
  tmp_win=SCWMWINDOW(win);

  return SCM_MAKINUM(tmp_win->w);
}

SCM window_desk(SCM win) {
  ScwmWindow *tmp_win;

  VALIDATE(win,"window-desk");
  return SCM_MAKINUM(SCWMWINDOW(win)->Desk);
}

SCM window_title(SCM win) {
  ScwmWindow *tmp_win;

  VALIDATE(win,"get-window-title");
  return gh_str02scm(SCWMWINDOW(win)->name);
}

SCM list_all_windows() {
  ScwmWindow *t;
  SCM result=SCM_EOL;

  SCM_REDEFER_INTS;
  for (t=Scr.ScwmRoot.next; NULL!=t; t=t->next) {
    result=scm_cons(t->schwin,result);
  }
  SCM_REALLOW_INTS;
  return result;
}


SCM keep_on_top(SCM win)
{
  ScwmWindow *tmp_win;
  SCM_REDEFER_INTS;
  VALIDATE(win,"keep-on-top");
  tmp_win=SCWMWINDOW(win);
  tmp_win->flags |=STAYSONTOP_FLAG;
  BroadcastConfig(M_CONFIGURE_WINDOW,tmp_win);
  SetTitleBar(tmp_win,(Scr.Hilite==tmp_win),True);
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}

SCM un_keep_on_top(SCM win)
{
  ScwmWindow *tmp_win;
  SCM_REDEFER_INTS;
  VALIDATE(win,"un-keep-on-top");
  tmp_win=SCWMWINDOW(win);
  tmp_win->flags &= ~STAYSONTOP_FLAG;
  BroadcastConfig(M_CONFIGURE_WINDOW,tmp_win);
  SetTitleBar(tmp_win,(Scr.Hilite==tmp_win),True);
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}

SCM kept_on_top_p(SCM win)
{
  VALIDATE(win,"kept-on-top?");
  return (SCWMWINDOW(win)->flags & STAYSONTOP) ? SCM_BOOL_T : SCM_BOOL_F;
}



