/****************************************************************************
 * This module is based on Twm, but has been siginificantly modified 
 * by Rob Nation 
 ****************************************************************************/
/*****************************************************************************/
/**       Copyright 1988 by Evans & Sutherland Computer Corporation,        **/
/**                          Salt Lake City, Utah                           **/
/**  Portions Copyright 1989 by the Massachusetts Institute of Technology   **/
/**                        Cambridge, Massachusetts                         **/
/**                                                                         **/
/**                           All Rights Reserved                           **/
/**                                                                         **/
/**    Permission to use, copy, modify, and distribute this software and    **/
/**    its documentation  for  any  purpose  and  without  fee is hereby    **/
/**    granted, provided that the above copyright notice appear  in  all    **/
/**    copies and that both  that  copyright  notice  and  this  permis-    **/
/**    sion  notice appear in supporting  documentation,  and  that  the    **/
/**    names of Evans & Sutherland and M.I.T. not be used in advertising    **/
/**    in publicity pertaining to distribution of the  software  without    **/
/**    specific, written prior permission.                                  **/
/**                                                                         **/
/**    EVANS & SUTHERLAND AND M.I.T. DISCLAIM ALL WARRANTIES WITH REGARD    **/
/**    TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES  OF  MERCHANT-    **/
/**    ABILITY  AND  FITNESS,  IN  NO  EVENT SHALL EVANS & SUTHERLAND OR    **/
/**    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAM-    **/
/**    AGES OR  ANY DAMAGES WHATSOEVER  RESULTING FROM LOSS OF USE, DATA    **/
/**    OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER    **/
/**    TORTIOUS ACTION, ARISING OUT OF OR IN  CONNECTION  WITH  THE  USE    **/
/**    OR PERFORMANCE OF THIS SOFTWARE.                                     **/
/*****************************************************************************/


/***********************************************************************
 *
 * scwm event handling
 *
 ***********************************************************************/

#include <config.h>

#ifdef ISC
#include <sys/bsdtypes.h>
#endif

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>
#include <signal.h>
/* Some people say that AIX and AIXV3 need 3 preceding underscores, other say
 * no. I'll do both */
#if defined ___AIX || defined _AIX || defined __QNX__ || defined ___AIXV3 || defined AIXV3 || defined _SEQUENT_
#include <sys/select.h>
#endif

#include <guile/gh.h>
#include "window.h"

#include "scwm.h"
#include <X11/Xatom.h>
#include "menus.h"
#include "misc.h"
#include "icons.h"
#include "screen.h"
#include <X11/extensions/shape.h>
#include "util.h"
#include "binding.h"
#include "Grab.h"
#include "add_window.h"
#include "borders.h"
#include "resize.h"
#include "window.h"
#include "colormaps.h"


unsigned int mods_used = (ShiftMask | ControlMask | Mod1Mask |
			  Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask);
extern int menuFromFrameOrWindowOrTitlebar;

extern Boolean debugging;

int Context = C_NO_CONTEXT;	/* current button press context */
int Button = 0;
ScwmWindow *ButtonWindow;	/* button press window structure */
XEvent Event;			/* the current event */
ScwmWindow *swCurrent;		/* the current scwm window */

int last_event_type = 0;
Window last_event_window = 0;

extern int interactive;
int repl_fd;

extern int ShapeEventBase;
void HandleShapeNotify(void);


Window PressedW;

/*
   ** LASTEvent is the number of X events defined - it should be defined
   ** in X.h (to be like 35), but since extension (eg SHAPE) events are
   ** numbered beyond LASTEvent, we need to use a bigger number than the
   ** default, so let's undefine the default and use 256 instead.
 */
#undef LASTEvent
#ifndef LASTEvent
#define LASTEvent 256
#endif /* !LASTEvent */
typedef void (*PFEH) ();
PFEH EventHandlerJumpTable[LASTEvent];

/*
   ** Procedure:
   **   InitEventHandlerJumpTable
 */
void 
InitEventHandlerJumpTable(void)
{
  int i;

  for (i = 0; i < LASTEvent; i++) {
    EventHandlerJumpTable[i] = NULL;
  }
  EventHandlerJumpTable[Expose] = HandleExpose;
  EventHandlerJumpTable[DestroyNotify] = HandleDestroyNotify;
  EventHandlerJumpTable[MapRequest] = HandleMapRequest;
  EventHandlerJumpTable[MapNotify] = HandleMapNotify;
  EventHandlerJumpTable[UnmapNotify] = HandleUnmapNotify;
  EventHandlerJumpTable[ButtonPress] = HandleButtonPress;
  EventHandlerJumpTable[EnterNotify] = HandleEnterNotify;
  EventHandlerJumpTable[LeaveNotify] = HandleLeaveNotify;
  EventHandlerJumpTable[FocusIn] = HandleFocusIn;
  EventHandlerJumpTable[ConfigureRequest] = HandleConfigureRequest;
  EventHandlerJumpTable[ClientMessage] = HandleClientMessage;
  EventHandlerJumpTable[PropertyNotify] = HandlePropertyNotify;
  EventHandlerJumpTable[KeyPress] = HandleKeyPress;
  EventHandlerJumpTable[VisibilityNotify] = HandleVisibilityNotify;
  EventHandlerJumpTable[ColormapNotify] = HandleColormapNotify;
  if (ShapesSupported)
    EventHandlerJumpTable[ShapeEventBase + ShapeNotify] = HandleShapeNotify;
}

/***********************************************************************
 *
 *  Procedure:
 *	DispatchEvent - handle a single X event stored in global var Event
 *
 ************************************************************************/
void 
DispatchEvent()
{
  Window w = Event.xany.window;

  DBUG("DispatchEvent", "Routine Entered");

  StashEventTime(&Event);

  swCurrent = SwFromWindow(dpy,w);
  last_event_type = Event.type;
  last_event_window = w;

  if (EventHandlerJumpTable[Event.type])
    (*EventHandlerJumpTable[Event.type]) ();

  DBUG("DispatchEvent", "Leaving Routine");
  return;
}


/***********************************************************************
 *
 *  Procedure:
 *	HandleEvents - handle X events
 *
 ************************************************************************/
void 
HandleEvents()
{
  SCM repl_th;

  repl_th = SCM_UNDEFINED;

  DBUG("HandleEvents", "Routine Entered");

#ifdef HAVE_SAFE_SCM_EVAL_STRING
  if (interactive) {
    repl_fd = SCM_INUM(scm_fileno(scm_current_input_port()));

    repl_th = gh_eval_str( 
			  /* FIXMS - gross hack alert: pretend we don't
			     have Scheme access to readline if we do so
			     that scwm -i works. Do this right later. */
#ifdef HAVE_SCM_READLINE
			   "(delq! 'readline *features*)"
#endif /* HAVE_SCM_READLINE */
			   "(letrec "
			   "    ((wrap-port-continuing-read "
			   "      (lambda (port cont) "
			   "	(make-soft-port (vector #f #f #f "
			   "				(lambda () "
			   "				  (let* ((pcont "
    "					  (call-with-current-continuation "
		  "					   (lambda (c) c))) "
		     "					 (escape-to-read "
	"					  (lambda () (pcont pcont)))) "
		   "				    (if (char-ready? port) "
		     "					(read-char port) "
			   "					(cont escape-to-read)))) #f) \"r\")))"
			   "     (continuing-top-repl "
			   "      (lambda () "
			   "	(call-with-current-continuation "
			   "	 (lambda (cont) "
			   "	   (let ((wrapped-input "
    "		  (wrap-port-continuing-read (current-input-port) cont))) "
			 "		 (with-input-from-port wrapped-input "
			   "		   (lambda () "
			   "		     (catch #t "
			   "			    (lambda () (top-repl)) "
			   "			    (lambda args args)))) "
			   "	     (close-port wrapped-input)))))))"
			   "  (set-repl-prompt! \"scwm> \")"
			   "  (continuing-top-repl))");
    if (!gh_procedure_p(repl_th)) {
      interactive = 0;
    }
  }
  #endif /* HAVE_SAFE_SCM_EVAL_STR */

  while (TRUE) {
    last_event_type = 0;
    switch (XNextEvent_orTimeout(dpy, &Event)) {
    case 0:
      DispatchEvent();
      break;
    case 1:
      gh_call0(repl_th);
    case 2:
      break;
    }
  }
}

/***********************************************************************
 *
 *  Procedure:
 *	Find the Scwm context for the Event.
 *
 ************************************************************************/
int 
GetContext(ScwmWindow * t, XEvent * e, Window * w)
{
  int Context, i;

  if (!t)
    return C_ROOT;

  Context = C_NO_CONTEXT;
  *w = e->xany.window;

  if (*w == Scr.NoFocusWin)
    return C_ROOT;

  /* Since key presses and button presses are grabbed in the frame
   * when we have re-parented windows, we need to find out the real
   * window where the event occured */
  if ((e->type == KeyPress) && (e->xkey.subwindow != None))
    *w = e->xkey.subwindow;

  if ((e->type == ButtonPress) && (e->xbutton.subwindow != None) &&
    ((e->xbutton.subwindow == t->w) || (e->xbutton.subwindow == t->Parent)))
    *w = e->xbutton.subwindow;

  if (*w == Scr.Root)
    Context = C_ROOT;
  if (t) {
    if (*w == t->title_w)
      Context = C_TITLE;
    if ((*w == t->w) || (*w == t->Parent))
      Context = C_WINDOW;
    if (*w == t->icon_w)
      Context = C_ICON;
    if (*w == t->icon_pixmap_w)
      Context = C_ICON;
    if (*w == t->frame)
      Context = C_SIDEBAR;
    for (i = 0; i < 4; i++)
      if (*w == t->corners[i]) {
	Context = C_FRAME;
	Button = i;
      }
    for (i = 0; i < 4; i++)
      if (*w == t->sides[i]) {
	Context = C_SIDEBAR;
	Button = i;
      }
    for (i = 0; i < Scr.nr_left_buttons; i++) {
      if (*w == t->left_w[i]) {
	Context = (1 << i) * C_L1;
	Button = i;
      }
    }
    for (i = 0; i < Scr.nr_right_buttons; i++) {
      if (*w == t->right_w[i]) {
	Context = (1 << i) * C_R1;
	Button = i;
      }
    }
  }
  return Context;
}

/***********************************************************************
 *
 *  Procedure:
 *	HandleFocusIn - handles focus in events
 *
 ************************************************************************/
void 
HandleFocusIn()
{
  XEvent d;
  Window w;

  DBUG("HandleFocusIn", "Routine Entered");

  w = Event.xany.window;
  while (XCheckTypedEvent(dpy, FocusIn, &d)) {
    w = d.xany.window;
  }
  swCurrent = SwFromWindow(dpy,w);
  if (!swCurrent) {
    if (w != Scr.NoFocusWin) {
      Scr.UnknownWinFocused = w;
    } else {
      SetBorder(Scr.Hilite, False, True, True, None);
      Broadcast(M_FOCUS_CHANGE, 5, 0, 0, 0,
		Scr.DefaultDecor.HiColors.fore,
		Scr.DefaultDecor.HiColors.back,
		0, 0);
      if (Scr.ColormapFocus == COLORMAP_FOLLOWS_FOCUS) {
	if ((Scr.Hilite) && (!(Scr.Hilite->flags & ICONIFIED))) {
	  InstallWindowColormaps(Scr.Hilite);
	} else {
	  InstallWindowColormaps(NULL);
	}
      }
    }
  } else if (swCurrent != Scr.Hilite) {
    SetBorder(swCurrent, True, True, True, None);
    Broadcast(M_FOCUS_CHANGE, 5, swCurrent->w,
	      swCurrent->frame, (unsigned long) swCurrent,
	      GetDecor(swCurrent, HiColors.fore),
	      GetDecor(swCurrent, HiColors.back),
	      0, 0);
    if (Scr.ColormapFocus == COLORMAP_FOLLOWS_FOCUS) {
      if ((Scr.Hilite) && (!(Scr.Hilite->flags & ICONIFIED))) {
	InstallWindowColormaps(Scr.Hilite);
      } else {
	InstallWindowColormaps(NULL);
      }
    }
  }
}

/***********************************************************************
 *
 *  Procedure:
 *	HandleKeyPress - key press event handler
 *
 ************************************************************************/
void 
HandleKeyPress()
{
  Binding *key;
  unsigned int modifier;

  modifier = (Event.xkey.state & mods_used);
  ButtonWindow = swCurrent;

  DBUG("HandleKeyPress", "Routine Entered");

  Context = GetContext(swCurrent, &Event, &PressedW);
  PressedW = None;

  /* Here's a real hack - some systems have two keys with the
   * same keysym and different keycodes. This converts all
   * the cases to one keycode. */
  Event.xkey.keycode =
    XKeysymToKeycode(dpy, XKeycodeToKeysym(dpy, Event.xkey.keycode, 0));


  for (key = Scr.AllBindings; key != NULL; key = key->NextBinding) {
    if ((key->Button_Key == Event.xkey.keycode) &&
	((key->Modifier == (modifier & (~LockMask))) ||
	 (key->Modifier == AnyModifier)) &&
	(key->Context & Context) &&
	(key->IsMouse == 0)) {
      if (!strcmp(key->Action, "Scheme")) {
	if (NULL != swCurrent) {
	  set_window_context(swCurrent->schwin);
	}
	call_thunk_with_message_handler(key->Thunk);
	if (NULL != swCurrent) {
	  unset_window_context();
	}
      } else {
      }
      return;
    }
  }

  /* if we get here, no function key was bound to the key.  Send it
   * to the client if it was in a window we know about.
   */
  if (swCurrent) {
    if (Event.xkey.window != swCurrent->w) {
      Event.xkey.window = swCurrent->w;
      XSendEvent(dpy, swCurrent->w, False, KeyPressMask, &Event);
    }
  }
  ButtonWindow = NULL;
}

static const int scwm_property_max_length = 8000;

/* Return NULL if not a valid string property */
char *
SzGetWindowProperty(Window w, const char *szPropertyName, Bool fDelete)
{
  Atom property = XInternAtom(dpy, szPropertyName, False);
  Atom xproptype;
  int xpropformat = 0;
  unsigned long citems = 0;
  unsigned long bytes_after = 0;
  unsigned char *pchReturn = 0;
  int retval = XGetWindowProperty(dpy, w, property, 0,
				  scwm_property_max_length,
				  fDelete, AnyPropertyType,
				  &xproptype, &xpropformat, &citems,
				  &bytes_after, &pchReturn);

  if (retval != Success || xproptype != XA_STRING) {
    scwm_msg(WARN, __FUNCTION__, "did not get string property!");
    return NULL;
  }
  return (char *) pchReturn;
}

void 
SetXPropertySz(Window w, const char *szPropertyName, const char *sz)
{
  Atom property = XInternAtom(dpy, szPropertyName, False);
  int len = strlen(sz);
  if (property == None) {
    scwm_msg(ERR,__FUNCTION__,"No property `%s'",szPropertyName);
    return;
  }
  XChangeProperty(dpy, w,
		  property, XA_STRING, 8 /* bits in a byte */ ,
		  PropModeReplace, sz, len);
  /* FIXGJB: should above be PropModeAppend to avoid race cond'ns
     hinted at in gwm */
  XFlush(dpy);
}


void
ScwmExecuteProperty()
{
  SCM retval;

  /* execute the XA_SCWM_EXECUTE X property */
  char *szExecute = SzGetWindowProperty(Scr.Root, "SCWM_EXECUTE", False);
  SCM scm;

  if (szExecute) {
    char *szResult;
    int len;
    scwm_msg(DBG, __FUNCTION__, "Executing %s", szExecute);
    /* WHICH OF THESE SHOULD I USE?? --gjb 
       gh.h:SCM gh_eval_str(char *scheme_code);
       gh.h:SCM gh_eval_str_with_catch(char *scheme_code, scm_catch_handler_t handler);
       gh.h:SCM gh_eval_str_with_standard_handler(char *scheme_code);
       gh.h:SCM gh_eval_str_with_stack_saving_handler(char *scheme_code);
     */
    retval = gh_eval_str_with_standard_handler(szExecute);
    /* Need to make this go to STDERR, or better, some 
       pre-opened interaction results port */
    gh_display(retval);
    gh_newline();
#ifdef FIXGJBNOW
    /* How the hell do you get the printable representation 
       of a SCM object as a char * ??? --gjb 11/13/97 */
    scm = gh_cons(gh_symbol2scm("'write"),retval);
    retval = gh_eval_str_with_standard_handler(scm)
    szResult = gh_scm2newstr(gh_evalretval,&len);
    SetXPropertySz(Scr.Root,"SCWM_RESULT",szResult);
    free(szResult);
#else
    SetXPropertySz(Scr.Root,"SCWM_RESULT","SCWM not returning results yet\n");
#endif
  }
}

/***********************************************************************
 *
 *  Procedure:
 *	HandlePropertyNotify - property notify event handler
 *
 ***********************************************************************/

void 
HandlePropertyNotify()
{
  XTextProperty text_prop;

  DBUG("HandlePropertyNotify", "Routine Entered");

  if (Event.xproperty.atom == XA_SCWM_EXECUTE) {
    ScwmExecuteProperty();
    return;
  }
  if ((!swCurrent) || (XGetGeometry(dpy, swCurrent->w, &JunkRoot, &JunkX, &JunkY,
			&JunkWidth, &JunkHeight, &JunkBW, &JunkDepth) == 0))
    return;

  switch (Event.xproperty.atom) {
  case XA_WM_NAME:
    if (!XGetWMName(dpy, swCurrent->w, &text_prop))
      return;

    free_window_names(swCurrent, True, False);

    swCurrent->name = (char *) text_prop.value;
    if (swCurrent->name == NULL)
      swCurrent->name = NoName;
    BroadcastName(M_WINDOW_NAME, swCurrent->w, swCurrent->frame,
		  (unsigned long) swCurrent, swCurrent->name);

    /* fix the name in the title bar */
    if (!(swCurrent->flags & ICONIFIED))
      SetTitleBar(swCurrent, (Scr.Hilite == swCurrent), True);

    /*
     * if the icon name is NoName, set the name of the icon to be
     * the same as the window 
     */
    if (swCurrent->icon_name == NoName) {
      swCurrent->icon_name = swCurrent->name;
      BroadcastName(M_ICON_NAME, swCurrent->w, swCurrent->frame,
		    (unsigned long) swCurrent, swCurrent->icon_name);
      RedoIconName(swCurrent);
    }
    break;

  case XA_WM_ICON_NAME:
    if (!XGetWMIconName(dpy, swCurrent->w, &text_prop))
      return;
    free_window_names(swCurrent, False, True);
    swCurrent->icon_name = (char *) text_prop.value;
    if (swCurrent->icon_name == NULL)
      swCurrent->icon_name = NoName;
    BroadcastName(M_ICON_NAME, swCurrent->w, swCurrent->frame,
		  (unsigned long) swCurrent, swCurrent->icon_name);
    RedoIconName(swCurrent);
    break;

  case XA_WM_HINTS:
    if (swCurrent->wmhints)
      XFree((char *) swCurrent->wmhints);
    swCurrent->wmhints = XGetWMHints(dpy, Event.xany.window);

    if (swCurrent->wmhints == NULL)
      return;

    if ((swCurrent->wmhints->flags & IconPixmapHint) ||
	(swCurrent->wmhints->flags & IconWindowHint))
      if (swCurrent->szIconFile == Scr.DefaultIcon)
	swCurrent->szIconFile = NULL;

    if ((swCurrent->wmhints->flags & IconPixmapHint) ||
	(swCurrent->wmhints->flags & IconWindowHint)) {
      if (!(swCurrent->flags & SUPPRESSICON)) {
	if (swCurrent->icon_w)
	  XDestroyWindow(dpy, swCurrent->icon_w);
	XDeleteContext(dpy, swCurrent->icon_w, ScwmContext);
	if (swCurrent->flags & ICON_OURS) {
	  if (swCurrent->icon_pixmap_w != None) {
	    XDestroyWindow(dpy, swCurrent->icon_pixmap_w);
	    XDeleteContext(dpy, swCurrent->icon_pixmap_w, ScwmContext);
	  }
	} else
	  XUnmapWindow(dpy, swCurrent->icon_pixmap_w);
      }
      swCurrent->icon_w = None;
      swCurrent->icon_pixmap_w = None;
      swCurrent->picIcon = NULL;
      if (swCurrent->flags & ICONIFIED) {
	swCurrent->flags &= ~ICONIFIED;
	swCurrent->flags &= ~ICON_UNMAPPED;
	CreateIconWindow(swCurrent, swCurrent->icon_x_loc, swCurrent->icon_y_loc);
	Broadcast(M_ICONIFY, 7, swCurrent->w, swCurrent->frame,
		  (unsigned long) swCurrent,
		  swCurrent->icon_x_loc,
		  swCurrent->icon_y_loc,
		  swCurrent->icon_w_width,
		  swCurrent->icon_w_height);
	BroadcastConfig(M_CONFIGURE_WINDOW, swCurrent);

	if (!(swCurrent->flags & SUPPRESSICON)) {
	  LowerWindow(swCurrent);
	  AutoPlace(swCurrent);
	  if (swCurrent->Desk == Scr.CurrentDesk) {
	    if (swCurrent->icon_w)
	      XMapWindow(dpy, swCurrent->icon_w);
	    if (swCurrent->icon_pixmap_w != None)
	      XMapWindow(dpy, swCurrent->icon_pixmap_w);
	  }
	}
	swCurrent->flags |= ICONIFIED;
	DrawIconWindow(swCurrent);
      }
    }
    break;

  case XA_WM_NORMAL_HINTS:
    {
      int new_width, new_height;
      /* Don't let shaded windows resize themselves */
      if (SHADED_P(swCurrent)) break;

      GetWindowSizeHints(swCurrent);
      new_width = swCurrent->frame_width;
      new_height = swCurrent->frame_height;
      ConstrainSize(swCurrent, &new_width, &new_height);
      if ((new_width != swCurrent->frame_width) ||
	  (new_height != swCurrent->frame_height))
	SetupFrame(swCurrent, swCurrent->frame_x, swCurrent->frame_y,
		   new_width, new_height, False);

      BroadcastConfig(M_CONFIGURE_WINDOW, swCurrent);
    }
    break;

  default:
    if (Event.xproperty.atom == _XA_WM_PROTOCOLS)
      FetchWmProtocols(swCurrent);
    else if (Event.xproperty.atom == _XA_WM_COLORMAP_WINDOWS) {
      FetchWmColormapWindows(swCurrent);	/* frees old data */
      ReInstallActiveColormap();
    } else if (Event.xproperty.atom == _XA_WM_STATE) {
      if ((swCurrent != NULL) && (swCurrent->flags & ClickToFocus)
	  && (swCurrent == Scr.Focus)) {
	Scr.Focus = NULL;
	SetFocus(swCurrent->w, swCurrent, 0);
      }
    }
    break;
  }
}


/***********************************************************************
 *
 *  Procedure:
 *	HandleClientMessage - client message event handler
 *
 ************************************************************************/
void 
HandleClientMessage()
{
  XEvent button;

  DBUG("HandleClientMessage", "Routine Entered");

  if ((Event.xclient.message_type == _XA_WM_CHANGE_STATE) &&
      (swCurrent) && (Event.xclient.data.l[0] == IconicState) &&
      !(swCurrent->flags & ICONIFIED)) {
    XQueryPointer(dpy, Scr.Root, &JunkRoot, &JunkChild,
		  &(button.xmotion.x_root),
		  &(button.xmotion.y_root),
		  &JunkX, &JunkY, &JunkMask);
    button.type = 0;
    iconify(swCurrent->schwin);
    return;
  }
  /*
     ** CKH - if we get here, it was an unknown client message, so send
     ** it to the client if it was in a window we know about.  I'm not so
     ** sure this should be done or not, since every other window manager
     ** I've looked at doesn't.  But it might be handy for a free drag and
     ** drop setup being developed for Linux.
   */
  if (swCurrent) {
    if (Event.xclient.window != swCurrent->w) {
      Event.xclient.window = swCurrent->w;
      XSendEvent(dpy, swCurrent->w, False, NoEventMask, &Event);
    }
  }
}

/***********************************************************************
 *
 *  Procedure:
 *	HandleExpose - expose event handler
 *
 ***********************************************************************/
void 
HandleExpose()
{
  if (Event.xexpose.count != 0)
    return;

  DBUG("HandleExpose", "Routine Entered");

  if (swCurrent) {
    if ((Event.xany.window == swCurrent->title_w)) {
      SetTitleBar(swCurrent, (Scr.Hilite == swCurrent), False);
    } else {
      SetBorder(swCurrent, (Scr.Hilite == swCurrent), True, True, Event.xany.window);
    }
  }
  return;
}



/***********************************************************************
 *
 *  Procedure:
 *	HandleDestroyNotify - DestroyNotify event handler
 *
 ***********************************************************************/
void 
HandleDestroyNotify()
{
  DBUG("HandleDestroyNotify", "Routine Entered");

  Destroy(swCurrent);
}




/***********************************************************************
 *
 *  Procedure:
 *	HandleMapRequest - MapRequest event handler
 *
 ************************************************************************/
void 
HandleMapRequest()
{
  DBUG("HandleMapRequest", "Routine Entered");

  HandleMapRequestKeepRaised(None);
}

void 
HandleMapRequestKeepRaised(Window KeepRaised)
{
  extern long isIconicState;
  extern Bool PPosOverride;

  Event.xany.window = Event.xmaprequest.window;

  swCurrent = SwFromWindow(dpy,Event.xany.window);

  if (!PPosOverride)
    XFlush(dpy);

  /* If the window has never been mapped before ... */
  if (!swCurrent) {
    /* Add decorations. */
    swCurrent = AddWindow(Event.xany.window);
    if (swCurrent == NULL)
      return;
    if (swCurrent->flags & ICONIFIED) {
      swCurrent->flags |= STARTICONIC;
    }
  }
  if (KeepRaised != None)
    XRaiseWindow(dpy, KeepRaised);
  /* If it's not merely iconified, and we have hints, use them. */
  if (!(swCurrent->flags & ICONIFIED) || (swCurrent->flags & STARTICONIC)) {
    int state;

    if (swCurrent->wmhints && (swCurrent->wmhints->flags & StateHint))
      state = swCurrent->wmhints->initial_state;
    else
      state = NormalState;

    if (swCurrent->flags & STARTICONIC)
      state = IconicState;

    if (isIconicState != DontCareState)
      state = isIconicState;

    XGrabServer_withSemaphore(dpy);
    switch (state) {
    case DontCareState:
    case NormalState:
    case InactiveState:
    default:
      if (swCurrent->Desk == Scr.CurrentDesk) {
	XMapWindow(dpy, swCurrent->w);
	XMapWindow(dpy, swCurrent->frame);
	swCurrent->flags |= MAP_PENDING;
	SetMapStateProp(swCurrent, NormalState);
	if ((swCurrent->flags & ClickToFocus) &&
	/* !(swCurrent->flags & SloppyFocus) && */
	    ((!Scr.Focus) || (Scr.Focus->flags & ClickToFocus))) {
	  SetFocus(swCurrent->w, swCurrent, 1);
	}
      } else {
	XMapWindow(dpy, swCurrent->w);
	SetMapStateProp(swCurrent, NormalState);
      }
      break;

    case IconicState:
      if (swCurrent->wmhints) {
	Iconify(swCurrent, swCurrent->wmhints->icon_x, swCurrent->wmhints->icon_y);
      } else {
	Iconify(swCurrent, 0, 0);
      }
      break;
    }
    swCurrent->flags &= ~STARTICONIC;
    if (!PPosOverride)
      XSync(dpy, 0);
    XUngrabServer_withSemaphore(dpy);
  }
  /* If no hints, or currently an icon, just "deiconify" */
  else {
    DeIconify(swCurrent);
  }
  if (!PPosOverride)
    KeepOnTop();
}


/***********************************************************************
 *
 *  Procedure:
 *	HandleMapNotify - MapNotify event handler
 *
 ***********************************************************************/
void 
HandleMapNotify()
{
  DBUG("HandleMapNotify", "Routine Entered");

  if (!swCurrent) {
    if ((Event.xmap.override_redirect == True) &&
	(Event.xmap.window != Scr.NoFocusWin)) {
      XSelectInput(dpy, Event.xmap.window, FocusChangeMask);
      Scr.UnknownWinFocused = Event.xmap.window;
    }
    return;
  }
  /* Except for identifying over-ride redirect window mappings, we
   * don't need or want windows associated with the sunstructurenotifymask */
  if (Event.xmap.event != Event.xmap.window) {
    return;
  }
  /*
   * Need to do the grab to avoid race condition of having server send
   * MapNotify to client before the frame gets mapped; this is bad because
   * the client would think that the window has a chance of being viewable
   * when it really isn't.
   */
  XGrabServer_withSemaphore(dpy);
  if (swCurrent->icon_w)
    XUnmapWindow(dpy, swCurrent->icon_w);
  if (swCurrent->icon_pixmap_w != None)
    XUnmapWindow(dpy, swCurrent->icon_pixmap_w);
  XMapSubwindows(dpy, swCurrent->frame);

  if (swCurrent->Desk == Scr.CurrentDesk) {
    XMapWindow(dpy, swCurrent->frame);
  }
  if (swCurrent->flags & ICONIFIED)
    Broadcast(M_DEICONIFY, 3, swCurrent->w, swCurrent->frame,
	      (unsigned long) swCurrent, 0, 0, 0, 0);
  else {
    Broadcast(M_MAP, 3, swCurrent->w, swCurrent->frame,
	      (unsigned long) swCurrent, 0, 0, 0, 0);
  }

  if ((swCurrent->flags & ClickToFocus) && (Scr.Focus) &&
      ((!Scr.Focus) || (Scr.Focus->flags & ClickToFocus))) {
    SetFocus(swCurrent->w, swCurrent, 1);
  }
  if ((!(swCurrent->flags & (BORDER | TITLE))) && (swCurrent->boundary_width < 2)) {
    SetBorder(swCurrent, False, True, True, swCurrent->frame);
  }
  XSync(dpy, 0);
  XUngrabServer_withSemaphore(dpy);
  XFlush(dpy);
  swCurrent->flags |= MAPPED;
  swCurrent->flags &= ~MAP_PENDING;
  swCurrent->flags &= ~ICONIFIED;
  swCurrent->flags &= ~ICON_UNMAPPED;
  KeepOnTop();
}


/***********************************************************************
 *
 *  Procedure:
 *	HandleUnmapNotify - UnmapNotify event handler
 *
 ************************************************************************/
void 
HandleUnmapNotify()
{
  int dstx, dsty;
  Window dumwin;
  XEvent dummy;
  extern ScwmWindow *colormap_win;
  int weMustUnmap;

  DBUG("HandleUnmapNotify", "Routine Entered");

  /*
   * Don't ignore events as described below.
   */
  if ((Event.xunmap.event != Event.xunmap.window) &&
      (Event.xunmap.event != Scr.Root || !Event.xunmap.send_event)) {
    return;
  }
  /*
   * The July 27, 1988 ICCCM spec states that a client wishing to switch
   * to WithdrawnState should send a synthetic UnmapNotify with the
   * event field set to (pseudo-)root, in case the window is already
   * unmapped (which is the case for Scwm for IconicState).  Unfortunately,
   * we looked for the ScwmContext using that field, so try the window
   * field also.
   */
  weMustUnmap = 0;
  if (!swCurrent) {
    Event.xany.window = Event.xunmap.window;
    weMustUnmap = 1;
    swCurrent = SwFromWindow(dpy, Event.xany.window);
  }
  if (!swCurrent)
    return;

  if (weMustUnmap)
    XUnmapWindow(dpy, Event.xunmap.window);

  if (swCurrent == Scr.Hilite)
    Scr.Hilite = NULL;

  if (Scr.PreviousFocus == swCurrent)
    Scr.PreviousFocus = NULL;

  if ((swCurrent == Scr.Focus) && (swCurrent->flags & ClickToFocus)) {
    if (swCurrent->next) {
      HandleHardFocus(swCurrent->next);
    } else
      SetFocus(Scr.NoFocusWin, NULL, 1);
  }
  if (Scr.Focus == swCurrent)
    SetFocus(Scr.NoFocusWin, NULL, 1);

  if (swCurrent == Scr.pushed_window)
    Scr.pushed_window = NULL;

  if (swCurrent == colormap_win)
    colormap_win = NULL;

  if ((!(swCurrent->flags & MAPPED) && !(swCurrent->flags & ICONIFIED))) {
    return;
  }
  XGrabServer_withSemaphore(dpy);

  if (XCheckTypedWindowEvent(dpy, Event.xunmap.window, DestroyNotify, &dummy)) {
    Destroy(swCurrent);
    XUngrabServer_withSemaphore(dpy);
    return;
  }
  /*
   * The program may have unmapped the client window, from either
   * NormalState or IconicState.  Handle the transition to WithdrawnState.
   *
   * We need to reparent the window back to the root (so that Scwm exiting 
   * won't cause it to get mapped) and then throw away all state (pretend 
   * that we've received a DestroyNotify).
   */
  if (XTranslateCoordinates(dpy, Event.xunmap.window, Scr.Root,
			    0, 0, &dstx, &dsty, &dumwin)) {
    XEvent ev;
    Bool reparented;

    reparented = XCheckTypedWindowEvent(dpy, Event.xunmap.window,
					ReparentNotify, &ev);
    SetMapStateProp(swCurrent, WithdrawnState);
    if (reparented) {
      if (swCurrent->old_bw)
	XSetWindowBorderWidth(dpy, Event.xunmap.window, swCurrent->old_bw);
      if ((!(swCurrent->flags & SUPPRESSICON)) &&
	  (swCurrent->wmhints && (swCurrent->wmhints->flags & IconWindowHint)))
	XUnmapWindow(dpy, swCurrent->wmhints->icon_window);
    } else {
      RestoreWithdrawnLocation(swCurrent, False);
    }
    XRemoveFromSaveSet(dpy, Event.xunmap.window);
    XSelectInput(dpy, Event.xunmap.window, NoEventMask);
    Destroy(swCurrent);		/* do not need to mash event before */
    /*
     * Flush any pending events for the window.
     */
    /* Bzzt! it could be about to re-map */
/*      while(XCheckWindowEvent(dpy, Event.xunmap.window,
   StructureNotifyMask | PropertyChangeMask |
   ColormapChangeMask | VisibilityChangeMask |
   EnterWindowMask | LeaveWindowMask, &dummy));
 */
  }				/* else window no longer exists and we'll get a destroy notify */
  XUngrabServer_withSemaphore(dpy);

  XFlush(dpy);
}


/***********************************************************************
 *
 *  Procedure:
 *	HandleButtonPress - ButtonPress event handler
 *
 ***********************************************************************/
void 
HandleButtonPress()
{
  unsigned int modifier;
  Binding *MouseEntry;
  Window x;
  int LocalContext;

  DBUG("HandleButtonPress", "Routine Entered");

  /* click to focus stuff goes here */
  if ((swCurrent) && (swCurrent->flags & ClickToFocus)
      && !(swCurrent->flags & SloppyFocus)
      && (swCurrent != Scr.Ungrabbed) &&
      ((Event.xbutton.state &
	(ControlMask | Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask)) == 0)) {
    if (swCurrent) {
      SetFocus(swCurrent->w, swCurrent, 1);
      if (Scr.ClickToFocusRaises 
	  /* FIXMS - these other conditions seem wrong to me. */
#if 0
	  ||
	  ((Event.xany.window != swCurrent->w) &&
	   (Event.xbutton.subwindow != swCurrent->w) &&
	   (Event.xany.window != swCurrent->Parent) &&
	   (Event.xbutton.subwindow != swCurrent->Parent))
#endif
	  )
      {
	RaiseWindow(swCurrent);
      }
      KeepOnTop();

      /* Why is this here? Seems to cause breakage with
       * non-focusing windows! */
      if (!(swCurrent->flags & ICONIFIED)) {
	XSync(dpy, 0);
	/* pass click event to just clicked to focus window? */
	if (Scr.ClickToFocusPassesClick)
	  XAllowEvents(dpy, ReplayPointer, CurrentTime);
	else			/* don't pass click to just focused window */
	  XAllowEvents(dpy, AsyncPointer, CurrentTime);
	XSync(dpy, 0);
	return;
      }
    }
  } else if ((swCurrent) && !(swCurrent->flags & ClickToFocus) &&
	     (Event.xbutton.window == swCurrent->frame) &&
	     Scr.MouseFocusClickRaises) {
    if (swCurrent != Scr.LastWindowRaised &&
	(Event.xbutton.state &
	 (ControlMask | Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask)) == 0 &&
	GetContext(swCurrent, &Event, &PressedW) == C_WINDOW) {
      RaiseWindow(swCurrent);
      KeepOnTop();
    }
    XSync(dpy, 0);
    XAllowEvents(dpy, ReplayPointer, CurrentTime);
    XSync(dpy, 0);
    return;
  }
  XSync(dpy, 0);
  XAllowEvents(dpy, ReplayPointer, CurrentTime);
  XSync(dpy, 0);

  Context = GetContext(swCurrent, &Event, &PressedW);
  LocalContext = Context;
  x = PressedW;
  if (Context == C_TITLE)
    SetTitleBar(swCurrent, (Scr.Hilite == swCurrent), False);
  else
    SetBorder(swCurrent, (Scr.Hilite == swCurrent), True, True, PressedW);

  ButtonWindow = swCurrent;

  /* we have to execute a function or pop up a menu
   */

  modifier = (Event.xbutton.state & mods_used);
  /* need to search for an appropriate mouse binding */
  for (MouseEntry = Scr.AllBindings; MouseEntry != NULL;
       MouseEntry = MouseEntry->NextBinding) {
    if (((MouseEntry->Button_Key == Event.xbutton.button) ||
	 (MouseEntry->Button_Key == 0)) &&
	(MouseEntry->Context & Context) &&
	((MouseEntry->Modifier == AnyModifier) ||
	 (MouseEntry->Modifier == (modifier & (~LockMask)))) &&
	(MouseEntry->IsMouse == 1)) {
      /* got a match, now process it */
      if (!strcmp(MouseEntry->Action, "Scheme")) {
	if (NULL != swCurrent) {
	  set_window_context(swCurrent->schwin);
	}
	find_mouse_event_type();
	call_thunk_with_message_handler(MouseEntry->Thunk);
	clear_mouse_event_type();
	if (NULL != swCurrent) {
	  unset_window_context();
	}
      } else {
      }
      break;
    }
  }
  PressedW = None;
  if (LocalContext != C_TITLE)
    SetBorder(ButtonWindow, (Scr.Hilite == ButtonWindow), True, True, x);
  else
    SetTitleBar(ButtonWindow, (Scr.Hilite == ButtonWindow), False);
  ButtonWindow = NULL;
}

/***********************************************************************
 *
 *  Procedure:
 *	HandleEnterNotify - EnterNotify event handler
 *
 ************************************************************************/
void 
HandleEnterNotify()
{
  XEnterWindowEvent *ewp = &Event.xcrossing;
  XEvent d;

  DBUG("HandleEnterNotify", "Routine Entered");

  /* look for a matching leaveNotify which would nullify this enterNotify */
  if (XCheckTypedWindowEvent(dpy, ewp->window, LeaveNotify, &d)) {
    StashEventTime(&d);
    if ((d.xcrossing.mode == NotifyNormal) &&
	(d.xcrossing.detail != NotifyInferior))
      return;
  }
/* an EnterEvent in one of the PanFrameWindows activates the Paging */
#ifndef NON_VIRTUAL
  if (ewp->window == Scr.PanFrameTop.win
      || ewp->window == Scr.PanFrameLeft.win
      || ewp->window == Scr.PanFrameRight.win
      || ewp->window == Scr.PanFrameBottom.win) {
    int delta_x = 0, delta_y = 0;

    /* this was in the HandleMotionNotify before, HEDU */
    HandlePaging(Scr.EdgeScrollX, Scr.EdgeScrollY,
		 &Event.xcrossing.x_root, &Event.xcrossing.y_root,
		 &delta_x, &delta_y, True);
    return;
  }
#endif /* NON_VIRTUAL */

  if (Event.xany.window == Scr.Root) {
    if ((Scr.Focus) && (!(Scr.Focus->flags & ClickToFocus)) &&
	(!(Scr.Focus->flags & SloppyFocus))) {
      SetFocus(Scr.NoFocusWin, NULL, 1);
    }
    if (Scr.ColormapFocus == COLORMAP_FOLLOWS_MOUSE) {
      InstallWindowColormaps(NULL);
    }
    return;
  }
  /* make sure its for one of our windows */
  if (!swCurrent)
    return;

  if (!(swCurrent->flags & ClickToFocus)) {
    if (Scr.Focus != swCurrent) {
      SetFocus(swCurrent->w, swCurrent, 0);
    } else
      SetFocus(swCurrent->w, swCurrent, 0);
  }
  if (Scr.ColormapFocus == COLORMAP_FOLLOWS_MOUSE) {
    if ((!(swCurrent->flags & ICONIFIED)) && (Event.xany.window == swCurrent->w))
      InstallWindowColormaps(swCurrent);
    else
      InstallWindowColormaps(NULL);
  }
  return;
}


/***********************************************************************
 *
 *  Procedure:
 *	HandleLeaveNotify - LeaveNotify event handler
 *
 ************************************************************************/
void 
HandleLeaveNotify()
{
  DBUG("HandleLeaveNotify", "Routine Entered");

  /* If we leave the root window, then we're really moving
   * another screen on a multiple screen display, and we
   * need to de-focus and unhighlight to make sure that we
   * don't end up with more than one highlighted window at a time */
  if (Event.xcrossing.window == Scr.Root) {
    if (Event.xcrossing.mode == NotifyNormal) {
      if (Event.xcrossing.detail != NotifyInferior) {
	if (Scr.Focus != NULL) {
	  SetFocus(Scr.NoFocusWin, NULL, 1);
	}
	if (Scr.Hilite != NULL)
	  SetBorder(Scr.Hilite, False, True, True, None);
      }
    }
  }
}


/***********************************************************************
 *
 *  Procedure:
 *	HandleConfigureRequest - ConfigureRequest event handler
 *
 ************************************************************************/
void 
HandleConfigureRequest()
{
  XWindowChanges xwc;
  unsigned long xwcm;
  int x, y, width, height;
  XConfigureRequestEvent *cre = &Event.xconfigurerequest;
  Bool sendEvent = False;

  DBUG("HandleConfigureRequest", "Routine Entered");

  /*
   * Event.xany.window is Event.xconfigurerequest.parent, so swCurrent will
   * be wrong
   */
  Event.xany.window = cre->window;	/* mash parent field */
  swCurrent = SwFromWindow(dpy, cre->window);

  /*
   * According to the July 27, 1988 ICCCM draft, we should ignore size and
   * position fields in the WM_NORMAL_HINTS property when we map a window.
   * Instead, we'll read the current geometry.  Therefore, we should respond
   * to configuration requests for windows which have never been mapped.
   */
  if (!swCurrent || (swCurrent->icon_w == cre->window)) {
    xwcm = cre->value_mask &
      (CWX | CWY | CWWidth | CWHeight | CWBorderWidth);
    xwc.x = cre->x;
    xwc.y = cre->y;
    if ((swCurrent) && ((swCurrent->icon_w == cre->window))) {
      swCurrent->icon_xl_loc = cre->x;
      swCurrent->icon_x_loc = cre->x +
	(swCurrent->icon_w_width - ICON_P_WIDTH(swCurrent) / 2);
      swCurrent->icon_y_loc = cre->y - ICON_P_HEIGHT(swCurrent);
      if (!(swCurrent->flags & ICON_UNMAPPED))
	Broadcast(M_ICON_LOCATION, 7, swCurrent->w, swCurrent->frame,
		  (unsigned long) swCurrent,
		  swCurrent->icon_x_loc, swCurrent->icon_y_loc,
		  swCurrent->icon_w_width,
		  swCurrent->icon_w_height + ICON_P_HEIGHT(swCurrent));
    }
    xwc.width = cre->width;
    xwc.height = cre->height;
    xwc.border_width = cre->border_width;
    XConfigureWindow(dpy, Event.xany.window, xwcm, &xwc);

    if (swCurrent) {
      xwc.x = swCurrent->icon_x_loc;
      xwc.y = swCurrent->icon_y_loc - ICON_P_HEIGHT(swCurrent);
      xwcm = cre->value_mask & (CWX | CWY);
      if (swCurrent->icon_pixmap_w != None)
	XConfigureWindow(dpy, swCurrent->icon_pixmap_w, xwcm, &xwc);
      xwc.x = swCurrent->icon_x_loc;
      xwc.y = swCurrent->icon_y_loc;
      xwcm = cre->value_mask & (CWX | CWY);
      if (swCurrent->icon_w != None)
	XConfigureWindow(dpy, swCurrent->icon_w, xwcm, &xwc);
    }
    return;
  }
  if (cre->value_mask & CWStackMode) {
    ScwmWindow *otherwin;
    Bool fSibling = cre->value_mask & CWSibling? True: False;
    if (fSibling && (otherwin = SwFromWindow(dpy,cre->above))) {
      xwc.sibling = otherwin->frame;
    } else {
      xwc.sibling = cre->above;
    }
    xwc.stack_mode = cre->detail;
    XConfigureWindow(dpy, swCurrent->frame,
		     cre->value_mask & (CWSibling | CWStackMode), &xwc);
    sendEvent = True;
  }
  if (ShapesSupported) {
    int xws, yws, xbs, ybs;
    unsigned wws, hws, wbs, hbs;
    int boundingShaped, clipShaped;

    XShapeQueryExtents(dpy, swCurrent->w, &boundingShaped, &xws, &yws, &wws,
		       &hws, &clipShaped, &xbs, &ybs, &wbs, &hbs);
    swCurrent->wShaped = boundingShaped;
  }

  /* Don't modify frame_XXX fields before calling SetupWindow! */
  x = swCurrent->frame_x;
  y = swCurrent->frame_y;
  width = swCurrent->frame_width;
  height = swCurrent->frame_height;

  /* for restoring */
  if (cre->value_mask & CWBorderWidth) {
    swCurrent->old_bw = cre->border_width;
  }
  /* override even if border change */

  if (cre->value_mask & CWX)
    x = cre->x - swCurrent->boundary_width - swCurrent->bw;
  if (cre->value_mask & CWY)
    y = cre->y - swCurrent->boundary_width - swCurrent->title_height - swCurrent->bw;
  if (cre->value_mask & CWWidth)
    width = cre->width + 2 * swCurrent->boundary_width;

  if (cre->value_mask & CWHeight)
    height = cre->height + swCurrent->title_height + 2 * swCurrent->boundary_width;


  /*
   * SetupWindow (x,y) are the location of the upper-left outer corner and
   * are passed directly to XMoveResizeWindow (frame).  The (width,height)
   * are the inner size of the frame.  The inner width is the same as the 
   * requested client window width; the inner height is the same as the
   * requested client window height plus any title bar slop.
   */
  SetupFrame(swCurrent, x, y, width, height, sendEvent);
  KeepOnTop();
}

/***********************************************************************
 *
 *  Procedure:
 *      HandleShapeNotify - shape notification event handler
 *
 ***********************************************************************/
void 
HandleShapeNotify(void)
{
  DBUG("HandleShapeNotify", "Routine Entered");

  if (ShapesSupported) {
    XShapeEvent *sev = (XShapeEvent *) & Event;

    if (!swCurrent)
      return;
    if (sev->kind != ShapeBounding)
      return;
    swCurrent->wShaped = sev->shaped;
    SetShape(swCurrent, swCurrent->frame_width);
  }
}

/***********************************************************************
 *
 *  Procedure:
 *	HandleVisibilityNotify - record fully visible windows for
 *      use in the RaiseLower function and the OnTop type windows.
 *
 ************************************************************************/
void 
HandleVisibilityNotify()
{
  XVisibilityEvent *vevent = (XVisibilityEvent *) & Event;

  DBUG("HandleVisibilityNotify", "Routine Entered");

  if (swCurrent) {
    if (vevent->state == VisibilityUnobscured)
      swCurrent->flags |= VISIBLE;
    else
      swCurrent->flags &= ~VISIBLE;

    /* For the most part, we'll raised partially obscured ONTOP windows
     * here. The exception is ONTOP windows that are obscured by
     * other ONTOP windows, which are raised in KeepOnTop(). This
     * complicated set-up saves us from continually re-raising
     * every on top window */
    if (((vevent->state == VisibilityPartiallyObscured) ||
	 (vevent->state == VisibilityFullyObscured)) &&
	(swCurrent->flags & ONTOP) && (swCurrent->flags & RAISED)) {
      RaiseWindow(swCurrent);
      swCurrent->flags &= ~RAISED;
    }
  }
}


/***************************************************************************
 *
 * Waits for next X event, or for an auto-raise timeout.
 *
 ****************************************************************************/
int 
XNextEvent_orTimeout(Display * dpy, XEvent * event)
{
  extern int fd_width, x_fd;
  fd_set in_fdset, out_fdset;
  int retval;
  struct timeval timeout;

  DBUG(__FUNCTION__, "Entered");

  /* Do this IMMEDIATELY prior to select, to prevent any nasty
   * queued up X events from just hanging around waiting to be
   * flushed */
  XFlush(dpy);
  if (XPending(dpy)) {
    DBUG(__FUNCTION__, "taking care of queued up events & returning");
    XNextEvent(dpy, event);
    StashEventTime(event);
    return 0;
  }
  DBUG(__FUNCTION__, "no X events waiting - about to reap children");
  /* Zap all those zombies! */
  /* If we get to here, then there are no X events waiting to be processed.
   * Just take a moment to check for dead children. */
  ReapChildren();

  FD_ZERO(&in_fdset);
  FD_SET(x_fd, &in_fdset);
#ifdef HAVE_SAFE_SCM_EVAL_STRING
  if (interactive) {
    FD_SET(repl_fd, &in_fdset);
  }
#endif /* HAVE_SAFE_SCM_EVAL_STRING */

  FD_ZERO(&out_fdset);

  XFlush(dpy);
  timerclear(&timeout);
#ifdef __hpux
  retval = select(fd_width, (int *) &in_fdset, (int *) &out_fdset, 0, NULL);
#else
  retval = select(fd_width, &in_fdset, &out_fdset, 0, NULL);
#endif

  if (interactive && FD_ISSET(repl_fd, &in_fdset)) {
    return 1;
  }
  DBUG(__FUNCTION__, "leaving");
  return 2;
}

/* Stolen from GWM 1.8c --gjb */
void
fill_x_button_event(XButtonEvent *evt, int type, int button, int modifier, 
		    int x, int y, int x_root, int y_root, 
		    Window child, Window sub_window)
{
  evt->type = type;
  evt->display = dpy;
  evt->window = child;
  evt->subwindow = sub_window;
  evt->root = Scr.Root;
  evt->time = lastTimestamp + (type == ButtonPress? 0 : 5);
  evt->x = x;
  evt->y = y;
  evt->x_root = x_root;
  evt->y_root = y_root;
  evt->same_screen = 1;
  evt->button = button;
  evt->state = modifier;
}

void
fill_x_keypress_event(XKeyEvent *evt, int type, KeySym keysym, int modifier, 
		      Window child)
{
  int keycode = 0;
  keycode = XKeysymToKeycode(dpy, keysym);

  evt->type = type;
  evt->display = dpy;
  evt->window = child;
  evt->subwindow = child;
  evt->root = Scr.Root;
  evt->time = lastTimestamp + (type == ButtonPress? 0 : 5);
  evt->same_screen = 1;
  evt->keycode = keycode;
  evt->state = modifier;
}


Window
WindowGettingButtonEvent(Window w, int x, int y)
{
    int x2, y2;
    Window child, w2 = w;
    XWindowAttributes wa;
    int c = 0;

 find_window:
    XTranslateCoordinates(dpy, w, w2, x, y, &x2, &y2, &child);
    if (child) {
	x = x2;
	y = y2;
	w = w2;
	w2 = child;
	c++;
	if (c>1000) {
	  scwm_msg(ERR,__FUNCTION__,"Infinite loop");
	  goto find_listener;
	}
	goto find_window;
    }
    x = x2;
    y = y2;
    w = w2;

 find_listener:
    XGetWindowAttributes(dpy, w, &wa);
    if (!(wa.all_event_masks & (ButtonPressMask | ButtonReleaseMask))) {
	Window d1, *d3, parent;
	unsigned int d4;
	
	XQueryTree(dpy, w, &d1, &parent, &d3, &d4);
	if (d3) XFree(d3);
	if (parent) {
	    w = parent;
	    goto find_listener;
	}
    }
    return w;
}

/* Inspired by GWM 1.8c --gjb */
/* FIXGJB: use button, not button + modifier */
SCM
send_button_press(SCM button, SCM modifier, SCM win,
		  SCM button_press_p, SCM button_release_p,
		  SCM propagate_p)
{
  int bnum;
  int mod_mask;
  Bool fPropagate = FALSE;
  Bool fPress = TRUE;
  Bool fRelease = TRUE;
  int iarg = 1;
  Window child;
  XButtonEvent event;
  int x = 0, y = 0, x_root = 0 , y_root = 0, x2, y2;
  ScwmWindow *sw;
  Window w;

  SCM_REDEFER_INTS;

  VALIDATEN(win, 3, __FUNCTION__);
  sw = SCWMWINDOW(win);
  w = sw->w;

  if (!gh_number_p(button)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg(__FUNCTION__, iarg++, button);
  }
  if (modifier != SCM_UNDEFINED && !gh_number_p(modifier)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg(__FUNCTION__, iarg++, modifier);
  }
  if (button_press_p != SCM_UNDEFINED) {
    fPress = gh_scm2bool(button_press_p);
  }
  if (button_release_p != SCM_UNDEFINED) {
    fRelease = gh_scm2bool(button_release_p);
  }
  if (propagate_p != SCM_UNDEFINED) {
    fPropagate = gh_scm2bool(propagate_p);
  }
  bnum = gh_scm2int(button);
  mod_mask = gh_scm2int(modifier);

  child = WindowGettingButtonEvent(w,x,y);
  x2 = x; y2 = y;

  if (fPress) {
    fill_x_button_event(&event, ButtonPress, bnum, mod_mask, 
			x, y, x_root, y_root, child, 0);
    XSendEvent(dpy, child, fPropagate, ButtonPressMask, 
	       (XEvent *) &event);
    DBUG(__FUNCTION__,"New Sent button press of %d at %d, %d; time = %ld\n",bnum,x,y,lastTimestamp);
  }
  if (fRelease) {
    fill_x_button_event(&event, ButtonRelease, bnum, mod_mask | (1 << (bnum+7)),
			x, y, x_root, y_root, child, 0);
    /* FIXGJB: was PointerWindow before -- why did that work? */
    XSendEvent(dpy, child, fPropagate, ButtonReleaseMask, 
	       (XEvent *) &event);
    DBUG(__FUNCTION__,"New Sent button release of %d at %d, %d; time = %ld\n",bnum,x,y,lastTimestamp);
  }
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}

SCM
send_key_press(SCM key, SCM win,
	       SCM button_press_p, SCM button_release_p,
	       SCM propagate_p)
{
  KeySym keysym;
  Bool fOkay;
  int mod_mask;
  Bool fPropagate = FALSE;
  Bool fPress = TRUE;
  Bool fRelease = TRUE;
  int iarg = 1;
  XKeyEvent event;
  ScwmWindow *sw;
  Window w;

  SCM_REDEFER_INTS;

  VALIDATEN(win, 2, __FUNCTION__);
  sw = SCWMWINDOW(win);
  w = sw->w;

  if (!gh_string_p(key)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg(__FUNCTION__, iarg++, key);
  }
  if (button_press_p != SCM_UNDEFINED) {
    fPress = gh_scm2bool(button_press_p);
  }
  if (button_release_p != SCM_UNDEFINED) {
    fRelease = gh_scm2bool(button_release_p);
  }
  if (propagate_p != SCM_UNDEFINED) {
    fPropagate = gh_scm2bool(propagate_p);
  }

  fOkay = FKeyToKeysymModifiers(key,&keysym,&mod_mask);
  if (fOkay) {
    if (fPress) {
      fill_x_keypress_event(&event, KeyPress, keysym, mod_mask, w);
      XSendEvent(dpy, w, fPropagate, KeyPressMask, 
		 (XEvent *) &event);
      DBUG(__FUNCTION__,"New Sent keypress of %s at %d, %d; time = %ld\n",szKeysym,x,y,lastTimestamp);
    }
    if (fRelease) {
      fill_x_keypress_event(&event, KeyRelease, keysym, mod_mask, w);
      XSendEvent(dpy, w, fPropagate, KeyReleaseMask, 
		 (XEvent *) &event);
      DBUG(__FUNCTION__,"New Sent keyrelease of %s at %d, %d; time = %ld\n",szKeysym,x,y,lastTimestamp);
    }
  } else {
    scwm_msg(WARN,__FUNCTION__,"Bad keysym");
  }
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}






/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
