/* $Id$
 * events.c
 * Copyright (C) 1998, 1999, 2000  Greg J. Badros and Maciej Stachowiak 
 * 
 * This module is derived from code based on fvwm which was
 * based on Twm, and was siginificantly modified by Rob Nation 
 */

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


/*
 * Scwm event handling
 */

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#ifdef ISC
#include <sys/bsdtypes.h>
#endif
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>
#include <signal.h>

#include <X11/Xatom.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#ifdef HAVE_SHAPE
#include <X11/extensions/shape.h>
#endif
#include <assert.h>

#define EVENTS_IMPLEMENTATION
#include "events.h"

#include "window.h"
#include "scwm.h"
#include "icons.h"
#include "screen.h"
#include "util.h"
#include "binding.h"
#include "Grab.h"
#include "add_window.h"
#include "borders.h"
#include "resize.h"
#include "window.h"
#include "colormaps.h"
#include "module-interface.h"
#include "focus.h"
#include "color.h"
#include "callbacks.h"
#include "guile-compat.h"
#include "syscompat.h"
#include "xmisc.h"
#include "xrm.h"
#include "dbug_resize.h"
#include "virtual.h"
#include "cursor.h"

#ifdef HAVE_LIBSM_LIBICE
#include "session-manager.h"
#endif

#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif

#ifndef WithdrawnState
#define WithdrawnState 0
#endif

extern SCM sym_root_window;
extern Bool fQuotingKeystrokes;

static SCM x_motionnotify_hook;
static SCM g_lastwin_entered = SCM_BOOL_F;

SCWM_GLOBAL_SYMBOL(sym_interactive,"interactive");

SCWM_SYMBOL(sym_press,"press");
SCWM_SYMBOL(sym_release,"release");
SCWM_SYMBOL(sym_desk_press,"desk-press");
SCWM_SYMBOL(sym_desk_release,"desk-release");
SCWM_SYMBOL(sym_desk_click,"desk-click");

SCM_VARIABLE_INIT(scm_configure_request_handled,"configure-request-handled",SCM_BOOL_F);
/** Set to #t by an X-ConfigureRequest-hook procedure if no C handling should be done.
See also `X-ConfigureRequest-hook'. */



SCWM_HOOK(x_selectionnotify_hook,"X-SelectionNotify-hook", 0,
"Called when there is no selection after a `X-convert-selection' request.");

SCWM_HOOK(x_configurerequest_hook,"X-ConfigureRequest-hook", 6,
"This hook is invoked upon ConfigureRequest events.\n\n"
"The arguments are: '(win icon? x y width height) where win\n"
"is the window requesting the configuration change, icon? is #t\n"
"iff that window's icon is requesting the change, x, y, width,\n"
"and height are either integers or #f to indicate that that\n"
"aspect was not part of the configure request event. \n"
"If `configure-request-handled' is #t after execution of the\n"
"hook procedures, then no C-level handling of the request\n"
"will be performed.");

SCWM_HOOK(x_propertynotify_hook,"X-PropertyNotify-hook", 2,
"This hook is invoked whenever a PropertyNotify event is received\n\n"
"for a window scwm is managing. This indicates that an X window\n"
"property has changed. Watching for window property changes can be used\n"
"to construct your own custom window manager protocols. The hook\n"
"procedures are invoked with two arguments, the name of the property\n"
"that changed (as a string) and the WINDOW that it changed for. See also\n"
"`X-root-PropertyNotify-hook' but beware it gets passed different\n"
"arguments.");

SCWM_HOOK(x_root_propertynotify_hook,"X-root-PropertyNotify-hook", 2,
"This hook is invoked whenever a PropertyNotify event is received\n\n"
"on the root window.  This indicates that an X window\n"
"property has changed. Watching for window property changes can be used\n"
"to construct your own custom window manager protocols, or interface\n"
"to other desktop environments such as KDE or GNOME. The hook\n"
"procedures are invoked with two arguments: the atom for the changed\n"
"property and a boolean telling whether the property was deleted. \n"
"These arguments are different from those passed to\n"
"X-PropertyNotify-hook's procedures.");

SCWM_HOOK(x_mappingnotify_hook,"X-MappingNotify-hook", 0,
"This hook is invoked whenever a MappingNotify X event is\n\n"
"received. A MappingNotify event indicates a change of keymapping - in\n"
"particular, it may indicate a change of available modifiers or mouse\n"
"buttons. The hook procedures are invoked with no arguments.");

SCWM_HOOK(x_destroynotify_hook,"X-DestroyNotify-hook", 1,
"This hook is invoked upon DestroyNotify X events.\n\n"
"It indicates a window was destroyed.  The hook procedures are\n"
"invoked with one argument, WINID, the X id of the window that was destroyed. \n"
"This hook is invoked for both the client window and the window frame\n"
"IDs (i.e., twice per top-level window).  You probably want to use\n"
"`window-close-hook' or `{X-UnmapNotify-hook' instead.");

SCWM_HOOK(x_unmapnotify_hook,"X-UnmapNotify-hook", 1,
"This hook is invoked upon UnmapNotify X events.  It indicates a\n\n"
"window is being unmapped (removed from display).  The hook procedures\n"
"are invoked with one argument, WIN, the window being destroyed.  The\n"
"WIN is still valid during the hook procedures.");

SCWM_HOOK(x_maprequest_hook,"X-MapRequest-hook", 1,
"This hook is invoked upon MapRequest X events.  It indicates a\n\n"
"window is trying to map itself (add itself to the display).  The hook \n"
"procedures are invoked with one argument, WIN, the window being mapped.  \n"
"The WIN is valid during the hook procedures.");

SCWM_HOOK(window_focus_change_hook,"window-focus-change-hook", 1,
"This hook is invoked whenever the keyboard focus is changed.\n\n"
"It is called with one argument, the window object of the window\n"
"that now has the focus, or #f if no window now has the focus. \n"
"See also `window-focus-lost-hook'.");

SCWM_HOOK(window_enter_hook, "window-enter-hook", 1,
"This hook is invoked whenever the mouse pointer enters a top-level window.\n\n"
"It is called with one argument, the window object of the window just\n"
"entered.");

SCWM_HOOK(window_leave_hook, "window-leave-hook", 1,
"This hook is invoked whenever the mouse pointer leaves a top-level window.\n\n"
"The hook procedures are invoked with one argument, the window object\n"
"of the window just left.");

SCWM_HOOK(window_fully_obscured_hook, "window-fully-obscured-hook", 2,
"Invoked when window receives a VisibilityFullyObscured event.\n\n"
"The hook procedures are invoked with two arguments: the window object\n"
"of the window that is now fully obscured, resulting-from-viewport-move? (a boolean).\n"
"See also `window-visibility'.");

SCWM_HOOK(window_partially_obscured_hook, "window-partially-obscured-hook", 2,
"Invoked when window receives a VisibilityPartiallyObscured\n\n"
"event.  The hook procedures are invoked with two arguments: the window object\n"
"of the window that is now fully obscured, resulting-from-viewport-move? (a boolean).\n"
"Beware that this event happens more often than you might expect and an action procedure\n"
"attached here should be very careful about manipulating windows in a way\n"
"that might cause more Visibility events. See also `window-visibility'.");

SCWM_HOOK(window_unobscured_hook, "window-unobscured-hook", 2,
"Invoked when window receives a VisibilityUnobscured event.\n\n"
"The hook procedures are invoked with two arguments: the window object\n"
"of the window that is now fully obscured, resulting-from-viewport-move? (a boolean).\n"
"Beware that this event happens more often than you might expect and an action procedure\n"
"attached here should be very careful about manipulating windows in a way\n"
"that might cause more Visibility events. See also `window-visibility'.");

SCWM_HOOK(client_message_hook,"client-message-hook", 4,
"This hook is invoked whenever Scwm receives an X/11 client message.\n\n"
"It is called with four arguments: the window, the message-type atom, the format (8, 16, or 32), \n"
"and the vector of data.");

unsigned int mods_used = (ShiftMask | ControlMask | Mod1Mask |
			  Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask);
extern int menuFromFrameOrWindowOrTitlebar;

extern Bool debugging;

int Context = C_NO_CONTEXT;	/* current button press context */
int Button = 0;
ScwmWindow *ButtonWindow;	/* button press window structure */
XEvent Event;			/* the current event */
ScwmWindow *pswCurrent;		/* the current scwm window */

int last_event_type = 0;
Window last_event_window = 0;

extern int ShapeEventBase;
static void HandleShapeNotify();
static void HandleMotionNotify();
static void HandleSelectionNotify();

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
  EventHandlerJumpTable[Expose          ] = HandleExpose;
  EventHandlerJumpTable[DestroyNotify   ] = HandleDestroyNotify;
  EventHandlerJumpTable[MapRequest      ] = HandleMapRequest;
  EventHandlerJumpTable[MapNotify       ] = HandleMapNotify;
  EventHandlerJumpTable[UnmapNotify     ] = HandleUnmapNotify;
  EventHandlerJumpTable[ButtonPress     ] = HandleButtonPress;
  EventHandlerJumpTable[EnterNotify     ] = HandleEnterNotify;
  EventHandlerJumpTable[LeaveNotify     ] = HandleLeaveNotify;
  EventHandlerJumpTable[FocusIn         ] = HandleFocusIn;
  EventHandlerJumpTable[ConfigureRequest] = HandleConfigureRequest;
  EventHandlerJumpTable[ClientMessage   ] = HandleClientMessage;
  EventHandlerJumpTable[PropertyNotify  ] = HandlePropertyNotify;
  EventHandlerJumpTable[KeyPress        ] = HandleKeyPress;
  EventHandlerJumpTable[KeyRelease      ] = HandleKeyRelease;
  EventHandlerJumpTable[VisibilityNotify] = HandleVisibilityNotify;
  EventHandlerJumpTable[ColormapNotify  ] = HandleColormapNotify;
  EventHandlerJumpTable[MappingNotify   ] = HandleMappingNotify;
  EventHandlerJumpTable[MotionNotify    ] = HandleMotionNotify;
  EventHandlerJumpTable[SelectionNotify ] = HandleSelectionNotify;

  if (ShapesSupported)
    EventHandlerJumpTable[ShapeEventBase + ShapeNotify] = HandleShapeNotify;
}

/*
 * DispatchEvent - handle a single X event stored in global var Event
 */
void 
DispatchEvent()
{
  Window w = Event.xany.window;

  DBUG((DBG,"DispatchEvent", "Entered"));

  StashEventTime(&Event);

  /* try to set the context window intelligently */
  pswCurrent = PswFromAnyWindow(dpy,w);
  if (NULL == pswCurrent && 
       (Event.type == KeyPress ||
        Event.type == KeyRelease )) {
    pswCurrent = Scr.Focus;
  }
  if (NULL == pswCurrent && 
      (Event.type == ButtonPress ||
       Event.type == KeyPress ||
       Event.type == KeyRelease)) {
    pswCurrent = PswFromPointerLocation(dpy);
  }
  last_event_type = Event.type;
  last_event_window = w;
#ifdef DEBUG_VISIBILITY_NOTIFY_EVENT_WINDOWS
  if (Event.type == VisibilityNotify) {
    fprintf(stderr,"Got visibility event on %ld, translates to psw->name %s\n",
            w,pswCurrent->name);
  }
#endif
  
  /* See X.h for figuring out what will
     run based on Event.type's value in debugger */
  if (EventHandlerJumpTable[Event.type])    
    (*EventHandlerJumpTable[Event.type]) ();

  DBUG((DBG,"DispatchEvent", "return"));
  return;
}


/*
 * HandleEvents - handle X events
 */
void 
HandleEvents(void)
{
  DBUG_EVENT((DBG,"HandleEvents", "Entered"));

  while (True) {
    last_event_type = 0;
    if (!NextScwmEvent(dpy, &Event, False)) {
      DispatchEvent();
    }
  }
  DBUG_EVENT((DBG,"HandleEvents", "return"));
}

SCM_DEFINE(handle_pending_events, "handle-pending-events", 0,0,0,
          (),
"Handle all pending Scwm events, returns number of dispatched events.\n\n"
"This is useful to maintain responsiveness of Scwm when in the middle\n"
"of a long computation.")
#define FUNC_NAME s_handle_pending_events
{
  int cevents = 0;
  if (Scr.fWindowsCaptured) {
    XSync(dpy, False);
    last_event_type = 0;
    while (!NextScwmEvent(dpy, &Event, True)) {
      ++cevents;
      DispatchEvent();
    }
  }
  return scm_from_int(cevents);
}
#undef FUNC_NAME


/* keyboard remapping has occurred */
void
HandleMappingNotify()
{
#define FUNC_NAME "HandleMappingNotify"
  DBUG_EVENT((DBG,FUNC_NAME,"Calling mapping notify hook (maybe empty)"));
  init_modifiers();
  init_pointer_mapping();
  scwm_run_hook0(x_mappingnotify_hook);
}
#undef FUNC_NAME

/*
 * Return the Scwm context for the Event.
 */
static
int 
GetContext(ScwmWindow * psw, XEvent * e, Window * w)
{
  int Context, i;

  if (!psw)
    return C_ROOT;

  Context = C_NO_CONTEXT;
  *w = e->xany.window;

  if (*w == Scr.NoFocusWin)
    return C_ROOT;

  /* Since key presses and button presses are grabbed in the frame
   * when we have re-parented windows, we need to find out the real
   * window where the event occured */
  if ((e->type == KeyPress || e->type == KeyRelease) && (e->xkey.subwindow != None))
    *w = e->xkey.subwindow;

  if ((e->type == ButtonPress) && (e->xbutton.subwindow != None) &&
    ((e->xbutton.subwindow == psw->w) || (e->xbutton.subwindow == psw->Parent)))
    *w = e->xbutton.subwindow;

  if (*w == Scr.Root)
    Context = C_ROOT;
  if (psw) {
    if (*w == psw->title_w)
      Context = C_TITLE;
    if ((*w == psw->w) || (*w == psw->Parent))
      Context = C_WINDOW;
    if (*w == psw->icon_w)
      Context = C_ICON;
    if (*w == psw->icon_pixmap_w)
      Context = C_ICON;
    if (*w == psw->frame)
      Context = C_SIDEBAR;
    for (i = 0; i < 4; i++)
      if (*w == psw->corners[i]) {
	Context = C_FRAME;
	Button = i;
      }
    for (i = 0; i < 4; i++)
      if (*w == psw->sides[i]) {
	Context = C_SIDEBAR;
	Button = i;
      }
    for (i = 0; i < psw->nr_left_buttons; i++) {
      if (*w == psw->left_w[i]) {
	Context = (1 << i) * C_L1;
	Button = i;
      }
    }
    for (i = 0; i < psw->nr_right_buttons; i++) {
      if (*w == psw->right_w[i]) {
	Context = (1 << i) * C_R1;
	Button = i;
      }
    }
  }
  return Context;
}


void 
HandleHardFocus(ScwmWindow *psw)
{
  int x, y;

  FocusOnNextTimeStamp = psw;
  Scr.Focus = NULL;
  /* Do something to guarantee a new time stamp! */
  WXGetPointerWindowOffsets(Scr.Root, &x, &y);
  GrabEm(XCursorByNumber(XC_watch));
  XWarpPointer(dpy, Scr.Root, Scr.Root, 0, 0, Scr.DisplayWidth,
	       Scr.DisplayHeight,
	       x + 1, y + 1);
  XSync(dpy, False);
  XWarpPointer(dpy, Scr.Root, Scr.Root, 0, 0, Scr.DisplayWidth,
	       Scr.DisplayHeight,
	       x, y);
  UngrabEm();
}

/*
 *  Procedure:
 *	HandleFocusIn - handles focus in events
 */
void 
HandleFocusIn()
{
  XEvent d;
  Window w;
  ScwmWindow *psw;

  DBUG_EVENT((DBG,"HandleFocusIn", "Entered"));
  
  w = Event.xany.window;
  while (XCheckTypedEvent(dpy, FocusIn, &d)) {
    w = d.xany.window;
  }
  psw = pswCurrent = PswFromAnyWindow(dpy,w);

  DBUG_EVENT((DBG,"HandleFocusIn", "psw = %s",psw?psw->name:"NULL"));
  
  if (!pswCurrent) {
    if (w != Scr.NoFocusWin) {
      /* Scr.UnknownWinFocused = w; */
    } else {
      scwm_run_hook1(window_focus_change_hook, SCM_BOOL_F);
      Scr.Focus = NULL;
      SetBorder(Scr.Hilite, False, True, True, None);
      Broadcast(M_FOCUS_CHANGE, 5, 0, 0, 0,
		XCOLOR(Scr.DefaultDecor.HiColors.fg),
		XCOLOR(Scr.DefaultDecor.HiColors.bg),
		0, 0);
      if (Scr.fColormapFollowsMouse) {
	if (Scr.Hilite && !Scr.Hilite->fIconified) {
	  InstallWindowColormaps(Scr.Hilite);
	} else {
	  InstallWindowColormaps(NULL);
	}
      }
    }
  } else if (pswCurrent != Scr.Hilite) {
    /* ClickToFocus focus queue manipulation - only performed for
     * Focus-by-mouse type focus events */
    if (psw && psw != Scr.Focus && psw != &Scr.ScwmRoot) {
      ScwmWindow *pswPrev, *pswNext;
      
      pswPrev = psw->prev;
      pswNext = psw->next;
    
      if (pswPrev)
        pswPrev->next = pswNext;
      if (pswNext)
        pswNext->prev = pswPrev;
      
      psw->next = Scr.ScwmRoot.next;
      if (Scr.ScwmRoot.next)
        Scr.ScwmRoot.next->prev = psw;
      Scr.ScwmRoot.next = psw;
      psw->prev = &Scr.ScwmRoot;
    }
    scwm_run_hook1(window_focus_change_hook, SCM_FROM_PSW(pswCurrent));
    SetBorder(pswCurrent, True, True, True, None);
    Broadcast(M_FOCUS_CHANGE, 5, pswCurrent->w,
              pswCurrent->frame, (unsigned long) pswCurrent,
              XCOLOR(GET_DECOR(pswCurrent, HiColors.fg)),
              XCOLOR(GET_DECOR(pswCurrent, HiColors.bg)),
              0, 0);
    if (Scr.fColormapFollowsMouse) {
      if (Scr.Hilite && !Scr.Hilite->fIconified) {
        InstallWindowColormaps(Scr.Hilite);
      } else {
        InstallWindowColormaps(NULL);
      }
    }
  }
}

/*
 * HandleKeyEvent - key press/release event handler
 */
static void 
HandleKeyEvent(Bool fPress)
{
  Binding *pbnd = NULL;
  unsigned int modifier;
  /* Here's a real hack - some systems have two keys with the
   * same keysym and different keycodes. This converts all
   * the cases to one keycode.  The first part breaks binding
   * of keycodes that don't have a corresponding keysym.  This
   * whole thing should be replaced with something that remembers
   * the keycode or keysym that the user actually asked to bind.  */
  unsigned int keycode =
    XKeysymToKeycode(dpy, 
                     XKeycodeToKeysym(dpy, Event.xkey.keycode, 0));
  if(keycode == 0)
    keycode = Event.xkey.keycode;

  DBUG_EVENT((DBG,"HandleKeyPress", "Entered"));
    
  if (!fQuotingKeystrokes) {
#ifdef USE_XALLOW_EVENTS
    XAllowEvents(dpy,AsyncKeyboard,CurrentTime);
#endif
    modifier = (Event.xkey.state & mods_used);
    ButtonWindow = pswCurrent;
    
    Context = GetContext(pswCurrent, &Event, &PressedW);
    PressedW = None;
    
    Event.xkey.keycode = keycode;
    
    pbnd = PBndFromKey(Event.xkey.keycode, modifier, Context);
  }
  
  if (pbnd) {
    XUngrabKeyboard(dpy, CurrentTime);
    if (NULL != pswCurrent) {
      set_window_context(SCM_FROM_PSW(pswCurrent));
    }
    if (fPress) {
      if (!UNSET_SCM(pbnd->Thunk)) {
        call_interactively(pbnd->Thunk,SCM_BOOL_F);
      }
    } else if (!UNSET_SCM(pbnd->ReleaseThunk)) {
      call_interactively(pbnd->ReleaseThunk,SCM_BOOL_F);
    }
    
    if (NULL != pswCurrent) {
      unset_window_context();
    }
  } else {
#if USE_XALLOW_EVENTS
    /* only has effect w/ synch keyboard grabs...
       not sure how to do them synchronously, though...
       HandleKeyEvent never gets called --07/04/99 gjb */
    XAllowEvents(dpy, ReplayKeyboard, CurrentTime);
#else
    /* if we get here, no function key was bound to the key.  Send it
     * to the client if it was in a window we know about
     */
    if (pswCurrent) {
      if (Event.xkey.window != pswCurrent->w) {
        Event.xkey.window = pswCurrent->w;
        XSendEvent(dpy, pswCurrent->w, False, KeyPressMask, &Event);
      }
    }
#endif
  }
  ButtonWindow = NULL;
}

void
HandleKeyPress()
{
  HandleKeyEvent(TRUE);
}

void
HandleKeyRelease()
{
  HandleKeyEvent(FALSE);
}


/**CONCEPT: SCWMEXEC Protocol 
  Scwm supports a protocol for other programs to send commands to the
window manager. Programs send ordinary configuration language
expressions and are returned a string representation of the return
value, and the output and error output generated, if any.

  For more information on how to make use of this protocol, see the
documentation for the scwmexec and scwmrepl programs, the scwm.el
emacs interaction mode, the libscwmexec library, and the details of
the SCWMEXEC protocol (as documented in
<filename>doc/scwmexec.proto</filename>).
FIXDOC: Link to file!
*/


SCM_DEFINE (reset_scwmexec_protocol, "reset-scwmexec-protocol", 0, 0, 0,
           (),
"Reset the scwmexec protocol.\n\n"
"This procedure removes the \"XA_SCWMEXEC_REQUEST\" property on the\n"
"root window.  It should not be necessary but may be useful in case\n"
"your X server goes awry (and otherwise you would have to restart your\n"
"X server).  Use if scwmexec or scwmrepl are not returning (e.g.,\n"
"if your Emacs hangs when you try evaluating a scwm expression).")
#define FUNC_NAME s_reset_scwmexec_protocol
{
  XDeleteProperty(dpy, Scr.Root, XA_SCWMEXEC_REQUEST);
  scwm_msg(WARN,FUNC_NAME,"Deleted XA_SCWMEXEC_REQUEST property -- expect a protocol error");
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* w_for_scwmexec_response is the window that is used by scwmexec
   protocol -- shutdown.c's Done function uses this too in case
   scwmexec executes a quit, or causes a segfault (which cases
   the HandleScwmExec function to not complete as it should) */
Window w_for_scwmexec_response;  

void
HandleScwmExec()
{
#define FUNC_NAME "HandleScwmExec"
  Window w;
  Window *pw;
  Atom type_ret;
  int form_ret;
  unsigned long nitems;
  unsigned long bytes_after;
  unsigned char *req;
  unsigned long last_offset=0;
  unsigned long saved_bytes_after=0;
  
  /* The SCWMEXEC_REQWIN property is treated as a queue of window IDs
     from which the request will be read. There may be more than one
     (or fewer than one in some cases) by the time we get here. We
     will loop and keep reading until we have snarfed the whole
     property, to make sure we can safely delete it. 

     See also the doc/scwmexec.proto file for a high-level 
     description of this protocol.
  */
  do {
    /* Read a single request window from the queue. */
    if (XGetWindowProperty(dpy, Scr.Root, XA_SCWMEXEC_REQWIN,
			   last_offset, 1, True, AnyPropertyType, 
			   &type_ret, &form_ret, &nitems, &bytes_after,
                          (unsigned char **) &pw)==Success && pw != NULL) {
      /* This is the window we want to look at: */
      w = *pw;
      XFree(pw);
      /* Increment the offset at which to read within the property. It
         will not get deleted until we read the very last bytes at the
         end. */
      last_offset += nitems * (form_ret/8);
      /* Save an indication of whether we need to read more or not. */
      saved_bytes_after=bytes_after;
      
      DBUG((DBG,FUNC_NAME,"Trying to get request from %ld",w));
      
      /* Get and delete its SCWMEXEC_REQUEST property. We do
         XGetWindowProperty twice, once to get the length, and again
         to read the whole length's worth. */
      if (XGetWindowProperty(dpy, w,
                             XA_SCWMEXEC_REQUEST,
                             0, 0, False, XA_STRING, 
                             &type_ret, &form_ret, &nitems, &bytes_after,
                             &req)==Success && 
          XGetWindowProperty(dpy, w,
                             XA_SCWMEXEC_REQUEST,
                             0, (bytes_after / 4) +
                             (bytes_after % 4 ? 1 : 0), True, XA_STRING, 
                             &type_ret, &form_ret, &nitems, &bytes_after,
                             &req)==Success) {
        SCM val, str_val;
        unsigned char *ret, *output, *error;
        size_t rlen, olen, elen;
        SCM o_port, e_port;
        SCM saved_def_e_port;
        
        /* Temporarily redirect output and error to string ports. 
           Note that the port setting functions return the current previous
           port. */
        o_port = scm_set_current_output_port(scm_open_output_string());
        e_port = scm_set_current_error_port(scm_open_output_string());

        /* before we eval the request, record the window to respond
           in a global, so Done can respond if necessary (in case
           the eval-d expression calls `quit' or seg faults, etc.) */
        w_for_scwmexec_response = w;
        /* Evaluate the request expression and free it. */
        val = scwm_safe_eval_str((char *) req);
        XFree(req); 
        str_val=scm_object_to_string(val, SCM_UNDEFINED);
        ret = (unsigned char *) scm_to_locale_stringn(str_val, &rlen);
        
        /* restore output and error ports; use returned o_port/e_port
           below for getting the strings back */
        o_port = scm_set_current_output_port(o_port);
        e_port = scm_set_current_error_port(e_port);
   
        /* Retrieve output and errors */
        output = (unsigned char *) scm_to_locale_stringn(scm_strport_to_string(o_port),
                                                 &olen);
        error = (unsigned char *) scm_to_locale_stringn(scm_strport_to_string(e_port),
                                                &elen);
        
        /* Set the output, error and reply properties appropriately. */
        XChangeProperty(dpy, w_for_scwmexec_response,
                        XA_SCWMEXEC_OUTPUT, XA_STRING,
                        8, PropModeReplace, output, olen);
        XChangeProperty(dpy, w_for_scwmexec_response,
                        XA_SCWMEXEC_ERROR, XA_STRING,
                        8, PropModeReplace, error, elen);
        XChangeProperty(dpy, w_for_scwmexec_response,
                        XA_SCWMEXEC_REPLY, XA_STRING,
                        8, PropModeReplace, ret, rlen);
          
        /* Since we successfully reset the reply properties,
           shutdown.c's Done no longer needs to, so reset
           the global */
        w_for_scwmexec_response = None;
        
        free(ret);
        free(output);
        free(error);
      } else {
        scwm_msg(WARN,FUNC_NAME,"Cannot get XA_SCWMEXEC_REQUEST atom from window %ld",
                 w_for_scwmexec_response);
      }
    } else {
      /* XGetWindowProperty returned False */
      DBUG((WARN,FUNC_NAME,"Done with last window in list of scwmexec requests"));
      saved_bytes_after = 0;
      last_offset = 0;
    }
  } while (saved_bytes_after != 0);
  /* Repeat until we get a saved_bytes_after of 0 on reading SCWMEXEC_REQWIN,
     indicating that we read it all and it was deleted. It may well have
     been re-created before we exit, but that doesn't matter because we'll
     get a PropertyNotify and re-enter, but the offset to use will correctly
     be 0. */

  return;
}
#undef FUNC_NAME

/*
 * HandlePropertyNotify - property notify event handler
 */
void 
HandlePropertyNotify()
{
#define FUNC_NAME "HandlePropertyNotify"
  XTextProperty text_prop;

  DBUG_EVENT((DBG,"HandlePropertyNotify", "Entered"));

  if (Event.xproperty.atom == XA_SCWMEXEC_REQWIN) {
    HandleScwmExec();
    return;
  }

  if (Event.xproperty.window == Scr.Root) {
    scwm_run_hook2(x_root_propertynotify_hook, 
                   scm_from_long(Event.xproperty.atom),
                   scm_from_bool(Event.xproperty.state == PropertyDelete));
  }

  if (!pswCurrent || !FXWindowAccessible(dpy, pswCurrent->w))
    return;

  switch (Event.xproperty.atom) {
  case XA_RESOURCE_MANAGER:
    IntegrateNewResourceManagerProperty(dpy);
    break;
  case XA_WM_NAME:
    if (!XGetWMName(dpy, pswCurrent->w, &text_prop))
      return;

    free_window_names(pswCurrent, True, False);

    pswCurrent->name = SzExtractTextPropValue(&text_prop);
    if (pswCurrent->name == NULL)
      pswCurrent->name = NoName;

    BroadcastName(M_WINDOW_NAME, pswCurrent->w, pswCurrent->frame,
		  (unsigned long) pswCurrent, pswCurrent->name);

    /* fix the name in the title bar */
    if (!pswCurrent->fIconified) {
      if (pswCurrent->fSquashedTitlebar) {
        ResizePswToCurrentSize(pswCurrent);
      } else {
        SetTitleBar(pswCurrent, (Scr.Hilite == pswCurrent), True);
      }
    }

    /*
     * if the icon name is NoName, set the name of the icon to be
     * the same as the window 
     */
    if (pswCurrent->icon_name == NoName) {
      pswCurrent->icon_name = pswCurrent->name;
      BroadcastName(M_ICON_NAME, pswCurrent->w, pswCurrent->frame,
		    (unsigned long) pswCurrent, pswCurrent->icon_name);
      RedoIconName(pswCurrent);
    }
    break;

  case XA_WM_ICON_NAME:
    if (!XGetWMIconName(dpy, pswCurrent->w, &text_prop))
      return;
    free_window_names(pswCurrent, False, True);
    pswCurrent->icon_name = (char *) text_prop.value;
    if (pswCurrent->icon_name == NULL)
      pswCurrent->icon_name = NoName;
    BroadcastName(M_ICON_NAME, pswCurrent->w, pswCurrent->frame,
		  (unsigned long) pswCurrent, pswCurrent->icon_name);
    RedoIconName(pswCurrent);
    break;

  case XA_WM_HINTS:
    if (pswCurrent->wmhints)
      XFree((char *) pswCurrent->wmhints);
    pswCurrent->wmhints = XGetWMHints(dpy, Event.xany.window);

    if (pswCurrent->wmhints == NULL)
      return;

    if ((pswCurrent->wmhints->flags & IconPixmapHint) ||
	(pswCurrent->wmhints->flags & IconWindowHint)) {
      if (!pswCurrent->fSuppressIcon) {
	if (pswCurrent->icon_w)
	  XDestroyWindow(dpy, pswCurrent->icon_w);
	XDeleteContext(dpy, pswCurrent->icon_w, ScwmContext);
	if (pswCurrent->fIconOurs) {
	  if (pswCurrent->icon_pixmap_w != None) {
	    XDestroyWindow(dpy, pswCurrent->icon_pixmap_w);
	    XDeleteContext(dpy, pswCurrent->icon_pixmap_w, ScwmContext);
	  }
	} else {
	  XUnmapWindow(dpy, pswCurrent->icon_pixmap_w);
	}
      }
      pswCurrent->icon_w = None;
      pswCurrent->icon_pixmap_w = None;
      pswCurrent->icon_image = SCM_BOOL_F;
      if (pswCurrent->fIconified) {
	pswCurrent->fIconified = False;
	pswCurrent->fIconUnmapped = False;
	CreateIconWindow(pswCurrent, 
                         ICON_X_VP(pswCurrent), ICON_Y_VP(pswCurrent));
	BroadcastIconInfo(M_ICONIFY, pswCurrent);
	BroadcastConfig(M_CONFIGURE_WINDOW, pswCurrent);

	if (!pswCurrent->fSuppressIcon) {
	  LowerWindow(pswCurrent);
	  AutoPlace(pswCurrent);
	  if (pswCurrent->Desk == Scr.CurrentDesk) {
	    if (pswCurrent->icon_w)
	      XMapWindow(dpy, pswCurrent->icon_w);
	    if (pswCurrent->icon_pixmap_w != None)
	      XMapWindow(dpy, pswCurrent->icon_pixmap_w);
	  }
	}
	pswCurrent->fIconified = True;
	DrawIconWindow(pswCurrent);
      }
    }
    break;

  case XA_WM_NORMAL_HINTS:
    {
      int new_width, new_height;
      /* Don't let shaded windows resize themselves */
      if (SHADED_P(pswCurrent)) break;

      GetWindowSizeHints(pswCurrent);
      new_width = FRAME_WIDTH(pswCurrent);
      new_height = FRAME_HEIGHT(pswCurrent);
      ConstrainSize(pswCurrent, 0, 0, &new_width, &new_height);
      if ((new_width != FRAME_WIDTH(pswCurrent)) ||
	  (new_height != FRAME_HEIGHT(pswCurrent)))
        ResizeTo(pswCurrent, new_width, new_height);

      BroadcastConfig(M_CONFIGURE_WINDOW, pswCurrent);
    }
    break;

  default:
    if (Event.xproperty.atom == XA_WM_PROTOCOLS)
      FetchWmProtocols(pswCurrent);
    else if (Event.xproperty.atom == XA_WM_COLORMAP_WINDOWS) {
      FetchWmColormapWindows(pswCurrent);	/* frees old data */
      ReInstallActiveColormap();
    } else if (Event.xproperty.atom == XA_WM_STATE) {
      if ((pswCurrent != NULL) && pswCurrent->fClickToFocus
	  && (pswCurrent == Scr.Focus)) {
	Scr.Focus = NULL;
	SetFocus(pswCurrent->w, pswCurrent, False);
      }
    }
    break;
  }
  { /* scope */
    char *szName = XGetAtomName(dpy,Event.xproperty.atom);
    
    scwm_run_hook2(x_propertynotify_hook, 
                   scm_from_locale_string(szName), SCM_FROM_PSW(pswCurrent));
    XFree(szName);
  }
}
#undef FUNC_NAME


/*
 * HandleClientMessage - client message event handler
 */
void 
HandleClientMessage()
{
  ScwmWindow *psw;
  SCM win;

  DBUG_EVENT((DBG,"HandleClientMessage", "Entered"));

  if ((Event.xclient.message_type == XA_WM_CHANGE_STATE) &&
      (Event.xclient.data.l[0] == IconicState) &&
      pswCurrent && !pswCurrent->fIconified) {
    /* GJB:FIXME:: What was this code supposed to do?
    XEvent button;
    WXGetPointerWindowOffsets(Scr.Root, &(button.xmotion.x_root), &(button.xmotion.y_root));
    button.type = 0;
    */
    Iconify(pswCurrent,0,0);
    return;
  }

  if (scm_is_false(scm_hook_empty_p(client_message_hook))) {
    /* hook is not empty */
    SCM data = SCM_BOOL_F;
    switch (Event.xclient.format) {
    case 8: /* interpret as a string */
      data = scm_from_locale_string(Event.xclient.data.b);
      break;
    case 16:
      /* GJB:FIXME:: 
         converting from C array to SCM vector is clumsy -- better way? */
      { /* scope */
        short *ps = Event.xclient.data.s;
        int i = 0;
        data = scm_make_vector(scm_from_int(10), SCM_BOOL_F);
        while (i < 10) {
          scm_vector_set_x(data,scm_from_int(i),scm_from_int(*ps));
          ++i;
          ++ps;
        }
      }
      break;
    case 32:
      { /* scope */
        long *pl = Event.xclient.data.l;
        int i = 0;
        data = scm_make_vector(scm_from_int(5), SCM_BOOL_F);
        while (i < 5) {
          scm_vector_set_x(data,scm_from_int(i),scm_from_long(*pl));
          ++i;
          ++pl;
        }
      }
      break;
    default:
      /* we should handle things when we get here */
      scwm_msg(WARN,"HandleClientMessage",
               "Got client message with format = %d that is not handled.",
               Event.xclient.format);
      break;
    } /* end switch */
    if (Event.xclient.window == Scr.Root) {
      win = sym_root_window;
    } else if (NULL != (psw = PswFromAnyWindow(dpy,Event.xclient.window))) {
      win = SCM_FROM_PSW(psw);
    } else {
      win = SCM_BOOL_F;
    }

    scwm_run_hook(client_message_hook,
                  scm_list_n(win,
			     scm_from_long(Event.xclient.message_type),
			     scm_from_int(Event.xclient.format),
			     data, SCM_UNDEFINED));
  }

  /*
     ** CKH - if we get here, it was an unknown client message, so send
     ** it to the client if it was in a window we know about.  I'm not so
     ** sure this should be done or not, since every other window manager
     ** I've looked at doesn't.  But it might be handy for a free drag and
     ** drop setup being developed for Linux.
   */
  if (pswCurrent) {
    if (Event.xclient.window != pswCurrent->w) {
      Event.xclient.window = pswCurrent->w;
      XSendEvent(dpy, pswCurrent->w, False, NoEventMask, &Event);
    }
  }
}

/*
 * HandleExpose - expose event handler
 */
void 
HandleExpose()
{
  ExposeProc pExposeFunc = NULL;

  if (Event.xexpose.count != 0)
    return;

  DBUG_EVENT((DBG,"HandleExpose", "Entered and xexpose.count is non-zero"));

  if ( XFindContext(dpy,Event.xany.window,ExposeWindowProcContext,(XPointer *)&pExposeFunc) == 0/* && pExposeFunc != NULL*/) {
    (*pExposeFunc)( Event.xany.window );
  }
  else if (pswCurrent) {
    if ((Event.xany.window == pswCurrent->title_w)) {
      SetTitleBar(pswCurrent, (Scr.Hilite == pswCurrent), False);
    } else {
      SetBorder(pswCurrent, (Scr.Hilite == pswCurrent), True, True, Event.xany.window);
    }
  }

  DBUG_EVENT((DBG,"HandleExpose", "return"));
  return;
}

/*
 * HandleDestroyNotify - DestroyNotify event handler
 */
void 
HandleDestroyNotify()
{
  Window w = Event.xdestroywindow.window;
  DBUG_EVENT((DBG,"HandleDestroyNotify", "Entered"));

  scwm_run_hook1(x_destroynotify_hook,scm_from_ulong(w));

  /* maybe use the window in the XDestroyWindowEvent structure
     if the one in xany did not correlate to a sw */
  if (!pswCurrent) {
    pswCurrent = PswFromAnyWindow(dpy,w);
  }
  
  if (pswCurrent) {
    DestroyScwmWindow(pswCurrent);
  }

  DBUG_EVENT((DBG,"HandleDestroyNotify", "return"));
}


/*
 * HandleMapRequest - MapRequest event handler
 */
void 
HandleMapRequest()
{
  DBUG_EVENT((DBG,"HandleMapRequest", "Entered"));
  HandleMapRequestKeepRaised(None);
  DBUG_EVENT((DBG,"HandleMapRequest", "return"));
}

void 
HandleMapRequestKeepRaised(Window KeepRaised)
{
  extern long isIconicState;
  extern Bool PPosOverride;

  Event.xany.window = Event.xmaprequest.window;

  pswCurrent = PswFromWindow(dpy,Event.xany.window);

  if (!PPosOverride)
    XFlush(dpy);

  /* If the window has never been mapped before ... */
  if (!pswCurrent) {
    /* Add decorations. */
    pswCurrent = AddWindow(Event.xany.window);
    if (pswCurrent == NULL)
      return;
    if (pswCurrent->fIconified) {
      pswCurrent->fStartIconic = True;
    }
  }
  if (KeepRaised != None)
    XRaiseWindow(dpy, KeepRaised);
  /* If it's not merely iconified, and we have hints, use them. */
  if (!pswCurrent->fIconified || pswCurrent->fStartIconic) {
    int state;
    /* GJB:FIXME:G1.2: Drop this dynamic saving when guile-1.3 is no longer supported;
       scwm_run_hook manages the complexity */
    ScwmWindow *psw = pswCurrent; /* save this value before the hooks are invoked */

    if (Scr.fWindowsCaptured) {
      scwm_run_hook1(x_maprequest_hook, SCM_FROM_PSW(pswCurrent));
    }

    pswCurrent = psw; /* restore from value before hooks were invoked */

    if (pswCurrent->wmhints && (pswCurrent->wmhints->flags & StateHint))
      state = pswCurrent->wmhints->initial_state;
    else
      state = NormalState;

    if (pswCurrent->fStartIconic)
      state = IconicState;

    if (isIconicState != DontCareState)
      state = isIconicState;

    XGrabServer_withSemaphore(dpy);
    switch (state) {
    case DontCareState:
    case NormalState:
    case InactiveState:
    default:
      if (pswCurrent->Desk == Scr.CurrentDesk) {
	XMapWindow(dpy, pswCurrent->w);
	XMapWindow(dpy, pswCurrent->frame);
	pswCurrent->fMapPending = True;
	SetMapStateProp(pswCurrent, NormalState);
	if (pswCurrent->fClickToFocus &&
	/* GJB:FIXME:: !(pswCurrent->fSloppyFocus) && */
	    (!Scr.Focus || Scr.Focus->fClickToFocus)) {
	  SetFocus(pswCurrent->w, pswCurrent, True);
	}
      } else {
	XMapWindow(dpy, pswCurrent->w);
	SetMapStateProp(pswCurrent, NormalState);
      }
      break;

    case IconicState:
      if (pswCurrent->wmhints) {
	Iconify(pswCurrent, pswCurrent->wmhints->icon_x, pswCurrent->wmhints->icon_y);
      } else {
	Iconify(pswCurrent, 0, 0);
      }
      break;
    }
    pswCurrent->fStartIconic = False;
    if (!PPosOverride)
      XSync(dpy, False);
    XUngrabServer_withSemaphore(dpy);
  }
  /* If no hints, or currently an icon, just "deiconify" */
  else {
    DeIconify(pswCurrent);
  }
  if (!PPosOverride)
    KeepOnTop();

  raisePanFrames();
}


/*
 * HandleMapNotify - MapNotify event handler
 */
void 
HandleMapNotify()
{
  DBUG_EVENT((DBG,"HandleMapNotify", "Entered for %s",
              pswCurrent?pswCurrent->name:"null"));

  if (!pswCurrent) {
    if ((Event.xmap.override_redirect == True) &&
	(Event.xmap.window != Scr.NoFocusWin)) {
      XSelectInput(dpy, Event.xmap.window, FocusChangeMask);
    }
    goto HMN_return;
  }
  /* Except for identifying over-ride redirect window mappings, we
   * don't need or want windows associated with the sunstructurenotifymask */
  if (Event.xmap.event != Event.xmap.window) {
    goto HMN_return;
  }
  /*
   * Need to do the grab to avoid race condition of having server send
   * MapNotify to client before the frame gets mapped; this is bad because
   * the client would think that the window has a chance of being viewable
   * when it really isn't.
   */
  XGrabServer_withSemaphore(dpy);
  if (pswCurrent->icon_w)
    XUnmapWindow(dpy, pswCurrent->icon_w);
  if (pswCurrent->icon_pixmap_w != None)
    XUnmapWindow(dpy, pswCurrent->icon_pixmap_w);
  XMapSubwindows(dpy, pswCurrent->frame);

  if (pswCurrent->Desk == Scr.CurrentDesk) {
    XMapWindow(dpy, pswCurrent->frame);
  }
  if (pswCurrent->fIconified)
    Broadcast(M_DEICONIFY, 3, pswCurrent->w, pswCurrent->frame,
	      (unsigned long) pswCurrent, 0, 0, 0, 0);
  else {
    Broadcast(M_MAP, 3, pswCurrent->w, pswCurrent->frame,
	      (unsigned long) pswCurrent, 0, 0, 0, 0);
  }

  if ((pswCurrent->fClickToFocus) && Scr.Focus &&
      (!Scr.Focus || Scr.Focus->fClickToFocus)) {
    SetFocus(pswCurrent->w, pswCurrent, True);
  }
  /* GJB:FIXME:: what is this all about? */
  if (!(pswCurrent->fBorder || pswCurrent->fTitle)
      && (pswCurrent->boundary_width < 2)) {
    SetBorder(pswCurrent, False, True, True, pswCurrent->frame);
  }
  SetupFrame(pswCurrent,
             FRAME_X_VP(pswCurrent),FRAME_Y_VP(pswCurrent),
             FRAME_WIDTH(pswCurrent),FRAME_HEIGHT(pswCurrent),
             WAS_MOVED,WAS_RESIZED);
  XSync(dpy, False);
  XUngrabServer_withSemaphore(dpy);
  XFlush(dpy);
  pswCurrent->fMapped = True;
  pswCurrent->fMapPending = False;
  pswCurrent->fIconified = False;;
  pswCurrent->fIconUnmapped = False;
  KeepOnTop();
 HMN_return:
  DBUG_EVENT((DBG,"HandleMapNotify", "return"));
  return;
}


/*
 * HandleUnmapNotify - UnmapNotify event handler
 */
void 
HandleUnmapNotify()
{
  int dstx, dsty;
  Window dumwin;
  XEvent dummy;
  extern ScwmWindow *colormap_win;
  Bool fWeMustUnmap = False;
  Bool fUsePointerFocus = False;
  ScwmWindow *pswNewFocus = NULL;

  /*
   * Don't ignore events as described below.
   */
  if ((Event.xunmap.event != Event.xunmap.window) &&
      (Event.xunmap.event != Scr.Root || !Event.xunmap.send_event)) {
    /* GJB:FIXME:: might need to be sure that the window in the
       event is the window that had the focus */
    pswNewFocus = PswFromPointerLocation(dpy);
    goto HUN_return;
  }

  DBUG_EVENT((DBG,"HandleUnmapNotify", "Entered and passed init test"));

  /*
   * The July 27, 1988 ICCCM spec states that a client wishing to switch
   * to WithdrawnState should send a synthetic UnmapNotify with the
   * event field set to (pseudo-)root, in case the window is already
   * unmapped (which is the case for Scwm for IconicState).  Unfortunately,
   * we looked for the ScwmContext using that field, so try the window
   * field also.
   */
  if (!pswCurrent) {
    Event.xany.window = Event.xunmap.window;
    fWeMustUnmap = True;
    pswCurrent = PswFromWindow(dpy, Event.xany.window);
  }
  if (!pswCurrent)
    goto HUN_return;
  
  if (SCM_FROM_PSW(pswCurrent) == g_lastwin_entered) {
    scwm_run_hook1(window_leave_hook, SCM_FROM_PSW(pswCurrent));
    g_lastwin_entered = SCM_BOOL_F;
  }
  scwm_run_hook1(x_unmapnotify_hook, SCM_FROM_PSW(pswCurrent));

  if (fWeMustUnmap)
    XUnmapWindow(dpy, Event.xunmap.window);

  if (pswCurrent == Scr.Focus) {
    if (pswCurrent->fClickToFocus && pswCurrent->next) {
      pswNewFocus = pswCurrent->next;
    } else {
      fUsePointerFocus = True;
    }
  }

  if (pswCurrent == Scr.Hilite)
    Scr.Hilite = NULL;

  if (pswCurrent == Scr.PreviousFocus)
    Scr.PreviousFocus = NULL;

  if (pswCurrent == Scr.Focus)
    Scr.Focus = NULL;

  if (pswCurrent == FocusOnNextTimeStamp)
    FocusOnNextTimeStamp = NULL;

  if (pswCurrent == Scr.Ungrabbed)
    Scr.Ungrabbed = NULL;

  if (pswCurrent == Scr.pushed_window)
    Scr.pushed_window = NULL;

  if (pswCurrent == colormap_win)
    colormap_win = NULL;

  if (!pswCurrent->fMapped && !pswCurrent->fIconified) {
    goto HUN_return;
  }
  XGrabServer_withSemaphore(dpy);

  if (XCheckTypedWindowEvent(dpy, Event.xunmap.window, DestroyNotify, &dummy)) {
    DestroyScwmWindow(pswCurrent);
    XUngrabServer_withSemaphore(dpy);
    goto HUN_return;
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
    SetMapStateProp(pswCurrent, WithdrawnState);
    if (reparented) {
      if (pswCurrent->old_bw)
	XSetWindowBorderWidth(dpy, Event.xunmap.window, pswCurrent->old_bw);
      if ((!(pswCurrent->fSuppressIcon)) &&
	  (pswCurrent->wmhints && (pswCurrent->wmhints->flags & IconWindowHint)))
	XUnmapWindow(dpy, pswCurrent->wmhints->icon_window);
    } else {
      RestoreWithdrawnLocation(pswCurrent, False);
    }
    XRemoveFromSaveSet(dpy, Event.xunmap.window);
    XSelectInput(dpy, Event.xunmap.window, NoEventMask);
    DestroyScwmWindow(pswCurrent); /* do not need to mash event before */
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
 HUN_return:
  if (fUsePointerFocus)
    pswNewFocus = PswFromPointerLocation(dpy);

  if (pswNewFocus) {
    HandleHardFocus(pswNewFocus);
  }
  DBUG_EVENT((DBG,"HandleUnmapNotify", "return"));
}

/*
  Call x_motionnotify_hook with
  (X-ROOT Y-ROOT MODIFIER-STATE WINDOW-WITH-POINTER X-WIN Y-WIN)
*/
static void
HandleMotionNotify()
{
#define FUNC_NAME "HandleMotionNotify"
  XMotionEvent *pev = &Event.xmotion;
  SCM x_root = scm_from_int(pev->x_root);
  SCM y_root = scm_from_int(pev->y_root);
  SCM state = scm_from_int(pev->state);
  SCM win = SCM_BOOL_F;
  SCM x = x_root;
  SCM y = y_root;
  ScwmWindow *psw = PswFromPointerLocation(dpy);
  DBUG_EVENT((DBG,"HandleMotionNotify","Entered"));
  if (psw) {
    win = SCM_FROM_PSW(psw);
    x = scm_from_int(pev->x_root - FRAME_X_VP(psw));
    y = scm_from_int(pev->y_root - FRAME_Y_VP(psw));
  }
  scm_run_hook(scm_variable_ref(x_motionnotify_hook),
               scm_list_n(x_root,y_root,state,win,x,y,
			  SCM_UNDEFINED));
  DBUG_EVENT((DBG,"HandleMotionNotify","return"));
}
#undef FUNC_NAME


/*
 * HandleButtonPress - ButtonPress event handler
 */
void 
HandleButtonPress()
{
#define FUNC_NAME "HandleButtonPress"
  unsigned int modifier;
  Binding *pbnd;
  Window x;
  int LocalContext;

  DBUG_EVENT((DBG,"HandleButtonPress", "Entered"));

  /* click to focus stuff goes here */
  if (pswCurrent && pswCurrent->fClickToFocus
      && !pswCurrent->fSloppyFocus
      && (pswCurrent != Scr.Ungrabbed) &&
      ((Event.xbutton.state &
	(ControlMask | Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask)) == 0)) {
    SetFocus(pswCurrent->w, pswCurrent, True);
    if (Scr.fClickToFocusRaises) {
      RaiseWindow(pswCurrent);
    } else if ((Event.xany.window != pswCurrent->w) &&
               (Event.xbutton.subwindow != pswCurrent->w) &&
               (Event.xany.window != pswCurrent->Parent) &&
               (Event.xbutton.subwindow != pswCurrent->Parent)) {
      DBUG((DBG,FUNC_NAME,"Would have raised window %s, but commented out -- did you want it to raise?  Tell Greg!",
               pswCurrent->name));
      /* RaiseWindow(pswCurrent);  -- above condition was an || of the fClickToFocusRaises
         cond'n above --07/26/98 gjb */
    }
    KeepOnTop();
    
    /* Why is this here? Seems to cause breakage with
     * non-focusing windows! */
    if (!pswCurrent->fIconified) {
      Bool fSendClick = Scr.fClickToFocusPassesClick;
      XSync(dpy, False);
      XAllowEvents(dpy, 
                   (fSendClick?ReplayPointer:AsyncPointer),
                   CurrentTime);
      XSync(dpy, False);
      goto HBP_return;
    }
  } else if (pswCurrent && !pswCurrent->fClickToFocus &&
	     (Event.xbutton.window == pswCurrent->frame) &&
	     Scr.fMouseFocusClickRaises) {
    if (pswCurrent != Scr.LastWindowRaised &&
	(Event.xbutton.state &
	 (ControlMask | Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask)) == 0 &&
	GetContext(pswCurrent, &Event, &PressedW) == C_WINDOW) {
      RaiseWindow(pswCurrent);
      KeepOnTop();
    }
    XSync(dpy, False);
    XAllowEvents(dpy, ReplayPointer, CurrentTime);
    XSync(dpy, False);
    goto HBP_return;
  }
  XSync(dpy, False);
  XAllowEvents(dpy, ReplayPointer, CurrentTime);
  XSync(dpy, False);

  Context = GetContext(pswCurrent, &Event, &PressedW);
  LocalContext = Context;
  x = PressedW;
  if (Context == C_TITLE)
    SetTitleBar(pswCurrent, (Scr.Hilite == pswCurrent), False);
  else
    SetBorder(pswCurrent, (Scr.Hilite == pswCurrent), True, True, PressedW);

  ButtonWindow = pswCurrent;

  /* we have to execute a function or pop up a menu
   */

  modifier = (Event.xbutton.state & mods_used);

  pbnd = PBndFromMouse(Event.xbutton.button,modifier,Context);

  if (pbnd) {
    SCM done = SCM_BOOL_F;
    if (NULL != pswCurrent) {
      set_window_context(SCM_FROM_PSW(pswCurrent));
    }
    /* First call the immediate proc */
    if (scm_is_true(scm_procedure_p(pbnd->ReleaseThunk))) {
      stash_orig_button_position(&Event.xbutton);
      done = call_interactively(pbnd->ReleaseThunk,SCM_BOOL_F);
    }
    /* GJB:FIXME:: maybe this should only not
       do the main action if immediate proc returns
       'done */
    if (scm_is_false(done) &&
        scm_is_true(scm_procedure_p(pbnd->Thunk))) {
      find_mouse_event_type(&Event.xbutton);
      call_interactively(pbnd->Thunk,SCM_BOOL_F);
      clear_mouse_event_type();
    }
    if (NULL != pswCurrent) {
      unset_window_context();
    }
  }

  PressedW = None;
  if (LocalContext != C_TITLE)
    SetBorder(ButtonWindow, (Scr.Hilite == ButtonWindow), True, True, x);
  else
    SetTitleBar(ButtonWindow, (Scr.Hilite == ButtonWindow), False);
  ButtonWindow = NULL;
 HBP_return:
  DBUG_EVENT((DBG,"HandleButtonPress", "return"));
}
#undef FUNC_NAME


/*
 * HandleEnterNotify - EnterNotify event handler
 */
void 
HandleEnterNotify()
{
  XEnterWindowEvent *ewp = &Event.xcrossing;
  XEvent d;
  
  DBUG_EVENT((DBG,"HandleEnterNotify", "Entered"));

  /* look for a matching leaveNotify which would nullify this enterNotify */
  if (XCheckTypedWindowEvent(dpy, ewp->window, LeaveNotify, &d)) {
    StashEventTime(&d);
    if ((d.xcrossing.mode == NotifyNormal) &&
	(d.xcrossing.detail != NotifyInferior)) {
      /* GJB:FIXME:: should we call both hooks here? */
      goto HEN_return;
    }
  }
  /* an EnterEvent in one of the PanFrameWindows activates the Paging */
  if (ewp->window == Scr.PanFrameTop.win || 
      ewp->window == Scr.PanFrameLeft.win ||
      ewp->window == Scr.PanFrameRight.win ||
      ewp->window == Scr.PanFrameBottom.win) {
    int delta_x = 0, delta_y = 0;
    
    GenerateEdgeEvents();
    
    HandlePaging(Scr.EdgeScrollX, Scr.EdgeScrollY,
                 &Event.xcrossing.x_root, &Event.xcrossing.y_root,
                 &delta_x, &delta_y, True);
    goto HEN_return;
  }

  if (Event.xany.window == Scr.Root) {
    if (scm_is_true(g_lastwin_entered)) {
      scwm_run_hook1(window_leave_hook, g_lastwin_entered);
      g_lastwin_entered = SCM_BOOL_F;
    }
  
    if (Scr.Focus && !Scr.Focus->fClickToFocus &&
	!Scr.Focus->fSloppyFocus) {
      SetFocus(Scr.NoFocusWin, NULL, True);
    }
    if (Scr.fColormapFollowsMouse) {
      InstallWindowColormaps(NULL);
    }
    goto HEN_return;
  }


  /* make sure its for one of our windows */
  if (!pswCurrent)
    goto HEN_return;

  if (SCM_FROM_PSW(pswCurrent) != g_lastwin_entered) {
    if (scm_is_true(g_lastwin_entered))
      scwm_run_hook1(window_leave_hook, g_lastwin_entered);
    g_lastwin_entered = SCM_FROM_PSW(pswCurrent);
    scwm_run_hook1(window_enter_hook, SCM_FROM_PSW(pswCurrent));
  }

  if (!pswCurrent->fClickToFocus) {
    SetFocus(pswCurrent->w, pswCurrent, False);
  }
  if (Scr.fColormapFollowsMouse) {
    if (!pswCurrent->fIconified && (Event.xany.window == pswCurrent->w))
      InstallWindowColormaps(pswCurrent);
    else
      InstallWindowColormaps(NULL);
  }
 HEN_return:
  DBUG_EVENT((DBG,"HandleEnterNotify", "return"));
  return;
}


/*
 * HandleLeaveNotify - LeaveNotify event handler
 */
void 
HandleLeaveNotify()
{
  XEnterWindowEvent *ewp = &Event.xcrossing;

  DBUG_EVENT((DBG,"HandleLeaveNotify", "Entered"));
  /* If we leave the root window, then we're really moving
   * another screen on a multiple screen display, and we
   * need to de-focus and unhighlight to make sure that we
   * don't end up with more than one highlighted window at a time */

  if (ewp->window == Scr.Root) {
    if (ewp->mode == NotifyNormal) {
      if (ewp->detail != NotifyInferior) {
	if (Scr.Focus != NULL) {
	  SetFocus(Scr.NoFocusWin, NULL, True);
	}
	if (Scr.Hilite != NULL)
	  SetBorder(Scr.Hilite, False, True, True, None);
      }
    }
  } else if (ewp->window == Scr.PanFrameTop.win ||
             ewp->window == Scr.PanFrameLeft.win ||
             ewp->window == Scr.PanFrameRight.win ||
             ewp->window == Scr.PanFrameBottom.win) {
    GenerateEdgeEvents();
  }

  DBUG_EVENT((DBG,"HandleLeaveNotify", "return"));
}


/*
 * HandleConfigureRequest - ConfigureRequest event handler
 */
void 
HandleConfigureRequest()
{
#define FUNC_NAME "HandleConfigureRequest"
  XWindowChanges xwc;
  unsigned long xwcm;
  int x, y, width, height;
  XConfigureRequestEvent *cre = &Event.xconfigurerequest;
  Bool sendEvent = False;
  Bool fX_spec = cre->value_mask & CWX;
  Bool fY_spec = cre->value_mask & CWY;
  Bool fWidth_spec = cre->value_mask &CWWidth;
  Bool fHeight_spec = cre->value_mask &CWHeight;
  Bool fIconConfigure = False;

  DBUG_EVENT((DBG,FUNC_NAME, "Entered"));
  DBUG_RESIZE((DBG,FUNC_NAME, "Routine Entered"));

  /*
   * Event.xany.window is Event.xconfigurerequest.parent, so pswCurrent will
   * be wrong
   */
  Event.xany.window = cre->window;	/* mash parent field */
  pswCurrent = PswFromWindow(dpy, cre->window);

  fIconConfigure = (!pswCurrent || 
                    (pswCurrent->icon_w == cre->window) ||
                    (pswCurrent->icon_pixmap_w == cre->window));


  scm_variable_set_x(scm_configure_request_handled, SCM_BOOL_F);

  scwm_run_hook(x_configurerequest_hook,
                scm_list_n(pswCurrent?SCM_FROM_PSW(pswCurrent):SCM_BOOL_F,
			   scm_from_bool(fIconConfigure),
			   fX_spec? scm_from_int(cre->x): SCM_BOOL_F,
			   fY_spec? scm_from_int(cre->y): SCM_BOOL_F,
			   fWidth_spec? scm_from_int(cre->width): SCM_BOOL_F,
			   fHeight_spec? scm_from_int(cre->height): SCM_BOOL_F,
			   SCM_UNDEFINED));
                        
  /* do nothing else in C if the hook handles it */
  // eq? #t  scm_is_bool() &&
  if (scm_is_true(scm_variable_ref(scm_configure_request_handled)))
    return;

  /*
   * According to the July 27, 1988 ICCCM draft, we should ignore size and
   * position fields in the WM_NORMAL_HINTS property when we map a window.
   * Instead, we'll read the current geometry.  Therefore, we should respond
   * to configuration requests for windows which have never been mapped.
   */
  if (fIconConfigure) {
    xwcm = cre->value_mask &
      (CWX | CWY | CWWidth | CWHeight | CWBorderWidth);
    xwc.x = cre->x;
    xwc.y = cre->y;
    if (pswCurrent && (pswCurrent->icon_w == cre->window)) {
      pswCurrent->icon_xl_loc = cre->x;
      pswCurrent->icon_x_loc = cre->x +
	((pswCurrent->icon_w_width - pswCurrent->icon_p_width) / 2);
      pswCurrent->icon_y_loc = cre->y - pswCurrent->icon_p_height ;
      if (!pswCurrent->fIconUnmapped) {
	BroadcastIconInfo(M_ICON_LOCATION, pswCurrent);
      }
    }
    xwc.width = cre->width;
    xwc.height = cre->height;
    xwc.border_width = cre->border_width;
#ifdef SCWM_DEBUG_RESIZE_MSGS
    scwm_msg(DBG,FUNC_NAME,"!pswCurrent && configure to %d,%d", xwc.x, xwc.y);
#endif
    /* GJB:FIXME:: this is just moving the icon, but when
       icon positions are exposed to cassowary, it'll need fixing */
    XConfigureWindow(dpy, Event.xany.window, xwcm, &xwc);

    if (pswCurrent) {
      xwcm = cre->value_mask & (CWX | CWY);
      if (pswCurrent->icon_pixmap_w != None) {
        xwc.x = ICON_X_VP(pswCurrent);
        xwc.y = ICON_Y_VP(pswCurrent);
	XConfigureWindow(dpy, pswCurrent->icon_pixmap_w, xwcm, &xwc);
      }
      if (pswCurrent->icon_w != None) {
        xwc.x = ICON_X_VP(pswCurrent);
        xwc.y = ICON_Y_VP(pswCurrent) + pswCurrent->icon_p_height;
	XConfigureWindow(dpy, pswCurrent->icon_w, xwcm, &xwc);
      }
    }
    return;
  }
  if (cre->value_mask & CWStackMode) {
    ScwmWindow *otherwin;
    Bool fSibling = cre->value_mask & CWSibling? True: False;
    if (fSibling && (otherwin = PswFromWindow(dpy,cre->above))) {
      xwc.sibling = otherwin->frame;
    } else {
      xwc.sibling = cre->above;
    }
    xwc.stack_mode = cre->detail;
    XConfigureWindow(dpy, pswCurrent->frame,
		     cre->value_mask & (CWSibling | CWStackMode), &xwc);
    sendEvent = True;
  }
#ifdef HAVE_SHAPE
  if (ShapesSupported) {
    int xws, yws, xbs, ybs;
    unsigned wws, hws, wbs, hbs;
    int boundingShaped, clipShaped;

    XShapeQueryExtents(dpy, pswCurrent->w, &boundingShaped, &xws, &yws, &wws,
		       &hws, &clipShaped, &xbs, &ybs, &wbs, &hbs);
    pswCurrent->fShaped = boundingShaped;
  }
#endif

  /* Don't modify frame_XXX fields before calling SetupWindow! */
  x = FRAME_X_VP(pswCurrent);
  y = FRAME_Y_VP(pswCurrent);
  width = FRAME_WIDTH(pswCurrent);
  height = FRAME_HEIGHT(pswCurrent);

  /* for restoring */
  if (cre->value_mask & CWBorderWidth) {
    pswCurrent->old_bw = cre->border_width;
  }
  /* override even if border change */

  if (fX_spec)
    x = cre->x - DecorationXOffset(pswCurrent);
  if (fY_spec)
    y = cre->y - DecorationYOffset(pswCurrent);
  if (fWidth_spec)
    width = cre->width + DecorationWidth(pswCurrent);
  if (fHeight_spec)
    height = cre->height + DecorationHeight(pswCurrent);

  /*
   * SetupWindow (x,y) are the location of the upper-left outer corner and
   * are passed directly to XMoveResizeWindow (frame).  The (width,height)
   * are the inner size of the frame.  The inner width is the same as the 
   * requested client window width; the inner height is the same as the
   * requested client window height plus any title bar slop.
   */

  /* If the window is already shaded, save original sizes and ignore
     the height request. Ignoring the height request does not DTRT if
     a title height change is requested while a window is shaded, but
     is closer to doing the right thing. */

  /* When the window is shaded, we need to ignore the height change
     request, but we should save the requested size as the original
     size. */

  if (SHADED_P(pswCurrent)) {
    pswCurrent->orig_width = width;
    pswCurrent->orig_height = height;
    height = FRAME_HEIGHT(pswCurrent);
  }

  if ((fX_spec || fY_spec)) {
#ifdef SCWM_DEBUG_RESIZE_MSGS
    scwm_msg(DBG,FUNC_NAME,"MoveResize to %d,%d %dx%d", 
             x, y, width, height);
#endif
    MoveResizeTo(pswCurrent, 
                 x + WIN_VP_OFFSET_X(pswCurrent),
                 y + WIN_VP_OFFSET_Y(pswCurrent), width, height);
  } else {
    /* just resize, and let gravity take effect */
    ResizeTo(pswCurrent, width, height);
  }
  KeepOnTop();
}
#undef FUNC_NAME

/*
 * HandleShapeNotify - shape notification event handler
 */
void 
HandleShapeNotify(void)
{
#ifdef HAVE_SHAPE
  DBUG_EVENT((DBG,"HandleShapeNotify", "Routine Entered"));

  if (ShapesSupported) {
    XShapeEvent *sev = (XShapeEvent *) & Event;

    if (!pswCurrent)
      return;
    if (sev->kind != ShapeBounding)
      return;
    pswCurrent->fShaped = sev->shaped;
    SetShape(pswCurrent, FRAME_WIDTH(pswCurrent));
  }
#endif
}

void
HandleSelectionNotify(void)
{
  scwm_run_hook0(x_selectionnotify_hook);
}


/*
 * HandleVisibilityNotify - record fully visible windows for
 *      use in the RaiseLower function and the OnTop type windows.
 */
void 
HandleVisibilityNotify()
{
  XVisibilityEvent *vevent = (XVisibilityEvent *) &Event;
  extern int fInMoveViewport_internal;

  DBUG_EVENT((DBG,"HandleVisibilityNotify", "Routine Entered"));

  if (pswCurrent && last_event_window == pswCurrent->frame) {
    SCM from_move_viewport_action = scm_from_bool(fInMoveViewport_internal);
    pswCurrent->fVisible = (vevent->state == VisibilityUnobscured);

    /* For the most part, we'll raised partially obscured fOnTop windows
     * here. The exception is fOnTop windows that are obscured by
     * other fOnTop windows, which are raised in KeepOnTop(). This
     * complicated set-up saves us from continually re-raising
     * every on top window */
    if (((vevent->state == VisibilityPartiallyObscured) ||
	 (vevent->state == VisibilityFullyObscured)) &&
	pswCurrent->fOnTop && pswCurrent->fRaised) {
      RaiseWindow(pswCurrent);
      pswCurrent->fRaised = False;
    }
    switch (vevent->state) {
    case VisibilityFullyObscured:
      scwm_run_hook2(window_fully_obscured_hook, 
                     SCM_FROM_PSW(pswCurrent), from_move_viewport_action);
      break;
    case VisibilityUnobscured:
      scwm_run_hook2(window_unobscured_hook, 
                     SCM_FROM_PSW(pswCurrent), from_move_viewport_action);
      break;
    case VisibilityPartiallyObscured:
      scwm_run_hook2(window_partially_obscured_hook, 
                     SCM_FROM_PSW(pswCurrent), from_move_viewport_action);
      break;
    }
    pswCurrent->visibility = vevent->state;
  }
}

/* CoerceEnterNotifyOnCurrentWindow()
 * Pretends to get a HandleEnterNotify on the
 * window that the pointer currently is in so that
 * the focus gets set correctly from the beginning
 * Note that this presently only works if the current
 * window is not click_to_focus;  I think that
 * that behaviour is correct and desirable. --11/08/97 gjb */
void
CoerceEnterNotifyOnCurrentWindow()
{
  extern ScwmWindow *pswCurrent; /* from events.c */
  Window child, root;
  int root_x, root_y;
  int win_x, win_y;
  Bool f = XQueryPointer(dpy, Scr.Root, &root,
			 &child, &root_x, &root_y, &win_x, &win_y, &JunkMask);
  if (f && child != None) {
    Event.xany.window = child;
    pswCurrent = PswFromWindow(dpy,child);
    HandleEnterNotify();
    pswCurrent = None;
  }
}


int
NoEventsScwmUpdate(Bool fNoBlock)
{
#define FUNC_NAME "NoEventsScwmUpdate"
  extern int fd_width, x_fd;
  fd_set in_fdset, out_fdset;
  int retval;
  struct timeval timeout;
  struct timeval *tp;
  int usec;

  /* Zap all those zombies! */
  /* If we get to here, then there are no X events waiting to be processed.
   * Just take a moment to check for dead children. */
  ReapChildren();

  /* Ensure that no newly-installed input hooks will unnecessarily
     block during the select(), if there is data in the buffer. */
  force_new_input_hooks();

  fd_width = 0;

  FD_ZERO(&in_fdset);
  FD_SET(x_fd, &in_fdset);

  fd_width = x_fd;

  add_hook_fds_to_set(&in_fdset, &fd_width);

  FD_ZERO(&out_fdset);

  XFlush(dpy);
  timerclear(&timeout);
  
  update_timer_hooks();
  
  while (True) {
    usec = shortest_timer_timeout ();
    if (usec == 0) {
      run_timed_out_timers();
    } else if (usec < 0 && fNoBlock == False) {
      tp = NULL;
      break;
    } else {
      if (fNoBlock) usec = 0;
      timeout.tv_usec = usec;
      tp = &timeout;
      break;
    }
  }

#ifdef HAVE_LIBSM_LIBICE
  if (IceSMfd != -1)
    FD_SET(IceSMfd, &in_fdset);
#endif

  retval = scm_std_select(fd_width + 1, &in_fdset, &out_fdset, 0, tp);

  if (retval == 0) {
    update_timer_hooks();
    run_timed_out_timers();
  } else {
#ifdef HAVE_LIBSM_LIBICE
    if (IceSMfd != -1 && FD_ISSET(IceSMfd, &in_fdset)) {
      Bool rep;
      if (IceProcessMessages(IceSMconn, NULL, &rep)
          == IceProcessMessagesIOError)
        {
          SmcCloseConnection(SMconn, 0, NULL);
          IceSMconn = NULL;
        }
    } else 
#endif
      { /* scope/else in if above */
        DBUG((DBG,FUNC_NAME,"Before input hooks"));
        if (CServerGrabs() == 0) {
          /* GJB:FIXME:: why do I need to do this in C code--
             I also made scwm-gtk-sync bail immediately if
             there are any grabs, but it still hung for me --04/11/99 gjb */
          /* only run input hooks if server is grabbed,
             otherwise gtk may hang */
          run_input_hooks(&in_fdset);
        }
        DBUG((DBG,FUNC_NAME,"After input hooks"));
      }
  }
  return retval;
}
#undef FUNC_NAME

/*
 * Waits for next X event, 
 * or for a timer timeout
 * or for an ICE message (for session management)
 * or for input from an input hook (e.g., the fvwm2 module pipe)
 */
int 
NextScwmEvent(Display *dpy, XEvent *event, Bool fNoBlock)
{
#define FUNC_NAME "NextScwmEvent"
  DBUG((DBG,FUNC_NAME, "Entered"));

  /* Do this IMMEDIATELY prior to select, to prevent any nasty
   * queued up X events from just hanging around waiting to be
   * flushed */
  XFlush(dpy);
  if (XPending(dpy)) {
    DBUG_EVENT((DBG,FUNC_NAME, "taking care of queued up events & returning"));
    XNextEvent(dpy, event);
    StashEventTime(event);
    DBUG((DBG,FUNC_NAME, "return -- got event"));
    return 0;
  }
  DBUG((DBG,FUNC_NAME, 
              "no X events waiting - calling NoEventsScwmUpdate"));
  NoEventsScwmUpdate(fNoBlock);
  DBUG((DBG,FUNC_NAME, "return"));
  return 1;
}
#undef FUNC_NAME


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

static
Window
WindowGettingButtonEvent(Window w, int x, int y)
{
#define FUNC_NAME "WindowGettingButtonEvent"
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
      scwm_msg(ERR,FUNC_NAME,"Infinite loop");
      goto find_listener;
    }
    goto find_window;
  }
  w = w2;

 find_listener:
  XGetWindowAttributes(dpy, w, &wa);
  if (!(wa.all_event_masks & (ButtonPressMask | ButtonReleaseMask))) {
    Window d1, *d3, parent;
    unsigned int d4;
	
    if (XQueryTree(dpy, w, &d1, &parent, &d3, &d4)) {
      /* success */
      if (d3) XFree(d3);
      if (parent) {
        w = parent;
        goto find_listener;
      }
    } 
  }
  return w;
}
#undef FUNC_NAME


extern long basic_event_mask;

/* GJB:FIXME:: Only for newer guiles for now */
SCM_DEFINE(add_motion_handler_x, "add-motion-handler!", 1, 0, 0,
          (SCM proc),
"Call PROC on XMotionEvents.\n\n"
"This can considerably slow Scwm down so use it only when\n"
"necessary.  See `remove-motion-handler' and `reset-motion-handlers'.")
#define FUNC_NAME s_add_motion_handler_x
{
  VALIDATE_ARG_PROC(1,proc);
  XSelectInput(dpy, Scr.Root,(basic_event_mask | PointerMotionMask | ButtonMotionMask));
  return scm_add_hook_x(x_motionnotify_hook,proc,SCM_BOOL_F);
}
#undef FUNC_NAME

SCM_DEFINE(remove_motion_handler_x, "remove-motion-handler!", 1, 0, 0,
          (SCM proc),
"No longer call PROC on XMotionEvents.\n\n"
"Handling motion events can considerably slow Scwm down so use it only when\n"
"necessary.  See `add-motion-handler' and `reset-motion-handlers'.")
#define FUNC_NAME s_remove_motion_handler_x
{
  SCM answer;
  VALIDATE_ARG_PROC(1,proc);
  answer = scm_remove_hook_x(x_motionnotify_hook,proc);
  if (scm_hook_empty_p(x_motionnotify_hook))
    XSelectInput(dpy, Scr.Root,basic_event_mask);
  return answer;
}
#undef FUNC_NAME

SCM_DEFINE(reset_motion_handlers_x, "reset-motion-handlers!", 0, 0, 0,
          (),
"Call no procedures on XMotionEvents.\n\n"
"Handling motion events can considerably slow Scwm down so use it only when\n"
"necessary.  See `add-motion-handler' and `remove-motion-handler'.")
#define FUNC_NAME s_reset_motion_handlers_x
{
  XSelectInput(dpy, Scr.Root,basic_event_mask);
  return scm_reset_hook_x(x_motionnotify_hook);
}
#undef FUNC_NAME

/* Inspired by GWM 1.8c --gjb */

SCM_DEFINE(send_key, "send-key", 1,4,0,
          (SCM key, SCM win, SCM key_press_p, SCM key_release_p, SCM propagate_p),
"Send a synthetic press/release of KEY.\n\n"
"The usual key specification format (with modifiers) is used. The event\n"
"is sent to window WIN if specified; otherwise the window to be used\n"
"defaults to the window context in the usual way. By default, both a\n"
"press and a release are sent. However, the boolean parameters\n"
"{KEY-PRESS? and KEY-RELEASE? allow you to specify which are sent\n"
"individually. PROPAGATE? indicates whether the propagate flag is set\n"
"on the event; the default is #f. You should not have to worry about\n"
"this unless you know what it means.")
#define FUNC_NAME s_send_key
{
  KeySym keysym;
  Bool fOkay;
  int mod_mask;
  Bool fPropagate = False;
  Bool fPress = True;
  Bool fRelease = True;
  XKeyEvent event;
  Window w;

  VALIDATE_ARG_WIN_ROOTSYM_OR_NUM_COPY_USE_CONTEXT(2, win, w);
  VALIDATE_ARG_BOOL_COPY_USE_T(3, key_press_p, fPress);
  VALIDATE_ARG_BOOL_COPY_USE_T(4, key_release_p, fRelease);
  VALIDATE_ARG_BOOL_COPY_USE_T(5, propagate_p, fPropagate);

  fOkay = FKeyToKeysymModifiers(key,&keysym,&mod_mask, FUNC_NAME, False, True);

  if (fOkay) {
    if (fPress) {
      fill_x_keypress_event(&event, KeyPress, keysym, mod_mask, w);
      XSendEvent(dpy, w, fPropagate, KeyPressMask, 
		 (XEvent *) &event);
    }
    if (fRelease) {
      fill_x_keypress_event(&event, KeyRelease, keysym, mod_mask, w);
      XSendEvent(dpy, w, fPropagate, KeyReleaseMask, 
		 (XEvent *) &event);
    }
  } else {
    scwm_message(WARN,FUNC_NAME,"Bad keysym `~A' not sent",scm_list_1(key));
  }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE(send_button, "send-button", 1, 5, 0,
          (SCM button, SCM win, SCM kind, SCM propagate_p, SCM dx, SCM dy),
"Send a synthetic mouse button/release event.\n\n"
"Create a synthetic event of a press of mouse button BUTTON. The usual\n"
"mouse button specification format (with modifiers) is used. Send the\n"
"event to window WIN if specified; otherwise the window to be used\n"
"defaults to the window context in the usual way. By default, both a\n"
"press and a release are sent---a click. KIND can be one of 'press, 'release,\n"
"'click, 'desk-press, 'desk-release, or 'desk-click.\n"
"If DX or DY is set, that value is used as the offset within WIN for\n"
"the button events to occur.  If one is not specified or #f, then the\n"
"pointer offset of that coordinate is used instead.\n"
"PROPAGATE? indicates whether the propagate flag is set\n"
"on the event; the default is #f. You should not have to worry about\n"
"this unless you know what it means.")
#define FUNC_NAME s_send_button
{
  int bnum;
  int mod_mask;
  Bool fButtonOK = True;
  Bool fPropagate = False;
  Window child;
  XButtonEvent event;
  int x = 0, y = 0, x_root = 0 , y_root = 0;
  int x2 = 0, y2 = 0;
  int wx_offset, wy_offset;
  Window w;
  Window pointer_win;

  VALIDATE_ARG_WIN_ROOTSYM_OR_NUM_COPY_USE_CONTEXT(2, win,w);
  VALIDATE_ARG_SYM_USE_DEF(3,kind,sym_click);
  VALIDATE_ARG_BOOL_COPY_USE_T(4, propagate_p, fPropagate);
  VALIDATE_ARG_INT_COPY_USE_DEF(5, dx, wx_offset, -1);
  VALIDATE_ARG_INT_COPY_USE_DEF(6, dy, wy_offset, -1);

  fButtonOK = FButtonToBnumModifiers(button, &bnum, &mod_mask, FUNC_NAME, False);

  if (!fButtonOK) {
    SCWM_WRONG_TYPE_ARG(1,button);
  }

  /* First fill in x_root, y_root */
  pointer_win = WXGetPointerOffsets( w, &x_root, &y_root,&x, &y );

  /* Now find the window we're in */
  child = WindowGettingButtonEvent(w,x,y);
  x2 = x; y2 = y;

  /* and now find the offset within that window */
  XTranslateCoordinates(dpy, pointer_win, child, x2, y2,
			&x, &y, &JunkChild);

  if (!UNSET_SCM(dx)) {
    x_root += (wx_offset - x);
    x = wx_offset;
  }

  if (!UNSET_SCM(dy)) {
    y_root += (wx_offset - y);
    y = wy_offset;
  }

  if (scm_is_eq(kind, sym_click) || scm_is_eq(kind, sym_press)) {
    fill_x_button_event(&event, ButtonPress, bnum, mod_mask, 
			x, y, x_root, y_root, child, 0);
    XSendEvent(dpy, child, fPropagate, ButtonPressMask, 
	       (XEvent *) &event);
    DBUG((DBG,FUNC_NAME,"New Sent button press of %d at %d, %d; time = %ld\n",
          bnum,x,y,lastTimestamp));
  }
  if (scm_is_eq(kind, sym_click) || scm_is_eq(kind, sym_release)) {
    fill_x_button_event(&event, ButtonRelease, bnum, mod_mask | (1 << (bnum+7)),
			x, y, x_root, y_root, child, 0);
    XSendEvent(dpy, child, fPropagate, ButtonReleaseMask, 
	       (XEvent *) &event);
    DBUG((DBG,FUNC_NAME,"New Sent button release of %d at %d, %d; time = %ld\n",
          bnum,x,y,lastTimestamp));
  }
    
  /* desk events use w, not child, as the window to receive the event */
  if (scm_is_eq(kind, sym_desk_click) || scm_is_eq(kind, sym_desk_press)) {
    XUngrabPointer(dpy,CurrentTime);
    fill_x_button_event(&event, ButtonPress, bnum, mod_mask, 
			x, y, x_root, y_root, w, 0);
    XSendEvent(dpy, w, fPropagate, SubstructureNotifyMask, 
	       (XEvent *) &event);
  }

  if (scm_is_eq(kind, sym_desk_click) || scm_is_eq(kind, sym_desk_release)) {
    fill_x_button_event(&event, ButtonRelease, bnum, mod_mask | (1 << (bnum+7)), 
			x, y, x_root, y_root, w, 0);
    XSendEvent(dpy, w, fPropagate, SubstructureNotifyMask, 
	       (XEvent *) &event);
  }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



void 
init_events()
{
  /* do not permit add-hook!, remove-hook! access to this */
  x_motionnotify_hook = SCWM_MAKE_HOOK("%X-MotionNotify-hook", scm_from_int(6));
  
#include "events.x"
}



/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

