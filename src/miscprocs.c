/* $Id$
 * miscprocs.c
 * Copyright (C) 1998-1999 Maciej Stachowiak and Greg J. Badros
 *
 * This module has been significantly modified from fvwm2
 * It may be used under the terms of the fvwm copyright (see COPYING.FVWM).
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <unistd.h>
#include <signal.h>
#include <sys/times.h>
#include <X11/Xlib.h>

#include <guile/gh.h>

#include "scwm.h"
#include "screen.h"
#include "errors.h"
#include "events.h"
#include "util.h"
#include "decor.h"
#include "font.h"
#include "xmisc.h"
#include "scwmpaths.h"

extern SCM sym_center, sym_left, sym_right, sym_mouse;
extern Bool Restarting, PPosOverride;

SCWM_SYMBOL(sym_focus, "focus");

SCWM_PROC(set_title_justify_x,"set-title-justify!", 1, 0, 0,
          (SCM just))
     /** Set the justification for the title to JUST.
JUST should be one of 'right, 'left, or 'center. Applies to the
current decor. */
#define FUNC_NAME s_set_title_justify_x
{
  ScwmDecor *fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  if (!gh_symbol_p(just)) {
    gh_allow_ints();
    SCWM_WRONG_TYPE_ARG(1, just);
  }
  if (gh_eq_p(just, sym_center)) {
    fl->titlebar.flags &= ~HOffCenter;
  } else if (gh_eq_p(just, sym_left)) {
    fl->titlebar.flags |= HOffCenter;
    fl->titlebar.flags &= ~HRight;
  } else if (gh_eq_p(just, sym_right)) {
    fl->titlebar.flags |= HOffCenter | HRight;
  } else {
    scwm_error(FUNC_NAME, "Justification must be \'left, \'right or \'center.");
  }
  /* XXX should redraw the title bars */
  redraw_titlebars(fl, 0);
  SCM_REALLOW_INTS;
  return (just);
}
#undef FUNC_NAME

SCWM_PROC(title_justify,"title-justify", 0, 0, 0,
          ())
     /** Return the current justification for the title, as set by `set-title-justify!'.
The return value will be one of 'right, 'left, or 'center. Applies to the
current decor. */
#define FUNC_NAME s_title_justify
{
  ScwmDecor *fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  if (fl->titlebar.flags & HOffCenter) {
    if (fl->titlebar.flags & HRight) {
      return sym_right;
    } else {
      return sym_left;
    }
  } else {
    return sym_center;
  }
}
#undef FUNC_NAME


SCWM_PROC(set_title_height_x, "set-title-height!", 1, 0, 0,
          (SCM height))
     /** Set the height of the titlebar in pixels to HEIGHT.
Applies to the current decor. */
#define FUNC_NAME s_set_title_height_x
{
  int th, extra_height;
  ScwmDecor *fl;

  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  if (!gh_number_p(height)) {
    SCWM_WRONG_TYPE_ARG(1, height);
  }
  th = gh_scm2int(height);
  if (th <= 4 || th > 256) {
    scwm_error(FUNC_NAME, "Bad height argument; must be from 5 to 256.");
  }
  extra_height = th - fl->TitleHeight;
  fl->TitleHeight = th;

  fl->window_font_y = FONTY(fl->window_font)
    + (th - (FONTHEIGHT(fl->window_font) + 3)) / 2;
  if (fl->window_font_y < FONTY(fl->window_font))
    fl->window_font_y = FONTY(fl->window_font);

  redraw_titlebars(fl, extra_height);

  return (height);
}
#undef FUNC_NAME


SCWM_PROC(title_height,"title-height", 0, 0, 0,
          ())
     /** Return the height of the titlebar in pixels, as set by `set-title-height!'.
Applies to the current decor. */
#define FUNC_NAME s_title_height
{
  ScwmDecor *fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  return gh_int2scm(fl->TitleHeight);
}
#undef FUNC_NAME



SCWM_PROC(restarted_p, "restarted?", 0, 0, 0,
          ())
/** Returns true if scwm is being restarted by itself. */
#define FUNC_NAME s_restarted_p
{
  return SCM_BOOL_FromBool(Restarting);
}
#undef FUNC_NAME

SCWM_PROC(capturing_p, "capturing?", 0, 0, 0,
          ())
     /** Returns #t when the windows are being captured.
This happens at two times: during initial startup, or during a
recapture operation. In either case, placement procedures should
probably avoid interaction and perhaps avoid moving the window being
placed at all. */
#define FUNC_NAME s_capturing_p
{
  return SCM_BOOL_FromBool(PPosOverride);
}
#undef FUNC_NAME


SCWM_PROC(refresh, "refresh", 0, 0, 0,
          ())
     /** Make sure all windows and their decorations are up to date.
This forces a redraw of the entire current viewport. Should not be
necessary during normal operation. */
#define FUNC_NAME s_refresh
{
  refresh_common(Scr.Root);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(set_click_delay_x, "set-click-delay!", 1, 0, 0,
          (SCM msec))
     /** Set the delay used in identifying mouse clicks and drags.
MSEC is specified in milliseconds. After MSEC milliseconds, a mouse-down
without a mouse-up is considered a drag.  Also, after MSEC milliseconds, a
single click is definitively identified as not a double click. */
#define FUNC_NAME s_set_click_delay_x
{
  if (!gh_number_p(msec)) {
    SCWM_WRONG_TYPE_ARG(1, msec);
  }

  Scr.ClickTime = gh_scm2long(msec);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(click_delay, "click-delay", 0, 0, 0,
          ())
     /** Returns the delay used in identifying mouse clicks and drags, in milliseconds. 
See also `set-click-delay!' */
#define FUNC_NAME s_click_delay
{
  return gh_long2scm(Scr.ClickTime);
}
#undef FUNC_NAME

SCWM_PROC(set_colormap_focus_x, "set-colormap-focus!", 1, 0, 0,
          (SCM ftype))
     /** Set the colormap focus policy to FTYPE. 
FTYPE can either be 'mouse, indicating that the window under the mouse
pointer should always have its colormap installed, or 'focus to
indicate that the window with the input focus should also get the
colormap focus. This makes a difference only when using focus policies
other than 'mouse. */
#define FUNC_NAME s_set_colormap_focus_x
{
  if (!gh_symbol_p(ftype)) {
    SCWM_WRONG_TYPE_ARG(1, ftype);
  }
  if (gh_eq_p(ftype, sym_focus)) {
    Scr.fColormapFollowsMouse = False;
  } else if (gh_eq_p(ftype, sym_mouse)) {
    Scr.fColormapFollowsMouse = True;
  } else {
    scwm_error(FUNC_NAME, "Colormap focus must be \'focus or \'mouse.");
  }

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(colormap_focus, "colormap-focus", 0, 0, 0,
          ())
     /** Return the colormap focus policy, as set by `set-colormap-focus!'. 
The value can be either be 'mouse, indicating that the window under
the mouse pointer will always colormap installed, or 'focus to
indicate that the window with the input focus should also get the
colormap focus.  */
#define FUNC_NAME s_colormap_focus
{
  return Scr.fColormapFollowsMouse ? sym_mouse : sym_focus; 
}
#undef FUNC_NAME

SCWM_PROC(pointer_position, "pointer-position", 0, 0, 0,
          ())
     /** Return the current position of the mouse pointer in pixels.
The return value is a two-element list of the x and y coordinates. */
#define FUNC_NAME s_pointer_position
{
  int x, y;

  WXGetPointerWindowOffsets(Scr.Root, &x, &y);

  return gh_list(SCM_MAKINUM(x), SCM_MAKINUM(y), SCM_UNDEFINED);
}
#undef FUNC_NAME


SCWM_PROC(move_pointer_to, "move-pointer-to", 2, 0, 0,
          (SCM sx, SCM sy))
     /** Move the mouse pointer to viewport coordinates SX, SY. */
#define FUNC_NAME s_move_pointer_to
{
  int x, y;

  if (!gh_number_p(sx)) {
    SCWM_WRONG_TYPE_ARG(1, sx);
  }
  if (!gh_number_p(sy)) {
    SCWM_WRONG_TYPE_ARG(2, sy);
  }
  x = gh_scm2int(sx);
  y = gh_scm2int(sy);
  XWarpPointer(dpy, Scr.Root, Scr.Root, 0, 0, Scr.DisplayWidth,
	       Scr.DisplayHeight, x, y);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(recapture, "recapture", 0, 0, 0,
          ())
     /** Recapture all the windows.
This destroys all the current frame windows and recreate them from
scratch. This is hopefully not necessary during normal operation. */
#define FUNC_NAME s_recapture
{
  BlackoutScreen();		/* if they want to hide the recapture */
  CaptureAllWindows();
  UnBlackoutScreen();

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(beep, "beep", 0, 0, 0,
          ())
     /** Ring the standard X bell. */
#define FUNC_NAME s_beep
{
  XBell(dpy, 0);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(set_smart_placement_is_really_smart_x, "set-smart-placement-is-really-smart!",1, 0, 0,
          (SCM flag))
     /** Determine whether or not `clever-place-window' will be used when smart-placing.
If FLAG is #t, then `clever-place-window' will be used instead of
`smart-place-window' when the default placement procedure is used, and
the window's smart-placement flag is on. */
#define FUNC_NAME s_set_smart_placement_is_really_smart_x
{
  VALIDATE_ARG_BOOL_COPY(1,flag,Scr.fSmartPlacementIsClever);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* FIXMS: this returns true or false always, give it a predicate name? */

SCWM_PROC(smart_placement_is_really_smart_p, "smart-placement-is-really-smart?", 0, 0, 0,
          ())
     /** Return whether or not `clever-place-window' will be used when smart-placing.
If the value is #t, then `clever-place-window' will be used instead of
smart-place-window when the default placement procedure is used, and
the window's smart-placement flag is on. */
#define FUNC_NAME s_smart_placement_is_really_smart_p
{
  return SCM_BOOL_FromBool(Scr.fSmartPlacementIsClever);
}
#undef FUNC_NAME



/* FIXMS - the functionality related to the next six procedures
   should be implemented by adding new event bindings eventually */

SCWM_PROC(set_click_to_focus_passes_click_x, "set-click-to-focus-passes-click!", 1, 0, 0,
          (SCM flag))
     /** Determine whether a click-to-focus window receives the click.
If FLAG is #t, the window will receive the event, if #f, scwm
will not pass the event on to the client. */
#define FUNC_NAME s_set_click_to_focus_passes_click_x
{
  VALIDATE_ARG_BOOL_COPY(1,flag,Scr.fClickToFocusPassesClick);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(click_to_focus_passes_click_p, "click-to-focus-passes-click?", 0, 0, 0,
          ())
     /** Returns a boolean valude indicating whether a click-to-focus window receives the click. */
#define FUNC_NAME s_click_to_focus_passes_click_p
{
  return SCM_BOOL_FromBool(Scr.fClickToFocusPassesClick);
}
#undef FUNC_NAME


/* FIXMS: remove this one in particular once window-focus-hook exists. */

SCWM_PROC(set_click_to_focus_raises_x, "set-click-to-focus-raises!", 1, 0, 0,
          (SCM flag))
     /** Determine whether a click to focus raises the clicked-on window.
If FLAG is #t, clicks which transfer focus will also raise the target 
window */
#define FUNC_NAME s_set_click_to_focus_raises_x
{
  VALIDATE_ARG_BOOL_COPY(1,flag,Scr.fClickToFocusRaises);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/* FIXMS - this seems to be redundant with auto-raise to some degree... */

SCWM_PROC(click_to_focus_raises_p, "click-to-focus-raises?", 0, 0, 0,
          ())
     /** Returns a boolean valude indicating whether a click-to-focus window gets raised on focus. */
#define FUNC_NAME s_click_to_focus_raises_p
{
  return SCM_BOOL_FromBool(Scr.fClickToFocusRaises);
}
#undef FUNC_NAME


/* FIXMS - this seems to be a pretty useless idea, or at least there
   must be a better way of implementing it. */

/* FIXMS - looking at it again, this idea seems utterly
   bogus. Couldn't you just (bind-mouse 'all 1 raise-window) and get
   the exact same effect?  Examine further... */

SCWM_PROC(set_mouse_focus_click_raises_x, "set-mouse-focus-click-raises!", 1, 0, 0,
          (SCM flag))
     /** Determine whether a mouse-focus-click will raise the window.
If FLAG is #t it will raise the window. Not sure if this function
makes sense any more. */
#define FUNC_NAME s_set_mouse_focus_click_raises_x
{
  VALIDATE_ARG_BOOL_COPY(1,flag,Scr.fMouseFocusClickRaises);
  return SCM_UNSPECIFIED;  
}
#undef FUNC_NAME


SCWM_PROC(mouse_focus_click_raises_p, "mouse-focus-click-raises?", 0, 0, 0,
          ())
     /** Returns a boolean value indicating whether a mouse-focus-click will raise the window. */
#define FUNC_NAME s_mouse_focus_click_raises_p
{
  return SCM_BOOL_FromBool(Scr.fMouseFocusClickRaises);
}
#undef FUNC_NAME



SCWM_PROC (X_rotate_cut_buffers, "X-rotate-cut-buffers", 1, 0, 0,
           (SCM n))
     /** Rotate the X cut buffers by N positions.
This means buffer 0 becomes buffer n, buffer 1 becomes n + 1 mod 8,
and so on.  This cut buffer numbering is global to the display. */
#define FUNC_NAME s_X_rotate_cut_buffers
{
  if (!gh_number_p(n)) {
    SCWM_WRONG_TYPE_ARG(1, n);
  }
  { /* scope */
    int num_to_rotate = gh_scm2int(n);
    XRotateBuffers(dpy,num_to_rotate);
  }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



/* CRW:FIXME:: If CLK_TCK is defined, the following works as documented;
otherwise, it returns the number of milliseconds of CPU time used
by the current process (both user time and system time), divided
by 60. */
/* CRW:FIXME:MS: Maybe this function should just be deleted?  It
doesn't seem to be used anywhere, and (as mentioned above) its implementation
is inconsistent... */
SCWM_PROC (elapsed_time, "elapsed-time", 0, 0, 0,
           ())
     /** Return the elapsed time in milliseconds since O.S. has been up. */
#define FUNC_NAME s_elapsed_time
{
  /* code borrowed from GWM's wool_used_time */
#ifdef CLK_TCK
#define TIME_UNIT CLK_TCK
#else
#define TIME_UNIT 60
#endif
  long time;
  struct tms buffer;

#ifdef CLK_TCK
  time = (times(&buffer) * 1000) / TIME_UNIT;
#else
  times(&buffer);
  time = ((buffer.tms_utime + buffer.tms_stime) * 1000 ) / TIME_UNIT;
#endif
  
  return gh_ulong2scm(time);
#undef TIME_UNIT
}
#undef FUNC_NAME


SCWM_PROC(scwm_last_timestamp, "scwm-last-timestamp", 0, 0, 0,
          ())
     /** Return the timestamp of the last event Scwm handled. */
#define FUNC_NAME s_scwm_last_timestamp
{
  extern Time lastTimestamp;
  return gh_long2scm(lastTimestamp);
}
#undef FUNC_NAME


SCWM_PROC(scwm_version, "scwm-version", 0, 0, 0,
          ())
     /** Return the version of scwm running. */
#define FUNC_NAME s_scwm_version
{
  return gh_str02scm(VERSION);
}
#undef FUNC_NAME


SCWM_PROC(scwm_version_date, "scwm-version-date", 0, 0, 0,
          ())
     /** Return the date that the running scwm was last changed as a string. */
#define FUNC_NAME s_scwm_version_date
{
  extern char *szRepoLastChanged;
  return gh_str02scm(szRepoLastChanged);
}
#undef FUNC_NAME


SCWM_PROC(scwm_path_prefix, "scwm-path-prefix", 0, 0, 0,
          ())
     /** Return the <envar>$PREFIX</envar> directory path that scwm was installed with. */
#define FUNC_NAME s_scwm_path_prefix
{
  return gh_str02scm(SCWM_PREFIX);
}
#undef FUNC_NAME


SCWM_PROC(scwm_path_exec_prefix, "scwm-path-exec-prefix", 0, 0, 0,
          ())
     /** Return the <envar>$EXEC_PREFIX</envar> directory path that scwm was installed with. */
#define FUNC_NAME s_scwm_path_exec_prefix
{
  return gh_str02scm(SCWM_PREFIX);
}
#undef FUNC_NAME


SCWM_PROC(set_X_server_synchronize_x, "set-X-server-synchronize!", 1, 0, 0,
          (SCM flag))
     /** Set X server sychronization flag to FLAG.
If FLAG is #t, then Scwm will turn on synchronous X behaviour; if FLAG 
is #f, Scwm will turn off synchronous behaviour.  Scwm is slower in
synchronous mode, but can be easier to debug. */
#define FUNC_NAME s_set_X_server_synchronize_x
{
  Bool fSynch;
  VALIDATE_ARG_BOOL_COPY(1,flag,fSynch);
  XSynchronize(dpy, fSynch);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/* FIXMS: this should probably be split into multiple procs. */
SCWM_PROC(X_version_information, "X-version-information", 0, 0, 0,
          ())
     /** Return some information about the version of the running X server.
Return value is a list of the X protocol version, the X protocol
revision, the X server vendor, and the vendor release number. */
#define FUNC_NAME s_X_version_information
{
  return gh_list(SCM_MAKINUM(ProtocolVersion(dpy)),
                 SCM_MAKINUM(ProtocolRevision(dpy)),
                 gh_str02scm(ServerVendor(dpy)),
                 SCM_MAKINUM(VendorRelease(dpy)),
                 SCM_UNDEFINED);
}
#undef FUNC_NAME


/* FIXMS: this should probably be split into multiple procs. Also, the
   visual type should probably be returned as a symbol, not a
   string.*/

SCWM_PROC(X_display_information, "X-display-information", 0, 0, 0,
          ())
     /** Return some information about the screen. In particular,
return a list of the horizontal resolution, the vertical resolution,
the number of planes on the current screen (i.e. the bit depth), the
bits per color supported by the hardware, the visual class (one of
"StaticGray", "GrayScale", "StaticColor", "PseudoColor", "DirectColor"
or "TrueColor") and a boolean indicating whether the display is color.
The resolutions mentioned above should in theory be pixels per
centimeter, rounded to the nearest integer. These parameters can be
used for various workarounds or conditional decisions in a scwmrc to
be shared among multiple machines. */
#define FUNC_NAME s_X_display_information
{
  int Mscreen = DefaultScreen(dpy);
  Screen *screen = ScreenOfDisplay(dpy, Mscreen);
  Visual *visual = DefaultVisualOfScreen(screen);
  /* RESOLUTION returns the pixels per cm, rounded */
#define RESOLUTION(pixels, mm) ((((pixels) * 100000 / (mm)) + 50) / 100)
  int xres = RESOLUTION(screen->width, screen->mwidth);
  int yres = RESOLUTION(screen->height, screen->mheight);
#undef Resolution
  int planes = DisplayPlanes(dpy,Mscreen);
  int bits_per_rgb = visual->bits_per_rgb;
  char *vc = NULL;

  /* class is res'd word in C++, struct member renamed to c_class in header */
#ifdef __cplusplus
  int visual_class = visual->c_class;
#else
  int visual_class = visual->class;
#endif

  Bool fColor = (visual_class != StaticGray && visual_class != GrayScale);

  switch(visual_class) 
  {
    case(StaticGray):
      vc = "StaticGray";
      break;
    case(GrayScale):
      vc = "GrayScale";
      break;
    case(StaticColor):
      vc = "StaticColor";
      break;
    case(PseudoColor):
      vc = "PseudoColor";
      break;
    case(TrueColor):
      vc = "TrueColor";
      break;
    case(DirectColor):
      vc = "DirectColor";
      break;
    default:
      vc = "NonStandard";
      break;
  }

  return gh_list(SCM_MAKINUM(xres),
                 SCM_MAKINUM(yres),
                 SCM_MAKINUM(planes),
                 SCM_MAKINUM(bits_per_rgb),
                 gh_str02scm(vc), /* class */
                 SCM_BOOL_FromBool(fColor),
                 SCM_UNDEFINED);
}
#undef FUNC_NAME

SCWM_PROC(user_name, "user-name", 0, 0, 0,
	  ())
     /** Return the current user's name.
This is set to one of the following (in order of relevance):
<envar>$USER</envar>, <envar>$LOGNAME</envar>,
the name field of the current uid's entry in the password file,
the constant string "nobody". */
#define FUNC_NAME s_user_name
{
  return gh_str02scm(UserName);
}
#undef FUNC_NAME

SCWM_PROC(user_home, "user-home", 0, 0, 0,
	  ())
     /** Return the current user's home directory.
This is set to one of the following (in order of relevance):
<envar>$HOME</envar>,
the directory field of the current uid's entry in the password file,
the constant string "/tmp". */
#define FUNC_NAME s_user_home
{
  return gh_str02scm(UserHome);
}
#undef FUNC_NAME

SCWM_PROC(force_segv_for_testing, "force-segv-for-testing", 0, 0, 0,
	  ())
     /** Cause a segmentation violation.
Do not do this unless you are testing segv handling! */
#define FUNC_NAME s_force_segv_for_testing
{
  int *pn = 0;
  *pn = 0;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(set_reset_on_segv_x, "set-reset-on-segv!", 1, 0, 0,
	  (SCM number_to_reset))
     /** Reset Scwm to the main event loop on the next NUMBER-TO-RESET segv signals.
The default is 100, but if you catch a segv and and are willing to 
track it or send in a bug report, use this to permit a C-level backtrace
by setting it to 0.  See also `set-handle-segv!'.
*/
#define FUNC_NAME s_set_reset_on_segv_x
{
  extern int segvs_to_reset;
  VALIDATE_ARG_INT_COPY(1,number_to_reset,segvs_to_reset);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(reset_on_segv, "reset-on-segv", 0, 0, 0,
	  ())
     /** Return the number of segv signals Scwm will reset on.
See `set-reset-on-segv!'.
*/
#define FUNC_NAME s_reset_on_segv
{
  extern int segvs_to_reset;
  return gh_int2scm(segvs_to_reset);
}
#undef FUNC_NAME



SCWM_PROC(set_handle_segv_x, "set-handle-segv!", 1, 0, 0,
	  (SCM flag))
     /** If FLAG, tell Scwm to catch segv signals.
This is the default, but if you catch a segv and really need it to 
dump right away to figure out what is wrong then use this.  No
cleanup is done if handling segv is #f.  Be sure to have an extra
terminal handy on a console or separate X server.
For developers and hackers only. */
#define FUNC_NAME s_set_handle_segv_x
{
  Bool f;
  VALIDATE_ARG_BOOL_COPY(1,flag,f);
  if (f) {
    newsegvhandler(SIGSEGV);
  } else {
    reset_signal_handler(SIGSEGV);
  }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(x_connection_number, "X-connection-number", 0, 0, 0,
          ())
     /** Return the X connection file descriptor number. */
#define FUNC_NAME s_x_connection_number
{
  int c = ConnectionNumber(dpy);
  return gh_int2scm(c);
}
#undef FUNC_NAME

#if 0
SCWM_PROC(get_key_event, "get-key-event", 0, 2, 0,
          (SCM async_mouse, SCM async_keyboard))
     /** Return a string representing the next key event.
If ASYNC-MOUSE is #t, mouse events are treated asynchronously.
If ASYNC-KEYBOARD is #t, keyboard events are treated asynchronously.
A non-modifier key must be pressed before it is considered an event. 
This has never been functioning and needs work to make right.
*/
#define FUNC_NAME s_get_key_event
{
  Bool fAsyncMouse;
  Bool fAsyncKeyboard;
  XEvent ev;
  VALIDATE_ARG_BOOL_COPY_USE_F(1,async_mouse,fAsyncMouse);
  VALIDATE_ARG_BOOL_COPY_USE_F(2,async_keyboard,fAsyncKeyboard);
  XSync(dpy,True);
  XGrabKeyboard(dpy, Scr.Root, False, 
                fAsyncMouse? GrabModeAsync: GrabModeSync, 
                fAsyncKeyboard? GrabModeAsync: GrabModeSync,
                CurrentTime);
  while (True) {
    XNextEvent(dpy, &ev);
    if (ev.type == KeyRelease)
      break;
  }
  XUngrabKeyboard(dpy, CurrentTime);
  return gh_list(gh_long2scm(ev.xkey.state),gh_long2scm(ev.xkey.keycode),SCM_UNDEFINED);
}
#endif

void 
init_miscprocs()
{
#ifndef SCM_MAGIC_SNARFER
#include "miscprocs.x"
#endif
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta */

