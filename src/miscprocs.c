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
#include <X11/keysym.h>

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
#include "cursor.h"
#ifdef SCWM_TEST_HOOK_PROCS
#include "callbacks.h"
#endif

extern SCM sym_center, sym_left, sym_right, sym_mouse;
extern Bool Restarting, PPosOverride;
extern Bool fHandleSegv;

SCWM_SYMBOL(sym_focus, "focus");

SCWM_HOOK(scwm_test_hook_0,"scwm-test-hook-0",0,
"Just a test hook that takes no arguments.\n\
See `scwm-run-test-hook-0'");

SCWM_HOOK(scwm_test_hook_1,"scwm-test-hook-1",1,
"Just a test hook that takes one argument.\n\
See `scwm-run-test-hook-1'.");

SCWM_PROC(set_title_justify_x,"set-title-justify!", 1, 0, 0,
          (SCM just),
"Set the justification for the title to JUST.\n\
JUST should be one of 'right, 'left, or 'center. Applies to the\n\
current decor.")
#define FUNC_NAME s_set_title_justify_x
{
  ScwmDecor *fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  VALIDATE_ARG_SYM(1,just);
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
  return (just);
}
#undef FUNC_NAME

SCWM_PROC(title_justify,"title-justify", 0, 0, 0,
          (),
"Return the current justification for the title, as set by `set-title-justify!'.\n\
The return value will be one of 'right, 'left, or 'center. Applies to the\n\
current decor.")
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
          (SCM height),
"Set the height of the titlebar in pixels to HEIGHT.\n\
Applies to the current decor.")
#define FUNC_NAME s_set_title_height_x
{
  int th, extra_height;
  ScwmDecor *fl;

  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  VALIDATE_ARG_INT_RANGE_COPY(1,height,5,256,th);
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
          (),
"Return the height of the titlebar in pixels, as set by `set-title-height!'.\n\
Applies to the current decor.")
#define FUNC_NAME s_title_height
{
  ScwmDecor *fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  return gh_int2scm(fl->TitleHeight);
}
#undef FUNC_NAME



SCWM_PROC(restarted_p, "restarted?", 0, 0, 0,
          (),
"Returns true if scwm is being restarted by itself.")
#define FUNC_NAME s_restarted_p
{
  return SCM_BOOL_FromBool(Restarting);
}
#undef FUNC_NAME

SCWM_PROC(capturing_p, "capturing?", 0, 0, 0,
          (),
"Returns #t when the windows are being captured.\n\
This happens at two times: during initial startup, or during a\n\
recapture operation. In either case, placement procedures should\n\
probably avoid interaction and perhaps avoid moving the window being\n\
placed at all.")
#define FUNC_NAME s_capturing_p
{
  return SCM_BOOL_FromBool(PPosOverride);
}
#undef FUNC_NAME


SCWM_IPROC(refresh, "refresh", 0, 0, 0,
           (), NULL,
"Make sure all windows and their decorations are up to date.\n\
This forces a redraw of the entire current viewport. Should not be\n\
necessary during normal operation.")
#define FUNC_NAME s_refresh
{
  refresh_common(Scr.Root);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(set_click_delay_x, "set-click-delay!", 1, 0, 0,
          (SCM msec),
"Set the delay used in identifying mouse clicks and drags.\n\
MSEC is specified in milliseconds. After MSEC milliseconds, a mouse-down\n\
without a mouse-up is considered a drag.  Also, after MSEC milliseconds, a\n\
single click is definitively identified as not a double click.")
#define FUNC_NAME s_set_click_delay_x
{
  int ms;
  VALIDATE_ARG_INT_COPY(1,msec,ms);
  Scr.ClickTime = ms;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(click_delay, "click-delay", 0, 0, 0,
          (),
"Returns the delay used in identifying mouse clicks and drags, in milliseconds. \n\
See also `set-click-delay!'")
#define FUNC_NAME s_click_delay
{
  return gh_long2scm(Scr.ClickTime);
}
#undef FUNC_NAME

SCWM_PROC(set_colormap_focus_x, "set-colormap-focus!", 1, 0, 0,
          (SCM ftype),
"Set the colormap focus policy to FTYPE. \n\
FTYPE can either be 'mouse, indicating that the window under the mouse\n\
pointer should always have its colormap installed, or 'focus to\n\
indicate that the window with the input focus should also get the\n\
colormap focus. This makes a difference only when using focus policies\n\
other than 'mouse.")
#define FUNC_NAME s_set_colormap_focus_x
{
  VALIDATE_ARG_SYM(1,ftype);

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
          (),
"Return the colormap focus policy, as set by `set-colormap-focus!'. \n\
The value can be either be 'mouse, indicating that the window under\n\
the mouse pointer will always colormap installed, or 'focus to\n\
indicate that the window with the input focus should also get the\n\
colormap focus.")
#define FUNC_NAME s_colormap_focus
{
  return Scr.fColormapFollowsMouse ? sym_mouse : sym_focus; 
}
#undef FUNC_NAME

SCWM_PROC(pointer_position, "pointer-position", 0, 0, 0,
          (),
"Return the current position of the mouse pointer in pixels.\n\
The return value is a two-element list of the x and y coordinates.")
#define FUNC_NAME s_pointer_position
{
  int x, y;

  WXGetPointerWindowOffsets(Scr.Root, &x, &y);

  return gh_list(SCM_MAKINUM(x), SCM_MAKINUM(y), SCM_UNDEFINED);
}
#undef FUNC_NAME


SCWM_PROC(move_pointer_to, "move-pointer-to", 2, 0, 0,
          (SCM sx, SCM sy),
"Move the mouse pointer to viewport coordinates SX, SY.")
#define FUNC_NAME s_move_pointer_to
{
  int x, y;
  VALIDATE_ARG_INT_COPY(1,sx,x);
  VALIDATE_ARG_INT_COPY(2,sy,y);

  XWarpPointer(dpy, Scr.Root, Scr.Root, 0, 0, Scr.DisplayWidth,
	       Scr.DisplayHeight, x, y);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_IPROC(recapture, "recapture", 0, 0, 0,
           (), NULL,
"Recapture all the windows.\n\
This destroys all the current frame windows and recreate them from\n\
scratch. This is hopefully not necessary during normal operation.")
#define FUNC_NAME s_recapture
{
  BlackoutScreen();		/* if they want to hide the recapture */
  CaptureAllWindows();
  UnBlackoutScreen();

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_IPROC(beep, "beep", 0, 0, 0,
           (), NULL,
"Ring the standard X bell.")
#define FUNC_NAME s_beep
{
  XBell(dpy, 0);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(set_smart_placement_is_really_smart_x, "set-smart-placement-is-really-smart!",1, 0, 0,
          (SCM flag),
"Determine whether or not `clever-place-window' will be used when smart-placing.\n\
If FLAG is #t, then `clever-place-window' will be used instead of\n\
`smart-place-window' when the default placement procedure is used, and\n\
the window's smart-placement flag is on.")
#define FUNC_NAME s_set_smart_placement_is_really_smart_x
{
  VALIDATE_ARG_BOOL_COPY(1,flag,Scr.fSmartPlacementIsClever);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* MS:FIXME:: this returns true or false always, give it a predicate name? */

SCWM_PROC(smart_placement_is_really_smart_p, "smart-placement-is-really-smart?", 0, 0, 0,
          (),
"Return whether or not `clever-place-window' will be used when smart-placing.\n\
If the value is #t, then `clever-place-window' will be used instead of\n\
smart-place-window when the default placement procedure is used, and\n\
the window's smart-placement flag is on.")
#define FUNC_NAME s_smart_placement_is_really_smart_p
{
  return SCM_BOOL_FromBool(Scr.fSmartPlacementIsClever);
}
#undef FUNC_NAME



/* MS:FIXME:: - the functionality related to the next six procedures
   should be implemented by adding new event bindings eventually */

SCWM_PROC(set_click_to_focus_passes_click_x, "set-click-to-focus-passes-click!", 1, 0, 0,
          (SCM flag),
"Determine whether a click-to-focus window receives the click.\n\
If FLAG is #t, the window will receive the event, if #f, scwm\n\
will not pass the event on to the client.")
#define FUNC_NAME s_set_click_to_focus_passes_click_x
{
  VALIDATE_ARG_BOOL_COPY(1,flag,Scr.fClickToFocusPassesClick);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(click_to_focus_passes_click_p, "click-to-focus-passes-click?", 0, 0, 0,
          (),
"Returns #t iff a click-to-focus window is sent the click, else #f.")
#define FUNC_NAME s_click_to_focus_passes_click_p
{
  return SCM_BOOL_FromBool(Scr.fClickToFocusPassesClick);
}
#undef FUNC_NAME


/* MS:FIXME:: remove this one in particular once window-focus-hook exists. */

SCWM_PROC(set_click_to_focus_raises_x, "set-click-to-focus-raises!", 1, 0, 0,
          (SCM flag),
"Determine whether a click to focus raises the clicked-on window.\n\
If FLAG is #t, clicks which transfer focus will also raise the target \n\
window.")
#define FUNC_NAME s_set_click_to_focus_raises_x
{
  VALIDATE_ARG_BOOL_COPY(1,flag,Scr.fClickToFocusRaises);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/* MS:FIXME:: - this seems to be redundant with auto-raise to some degree... */

SCWM_PROC(click_to_focus_raises_p, "click-to-focus-raises?", 0, 0, 0,
          (),
"Returns #t iff a click-to-focus window gets raised on focus, else #f.")
#define FUNC_NAME s_click_to_focus_raises_p
{
  return SCM_BOOL_FromBool(Scr.fClickToFocusRaises);
}
#undef FUNC_NAME


/* MS:FIXME:: - this seems to be a pretty useless idea, or at least there
   must be a better way of implementing it. */

/* MS:FIXME:: - looking at it again, this idea seems utterly
   bogus. Couldn't you just (bind-mouse 'all 1 raise-window) and get
   the exact same effect?  Examine further... */

SCWM_PROC(set_mouse_focus_click_raises_x, "set-mouse-focus-click-raises!", 1, 0, 0,
          (SCM flag),
"Determine whether a mouse-focus-click will raise the window.\n\
If FLAG is #t it will raise the window. Not sure if this function\n\
makes sense any more.")
#define FUNC_NAME s_set_mouse_focus_click_raises_x
{
  VALIDATE_ARG_BOOL_COPY(1,flag,Scr.fMouseFocusClickRaises);
  return SCM_UNSPECIFIED;  
}
#undef FUNC_NAME


SCWM_PROC(mouse_focus_click_raises_p, "mouse-focus-click-raises?", 0, 0, 0,
          (),
"Returns a boolean value indicating whether a mouse-focus-click will raise the window.")
#define FUNC_NAME s_mouse_focus_click_raises_p
{
  return SCM_BOOL_FromBool(Scr.fMouseFocusClickRaises);
}
#undef FUNC_NAME



SCWM_PROC(X_rotate_cut_buffers, "X-rotate-cut-buffers", 1, 0, 0,
          (SCM n),
"Rotate the X cut buffers by N positions.\n\
This means buffer 0 becomes buffer n, buffer 1 becomes n + 1 mod 8,\n\
and so on.  This cut buffer numbering is global to the display.")
#define FUNC_NAME s_X_rotate_cut_buffers
{
  int num_to_rotate;
  VALIDATE_ARG_INT_COPY(1,n,num_to_rotate);
  XRotateBuffers(dpy,num_to_rotate);
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
SCWM_PROC(elapsed_time, "elapsed-time", 0, 0, 0,
          (),
"Return the elapsed time in milliseconds since O.S. has been up.")
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
          (),
"Return the timestamp of the last event Scwm handled.")
#define FUNC_NAME s_scwm_last_timestamp
{
  extern Time lastTimestamp;
  return gh_long2scm(lastTimestamp);
}
#undef FUNC_NAME


SCWM_PROC(scwm_version, "scwm-version", 0, 0, 0,
          (),
"Return the version of scwm running.")
#define FUNC_NAME s_scwm_version
{
  return gh_str02scm(VERSION);
}
#undef FUNC_NAME


SCWM_PROC(scwm_version_date, "scwm-version-date", 0, 0, 0,
          (),
"Return the date that the running scwm was last changed as a string.")
#define FUNC_NAME s_scwm_version_date
{
  extern char *szRepoLastChanged;
  return gh_str02scm(szRepoLastChanged);
}
#undef FUNC_NAME


SCWM_PROC(scwm_path_prefix, "scwm-path-prefix", 0, 0, 0,
          (),
"Return the <envar>$PREFIX</envar> directory path that scwm was installed with.")
#define FUNC_NAME s_scwm_path_prefix
{
  return gh_str02scm(SCWM_PREFIX);
}
#undef FUNC_NAME


SCWM_PROC(scwm_path_exec_prefix, "scwm-path-exec-prefix", 0, 0, 0,
          (),
"Return the <envar>$EXEC_PREFIX</envar> directory path that scwm was installed with.")
#define FUNC_NAME s_scwm_path_exec_prefix
{
  return gh_str02scm(SCWM_PREFIX);
}
#undef FUNC_NAME


SCWM_PROC(set_X_server_synchronize_x, "set-X-server-synchronize!", 1, 0, 0,
          (SCM flag),
"Set X server sychronization flag to FLAG.\n\
If FLAG is #t, then Scwm will turn on synchronous X behaviour; if FLAG \n\
is #f, Scwm will turn off synchronous behaviour.  Scwm is slower in\n\
synchronous mode, but can be easier to debug.")
#define FUNC_NAME s_set_X_server_synchronize_x
{
  Bool fSynch;
  VALIDATE_ARG_BOOL_COPY(1,flag,fSynch);
  XSynchronize(dpy, fSynch);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/* MS:FIXME:: this should probably be split into multiple procs. */
SCWM_PROC(X_version_information, "X-version-information", 0, 0, 0,
          (),
"Return some information about the version of the running X server.\n\
Return value is a list of the X protocol version, the X protocol\n\
revision, the X server vendor, and the vendor release number.")
#define FUNC_NAME s_X_version_information
{
  return gh_list(SCM_MAKINUM(ProtocolVersion(dpy)),
                 SCM_MAKINUM(ProtocolRevision(dpy)),
                 gh_str02scm(ServerVendor(dpy)),
                 SCM_MAKINUM(VendorRelease(dpy)),
                 SCM_UNDEFINED);
}
#undef FUNC_NAME


/* MS:FIXME:: this should probably be split into multiple procs. Also, the
   visual type should probably be returned as a symbol, not a
   string.*/

SCWM_PROC(X_display_information, "X-display-information", 0, 0, 0,
          (),
"Return some information about the screen. In particular,\n\
return a list of the horizontal resolution, the vertical resolution,\n\
the number of planes on the current screen (i.e. the bit depth), the\n\
bits per color supported by the hardware, the visual class (one of\n\
\"StaticGray\", \"GrayScale\", \"StaticColor\", \"PseudoColor\", \"DirectColor\"\n\
or \"TrueColor\") and a boolean indicating whether the display is color.\n\
The resolutions mentioned above should in theory be pixels per\n\
centimeter, rounded to the nearest integer. These parameters can be\n\
used for various workarounds or conditional decisions in a scwmrc to\n\
be shared among multiple machines.")
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
	  (),
"Return the current user's name.\n\
This is set to one of the following (in order of relevance):\n\
<envar>$USER</envar>, <envar>$LOGNAME</envar>,\n\
the name field of the current uid's entry in the password file,\n\
the constant string \"nobody\".")
#define FUNC_NAME s_user_name
{
  return gh_str02scm(UserName);
}
#undef FUNC_NAME

SCWM_PROC(user_home, "user-home", 0, 0, 0,
	  (),
"Return the current user's home directory.\n\
This is set to one of the following (in order of relevance):\n\
<envar>$HOME</envar>,\n\
the directory field of the current uid's entry in the password file,\n\
the constant string \"/tmp\".")
#define FUNC_NAME s_user_home
{
  return gh_str02scm(UserHome);
}
#undef FUNC_NAME


extern Bool fDoneStartup;

SCWM_PROC(done_startup_p, "done-startup?", 0, 0, 0,
          (),
"Return #t iff Scwm is fully initialized, else #f.\n\
Windows must all be captured and the `startup-hook' must have\n\
already run for this to return #t.")
#define FUNC_NAME s_done_startup_p
{
  return gh_bool2scm(fDoneStartup);
}
#undef FUNC_NAME


SCWM_IPROC(force_segv_for_testing, "force-segv-for-testing", 0, 0, 0,
           (), NULL,
"Cause a segmentation violation.\n\
Do not do this unless you are testing segv handling!")
#define FUNC_NAME s_force_segv_for_testing
{
  int *pn = 0;
  *pn = 0;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(set_reset_on_segv_x, "set-reset-on-segv!", 1, 0, 0,
	  (SCM number_to_reset),
"Reset Scwm to the main event loop on the next NUMBER-TO-RESET segv signals.\n\
The default is 100, but if you catch a segv and and are willing to \n\
track it or send in a bug report, use this to permit a C-level backtrace\n\
by setting it to 0.  See also `set-handle-segv!'.")
#define FUNC_NAME s_set_reset_on_segv_x
{
  extern int segvs_to_reset;
  VALIDATE_ARG_INT_COPY(1,number_to_reset,segvs_to_reset);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(reset_on_segv, "reset-on-segv", 0, 0, 0,
	  (),
"Return the number of segv signals Scwm will reset on.\n\
See `set-reset-on-segv!'.")
#define FUNC_NAME s_reset_on_segv
{
  extern int segvs_to_reset;
  return gh_int2scm(segvs_to_reset);
}
#undef FUNC_NAME



SCWM_PROC(set_handle_segv_x, "set-handle-segv!", 1, 0, 0,
	  (SCM flag),
"If FLAG, tell Scwm to catch segv signals.\n\
This is the default, but if you catch a segv and really need it to \n\
dump right away to figure out what is wrong then use this.  No\n\
cleanup is done if handling segv is #f.  Be sure to have an extra\n\
terminal handy on a console or separate X server.\n\
For developers and hackers only.")
#define FUNC_NAME s_set_handle_segv_x
{
  VALIDATE_ARG_BOOL_COPY(1,flag,fHandleSegv);
  if (fHandleSegv) {
    newsegvhandler(SIGSEGV);
  } else {
    reset_signal_handler(SIGSEGV);
  }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(x_connection_number, "x-connection-number", 0, 0, 0,
          (),
"Return the X connection file descriptor number.")
#define FUNC_NAME s_x_connection_number
{
  int c = ConnectionNumber(dpy);
  return gh_int2scm(c);
}
#undef FUNC_NAME

#ifdef SCWM_TEST_HOOK_PROCS

SCWM_IPROC(scwm_run_test_hook_0, "scwm-run-test-hook-0", 1, 0, 0,
           (SCM count), NULL,
"Invoke `scwm-test-hook-0' COUNT times.")
#define FUNC_NAME s_scwm_run_test_hook_0
{
  int c, i;
  VALIDATE_ARG_INT_COPY(1,count,c);
  for (i=0; i<c; ++i) {
    call0_hooks(scwm_test_hook_0);
  }
  return SCM_UNDEFINED;
}
#undef FUNC_NAME

SCWM_IPROC(scwm_run_test_hook_1, "scwm-run-test-hook-1", 2, 0, 0,
           (SCM count, SCM arg), NULL,
"Invoke `scwm-test-hook-1' COUNT times with ARG as the single argument.")
#define FUNC_NAME s_scwm_run_test_hook_1
{
  int c, i;
  VALIDATE_ARG_INT_COPY(1,count,c);
  for (i=0; i<c; ++i) {
    call1_hooks(scwm_test_hook_1,arg);
  }
  return SCM_UNDEFINED;
}
#undef FUNC_NAME

#endif


SCWM_PROC(get_next_event, "get-next-event", 0, 0, 0,
          (),
"Return a represention of the next key or mouse event.\n\
The return value is (string modmask keycode) for key events\n\
or (string modmask button-number #t). The\n\
`cdr' of the return value can be used as the arguments to \n\
`undo-passive-grab' and `redo-passive-grab'.\n\
The string is usable as a key binding string.  Modifiers \n\
are listed first, separated by \"-\" followed by a \"-\" and the\n\
keysym name.  E.g., \"S-C-M-z\" is Shift+Control+Meta + 'z' key.\n\
If the event is only modifier keys, then the string will\n\
end in a \"-\"; e.g., \"S-C-M-\".\n\
See also `get-mouse-event' and `get-key-event'.")
#define FUNC_NAME s_get_next_event
{
  Bool fAsyncMouse = True;
  Bool fAsyncKeyboard = True;
  XEvent ev;
  XEvent evDiscard;
  Bool fGotPress = False;
  Bool fKey = False;

  XSync(dpy,True);

  if (XGrabKeyboard(dpy, Scr.NoFocusWin, False /* no owner events */, 
                    fAsyncMouse? GrabModeAsync: GrabModeSync, 
                    fAsyncKeyboard? GrabModeAsync: GrabModeSync,
                    CurrentTime) != Success) {
    return SCM_BOOL_F;
  }

  if (XGrabPointer(dpy, Scr.NoFocusWin, False /* no owner events */, 
                   (ButtonPressMask | ButtonReleaseMask),
                   fAsyncMouse? GrabModeAsync: GrabModeSync, 
                   fAsyncKeyboard? GrabModeAsync: GrabModeSync,
                   False,XCURSOR_ICON,
                   CurrentTime) != Success) {
    return SCM_BOOL_F;
  }
    
  while (True) {
    KeySym keysym;
    XWindowEvent(dpy, Scr.NoFocusWin, (ButtonPressMask | KeyPressMask | KeyReleaseMask), &ev);
    if (ev.type == ButtonPress) 
      break;
    keysym = XKeycodeToKeysym(dpy,ev.xkey.keycode,0);
    if (ev.type == KeyPress) {
      fGotPress = True;
      continue;
    }
#ifdef DEBUG_GET_KEY_EVENT
    scwm_msg(WARN,FUNC_NAME,"Got keycode = %d, keysym = %d",
             ev.xkey.keycode,keysym);
#endif
    /* GJB:FIXME:: is this portable? Want to not list modifier
       keys as keysym strings */
    if (!(keysym >= XK_Shift_L && keysym <= XK_Hyper_R)
        &&(ev.type == KeyRelease && fGotPress)) {
      fKey = True;
      break; /* got a real key, not a modifier, so exit the loop */
    }
  }

  /* GJB:FIXME:: race? */
  while (XCheckWindowEvent(dpy, Scr.NoFocusWin, 
                           ButtonPressMask|ButtonReleaseMask|KeyReleaseMask|KeyPressMask, 
                           &evDiscard)) {
    scwm_msg(WARN,FUNC_NAME,"Discarding with state %d",evDiscard.xbutton.state);
  }

  XUngrabKeyboard(dpy, CurrentTime);
  XUngrabPointer(dpy, CurrentTime);

  if (fKey) {
    char *sz = SzNewForModMaskKeyCode(ev.xkey.state,
                                      ev.xkey.keycode);
    SCM answer = gh_str02scm(sz);
    FREE(sz);
    return gh_list(answer,gh_int2scm(ev.xkey.state),
                   gh_int2scm(ev.xkey.keycode), SCM_UNDEFINED);
  } else {
    /* mouse button */
    char *sz = SzNewModifierStringForModMask(ev.xbutton.state);
    char *szFull = NEWC(strlen(sz)+4,char);
    SCM answer;
    sprintf(szFull,"%sButton%d",sz,ev.xbutton.button);
    answer = gh_str02scm(szFull);
    FREE(sz);
    FREE(szFull);
    return gh_list(answer,gh_int2scm(ev.xbutton.state),
                   gh_int2scm(ev.xbutton.button),SCM_BOOL_T,
                   SCM_UNDEFINED);
  }
}
#undef FUNC_NAME


SCWM_PROC(get_key_event, "get-key-event", 0, 0, 0,
          (),
"Return a represention of the next key event.\n\
The return value is (string modmask keycode). The\n\
`cdr' of the return value can be used as the arguments to \n\
`undo-passive-grab' and `redo-passive-grab'.\n\
The string is usable as a key binding string.  Modifiers \n\
are listed first, separated by \"-\" followed by a \"-\" and the\n\
keysym name.  E.g., \"S-C-M-z\" is Shift+Control+Meta + 'z' key.\n\
If the event is only modifier keys, then the string will\n\
end in a \"-\"; e.g., \"S-C-M-\".  See also `get-next-event'.")
#define FUNC_NAME s_get_key_event
{
  Bool fAsyncMouse = False;
  Bool fAsyncKeyboard = True;
  XEvent ev;
  XEvent evDiscard;
  Bool fGotPress = False;

  XSync(dpy,True);

  if (XGrabKeyboard(dpy, Scr.NoFocusWin, False /* no owner events */, 
                    fAsyncMouse? GrabModeAsync: GrabModeSync, 
                    fAsyncKeyboard? GrabModeAsync: GrabModeSync,
                    CurrentTime) != Success) {
    return SCM_BOOL_F;
  }
    
  while (True) {
    KeySym keysym;
    XWindowEvent(dpy, Scr.NoFocusWin, KeyPressMask | KeyReleaseMask, &ev);
    keysym = XKeycodeToKeysym(dpy,ev.xkey.keycode,0);
    if (ev.type == KeyPress) {
      fGotPress = True;
      continue;
    }
#ifdef DEBUG_GET_KEY_EVENT
    scwm_msg(WARN,FUNC_NAME,"Got keycode = %d, keysym = %d",
             ev.xkey.keycode,keysym);
#endif
    /* GJB:FIXME:: is this portable? Want to not list modifier
       keys as keysym strings */
    if (!(keysym >= XK_Shift_L && keysym <= XK_Hyper_R)
        &&(ev.type == KeyRelease && fGotPress))
      break; /* got a real key, not a modifier, so exit the loop */
  }

  /* GJB:FIXME:: race? */
  while (XCheckWindowEvent(dpy, Scr.NoFocusWin, KeyReleaseMask|KeyPressMask, &evDiscard)) {
    scwm_msg(WARN,FUNC_NAME,"Discarding with state %d",evDiscard.xbutton.state);
  }

  XUngrabKeyboard(dpy, CurrentTime);

  { /* scope */
    char *sz = SzNewForModMaskKeyCode(ev.xkey.state,
                                      ev.xkey.keycode);
    SCM answer = gh_str02scm(sz);
    FREE(sz);
    return gh_list(answer,gh_int2scm(ev.xkey.state),
                   gh_int2scm(ev.xkey.keycode), SCM_UNDEFINED);
  }
}
#undef FUNC_NAME

SCWM_PROC(get_mouse_event, "get-mouse-event", 0, 0, 0,
          (),
"Return a represention of the next mouse event.\n\
The return value is (string modmask button-number #t).  The\n\
`cdr' of the return value can be used as the arguments to \n\
`undo-passive-grab' and `redo-passive-grab'.\n\
The string is usable as a mouse binding string.  Modifiers \n\
are listed first, separated by \"-\" followed by a \"-\" and the\n\
button number.  E.g., \"S-C-M-1\" is Shift+Control+Meta + button 1.\n\
See also `get-next-event'.")
#define FUNC_NAME s_get_mouse_event
{
  Bool fAsyncMouse = True;
  Bool fAsyncKeyboard = True;
  XEvent ev;
  XEvent evDiscard;

  XSync(dpy,True);
  
  if (XGrabPointer(dpy, Scr.NoFocusWin, False /* no owner events */, 
                   (ButtonPressMask | ButtonReleaseMask),
                   fAsyncMouse? GrabModeAsync: GrabModeSync, 
                   fAsyncKeyboard? GrabModeAsync: GrabModeSync,
                   False,XCURSOR_ICON,
                   CurrentTime) != Success) {
    return SCM_BOOL_F;
  }
    
  while (True) {
    XWindowEvent(dpy, Scr.NoFocusWin, 
                 ButtonPressMask | KeyPressMask | KeyReleaseMask, &ev);
    if (ev.type == ButtonPress)
      break;
  }

  while (XCheckWindowEvent(dpy, Scr.NoFocusWin, 
                           ButtonPressMask|ButtonReleaseMask, &evDiscard)) {
    scwm_msg(WARN,FUNC_NAME,"Discarding with state %d",evDiscard.xbutton.state);
  }

  XUngrabPointer(dpy, CurrentTime);

  { /* scope */
    char *sz = SzNewModifierStringForModMask(ev.xbutton.state);
    char *szFull = NEWC(strlen(sz)+4,char);
    SCM answer;
    sprintf(szFull,"%sButton%d",sz,ev.xbutton.button);
    answer = gh_str02scm(szFull);
    FREE(sz);
    FREE(szFull);
    return gh_list(answer,gh_int2scm(ev.xbutton.state),
                   gh_int2scm(ev.xbutton.button),SCM_BOOL_T,
                   SCM_UNDEFINED);
  }
}
#undef FUNC_NAME


SCWM_PROC(X_fetch_bytes, "X-fetch-bytes", 0, 0, 0,
	  (),
"Returns a string representing the value of the cut buffer.\n\
XFetchBytes is called.")
#define FUNC_NAME s_X_fetch_bytes
{
  char *str;
  int len;
  SCM result;

  str = XFetchBytes(dpy, &len);
  if (str == NULL)
    return SCM_BOOL_F;

  /* EJB:FIXME:: XStoreBytes allows embedded NULLs.  Does this? Do we care? */
  result = gh_str02scm(str);
  XFree(str);
  return result;
}
#undef FUNC_NAME

SCWM_PROC(X_store_bytes, "X-store-bytes", 1, 0, 0,
	  (SCM string),
"Set the cut buffer to STRING by calling XStoreBytes.")
#define FUNC_NAME s_X_store_bytes
{
  char *sz;

  VALIDATE_ARG_STR_NEWCOPY(1,string,sz);
  sz=gh_scm2newstr(string, NULL);
  /* EJB:FIXME:: XStoreBytes allows embedded NULLs.  This doesn't.
   * Do we care? */
  XStoreBytes(dpy, sz, strlen(sz));
  gh_free(sz);
  XSetSelectionOwner(dpy, XA_PRIMARY, None, CurrentTime);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* GJB:FIXME:  Need to link with Intrinsics
   to get XtGetSelectionValue;  maybe not worth it */
#if 0
SCWM_PROC(X_get_primary_selection, "X-get-primary-selection", 0, 0, 0,
	  (),
"Return the X primary selection.")
#define FUNC_NAME s_X_get_primary_selection
{
  
}
#undef FUNC_NAME

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
/* vim:ts=8:sw=2:sta 
 */

