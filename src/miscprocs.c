/* $Id$
 * miscprocs.c
 * (C) 1998 Maciej Stachowiak and Greg J. Badros
 *
 * This module has been significantly modified from fvwm2
 * It may be used under the terms of the fvwm copyright (see COPYING.FVWM).
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <X11/Xlib.h>
#include <unistd.h>
#include <guile/gh.h>
#include <signal.h>
#include "scwm.h"
#include "screen.h"
#include "errors.h"
#include "events.h"
#include "util.h"
#include "decor.h"
#include "font.h"
#include "xmisc.h"

extern SCM sym_center, sym_left, sym_right, sym_mouse;
extern Bool Restarting, PPosOverride;

SCM sym_focus;

/* GJBFIX: Maybe the menus should do something w/ this? */
SCWM_PROC(set_menu_mwm_style_x, "set-menu-mwm-style!", 0, 1, 0,
          (SCM flag))
     /** Set the menu mwm style according to the boolean FLAG. This
         option is currently ignored. */
#define FUNC_NAME s_set_menu_mwm_style_x
{
  SCM_REDEFER_INTS;
  if (SCM_IMP(flag)) {
    if (flag == SCM_BOOL_T || flag == SCM_UNDEFINED) {
      Scr.flags |= MWMMenus;
      SCM_REALLOW_INTS;
      return (SCM_BOOL_T);
    } else if (flag == SCM_BOOL_F) {
      Scr.flags &= ~MWMMenus;
      SCM_REALLOW_INTS;
      return (SCM_BOOL_F);
    }
  }
  SCM_ALLOW_INTS;
  scm_wrong_type_arg(FUNC_NAME, 1, flag);
}
#undef FUNC_NAME


/* MSFIX: Can't easily be a color w/o overlay planes-- needs to be really 
   fast to erase */
SCWM_PROC(set_rubber_band_mask_x, "set-rubber-band-mask!", 1, 0, 0,
          (SCM value))
     /** Set the rubber band mask, the value XORed with the background
when dragging non-opaque move or resize frames to VALUE. VALUE should
be an integer. */
#define FUNC_NAME s_set_rubber_band_mask_x
{
  XGCValues gcv;
  unsigned long gcm;

  SCM_REDEFER_INTS;

  if (!gh_number_p(value)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg(FUNC_NAME, 1, value);
  }
  gcm = GCFunction | GCLineWidth | GCForeground | GCSubwindowMode;
  gcv.function = GXxor;
  gcv.line_width = 0;
  gcv.foreground = gh_scm2long(value);
  gcv.subwindow_mode = IncludeInferiors;
  if (NULL != Scr.DrawGC) {
    XFreeGC(dpy, Scr.DrawGC);
  }
  Scr.DrawGC = XCreateGC(dpy, Scr.Root, gcm, &gcv);
  SCM_REALLOW_INTS;
  return (value);
}
#undef FUNC_NAME


SCWM_PROC(set_title_justify_x,"set-title-justify!", 1, 0, 0,
          (SCM just))
     /** Set the justification used for the title in the current decor
to JUST. JUST should be one of 'right, 'left, or 'center. */
#define FUNC_NAME s_set_title_justify_x
{
  ScwmDecor *fl;

  SCM_REDEFER_INTS;

  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  if (!gh_symbol_p(just)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg(FUNC_NAME, 1, just);
  }
  if (gh_eq_p(just, sym_center)) {
    fl->titlebar.flags &= ~HOffCenter;
  } else if (gh_eq_p(just, sym_left)) {
    fl->titlebar.flags |= HOffCenter;
    fl->titlebar.flags &= ~HRight;
  } else if (gh_eq_p(just, sym_right)) {
    fl->titlebar.flags |= HOffCenter | HRight;
  } else {
    scwm_error(FUNC_NAME, 5);
  }
  /* XXX should redraw the title bars */
  redraw_titlebars(fl, 0);
  SCM_REALLOW_INTS;
  return (just);
}
#undef FUNC_NAME


SCWM_PROC(set_title_height_x, "set-title-height!", 1, 0, 0,
          (SCM height))
     /** Set the height of the titlebar in pixels to HEIGHT in the
current decor. */
#define FUNC_NAME s_set_title_height_x
{
  int th, extra_height;
  ScwmDecor *fl;

  SCM_REDEFER_INTS;
  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  if (!gh_number_p(height)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg(FUNC_NAME, 1, height);
  }
  th = gh_scm2int(height);
  if (th <= 4 || th > 256) {
    SCM_ALLOW_INTS;
    scwm_error(FUNC_NAME, 7);
  }
  extra_height = fl->TitleHeight;
  fl->TitleHeight = th;


  extra_height -= fl->TitleHeight;


  fl->window_font_y = FONTY(fl->window_font)
    + (th - (FONTHEIGHT(fl->window_font) + 3)) / 2;
  if (fl->window_font_y < FONTY(fl->window_font))
    fl->window_font_y = FONTY(fl->window_font);

  redraw_titlebars(fl, extra_height);

  SCM_REALLOW_INTS;
  return (height);
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
     /** Returns true when the windows are being caputured, either
during initial startup, or during a recapture operation. In either
case, placement procedures should probably avoid interaction and
perhaps avoid moving the window being placed at all. */
#define FUNC_NAME s_capturing_p
{
  return SCM_BOOL_FromBool(PPosOverride);
}
#undef FUNC_NAME


SCWM_PROC(refresh, "refresh", 0, 0, 0,
          ())
     /** Make sure all decorations for all windows are up to date. In theory,
this should not be needed. */
#define FUNC_NAME s_refresh
{
  refresh_common(Scr.Root);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(set_click_time_x, "set-click-time!", 1, 0, 0,
          (SCM ctime))
     /** Set the delay before a mouse-down as considered a drag, and
before a single click is definitively identified as not a double
click, to CTIME microseconds. */
#define FUNC_NAME s_set_click_time_x
{
  SCM_REDEFER_INTS;
  if (!gh_number_p(ctime)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg(FUNC_NAME, 1, ctime);
  }
  Scr.ClickTime = gh_scm2long(ctime);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(set_colormap_focus_x, "set-colormap-focus!", 1, 0, 0,
          (SCM ftype))
     /** Set the colormap focus policy to FTYPE. FTYPE can either be
'mouse, indicating that the window under the mouse pointer should
always have it's colormap installed, or 'focus to indicate that the
window with the input focus should also get the colormap focus. This
makes a difference onl when using focus policies other than 'mouse. */
#define FUNC_NAME s_set_colormap_focus_x
{
  SCM_REDEFER_INTS;
  if (!gh_symbol_p(ftype)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg(FUNC_NAME, 1, ftype);
  }
  if (gh_eq_p(ftype, sym_focus)) {
    Scr.ColormapFocus = COLORMAP_FOLLOWS_FOCUS;
  } else if (gh_eq_p(ftype, sym_mouse)) {
    Scr.ColormapFocus = COLORMAP_FOLLOWS_MOUSE;
  } else {
    SCM_ALLOW_INTS;
    scwm_error(FUNC_NAME, 10);
  }
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(set_opaque_move_size_x, "set-opaque-move-size!", 1, 0, 0,
          (SCM size))
     /** Set the opaque move size limit to SIZE, which is given as a
percentage of the screen area. If the area of a window is greater than
this percentage, the window will be moved with a rubber band
instead. (FIXGJB: this should be determined more flexibly) */
#define FUNC_NAME s_set_opaque_move_size_x
{
  SCM_REDEFER_INTS;
  if (!gh_number_p(size)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg(FUNC_NAME, 1, size);
  }
  Scr.OpaqueSize = gh_scm2long(size);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(pointer_position, "pointer-position", 0, 0, 0,
          ())
     /** Return the current position of the mouse pointer in pixels.
The return value is a two-element list of the x and y coordinates. */
#define FUNC_NAME s_pointer_position
{
  int x, y;

  SCM_REDEFER_INTS;
  FXGetPointerWindowOffsets(Scr.Root, &x, &y);
  SCM_REALLOW_INTS;
  return scm_listify(SCM_MAKINUM(x), SCM_MAKINUM(y), SCM_UNDEFINED);
}
#undef FUNC_NAME


SCWM_PROC(move_pointer_to, "move-pointer-to", 2, 0, 0,
          (SCM sx, SCM sy))
     /** Move the mouse pointer to SX, SY (given in pixels). */
#define FUNC_NAME s_move_pointer_to
{
  int x, y;

  SCM_REDEFER_INTS;

  if (!gh_number_p(sx)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg(FUNC_NAME, 1, sx);
  }
  if (!gh_number_p(sy)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg(FUNC_NAME, 2, sy);
  }
  x = gh_scm2int(sx);
  y = gh_scm2int(sy);
  XWarpPointer(dpy, Scr.Root, Scr.Root, 0, 0, Scr.MyDisplayWidth,
	       Scr.MyDisplayHeight, x, y);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(recapture, "recapture", 0, 0, 0,
          ())
     /** Recapture all the windows, in other words, destroy all the
current frame windows and recreate them from scratch. This is
hopefully not necessary during normal operation. */
#define FUNC_NAME s_recapture
{
  SCM_REDEFER_INTS;
  BlackoutScreen();		/* if they want to hide the recapture */
  CaptureAllWindows();
  UnBlackoutScreen();
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(wait_for_window, "wait-for-window", 1, 0, 0,
          (SCM predicate))
     /** Wait until a window appears which satisfies PREDICATE. Given
the existence of before-new-window-hook, this is of questionable
usefulness. */
#define FUNC_NAME s_wait_for_window
{
  Bool done = False;
  extern ScwmWindow *pswCurrent;

  if (!gh_procedure_p(predicate)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg(FUNC_NAME, 1, predicate);
  }
  while (!done) {
    if (XNextEvent_orTimeout(dpy, &Event)) {
      SCM_DEFER_INTS;
      DispatchEvent();
      SCM_ALLOW_INTS;
      if (Event.type == MapNotify) {
	if (gh_call1(predicate, pswCurrent->schwin) == SCM_BOOL_T) {
	  done = True;
	}
      }
    }
  }
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
     /** Determine wether or not clever-place-window will be used in
place of smart-place-window when the default placement procedure is
used, and the window's smart-placement flag is on, according to the
boolean value FLAG. */
#define FUNC_NAME s_set_smart_placement_is_really_smart_x
{
  if (!gh_boolean_p(flag)) {
    scm_wrong_type_arg(FUNC_NAME,1,flag);
  }
  Scr.SmartPlacementIsClever= SCM_NFALSEP(flag) ? True : False;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* FIXMS - the functionality related to the next three procedures
   should be implemented by adding new event bindings eventually */

SCWM_PROC(set_click_to_focus_passes_click_x, "set-click-to-focus-passes-click!", 1, 0, 0,
          (SCM flag))
     /** Determine wether or not a click-to-focus window will actually
receive the click event that causes it to gain focus, according to the
boolean value FLAG. */
#define FUNC_NAME s_set_click_to_focus_passes_click_x
{
  if (!gh_boolean_p(flag)) {
    scm_wrong_type_arg(FUNC_NAME,1,flag);
  }
  Scr.ClickToFocusPassesClick= SCM_NFALSEP(flag) ? True : False;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* FIXMS: remove this one in particular once window-focus-hook exists. */

SCWM_PROC(set_click_to_focus_raises_x, "set-click-to-focus-raises!", 1, 0, 0,
          (SCM flag))
     /** Determine whether a click to focus raises the clicked-on window
accoring to the boolean value FLAG.*/
#define FUNC_NAME s_set_click_to_focus_raises_x
{
  if (!gh_boolean_p(flag)) {
    scm_wrong_type_arg(FUNC_NAME,1,flag);
  }
  Scr.ClickToFocusRaises =  SCM_NFALSEP(flag) ? True : False;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* FIXMS - this seems to be a pretty useless idea, or at least there
   must be a better way of implementing it. */

/* FIXMS - looking at it again, this idea seems utterly
   bogus. Couldn't you just (bind-mouse 'all 1 raise-window) and get
   the exact same effect?  Examine further... */

SCWM_PROC(set_mouse_focus_click_raises_x, "set-mouse-focus-click-raises!", 1, 0, 0,
          (SCM flag))
     /** Determine wether or not a mouse-focus window will always be
raised by a click on the frame according to the boolean value FLAG.*/
#define FUNC_NAME s_set_mouse_focus_click_raises_x
{
  if (!gh_boolean_p(flag)) {
    scm_wrong_type_arg(FUNC_NAME,1,flag);
  }
  Scr.MouseFocusClickRaises= SCM_NFALSEP(flag) ? True : False;
  return SCM_UNSPECIFIED;  
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

/* FIXMS: this should probably be split into multiple procs. */
SCWM_PROC(X_version_information, "X-version-information", 0, 0, 0,
          ())
     /** Return some information about the version of the running X
server.  In particular, return a list of the X protocol version, the X
protocol revision, the X server vendor, and the vendor release
number. */
#define FUNC_NAME s_X_version_information
{
  return scm_listify(SCM_MAKINUM(ProtocolVersion(dpy)),
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
bits per color supported by the hadware, the visual class (one of
"StaticGray", "GrayScale", "StaticColor", "PseudoColor", "DirectColor"
or "TrueColor") and a boolean indicating wether the display is color.
The resolutions mentioned above should in theory be pixels per
centimeter, rounded to the nearest integer. These parameters can be
used for various workarounds or conditional decisions in a scwmrc to
be shared anmong multiple machines. */
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

  return scm_listify(SCM_MAKINUM(xres),
		     SCM_MAKINUM(yres),
		     SCM_MAKINUM(planes),
		     SCM_MAKINUM(bits_per_rgb),
		     gh_str02scm(vc), /* class */
		     SCM_BOOL_FromBool(fColor),
		     SCM_UNDEFINED);
}
#undef FUNC_NAME

void 
init_miscprocs()
{
  sym_focus = gh_symbol2scm("focus");
  scm_protect_object(sym_focus);

#ifndef SCM_MAGIC_SNARFER
#include "miscprocs.x"
#endif
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
