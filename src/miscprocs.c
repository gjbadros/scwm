/* $Id$
 * miscprocs.c
 * (C) 1998 Maciej Stachowiak and Greg J. Badros
 *
 * This module has been significantly modified from fvwm2
 * It may be used under the terms of the fvwm copyright (see COPYING.FVWM).
 */

#include <X11/Xlib.h>
#include <unistd.h>
#include <guile/gh.h>
#include <signal.h>
#include <config.h>
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

SCWM_PROC(set_menu_mwm_style, "set-menu-mwm-style!", 0, 1, 0,
          (SCM should))
{
  SCM_REDEFER_INTS;
  if (SCM_IMP(should)) {
    if (should == SCM_BOOL_T || should == SCM_UNDEFINED) {
      Scr.flags |= MWMMenus;
      SCM_REALLOW_INTS;
      return (SCM_BOOL_T);
    } else if (should == SCM_BOOL_F) {
      Scr.flags &= ~MWMMenus;
      SCM_REALLOW_INTS;
      return (SCM_BOOL_F);
    }
  }
  SCM_ALLOW_INTS;
  scm_wrong_type_arg("set-mwm-menu-style!", 1, should);
}


SCWM_PROC(set_rubber_band_mask_x, "set-rubber-band-mask!", 1, 0, 0,
          (SCM value))
{
  XGCValues gcv;
  unsigned long gcm;

  SCM_REDEFER_INTS;

  if (!gh_number_p(value)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("set-rubber-band-mask!", 1, value);
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


SCWM_PROC(set_title_justify,"set-title-justify!", 1, 0, 0,
          (SCM just))
{
  ScwmDecor *fl;

  SCM_REDEFER_INTS;

  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  if (!gh_symbol_p(just)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("set-title-justify!", 1, just);
  }
  if (gh_eq_p(just, sym_center)) {
    fl->titlebar.flags &= ~HOffCenter;
  } else if (gh_eq_p(just, sym_left)) {
    fl->titlebar.flags |= HOffCenter;
    fl->titlebar.flags &= ~HRight;
  } else if (gh_eq_p(just, sym_right)) {
    fl->titlebar.flags |= HOffCenter | HRight;
  } else {
    scwm_error("set-title-justify!", 5);
  }
  /* XXX should redraw the title bars */
  redraw_titlebars(fl, 0);
  SCM_REALLOW_INTS;
  return (just);
}


SCWM_PROC(set_title_height, "set-title-height!", 1, 0, 0,
          (SCM height))
{
  int th, extra_height;
  ScwmDecor *fl;

  SCM_REDEFER_INTS;
  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  if (!gh_number_p(height)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("set-title-height!", 1, height);
  }
  th = gh_scm2int(height);
  if (th <= 4 || th > 256) {
    SCM_ALLOW_INTS;
    scwm_error("set-title-height!", 7);
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

SCWM_PROC(restarted_p, "restarted?", 0, 0, 0,
          ())
{
  return SCM_BOOL_FromBool(Restarting);
}

SCWM_PROC(capturing_p, "capturing?", 0, 0, 0,
          ())
     /** Returns true when the windows are being caputured, either
	 during initial startup, or during a recapture operation. In
	 either case, placement procedures should probably avoid
	 interaction and perhaps avoid moving the window being placed
	 at all. */
{
  return SCM_BOOL_FromBool(PPosOverride);
}


SCWM_PROC(refresh, "refresh", 0, 0, 0,
          ())
{
  refresh_common(Scr.Root);
  return SCM_UNSPECIFIED;
}


SCWM_PROC(set_click_time_x, "set-click-time!", 1, 0, 0,
          (SCM ctime))
{
  SCM_REDEFER_INTS;
  if (!gh_number_p(ctime)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("set-click-time!", 1, ctime);
  }
  Scr.ClickTime = gh_scm2long(ctime);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}


SCWM_PROC(set_colormap_focus_x, "set-colormap-focus!", 1, 0, 0,
          (SCM ftype))
{
  SCM_REDEFER_INTS;
  if (!gh_symbol_p(ftype)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("set-colormap-focus!", 1, ftype);
  }
  if (gh_eq_p(ftype, sym_focus)) {
    Scr.ColormapFocus = COLORMAP_FOLLOWS_FOCUS;
  } else if (gh_eq_p(ftype, sym_mouse)) {
    Scr.ColormapFocus = COLORMAP_FOLLOWS_MOUSE;
  } else {
    SCM_ALLOW_INTS;
    scwm_error("set-click-time!", 10);
  }
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}


SCWM_PROC(set_opaque_move_size_x, "set-opaque-move-size!", 1, 0, 0,
          (SCM size))
{
  SCM_REDEFER_INTS;
  if (!gh_number_p(size)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("set-opaque-move-size!", 1, size);
  }
  Scr.OpaqueSize = gh_scm2long(size);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}


SCWM_PROC(pointer_position, "pointer-position", 0, 0, 0,
          ())
{
  int x, y;

  SCM_REDEFER_INTS;
  XGetPointerWindowOffsets(Scr.Root, &x, &y);
  SCM_REALLOW_INTS;
  return scm_listify(SCM_MAKINUM(x), SCM_MAKINUM(y), SCM_UNDEFINED);
}


SCWM_PROC(move_pointer_to, "move-pointer-to", 2, 0, 0,
          (SCM sx, SCM sy))
{
  int x, y;

  SCM_REDEFER_INTS;

  if (!gh_number_p(sx)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("move-pointer-to", 1, sx);
  }
  if (!gh_number_p(sy)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("move-pointer-to", 2, sy);
  }
  x = gh_scm2int(sx);
  y = gh_scm2int(sy);
  XWarpPointer(dpy, Scr.Root, Scr.Root, 0, 0, Scr.MyDisplayWidth,
	       Scr.MyDisplayHeight, x, y);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}


SCWM_PROC(recapture, "recapture", 0, 0, 0,
          ())
{
  SCM_REDEFER_INTS;
  BlackoutScreen();		/* if they want to hide the recapture */
  CaptureAllWindows();
  UnBlackoutScreen();
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}


SCWM_PROC(wait_for_window, "wait-for-window", 1, 0, 0,
          (SCM predicate))
{
  Bool done = False;
  extern ScwmWindow *pswCurrent;

  if (!gh_procedure_p(predicate)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("wait-for-window", 1, predicate);
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


SCWM_PROC(beep, "beep", 0, 0, 0,
          ())
{
  XBell(dpy, 0);
  return SCM_UNSPECIFIED;
}


SCWM_PROC(set_smart_placement_is_really_smart_x, "set-smart-placement-is-really-smart!",1, 0, 0,
          (SCM val))
{
  if (!gh_boolean_p(val)) {
    scm_wrong_type_arg("set-smart-placement-is-really-smart!",1,val);
  }
  Scr.SmartPlacementIsClever= SCM_NFALSEP(val) ? True : False;
  return SCM_UNSPECIFIED;
}

/* FIXMS - the functionality related to the next three procedures
   should be implemented by adding new event bindings eventually */

SCWM_PROC(set_click_to_focus_passes_click_x, "set-click-to-focus-passes-click!", 1, 0, 0,
          (SCM val))
{
  if (!gh_boolean_p(val)) {
    scm_wrong_type_arg("set-click-to-focus-passes-click!",1,val);
  }
  Scr.ClickToFocusPassesClick= SCM_NFALSEP(val) ? True : False;
  return SCM_UNSPECIFIED;
}


SCWM_PROC(set_click_to_focus_raises_x, "set-click-to-focus-raises!", 1, 0, 0,
          (SCM val))
{
  if (!gh_boolean_p(val)) {
    scm_wrong_type_arg("set-click-to-focus-raises!",1,val);
  }
  Scr.ClickToFocusRaises=  SCM_NFALSEP(val) ? True : False;
  return SCM_UNSPECIFIED;
}

/* FIXMS - this seems to be a pretty useless idea, or at least there
   must be a better way of implementing it. */


SCWM_PROC(set_mouse_focus_click_raises_x, "set-mouse-focus-click-raises!", 1, 0, 0,
          (SCM val))
{
  if (!gh_boolean_p(val)) {
    scm_wrong_type_arg("set-mouse-focus-click-raises!",1,val);
  }
  Scr.MouseFocusClickRaises= SCM_NFALSEP(val) ? True : False;
  return SCM_UNSPECIFIED;  
}


SCWM_PROC(scwm_version, "scwm-version", 0, 0, 0,
          ())
{
  return gh_str02scm(VERSION);
}


SCWM_PROC(x_version_information, "X-version-information", 0, 0, 0,
          ())
{
  return scm_listify(SCM_MAKINUM(ProtocolVersion(dpy)),
		     SCM_MAKINUM(ProtocolRevision(dpy)),
		     gh_str02scm(ServerVendor(dpy)),
		     SCM_MAKINUM(VendorRelease(dpy)),
		     SCM_UNDEFINED);
}


SCWM_PROC(x_display_information, "X-display-information", 0, 0, 0,
          ())
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
