/****************************************************************************
 * This module has been significantly modified by Maciej Stachowiak.
 * It may be used under the terms of the fvwm copyright (see COPYING.FVWM).
 * Changes Copyright 1997, Maciej stachowiak
 ****************************************************************************/

#include <unistd.h>
#include <guile/gh.h>
#include <signal.h>
#include <config.h>
#include "scwm.h"
#include "screen.h"
#include "errors.h"
#include "util.h"
#include "decor.h"
#include "font.h"
#include "../version.h"

extern SCM sym_center, sym_left, sym_right, sym_mouse;

SCM 
set_menu_mwm_style(SCM should)
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

SCM 
set_rubber_band_mask_x(SCM value)
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

SCM 
set_title_justify(SCM just)
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

SCM 
set_title_height(SCM height)
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





SCM sym_focus;

void 
init_miscprocs()
{
  sym_focus = gh_symbol2scm("focus");
  scm_protect_object(sym_focus);
}

SCM 
refresh()
{
  refresh_common(Scr.Root);
  return SCM_UNSPECIFIED;
}


SCM 
set_click_time_x(SCM ctime)
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

SCM 
set_colormap_focus_x(SCM ftype)
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

SCM 
set_opaque_move_size_x(SCM size)
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

SCM 
scwm_quit(SCM args)
{
  SCM_REDEFER_INTS;
  if (master_pid != getpid())
    kill(master_pid, SIGTERM);
  Done(0, NULL);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;	/* you never know... */
}

SCM 
pointer_position()
{
  int x, y;

  SCM_REDEFER_INTS;
  XQueryPointer(dpy, Scr.Root, &JunkRoot, &JunkChild,
		&x, &y, &JunkX, &JunkY, &JunkMask);
  SCM_REALLOW_INTS;
  return scm_listify(SCM_MAKINUM(x), SCM_MAKINUM(y), SCM_UNDEFINED);
}

SCM 
move_pointer_to(SCM sx, SCM sy)
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


SCM 
recapture()
{
  SCM_REDEFER_INTS;
  BlackoutScreen();		/* if they want to hide the recapture */
  CaptureAllWindows();
  UnBlackoutScreen();
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}

SCM 
restart(SCM command)
{
  int dummy;
  char *n;

  SCM_REDEFER_INTS;
  if (!gh_string_p(command)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("restart", 1, command);
  }
  n = gh_scm2newstr(command, &dummy);
  Done(1, n);
  free(n);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;	/* you never know... */
}


SCM 
wait_for_window(SCM predicate)
{
  Bool done = False;
  extern ScwmWindow *swCurrent;

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
	if (gh_call1(predicate, swCurrent->schwin) == SCM_BOOL_T) {
	  done = True;
	}
      }
    }
  }
  return SCM_UNSPECIFIED;
}

SCM 
beep()
{
  XBell(dpy, 0);
  return SCM_UNSPECIFIED;
}

SCM
set_smart_placement_is_really_smart_x(SCM val)
{
  if (!gh_boolean_p(val)) {
    scm_wrong_type_arg("set-smart-placement-is-really-smart!",1,val);
  }
  Scr.SmartPlacementIsClever= SCM_NFALSEP(val) ? True : False;
  return SCM_UNSPECIFIED;
}

/* FIXMS - the functionality related to the next three procedures
   should be implemented by adding new event bindings eventually */

SCM
set_click_to_focus_passes_click_x(SCM val)
{
  if (!gh_boolean_p(val)) {
    scm_wrong_type_arg("set-click-to-focus-passes-click!",1,val);
  }
  Scr.ClickToFocusPassesClick= SCM_NFALSEP(val) ? True : False;
  return SCM_UNSPECIFIED;
}

SCM
set_click_to_focus_raises_x(SCM val)
{
  if (!gh_boolean_p(val)) {
    scm_wrong_type_arg("set-click-to-focus-raises!",1,val);
  }
  Scr.ClickToFocusRaises= SCM_NFALSEP(val) ? True : False;
  return SCM_UNSPECIFIED;
}

/* FIXMS - this seems to be a pretty useless idea, or at least there
   must be a better way of implementing it. */
SCM
set_mouse_focus_click_raises_x(SCM val)
{
  if (!gh_boolean_p(val)) {
    scm_wrong_type_arg("set-mouse-focus-click-raises!",1,val);
  }
  Scr.MouseFocusClickRaises= SCM_NFALSEP(val) ? True : False;
  return SCM_UNSPECIFIED;  
}

SCM
scwm_version ()
{
  return gh_str02scm(VERSION);
}




/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
