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

#ifdef USEDECOR
extern ScwmDecor *last_decor, *cur_decor;

#endif

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
set_xor_value(SCM value)
{
  XGCValues gcv;
  unsigned long gcm;

  SCM_REDEFER_INTS;

  if (!gh_number_p(value)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("set_xor_value", 1, value);
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

SCM sym_left;
SCM sym_right;
SCM sym_center;


SCM 
set_title_justify(SCM just)
{
  ScwmDecor *fl;

  SCM_REDEFER_INTS;

#ifdef USEDECOR
  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;
#else
  fl = &Scr.DefaultDecor;
#endif

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
#ifdef USEDECOR
  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;
#else
  fl = &Scr.DefaultDecor;
#endif

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


  fl->WindowFont.y = fl->WindowFont.font->ascent
    + (th - (fl->WindowFont.font->ascent
	     + fl->WindowFont.font->descent + 3)) / 2;
  if (fl->WindowFont.y < fl->WindowFont.font->ascent)
    fl->WindowFont.y = fl->WindowFont.font->ascent;

  redraw_titlebars(fl, extra_height);

  SCM_REALLOW_INTS;
  return (height);
}



#if 0

void 
SetTitleStyle(XEvent * eventp, Window w, ScwmWindow * tmp_win,
	      unsigned long context, char *action, int *Module)
{
  char *parm = NULL, *prev = action;

#ifdef USEDECOR
  ScwmDecor *fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

#else
  ScwmDecor *fl = &Scr.DefaultDecor;

#endif

  action = GetNextToken(action, &parm);
  while (parm && parm[0] != '\0') {
    if (mystrncasecmp(parm, "centered", 8) == 0) {
      fl->titlebar.flags &= ~HOffCenter;
    } else if (mystrncasecmp(parm, "leftjustified", 13) == 0) {
      fl->titlebar.flags |= HOffCenter;
      fl->titlebar.flags &= ~HRight;
    } else if (mystrncasecmp(parm, "rightjustified", 14) == 0) {
      fl->titlebar.flags |= HOffCenter | HRight;
    }
#ifdef EXTENDED_TITLESTYLE
    else if (mystrncasecmp(parm, "height", 6) == 0) {
      int height, next;

      if (sscanf(action, "%d%n", &height, &next) > 0
	  && height > 4
	  && height <= 256) {
	int x, y, w, h, extra_height;
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
	while (tmp != NULL) {
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
	  h = tmp->frame_height - extra_height;
	  tmp->frame_x = 0;
	  tmp->frame_y = 0;
	  tmp->frame_height = 0;
	  tmp->frame_width = 0;
	  SetupFrame(tmp, x, y, w, h, True);
	  SetTitleBar(tmp, True, True);
	  SetTitleBar(tmp, False, True);
	  tmp = tmp->next;
	}
	SetTitleBar(hi, True, True);
      } else
	scwm_msg(ERR, "SetTitleStyle",
		 "bad height argument (height must be from 5 to 256)");
      action += next;
    } else {
      if (!(action = ReadTitleButton(prev, &fl->titlebar, False, -1))) {
	free(parm);
	break;
      }
    }
#else /* ! EXTENDED_TITLESTYLE */
    else if (strcmp(parm, "--") == 0) {
      if (!(action = ReadTitleButton(prev, &fl->titlebar, False, -1))) {
	free(parm);
	break;
      }
    }
#endif /* EXTENDED_TITLESTYLE */
    free(parm);
    prev = action;
    action = GetNextToken(action, &parm);
  }
}				/* SetTitleStyle */

#endif


SCM sym_focus, sym_mouse;

void 
init_miscprocs()
{
  sym_left = gh_symbol2scm("left");
  scm_protect_object(sym_left);
  sym_right = gh_symbol2scm("right");
  scm_protect_object(sym_right);
  sym_center = gh_symbol2scm("center");
  scm_protect_object(sym_center);
  sym_focus = gh_symbol2scm("focus");
  scm_protect_object(sym_focus);
  sym_mouse = gh_symbol2scm("mouse");
  scm_protect_object(sym_mouse);
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
wait_for_window(SCM name)
{
  Bool done = False;
  extern ScwmWindow *Tmp_win;
  char *n;
  int dummy;

  if (!gh_string_p(name)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("wait-for-window", 1, name);
  }
  n = gh_scm2newstr(name, &dummy);
  while (!done) {
    if (My_XNextEvent(dpy, &Event)) {
      SCM_DEFER_INTS;
      DispatchEvent();
      SCM_ALLOW_INTS;
      if (Event.type == MapNotify) {
	if ((Tmp_win) && (matchWildcards(n, Tmp_win->name) == True))
	  done = True;
	if ((Tmp_win) && (Tmp_win->class.res_class) &&
	    (matchWildcards(n, Tmp_win->class.res_class) == True))
	  done = True;
	if ((Tmp_win) && (Tmp_win->class.res_name) &&
	    (matchWildcards(n, Tmp_win->class.res_name) == True))
	  done = True;
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




