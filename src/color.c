
/****************************************************************************
 * This module has been significantly modified by Maciej Stachowiak.
 * It may be used under the terms indicated by the copyright below.
 * Changes Copyright 1997, Maciej stachowiak
 ****************************************************************************/
/****************************************************************************
 * This module is all original code 
 * by Rob Nation 
 * Copyright 1993, Robert Nation
 *     You may use this code for any purpose, as long as the original
 *     copyright remains in the source code and all documentation
 ****************************************************************************/
/****************************************************************************
 * This module is all new
 * by Rob Nation 
 * Copyright 1993 Robert Nation. No restrictions are placed on this code,
 * as long as the copyright notice is preserved
 ****************************************************************************/

#include <guile/gh.h>
#include <libguile.h>
#include <X11/Xproto.h>
#include <X11/Xatom.h>

#include <config.h>
#include "scwm.h"
#include "screen.h"
#include "color.h"
#include "errors.h"
#include "borders.h"
#include "decor.h"
#include "colors.h"

/* XXX - these should be made visible as bindings */
#define BLACK COLOR(load_color(gh_str02scm("black")))
#define WHITE COLOR(load_color(gh_str02scm("white")))

long scm_tc16_scwm_color;

int 
print_color(SCM obj, SCM port, scm_print_state * pstate)
{
#ifdef HAVE_SCM_PUTS
  scm_puts("#<color ", port);
  scm_write(gh_int2scm(COLOR(obj)), port);
  scm_putc('>', port);
#else /* HAVE_SCM_PUTS */
  scm_gen_puts(scm_regular_port, "#<color ", port);
  scm_write(gh_int2scm(COLOR(obj)), port);
  scm_gen_putc('>', port);
#endif /* HAVE_SCM_PUTS */

  return 1;
}

/* XXX - should be memoized - store linked list of colors somewhere w/ ref
   counts and cleaned by gc? */

SCM 
load_color(SCM cname)
{
  SCM answer;
  XColor color;
  char *cn;
  int len;

  SCM_REDEFER_INTS;
  if (!gh_string_p(cname)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("load-color", 1, cname);
  }
  cn = gh_scm2newstr(cname, &len);
  color.pixel = 0;
  if (!XParseColor(dpy, Scr.ScwmRoot.attr.colormap, cn, &color)) {
    /*    scm_error(gh_symbol2scm("scwm-error"),"load-color","%s",
       gh_list(gh_str02scm("Unable to parse color."),SCM_UNDEFINED),
       gh_list(gh_int2scm(1),SCM_UNDEFINED)); */
  } else if (!XAllocColor(dpy, Scr.ScwmRoot.attr.colormap, &color)) {
    /*  scm_error(gh_symbol2scm("scwm-error"),"load-color","%s",
       gh_list(gh_str02scm("Unable to allocate color."),SCM_UNDEFINED),
       gh_list(gh_int2scm(1),SCM_UNDEFINED));
     */
  }
  free(cn);
  SCM_NEWCELL(answer);
  SCM_SETCAR(answer, scm_tc16_scwm_color);
  SCM_SETCDR(answer, (SCM) (color.pixel));
  SCM_REALLOW_INTS;
  return (answer);
}

/* eventually add option to pass background relief, shadow? */
SCM 
set_hilight_colors(SCM fg, SCM bg)
{
  XGCValues gcv;
  unsigned long gcm;
  ScwmDecor *fl;
  ScwmWindow *hilight;

  SCM_REDEFER_INTS;
  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  if (gh_string_p(fg)) {
    fg = load_color(fg);
  } else if (fg == SCM_UNDEFINED) {
    fg = SCM_BOOL_F;
  } else if (!((SCM_NIMP(fg) && COLORP(fg)) || fg == SCM_BOOL_F)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("set-highlight-colors!", 1, fg);
  }
  if (gh_string_p(bg)) {
    bg = load_color(bg);
  } else if (bg == SCM_UNDEFINED) {
    bg = SCM_BOOL_F;
  } else if (!((SCM_NIMP(bg) && COLORP(bg)) || bg == SCM_BOOL_F)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("set-highlight-colors!", 2, bg);
  }
  if (Scr.d_depth > 2) {
    if (fg != SCM_BOOL_F) {
      fl->HiColors.fore = COLOR(fg);
    }
    if (bg != SCM_BOOL_F) {
      fl->HiColors.back = COLOR(bg);
    }
    fl->HiRelief.back = GetShadow(fl->HiColors.back);
    fl->HiRelief.fore = GetHilite(fl->HiColors.back);
  } else {
    fl->HiColors.back = WHITE;
    fl->HiColors.fore = BLACK;
    fl->HiRelief.back = BLACK;
    fl->HiRelief.fore = WHITE;
  }
  gcm = GCFunction | GCPlaneMask | GCGraphicsExposures | GCLineWidth | GCForeground |
    GCBackground;
  gcv.foreground = fl->HiRelief.fore;
  gcv.background = fl->HiRelief.back;
  gcv.fill_style = FillSolid;
  gcv.plane_mask = AllPlanes;
  gcv.function = GXcopy;
  gcv.graphics_exposures = False;
  gcv.line_width = 0;
  if (fl->HiReliefGC != NULL) {
    XFreeGC(dpy, fl->HiReliefGC);
  }
  fl->HiReliefGC = XCreateGC(dpy, Scr.Root, gcm, &gcv);
  gcv.foreground = fl->HiRelief.back;
  gcv.background = fl->HiRelief.fore;
  if (fl->HiShadowGC != NULL) {
    XFreeGC(dpy, fl->HiShadowGC);
  }
  fl->HiShadowGC = XCreateGC(dpy, Scr.Root, gcm, &gcv);

  if ((Scr.flags & WindowsCaptured) && (Scr.Hilite != NULL)) {
    hilight = Scr.Hilite;
    SetBorder(Scr.Hilite, False, True, True, None);
    SetBorder(hilight, True, True, True, None);
  }
  SCM_REALLOW_INTS;
  return (SCM_UNSPECIFIED);
}

SCM 
color_p(SCM obj)
{
  return ((SCM_NIMP(obj) && COLORP(obj)) ? SCM_BOOL_T : SCM_BOOL_F);
}

SCM 
set_menu_colors(SCM fg, SCM bg, SCM stipple)
{
  XGCValues gcv;
  unsigned long gcm;

  SCM_REDEFER_INTS;
  if (gh_string_p(fg)) {
    fg = load_color(fg);
  } else if (fg == SCM_UNDEFINED) {
    fg = SCM_BOOL_F;
  } else if (!((SCM_NIMP(fg) && COLORP(fg)) || fg == SCM_BOOL_F)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("set-highlight-colors!", 1, fg);
  }
  if (gh_string_p(bg)) {
    bg = load_color(bg);
  } else if (bg == SCM_UNDEFINED) {
    bg = SCM_BOOL_F;
  } else if (!((SCM_NIMP(bg) && COLORP(bg)) || bg == SCM_BOOL_F)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("set-highlight-colors!", 2, bg);
  }
  if (gh_string_p(stipple)) {
    stipple = load_color(stipple);
  } else if (stipple == SCM_UNDEFINED) {
    stipple = SCM_BOOL_F;
  } else if (!((SCM_NIMP(stipple) && COLORP(stipple)) || stipple == SCM_BOOL_F)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("set-highlight-colors!", 3, stipple);
  }
  if (Scr.d_depth > 2) {
    if (fg != SCM_BOOL_F) {
      Scr.MenuColors.fore = COLOR(fg);
    }
    if (bg != SCM_BOOL_F) {
      Scr.MenuColors.back = COLOR(bg);
    }
    Scr.MenuRelief.back = GetShadow(Scr.MenuColors.back);
    Scr.MenuRelief.fore = GetHilite(Scr.MenuColors.back);
    Scr.MenuStippleColors.back = Scr.MenuColors.back;
    if (stipple != SCM_BOOL_F) {
      Scr.MenuStippleColors.fore = COLOR(stipple);
    }
  } else {
    Scr.MenuColors.back = WHITE;
    Scr.MenuColors.fore = BLACK;
    Scr.MenuRelief.back = BLACK;
    Scr.MenuRelief.fore = WHITE;
    Scr.MenuStippleColors.back = WHITE;
    Scr.MenuStippleColors.fore = BLACK;
  }

  gcm = GCForeground | GCBackground;
  gcv.foreground = Scr.MenuRelief.fore;
  gcv.background = Scr.MenuRelief.back;
  XChangeGC(dpy, Scr.MenuReliefGC, gcm, &gcv);
  gcv.foreground = Scr.MenuRelief.back;
  gcv.background = Scr.MenuRelief.fore;
  XChangeGC(dpy, Scr.MenuShadowGC, gcm, &gcv);
  gcv.foreground = Scr.MenuColors.fore;
  gcv.background = Scr.MenuColors.back;
  XChangeGC(dpy, Scr.MenuGC, gcm, &gcv);
  if (Scr.d_depth < 2) {
    gcv.fill_style = FillStippled;
    gcv.stipple = Scr.gray_bitmap;
    gcm = GCForeground | GCBackground | GCStipple | GCFillStyle;
  } else {
    gcv.foreground = Scr.MenuStippleColors.fore;
    gcv.background = Scr.MenuStippleColors.back;
  }
  XChangeGC(dpy, Scr.MenuStippleGC, gcm, &gcv);
  MakeMenus();

  SCM_REALLOW_INTS;
  return (SCM_UNSPECIFIED);
}
