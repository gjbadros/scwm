
/****************************************************************************
 * This module has been significantly modified by Maciej Stachowiak.
 * It may be used under the terms indicated by the copyright below.
 * Changes Copyright 1997, Maciej stachowiak
 ****************************************************************************/
/****************************************************************************
 * This module is based partly on original code 
 * by Rob Nation 
 * Copyright 1993, Robert Nation
 *     You may use this code for any purpose, as long as the original
 *     copyright remains in the source code and all documentation
 ****************************************************************************/

#include <config.h>

#include <stdio.h>
#include <string.h>
#include <guile/gh.h>
#include <libguile.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "scwm.h"
#include "screen.h"
#include "errors.h"
#include "font.h"

#ifdef USEDECOR
extern ScwmDecor *last_decor, *cur_decor;

#endif


long scm_tc16_scwm_font;
SCM window_font = SCM_UNDEFINED, icon_font = SCM_UNDEFINED, menu_font = SCM_UNDEFINED;

size_t 
free_font(SCM obj)
{
  XFreeFont(dpy, XFONT(obj));
  free(FONTNAME(obj));
  free(FONT(obj));
  return (0);
}

int 
print_font(SCM obj, SCM port, scm_print_state * pstate)
{
  scm_gen_puts(scm_regular_port, "#<font ", port);
  scm_gen_puts(scm_regular_port, FONTNAME(obj), port);
  scm_gen_putc('>', port);
  return 1;
}


/* Load a font from a string name. If it fails to load, try
   to load "fixed". Throw an error if this fails, else return
   a font object. */
SCM 
load_font(SCM fname)
{
  SCM answer;
  scwm_font *font;
  XFontStruct *xfs;
  char *fn;
  int len;

  SCM_REDEFER_INTS;
  if (!gh_string_p(fname)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("load-font", 1, fname);
  }
  fn = gh_scm2newstr(fname, &len);
  if (NULL == fn) {
  allocation:
    SCM_ALLOW_INTS;
    scm_memory_error("load-font");
  }
  xfs = XLoadQueryFont(dpy, fn);
  if (NULL == xfs) {
    free(fn);
    fn = strdup("fixed");
    if (NULL == fn)
      goto allocation;
    xfs = XLoadQueryFont(dpy, fn);
  }
  if (NULL == xfs) {
    free(fn);
    SCM_ALLOW_INTS;
    scwm_error("load-font", 1);
  }
  font = malloc(sizeof(*font));
  if (NULL == font) {
    free(fn);
    XFreeFont(dpy, xfs);
    goto allocation;
  }
  SCM_NEWCELL(answer);
  SCM_SETCAR(answer, scm_tc16_scwm_font);
  SCM_SETCDR(answer, (SCM) font);
  XFONT(answer) = xfs;
  FONTNAME(answer) = fn;

  SCM_REALLOW_INTS;
  return answer;
}

SCM 
font_p(SCM obj)
{
  return ((SCM_NIMP(obj) && FONTP(obj)) ? SCM_BOOL_T : SCM_BOOL_F);
}


SCM 
set_icon_font(SCM font)
{
  ScwmWindow *tmp;

  SCM_REDEFER_INTS;

  if (gh_string_p(font)) {
    font = load_font(font);
  }
  if (!(SCM_NIMP(font) && FONTP(font))) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("set-icon-font!", 1, font);
  }
  scm_unprotect_object(icon_font);
  scm_protect_object(font);
  icon_font = font;
  Scr.IconFont.font = XFONT(font);
  Scr.IconFont.height =
    Scr.IconFont.font->ascent + Scr.IconFont.font->descent;
  Scr.IconFont.y = Scr.IconFont.font->ascent;
  tmp = Scr.ScwmRoot.next;
  while (tmp != NULL) {
    RedoIconName(tmp);
    if (tmp->flags & ICONIFIED) {
      DrawIconWindow(tmp);
    }
    tmp = tmp->next;
  }

  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}

SCM 
set_window_font(SCM font)
{
  int extra_height;
  ScwmDecor *fl;

  SCM_REDEFER_INTS;

#ifdef USEDECOR
  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;
#else
  fl = &Scr.DefaultDecor;
#endif


  if (gh_string_p(font)) {
    font = load_font(font);
  }
  if (!(SCM_NIMP(font) && FONTP(font))) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("set-window-font!", 1, font);
  }
  scm_unprotect_object(window_font);
  scm_protect_object(font);
  window_font = font;
  fl->WindowFont.font = XFONT(font);
  fl->WindowFont.height =
    fl->WindowFont.font->ascent + fl->WindowFont.font->descent;
  fl->WindowFont.y = fl->WindowFont.font->ascent;

  extra_height = fl->TitleHeight;
  fl->TitleHeight = fl->WindowFont.font->ascent + fl->WindowFont.font->descent + 3;
  extra_height -= fl->TitleHeight;
  redraw_titlebars(fl, extra_height);
  SCM_REALLOW_INTS;
  return font;
}


SCM 
set_menu_font(SCM font)
{
  XGCValues gcv;
  unsigned long gcm;

  SCM_REDEFER_INTS;

  if (gh_string_p(font)) {
    SCM_ALLOW_INTS;
    font = load_font(font);
  }
  if (!(SCM_NIMP(font) && FONTP(font))) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("set-menu-font!", 1, font);
  }
  scm_unprotect_object(menu_font);
  scm_protect_object(font);
  menu_font = font;
  Scr.StdFont.font = XFONT(font);
  Scr.StdFont.height = Scr.StdFont.font->ascent + Scr.StdFont.font->descent;
  Scr.StdFont.y = Scr.StdFont.font->ascent;
  Scr.EntryHeight = Scr.StdFont.height + HEIGHT_EXTRA;
  gcm = GCFont;
  gcv.font = Scr.StdFont.font->fid;
  /* are all these needed? */
  XChangeGC(dpy, Scr.MenuReliefGC, gcm, &gcv);
  XChangeGC(dpy, Scr.MenuShadowGC, gcm, &gcv);
  XChangeGC(dpy, Scr.MenuGC, gcm, &gcv);
  XChangeGC(dpy, Scr.MenuStippleGC, gcm, &gcv);
  MakeMenus();

  SCM_REALLOW_INTS;
  return (font);
}

#if 0

void 
redraw_menus(void)
{
  if (Scr.SizeWindow != None) {
    Scr.SizeStringWidth = XTextWidth(Scr.StdFont.font,
				     " +8888 x +8888 ", 15);
    wid = Scr.SizeStringWidth + SIZE_HINDENT * 2;
    hei = Scr.StdFont.height + SIZE_VINDENT * 2;
    if (Scr.flags & MWMMenus) {
      XMoveResizeWindow(dpy, Scr.SizeWindow,
			Scr.MyDisplayWidth / 2 - wid / 2,
			Scr.MyDisplayHeight / 2 - hei / 2,
			wid, hei);
    } else {
      XMoveResizeWindow(dpy, Scr.SizeWindow, 0, 0, wid, hei);
    }
  }
  if (Scr.SizeWindow != None) {
    XSetWindowBackground(dpy, Scr.SizeWindow, Scr.MenuColors.back);
  }
  MakeMenus();
}

#endif
