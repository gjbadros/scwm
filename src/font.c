
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

#define FONT_IMPLEMENTATION

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
#include "util.h"
#include "icons.h"
#include "decor.h"

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
#ifdef HAVE_SCM_PUTS
  scm_puts("#<font ", port);
  scm_puts(FONTNAME(obj), port);
  scm_putc('>', port);
#else /* !HAVE_SCM_PUTS */
  scm_gen_puts(scm_regular_port, "#<font ", port);
  scm_gen_puts(scm_regular_port, FONTNAME(obj), port);
  scm_gen_putc('>', port);
#endif /* HAVE_SCM_PUTS */

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
    scwm_msg(WARN,__FUNCTION__,"Could not load `%s' -- trying `fixed'",fn);
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

  font = (scwm_font *)safemalloc(sizeof(*font));
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
  return (FONT_P(obj) ? SCM_BOOL_T : SCM_BOOL_F);
}


SCM 
set_icon_font(SCM font)
{
  ScwmWindow *tmp;

  SCM_REDEFER_INTS;

  if (gh_string_p(font)) {
    font = load_font(font);
  }
  if (!FONT_P(font)) {
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

  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;


  if (gh_string_p(font)) {
    font = load_font(font);
  }
  if (!FONT_P(font)) {
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
  if (!FONT_P(font)) {
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

  SCM_REALLOW_INTS;
  return (font);
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
