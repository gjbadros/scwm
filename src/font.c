/* $Id$ */
/****************************************************************************
 * This module is all original code 
 * by Maciej Stachowiak and Greg J. Badros.
 * It may be used or distributed under either the FVWM license 
 * (see COPYING.FVWM) or the GNU General Public License (see COPYING.GPL and
 * the description below)
 * Copyright 1997, Maciej Stachowiak and Greg J. Badros
 ****************************************************************************/
/*      Copyright (C) 1997, Maciej Stachowiak and Greg J. Badros

 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.GPL.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 *
 * As a special exception, this file may alternatively be distributed under 
 * the fvwm license (see COPYING.FVWM).
 *
 */


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
#include "drawmenu.h"

#ifndef HAVE_GH_VECTOR_SET_X
#define gh_vector_set_x gh_vset
#endif

#define FONT_HASH_SIZE 7

static SCM font_hash_table = SCM_UNDEFINED;
static SCM protected_fonts = SCM_UNDEFINED;

static SCM str_fixed;
SCM_SYMBOL (sym_name,"name");
SCM_SYMBOL (sym_height,"height");

SCM
mark_font(SCM obj)
{
  SCM_SETGC8MARK(obj);
  scm_gc_mark (FONT(obj)->name);
  return SCM_BOOL_F;
}


size_t 
free_font(SCM obj)
{
  XFreeFont(dpy, XFONT(obj));
  free(FONT(obj));
  return (0);
}

int 
print_font(SCM obj, SCM port, scm_print_state * pstate)
{
  scm_puts("#<font ", port);
  scm_write(FONTNAME(obj), port);
  scm_putc('>', port);

  return 1;
}

/* Load a font from a string name. If it fails to load, try
   to load "fixed". Throw an error if this fails, else return
   a font object. */

SCM_PROC (s_make_font, "make-font", 1, 0, 0, make_font);

SCM 
make_font(SCM fname)
{
  SCM answer;
  scwm_font *font;
  XFontStruct *xfs;
  char *fn;
  int len;

  if (!gh_string_p(fname)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg(s_make_font, 1, fname);
  }
  
  answer=scm_hash_ref(font_hash_table, fname, SCM_BOOL_F);
  if (answer!=SCM_BOOL_F) {
    return answer;
  }

  fn = gh_scm2newstr(fname, &len);
  if (NULL == fn) {
  allocation:
    scm_memory_error(s_make_font);
  }
  xfs = XLoadQueryFont(dpy, fn);
  if (NULL == xfs) {
    scwm_msg(WARN,s_make_font,"Could not load `%s' -- trying `fixed'",fn);
    free(fn);

    answer=scm_hash_ref(font_hash_table, str_fixed, SCM_BOOL_F);
    if (answer!=SCM_BOOL_F) {
      return answer;
    }
    
    fn = strdup("fixed");
    if (NULL == fn)
      goto allocation;
    xfs = XLoadQueryFont(dpy, fn);
  } 
  if (NULL == xfs) {
    free(fn);
    scwm_error(s_make_font, 1);
  }

  font = (scwm_font *)safemalloc(sizeof(*font));
  if (NULL == font) {
    free(fn);
    XFreeFont(dpy, xfs);
    goto allocation;
  }
  SCM_REDEFER_INTS;
  SCM_NEWCELL(answer);
  SCM_SETCAR(answer, scm_tc16_scwm_font);
  SCM_SETCDR(answer, (SCM) font);
  XFONT(answer) = xfs;
  FONTNAME(answer) = gh_str02scm(fn);
  FONT(answer)->height = XFONT(answer)->ascent + XFONT(answer)->descent;
  SCM_REALLOW_INTS;
  free(fn);

  scm_hash_set_x(font_hash_table, FONTNAME(answer), answer);
  return answer;
}


SCM_PROC (s_font_p, "font?", 1, 0, 0, font_p);

SCM 
font_p(SCM obj)
{
  return (FONT_P(obj) ? SCM_BOOL_T : SCM_BOOL_F);
}


SCM_PROC (s_font_properties, "font-properties", 1, 0, 0, font_properties);

SCM
font_properties(SCM font)
{
  scwm_font *psfont = SAFE_FONT(font);
  if (!psfont) {
    scm_wrong_type_arg(s_font_properties, 1, font);
  } 
  return gh_list(gh_cons(sym_name, FONTNAME(font)),
		 gh_cons(sym_height, gh_int2scm(FONTHEIGHT(font))),
		 SCM_UNDEFINED);
}

SCM_PROC (s_set_icon_font_x, "set-icon-font!", 1, 0, 0, set_icon_font_x);

SCM 
set_icon_font_x(SCM font)
{
  if (gh_string_p(font)) {
    font = make_font(font);
  }
  if (!FONT_P(font)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg(s_set_icon_font_x, 1, font);
  }

  gh_vector_set_x(protected_fonts,SCM_MAKINUM(0),font);

  Scr.icon_font = font;

  redraw_icon_titles();
  return SCM_UNSPECIFIED;
}

SCM_PROC (s_set_window_font_x, "set-window-font!", 1, 0, 0, set_window_font_x);
SCM 
set_window_font_x(SCM font)
{
  int extra_height;
  ScwmDecor *fl;

  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  if (gh_string_p(font)) {
    font = make_font(font);
  }

  if (!FONT_P(font)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg(s_set_window_font_x, 1, font);
  }

  fl->window_font = font;
  fl->window_font_y = FONTY(font);

  extra_height = fl->TitleHeight;
  fl->TitleHeight = FONTHEIGHT(font) + 3;
  extra_height -= fl->TitleHeight;
  redraw_titlebars(fl, extra_height);
  return font;
}

SCM_PROC (s_set_menu_font_x, "set-menu-font!", 1, 0, 0, set_menu_font_x);

SCM 
set_menu_font_x(SCM font)
{

  if (gh_string_p(font)) {
    font = make_font(font);
  }
  if (!FONT_P(font)) {
    scm_wrong_type_arg(s_set_menu_font_x, 1, font);
  }

  gh_vector_set_x(protected_fonts,SCM_MAKINUM(1),font);

  Scr.menu_font=font;
  menu_font_update();

  return (font);
}

SCM_PROC (s_clear_font_cache_entry, "clear-font-cache-entry", 1, 0, 0, clear_font_cache_entry);

SCM clear_font_cache_entry(SCM name)
{
  scm_hash_remove_x(font_hash_table, name);
  return SCM_UNSPECIFIED;
}

static scm_smobfuns font_smobfuns =
{
  &mark_font,
  &free_font,
  &print_font,
  0
};


void init_font() 
{
  str_fixed=gh_str02scm("fixed");
  scm_protect_object(str_fixed);
  font_hash_table = 
    scm_make_weak_value_hash_table (SCM_MAKINUM(FONT_HASH_SIZE));
  scm_protect_object(font_hash_table);
  protected_fonts =
    scm_make_vector (SCM_MAKINUM(2), SCM_EOL, 
		     SCM_BOOL_F);
  scm_protect_object(protected_fonts);

  scm_tc16_scwm_font = scm_newsmob(&font_smobfuns);

#ifndef SCM_MAGIC_SNARFER
#include "font.x"
#endif
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
