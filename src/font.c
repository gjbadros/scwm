/* $Id$
 * Copyright (C) 1997, 1998, Maciej Stachowiak and Greg J. Badros
 *
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
 */


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

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
#include "guile-compat.h"
#ifdef USE_DMALLOC
#include "dmalloc.h"
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
#ifdef I18N
  XFreeFontSet(dpy,XFONT(obj));
#else
  XFreeFont(dpy, XFONT(obj));
#endif
  FREE(FONT(obj));
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

SCWM_PROC (make_font, "make-font", 1, 0, 0,
           (SCM fname))
{
  SCM answer;
  scwm_font *font;
#ifdef I18N
  XFontSet fontset;
  char **list_names;
  char *defstrreturn;
  int missings,loadedfonts;
  int height = 0, ascent = 0;
  XFontStruct **xfss;
#else
  XFontStruct *xfs;
#endif
  char *fn;
  int len,i;

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
#ifdef I18N
  fontset = XCreateFontSet(dpy,fn,&list_names,&missings,&defstrreturn);
  if (NULL == fontset) {
    scwm_msg(WARN,s_make_font,"Could not load fontset`%s' -- trying `fixed'",fn);
    FREE(fn);

    answer=scm_hash_ref(font_hash_table, str_fixed, SCM_BOOL_F);
    if (answer!=SCM_BOOL_F) {
      return answer;
    }
    
    fn = strdup(XFIXEDFONTSET);
    if (NULL == fn)
      goto allocation;
    fontset = XCreateFontSet(dpy,fn,&list_names,&missings,&defstrreturn);
  } 
  if (NULL == fontset) {
    FREE(fn);
    scwm_error(s_make_font, 1);
  }

  font = NEW(*font);
  if (NULL == font) {
    FREE(fn);
    XFreeFontSet(dpy, fontset);
    goto allocation;
  }
  for ( i = 0 ; i < missings ; i++ ) {
    scwm_msg(WARN,s_make_font,"Missing charset in `%s' for `%s'.",
	     fn,list_names[i]);
  }

  if (missings > 0)
      XFreeStringList(list_names);

#else
  xfs = XLoadQueryFont(dpy, fn);
  if (NULL == xfs) {
    scwm_msg(WARN,s_make_font,"Could not load `%s' -- trying `fixed'",fn);
    FREE(fn);

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
    FREE(fn);
    scwm_error(s_make_font, 1);
  }

  font = NEW(scwm_font);
  if (NULL == font) {
    FREE(fn);
    XFreeFont(dpy, xfs);
    goto allocation;
  }
#endif

  SCM_REDEFER_INTS;
  SCM_NEWCELL(answer);
  SCM_SETCAR(answer, scm_tc16_scwm_font);
  SCM_SETCDR(answer, (SCM) font);
#ifdef I18N  
  XFONT(answer) = fontset;
  loadedfonts = XFontsOfFontSet (fontset,&xfss,&list_names);
  for ( i = 0 ; i < loadedfonts ; i++ ) {
      height = ( height > xfss[i]->ascent + xfss[i]->descent )?
	  height:(xfss[i]->ascent + xfss[i]->descent);
      ascent = ( ascent > xfss[i]->ascent )? ascent:( xfss[i]->ascent );
  }
  XFONT(answer) = fontset;
  FONT(answer)->xfs = xfss[0];
  FONT(answer)->height = height;
  FONT(answer)->ascent = ascent;

#else
  XFONT(answer) = xfs;
  FONT(answer)->height = XFONT(answer)->ascent + XFONT(answer)->descent;
#endif
  FONTNAME(answer) = gh_str02scm(fn);
  SCM_REALLOW_INTS;
  FREE(fn);

  scm_hash_set_x(font_hash_table, FONTNAME(answer), answer);
  return answer;
}


SCWM_PROC (font_p, "font?", 1, 0, 0,
           (SCM obj))
{
  return SCM_BOOL_FromBool(FONT_P(obj));
}


SCWM_PROC (font_properties, "font-properties", 1, 0, 0,
           (SCM font))
{
  scwm_font *psfont = SAFE_FONT(font);
  if (!psfont) {
    scm_wrong_type_arg(s_font_properties, 1, font);
  } 
  return gh_list(gh_cons(sym_name, FONTNAME(font)),
		 gh_cons(sym_height, gh_int2scm(FONTHEIGHT(font))),
		 SCM_UNDEFINED);
}

SCWM_PROC (set_icon_font_x, "set-icon-font!", 1, 0, 0,
           (SCM font))
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

SCWM_PROC (set_window_font_x, "set-window-font!", 1, 0, 0,
           (SCM font))
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


static
void
menu_font_update()
{
  XGCValues gcv;
  unsigned long gcm;
#ifdef I18N
  XRectangle dummy,log_ret;

  XmbTextExtents(XFONT(Scr.menu_font),"WWWWWWWWWWWWWWW", 15,&dummy,&log_ret);
  Scr.EntryHeight = log_ret.height + HEIGHT_EXTRA;
  Scr.SizeStringWidth = log_ret.width;
#else
  Scr.EntryHeight = FONTHEIGHT(Scr.menu_font) + HEIGHT_EXTRA;
  Scr.SizeStringWidth = XTextWidth(XFONT(Scr.menu_font),
					 "WWWWWWWWWWWWWWW", 15);
#endif
				   
  gcm = GCFont;
#ifdef I18N
  gcv.font = FONT(Scr.menu_font)->xfs->fid;
#else
  gcv.font = XFONT(Scr.menu_font)->fid;
#endif
  /* are all these needed? */
  /* MSFIX: This stuff should really be handled by other code. */
  XChangeGC(dpy, Scr.MenuReliefGC, gcm, &gcv);
  XChangeGC(dpy, Scr.MenuShadowGC, gcm, &gcv);
  XChangeGC(dpy, Scr.MenuGC, gcm, &gcv);
  XChangeGC(dpy, Scr.MenuStippleGC, gcm, &gcv);
}


SCWM_PROC (set_menu_font_x, "set-menu-font!", 1, 0, 0,
           (SCM font))
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


SCWM_PROC (clear_font_cache_entry, "clear-font-cache-entry", 1, 0, 0,
           (SCM name))
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
#ifdef I18N
  str_fixed=gh_str02scm(XFIXEDFONTSET);
#else
  str_fixed=gh_str02scm("fixed");
#endif
  scm_protect_object(str_fixed);
  font_hash_table = 
    scm_make_weak_value_hash_table (SCM_MAKINUM(FONT_HASH_SIZE));
  scm_protect_object(font_hash_table);
  protected_fonts =
    scm_make_vector (SCM_MAKINUM(2), SCM_EOL);
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
