/* $Id$
 * Copyright (C) 1997-1999, Maciej Stachowiak and Greg J. Badros
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

#define FONT_IMPLEMENTATION

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

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
#include "guile-compat.h"
#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif

#define FONT_HASH_SIZE 7

/**CONCEPT: Fonts
  Fonts are first-class objects. However, anywhere that a font is
taken as an argument, a string containing an X font specification
will also be accepted, and will be automatically converted to the
proper font object. Using the same font specifier string more than
once is not inefficient, as caching ensures that font objects are
shared.
*/

SCM scmFixedFont = SCM_UNDEFINED;

static SCM font_hash_table = SCM_UNDEFINED;

SCM_SYMBOL (sym_name,"name");
SCM_SYMBOL (sym_height,"height");

SCM
mark_font(SCM obj)
{
  GC_MARK_SCM_IF_SET(FONT(obj)->name);
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
print_font(SCM obj, SCM port, scm_print_state *ARG_IGNORE(pstate))
{
  scm_puts("#<font ", port);
  scm_write(FONTNAME(obj), port);
  scm_putc('>', port);

  return 1;
}

/* Load a font from a string name. If it fails to load, try
   to load "fixed". Throw an error if this fails, else return
   a font object. */

SCWM_PROC(make_font, "make-font", 1, 0, 0,
          (SCM fname),
"Return the font object for the X font specifier FNAME.\n\
If FNAME is not a valid X font name, or cannot be\n\
allocated, an error results.")
#define FUNC_NAME s_make_font
{
  SCM answer;
  scwm_font *font;
#ifdef I18N
  XFontSet fontset;
  char **list_names;
  char *defstrreturn;
  int missings,loadedfonts;
  int height = 0, ascent = 0;
  int i = 0;
  XFontStruct **xfss;
#else
  XFontStruct *xfs;
#endif
  char *fn;

  VALIDATE_ARG_STR_NEWCOPY(1,fname,fn);
  
  answer=scm_hash_ref(font_hash_table, fname, SCM_BOOL_F);
  if (answer!=SCM_BOOL_F) {
    return answer;
  }

  if (NULL == fn) {
  allocation:
    scm_memory_error(FUNC_NAME);
  }

/* GJB:FIXME:: merge these two paths, and use gh_free/FREE correctly */
#ifdef I18N
  fontset = XCreateFontSet(dpy,fn,&list_names,&missings,&defstrreturn);
  if (NULL == fontset) {
    scwm_msg(WARN,FUNC_NAME,"Could not load fontset`%s' -- trying `fixed'",fn);
    FREE(fn);

    answer=scm_hash_ref(font_hash_table, str_fixed, SCM_BOOL_F);
    if (answer!=SCM_BOOL_F) {
      return answer;
    }
    
    fn = strdup(XFIXEDFONTNAME);
    if (NULL == fn)
      goto allocation;
    fontset = XCreateFontSet(dpy,fn,&list_names,&missings,&defstrreturn);
  } 
  if (NULL == fontset) {
    FREE(fn);
    scwm_error(FUNC_NAME, "Unable to load `fixed' font.");
  }

  font = NEW(scwm_font);
  if (NULL == font) {
    FREE(fn);
    XFreeFontSet(dpy, fontset);
    goto allocation;
  }
  for ( i = 0 ; i < missings ; i++ ) {
    scwm_msg(WARN,FUNC_NAME,"Missing charset in `%s' for `%s'.",
	     fn,list_names[i]);
  }

  if (missings > 0)
      XFreeStringList(list_names);

#else
  xfs = XLoadQueryFont(dpy, fn);
  if (NULL == xfs) {
    scwm_msg(WARN,FUNC_NAME,"Could not load `%s' -- trying `fixed'",fn);
    FREE(fn);

    answer=scm_hash_ref(font_hash_table, str_fixed, SCM_BOOL_F);
    if (answer!=SCM_BOOL_F) {
      return answer;
    }
    
    xfs = XLoadQueryFont(dpy, XFIXEDFONTNAME);
  } 
  if (NULL == xfs) {
    FREE(fn);
    scwm_error(FUNC_NAME, "Unable to load `fixed' font.");
  }

  font = NEW(scwm_font);
  if (NULL == font) {
    FREE(fn);
    XFreeFont(dpy, xfs);
    goto allocation;
  }
#endif

  scwm_defer_ints();
  SCWM_NEWCELL_SMOB(answer, scm_tc16_scwm_font, font);
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
  scwm_allow_ints();
  FREE(fn);

  scm_hash_set_x(font_hash_table, FONTNAME(answer), answer);
  return answer;
}
#undef FUNC_NAME


SCWM_PROC(font_p, "font?", 1, 0, 0,
          (SCM obj),
"Returns #t if OBJ is a font object, otherwise #f.")
#define FUNC_NAME s_font_p
{
  return SCM_BOOL_FromBool(FONT_P(obj));
}
#undef FUNC_NAME


SCWM_PROC(font_properties, "font-properties", 1, 0, 0,
          (SCM font),
"Return an association list giving some properties of FONT.\n\
Currently defined properties are 'name, the string name of the\n\
color, and 'height, its total height in pixels.")
#define FUNC_NAME s_font_properties
{
  VALIDATE_ARG_FONT(1,font);
  return gh_list(gh_cons(sym_name, FONTNAME(font)),
		 gh_cons(sym_height, gh_int2scm(FONTHEIGHT(font))),
		 SCM_UNDEFINED);
}
#undef FUNC_NAME

SCWM_PROC(set_icon_font_x, "set-icon-font!", 1, 0, 0,
          (SCM font),
"Set the font used for drawing icon titles to FONT.")
#define FUNC_NAME s_set_icon_font_x
{
  VALIDATE_ARG_FONT_OR_STRING(1,font);

  Scr.icon_font = font;

  redraw_icon_titles();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(icon_font, "icon-font", 0, 0, 0,
          (),
"Return the font used for drawing icon titles.")
#define FUNC_NAME s_icon_font
{
  return Scr.icon_font;
}
#undef FUNC_NAME


SCWM_PROC(set_title_font_x, "set-title-font!", 1, 0, 0,
          (SCM font),
"Set the font for window titles In the current decor to FONT.")
#define FUNC_NAME s_set_title_font_x
{
  int extra_height;
  ScwmDecor *fl;

  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  VALIDATE_ARG_FONT_OR_STRING(1,font);

  fl->window_font = font;
  fl->window_font_y = FONTY(font);

  extra_height = FONTHEIGHT(font) + 3 - fl->TitleHeight;
  fl->TitleHeight = FONTHEIGHT(font) + 3;
  redraw_titlebars(fl, extra_height);
  return font;
}
#undef FUNC_NAME

SCWM_PROC(title_font, "title-font", 0, 0, 0,
          (),
"Return the font used for drawing window titles in the current decor.")
#define FUNC_NAME s_title_font
{
  ScwmDecor *fl;

  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  return fl->window_font;
}
#undef FUNC_NAME

SCWM_PROC(clear_font_cache_entry, "clear-font-cache-entry", 1, 0, 0,
          (SCM name),
"Fonts are cached by name. It is remotely possible that the\n\
meaning of a particular string as a fonts will change in your X\n\
server, if you try hard enough (perhaps if you add or remove font\n\
servers). For this unlikely eventuality, `clear-font-cache-entry' is\n\
provided - it removes the font associated with NAME from the font\n\
cache")
#define FUNC_NAME s_clear_font_cache_entry
{
  scm_hash_remove_x(font_hash_table, name);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



MAKE_SMOBFUNS(font);

void init_font() 
{
  REGISTER_SCWMSMOBFUNS(font);

  /* GJB:FIXME:: should make the font object containing
     the fixed font used throughout and made permanent.
     The string should not be used in C code --03/22/99 gjb */
  scwm_defer_ints();
  str_fixed=gh_str02scm(XFIXEDFONTNAME);
  scm_permanent_object(str_fixed);

  font_hash_table = 
    scm_make_weak_value_hash_table (SCM_MAKINUM(FONT_HASH_SIZE));
  scm_permanent_object(font_hash_table);

  scwm_allow_ints();
#ifndef SCM_MAGIC_SNARFER
#include "font.x"
#endif
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

