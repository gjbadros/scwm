/* $Id$
 * font.h
 * Copyright (C) 1997, 1998, 1999, 2000 Greg J. Badros and Maciej Stachowiak
 *
 * This module is all original code 
 * by Maciej Stachowiak and Greg J. Badros.
 * It may be used or distributed under either the FVWM license 
 * (see COPYING.FVWM) or the GNU General Public License (see COPYING.GPL and
 * the description below)
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
 * As a special exception, this file may alternatively be distributed under 
 * the fvwm license (see COPYING.FVWM).
 *
 */


#ifndef FONT_H
#define FONT_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include <libguile.h>

#undef EXTERN
#undef EXTERN_SET
#ifdef FONT_IMPLEMENTATION
#define EXTERN
#define EXTERN_SET(x,y) x = y
#else
#define EXTERN extern
#define EXTERN_SET(x,y) extern x
#endif

EXTERN long scm_tc16_scwm_font;
EXTERN SCM str_fixed;

typedef struct {
#ifdef I18N
  XFontSet fontset;
  int ascent;
  XFontStruct *xfs;		/* Dummy of font structure */
#else
  XFontStruct *xfs;		/* font structure */
#endif
  int height;			/* height of the font */
  SCM name;
} scwm_font;

#ifdef I18N
#define XFIXEDFONTNAME "-*-fixed-*"
#else
#define XFIXEDFONTNAME "fixed"
#endif

#define FONT_P(X) (SCM_NIMP((X)) && gh_car((X)) == (SCM)scm_tc16_scwm_font)
#define FONT(X)  ((scwm_font *)gh_cdr((X)))
#define SAFE_FONT(X)  (FONT_P((X))?FONT((X)):NULL)
#define DYNAMIC_FONT_P(X) (gh_symbol_p((X))? \
			   FONT_P(scm_symbol_binding(SCM_BOOL_F,(X))) : \
			   FONT_P((X)))
#define FONT_OR_SYMBOL_P(x) (FONT_P((x)) || gh_symbol_p((x)))

#define DYNAMIC_SAFE_FONT(X) (gh_symbol_p((X))? \
			      SAFE_FONT(scm_symbol_binding(SCM_BOOL_F,(X))) : \
			      SAFE_FONT((X)))

#ifdef I18N
#define XFONT(X) (((scwm_font *)gh_cdr((X)))->fontset)
#define FONTY(X) (((scwm_font *)gh_cdr((X)))->ascent)
#else
#define XFONT(X) (((scwm_font *)gh_cdr((X)))->xfs)
#define FONTY(X) ((XFONT(X))->ascent)
#endif

#define XFONTID(X) (((scwm_font *)gh_cdr((X)))->xfs->fid)

#define SAFE_XFONT(X) (FONT_P((X))?XFONT((X)):NULL)
#define FONTNAME(X) (((scwm_font *)gh_cdr(X))->name)
#define SAFE_FONTNAME(X) (FONT_P((X))?FONTNAME((X)):NULL)
#define FONTHEIGHT(X) (((scwm_font *)gh_cdr(X))->height)

SCM make_font(SCM fname);

/* GJB:FIXME:: this primitive should not be exposed
   it is used by decor.c now --03/22/99 gjb */
SCM set_title_font_x(SCM font);

extern SCM scmFixedFont;

#define VALIDATE_ARG_FONT(pos,scm) \
  do { \
  if (!FONT_P(scm)) scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  } while (0)

#define VALIDATE_ARG_FONT_OR_SYM(pos,scm) \
  do { \
  if (!FONT_OR_SYMBOL_P(scm)) scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  } while (0)


#define VALIDATE_ARG_FONT_COPY(pos,scm,cvar) \
  do { \
  if (!FONT_P(scm)) scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  else cvar = FONT(scm); \
  } while (0)


#define VALIDATE_ARG_FONT_OR_STRING(pos,scm) \
  do { \
  if (gh_string_p(scm)) scm = make_font(scm); \
  if (!FONT_P(scm)) scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  } while (0)



#endif /* FONT_H */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */


