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


#ifndef FONT_H
#define FONT_H

#include <libguile.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

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

typedef struct {
  XFontStruct *xfs;		/* font structure */
  int height;			/* height of the font */
  SCM name;
} scwm_font;

#define FONT_P(X) (SCM_NIMP((X)) && SCM_CAR((X)) == (SCM)scm_tc16_scwm_font)
#define FONT(X)  ((scwm_font *)SCM_CDR((X)))
#define SAFE_FONT(X)  (FONT_P((X))?FONT((X)):NULL)
#define DYNAMIC_FONT_P(X) (gh_symbol_p((X))? \
			   FONT_P(scm_symbol_binding(SCM_BOOL_F,(X))) : \
			   FONT_P((X)))
#define FONT_OR_SYMBOL_P(x) (FONT_P((x)) || gh_symbol_p((x)))

#define DYNAMIC_SAFE_FONT(X) (gh_symbol_p((X))? \
			      SAFE_FONT(scm_symbol_binding(SCM_BOOL_F,(X))) : \
			      SAFE_FONT((X)))

#define XFONT(X) (((scwm_font *)SCM_CDR((X)))->xfs)
#define SAFE_XFONT(X) (FONT_P((X))?XFONT((X)):NULL)
#define FONTNAME(X) (((scwm_font *)SCM_CDR(X))->name)
#define SAFE_FONTNAME(X) (FONT_P((X))?FONTNAME((X)):NULL)
#define FONTHEIGHT(X) (((scwm_font *)SCM_CDR(X))->height)
#define FONTY(X) ((XFONT(X))->ascent)

size_t free_font(SCM obj);
int print_font(SCM obj, SCM port, scm_print_state * pstate);
SCM mark_font(SCM obj);

SCM make_font(SCM fname);
SCM font_p(SCM obj);
SCM font_properties(SCM font);
SCM set_icon_font_x(SCM font);
SCM set_window_font_x(SCM font);
SCM set_menu_font_x(SCM font);

void init_font() ;

#endif /* FONT_H */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */

