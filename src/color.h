/* $Id$ 
 * color.h
 * Copyright (C) 1998, 1999, 2000  Greg J. Badros and Maciej Stachowiak 
 */


#ifndef COLOR_H
#define COLOR_H

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xproto.h>
#include <X11/Xatom.h>
#include <X11/Intrinsic.h>

#include <guile/gh.h>

#undef EXTERN
#undef EXTERN_SET
#ifdef COLOR_IMPLEMENTATION
#define EXTERN
#define EXTERN_SET(x,y) x = y
#else
#define EXTERN extern
#define EXTERN_SET(x,y) extern x
#endif

/* Throw an error if X is not a color or a string. Usage here implies
   that make_color should indeed throw an error if it fails to parse or
   allocate the color. */

#define VALIDATE_ARG_COLOR(pos,X)                            \
  do {                                                       \
    if (gh_string_p(X)) { X = make_color(X); }               \
    if (!COLOR_P(X)) { scm_wrong_type_arg(FUNC_NAME,pos,X);} \
  } while (0)

#define VALIDATE_ARG_COLOR_OR_SYM(pos,X)                     \
  do {                                                       \
    if (gh_string_p(X)) { X = make_color(X); }               \
    if (!COLOR_OR_SYMBOL_P(X)) { scm_wrong_type_arg(FUNC_NAME,pos,X);} \
  } while (0)

#define VALIDATE_ARG_COLOR_OR_SYM_USE_WHITE(pos,X)           \
  do {                                                       \
    if (gh_string_p(X)) { X = make_color(X); }               \
    if (UNSET_SCM(X)) { X = WHITE_COLOR; }                   \
    else if (!COLOR_OR_SYMBOL_P(X)) { scm_wrong_type_arg(FUNC_NAME,pos,X);} \
  } while (0)

#define VALIDATE_ARG_COLOR_OR_SYM_USE_BLACK(pos,X)           \
  do {                                                       \
    if (gh_string_p(X)) { X = make_color(X); }               \
    if (UNSET_SCM(X)) { X = BLACK_COLOR; }                   \
    else if (!COLOR_OR_SYMBOL_P(X)) { scm_wrong_type_arg(FUNC_NAME,pos,X);} \
  } while (0)

#define VALIDATE_ARG_COLOR_COPY_USE_WHITE(pos,X,xcolor)       \
  do {                                                        \
    if (UNSET_SCM(X)) { X = WHITE_COLOR; }                    \
    if (gh_string_p(X)) { X = make_color(X); }                \
    if (!COLOR_P(X)) { scm_wrong_type_arg(FUNC_NAME,pos,X); } \
    xcolor = XCOLOR(X);                                       \
  } while (0)


#define VALIDATE_ARG_COLOR_COPY_USE_BLACK(pos,X,xcolor)       \
  do {                                                        \
    if (UNSET_SCM(X)) { X = BLACK_COLOR; }                    \
    if (gh_string_p(X)) { X = make_color(X); }                \
    if (!COLOR_P(X)) { scm_wrong_type_arg(FUNC_NAME,pos,X); } \
    xcolor = XCOLOR(X);                                       \
  } while (0)


EXTERN long scm_tc16_scwm_color;

EXTERN SCM str_black;
EXTERN SCM str_white;

/* FIXJTL: these should be private to menu.c I think */
EXTERN_SET(double menu_highlight_factor_val, 1.2);
EXTERN_SET(double menu_shadow_factor_val, 0.5);

typedef struct {
  Pixel pixel;
  SCM name;
  Bool borrowed;  /* true if the color was not allocated by scwm
                     and we're borrowing it from another application
                     (for a closest color match) */
} scwm_color;

#define COLOR_P(X) (SCM_NIMP(X) && gh_car(X) == (SCM)scm_tc16_scwm_color)
#define COLOR(X)  ((scwm_color *)(gh_cdr(X)))
#define XCOLOR(X) (COLOR(X)->pixel)
#define COLORNAME(X) (COLOR(X)->name)

/* Not as inefficient as it looks - comes down to a hash table lookup,
   after the first time. */

#define BLACK_COLOR make_color(str_black)
#define WHITE_COLOR make_color(str_white)

#define SAFE_COLOR(X) (COLOR_P((X))?XCOLOR((X)):0)
#define SAFE_COLOR_USE_DEF(X,def) (COLOR_P((X))?XCOLOR((X)):def)

#define SAFE_XCOLOR_OR_WHITE(X) (COLOR_P((X))?XCOLOR((X)):XCOLOR(WHITE_COLOR))
#define SAFE_XCOLOR_OR_BLACK(X) (COLOR_P((X))?XCOLOR((X)):XCOLOR(BLACK_COLOR))

#define COLOR_OR_SYMBOL_P(x) (COLOR_P((x)) || gh_symbol_p((x)))

#define DYNAMIC_COLOR_P(X) (gh_symbol_p((X))? \
			    COLOR_P(scm_symbol_binding(SCM_BOOL_F,(X))) : \
			    COLOR_P((X)))

#define DYNAMIC_SAFE_COLOR(X) (gh_symbol_p((X))? \
			       SAFE_COLOR(scm_symbol_binding(SCM_BOOL_F,(X))) : \
			       SAFE_COLOR((X)))

#define DYNAMIC_SAFE_COLOR_USE_DEF(X,def) (gh_symbol_p((X))? \
			       SAFE_COLOR_USE_DEF(scm_symbol_binding(SCM_BOOL_F,(X)),def) : \
			       SAFE_COLOR_USE_DEF((X),def))


/* GJB:FIXME:: colors have an especially poor interfaces --
   C code should use lower level primitives that
   these primitives just wrap */
SCM make_color (SCM cname);
SCM clear_color_cache_entry(SCM name);

SCM adjust_brightness (SCM color, double factor);

SCM set_highlight_factor_x (SCM factor);
SCM highlight_factor ();

SCM set_shadow_factor_x (SCM factor);
SCM shadow_factor ();

SCM set_menu_highlight_factor_x (SCM factor);
SCM menu_highlight_factor ();

SCM set_menu_shadow_factor_x (SCM factor);
SCM menu_shadow_factor ();

SCM set_highlight_foreground_x(SCM fg);
SCM set_highlight_background_x(SCM bg);
SCM set_menu_foreground_x(SCM fg);
SCM set_menu_background_x(SCM bg);
SCM set_menu_stipple_x(SCM st);

Pixel adjust_pixel_brightness(Pixel pixel, double factor);
Pixel *AllocNonlinearGradient(char *s_colors[], int clen[], 
			      int nsegs, int npixels);

 
#endif /* COLOR_H */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */


