/* $Id$ 
 * color.h
 * (C) 1997, 1998 Maciej Stachowiak and Greg J. Badros
 */


#ifndef COLOR_H
#define COLOR_H

#ifdef HAVE_CONFIG_H
#include <config.h>
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

EXTERN long scm_tc16_scwm_color;

EXTERN SCM str_black;
EXTERN SCM str_white;

/* FIXJTL: these should be private to menu.c I think */
EXTERN_SET(double menu_hilight_factor_val, 1.2);
EXTERN_SET(double menu_shadow_factor_val, 0.5);

typedef struct {
  Pixel pixel;
  SCM name;
} scwm_color;

#define COLOR_P(X) (SCM_NIMP(X) && gh_car(X) == (SCM)scm_tc16_scwm_color)
#define COLOR(X)  ((scwm_color *)(gh_cdr(X)))
#define XCOLOR(X) (COLOR(X)->pixel)
#define COLORNAME(X) (COLOR(X)->name)

/* Not as inefficient as it looks - comes down to a hash table lookup,
   after the first time. */

#define BLACK_COLOR make_color(str_black)
#define WHITE_COLOR make_color(str_white)

/* Throw an error if X is not a color or a string. Usage here implies
   that make_color should indeed throw an error if it fails to parse or
   allocate the color. */

#define VALIDATE_COLOR(X, proc, pos) do { if (gh_string_p(X)) {X=make_color(X);} else if (!COLOR_P(X)) {scm_wrong_type_arg(proc,pos,X);}; } while (0)

/* FIXMS: is this needed? */
#define VALIDATE_COLOR_OR_NONE do { if (gh_string_p(X) {X=make_color(X)} else if (!COLOR_P(X) || X == SCM_BOOL_F) {scm_wrong_type_arg(proc,pos,X);}; } while(0)



#define SAFE_COLOR(X) (COLOR_P((X))?XCOLOR((X)):0)

#define SAFE_XCOLOR_OR_WHITE(X) (COLOR_P((X))?XCOLOR((X))->pixel:XCOLOR(WHITE_COLOR)->pixel)
#define SAFE_XCOLOR_OR_BLACK(X) (COLOR_P((X))?XCOLOR((X))->pixel:XCOLOR(BLACK_COLOR)->pixel)

#define COLOR_OR_SYMBOL_P(x) (COLOR_P((x)) || gh_symbol_p((x)))

#define DYNAMIC_COLOR_P(X) (gh_symbol_p((X))? \
			    COLOR_P(scm_symbol_binding(SCM_BOOL_F,(X))) : \
			    COLOR_P((X)))

#define DYNAMIC_SAFE_COLOR(X) (gh_symbol_p((X))? \
			       SAFE_COLOR(scm_symbol_binding(SCM_BOOL_F,(X))) : \
			       SAFE_COLOR((X)))


/* FIXGJB: colors have an especially poor interfaces --
   C code should use lower level primitives that
   these primitives just wrap */
SCM make_color (SCM cname);
SCM clear_color_cache_entry(SCM name);

SCM adjust_brightness (SCM color, double factor);

SCM set_hilight_factor_x (SCM factor);
SCM hilight_factor ();

SCM set_shadow_factor_x (SCM factor);
SCM shadow_factor ();

SCM set_menu_hilight_factor_x (SCM factor);
SCM menu_hilight_factor ();

SCM set_menu_shadow_factor_x (SCM factor);
SCM menu_shadow_factor ();

SCM set_hilight_foreground_x(SCM fg);
SCM set_hilight_background_x(SCM bg);
SCM set_menu_foreground_x(SCM fg);
SCM set_menu_background_x(SCM bg);
SCM set_menu_stipple_x(SCM st);

Pixel adjust_pixel_brightness(Pixel pixel, double factor);
 
#endif /* COLOR_H */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */

