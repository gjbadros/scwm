/* $Id$ 
 * color.h
 * (C) 1997, 1998 Maciej Stachowiak and Greg J. Badros
 */


#ifndef COLOR_H
#define COLOR_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <guile/gh.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xproto.h>
#include <X11/Xatom.h>


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

typedef struct {
  Pixel pixel;
  SCM name;
} scwm_color;

#define COLOR_P(X) (SCM_NIMP(X) && SCM_CAR(X) == (SCM)scm_tc16_scwm_color)
#define COLOR(X)  ((scwm_color *)(SCM_CDR(X)))
#define XCOLOR(X) (COLOR(X)->pixel)
#define COLORNAME(X) (COLOR(X)->name)

/* Not as inefficient as it looks - comes down to a hash table lookup,
   after the first time. */

#define BLACK_COLOR make_color(str_black)
#define WHITE_COLOR make_color(str_white)

/* Throw an error if X is not a color or a string. Usage here implies
   that make_color should indeed throw an error if it fails to parse or
   allocate the color. */

#define VALIDATE_COLOR(X, proc, pos) if (gh_string_p(X)) {X=make_color(X);} else if (!COLOR_P(X)) {scm_wrong_type_arg(proc,pos,X);};

/* FIXMS: is this needed? */
#define VALIDATE_COLOR_OR_NONE if (gh_string_p(X) {X=make_color(X)} else if (!COLOR_P(X) || X == SCM_BOOL_F) {scm_wrong_type_arg(proc,pos,X);};



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



SCM mark_color(SCM obj);
size_t free_color(SCM obj);
int print_color(SCM obj, SCM port, scm_print_state * pstate);

SCM color_p (SCM obj);
SCM color_properties (SCM color);
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

void init_color();

#endif /* COLOR_H */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */

