
/****************************************************************************
 * This module has been significantly modified by Maciej Stachowiak.
 * It may be used under the terms indicated by the copyright below.
 * Changes Copyright 1997, Maciej stachowiak
 ****************************************************************************/
/****************************************************************************
 * This module is all original code 
 * by Rob Nation 
 * Copyright 1993, Robert Nation
 *     You may use this code for any purpose, as long as the original
 *     copyright remains in the source code and all documentation
 ****************************************************************************/
/****************************************************************************
 * This module is all new
 * by Rob Nation 
 * Copyright 1993 Robert Nation. No restrictions are placed on this code,
 * as long as the copyright notice is preserved
 ****************************************************************************/

#include <guile/gh.h>
#include <libguile.h>
#include <X11/Xproto.h>
#include <X11/Xatom.h>

#define COLOR_IMPLEMENTATION

#include <config.h>
#include "scwm.h"
#include "color.h"
#include "screen.h"
#include "errors.h"
#include "borders.h"
#include "decor.h"
#include "colors.h"
#include "system.h"
#include "guile-compat.h"
#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif

#define COLOR_HASH_SIZE 20

static SCM color_hash_table = SCM_UNDEFINED;
static SCM protected_colors = SCM_UNDEFINED;

/* Not as inefficient as it looks - after the first time, this just
   amounts to a hash lookup. */



SCM_SYMBOL (sym_name,"name");
SCM_SYMBOL (sym_pixel,"pixel");

SCM
mark_color(SCM obj)
{
  SCM_SETGC8MARK(obj);
  scm_gc_mark (COLORNAME(obj));

  return SCM_BOOL_F;
}

size_t 
free_color(SCM obj)
{
  scwm_color *sc=COLOR(obj);
  XFreeColors(dpy, Scr.ScwmRoot.attr.colormap, &sc->pixel, 1, 0);
  free (sc);

  return 0;
}

int 
print_color(SCM obj, SCM port, scm_print_state * pstate)
{
  scm_puts("#<color ", port);
  scm_write(COLORNAME(obj), port);
  scm_putc('>', port);
  return 1;
}

SCWM_PROC (color_p, "color?", 1, 0, 0, 
           (SCM obj))
{
  return SCM_BOOL_FromBool(COLOR_P(obj));
}

/* FIXMS: Should we extend this to return r, g and b values? Should we
   perhaps even store those in the color structure? */

SCWM_PROC (color_properties, "color-properties", 1, 0, 0,
           (SCM color))
{
  VALIDATE_COLOR (color, s_color_properties, 1);

  return gh_list(gh_cons(sym_name, COLORNAME(color)),
		 gh_cons(sym_pixel, gh_int2scm(XCOLOR(color))),
		 SCM_UNDEFINED);
}

SCWM_PROC (make_color, "make-color", 1, 0, 0,
           (SCM cname))
{
  SCM answer;
  XColor color;
  char *cn;
  int len;
  Bool fBad = False;
  scwm_color *sc;

  if (!gh_string_p(cname)) {
    scm_wrong_type_arg(s_make_color, 1, cname);
  }

  answer=scm_hash_ref(color_hash_table, cname, SCM_BOOL_F);
  if (answer!=SCM_BOOL_F) {
    return answer;
  }

  cn = gh_scm2newstr(cname, &len);
  color.pixel = 0;


  /* MSFIX: for now just throw errors for the sake of robustness (!)
     make it nicer later. */
  if (!XParseColor(dpy, Scr.ScwmRoot.attr.colormap, cn, &color)) {
    free(cn);
    scwm_error("make-color",2);
#if 0   
    scwm_msg(WARN,s_make_color,"Unable to parse color `%s'",cn);
    fBad = True;
#endif
  } else if (!XAllocColor(dpy, Scr.ScwmRoot.attr.colormap, &color)) {
    free(cn);
    scwm_error("make-color",3);
#if 0
    scwm_msg(WARN,s_make_color,"Unable to allocate color `%s'",cn);
    fBad = True;
#endif
  }

  free(cn);

  if (fBad) {
    return SCM_BOOL_F;
  }

  sc = (scwm_color *) safemalloc(sizeof(scwm_color));
  sc->pixel = color.pixel;
  sc->name = cname;

  SCM_DEFER_INTS;
  SCM_NEWCELL(answer);
  SCM_SETCAR(answer, (SCM) scm_tc16_scwm_color);
  SCM_SETCDR(answer, (SCM) sc);
  SCM_ALLOW_INTS;


  scm_hash_set_x (color_hash_table, scm_string_copy(cname), answer);
  return (answer);  
}


SCWM_PROC (clear_color_cache_entry, "clear-color-cache-entry", 1, 0, 0,
           (SCM name))
{
  scm_hash_remove_x(color_hash_table, name);
  return SCM_UNSPECIFIED;
}



/* The hilight/shadow stuff should maybe be in a separate relief.c? */

#define SCALE 65535.0
#define HALF_SCALE (SCALE / 2)
typedef enum {
  R_MAX_G_MIN, R_MAX_B_MIN, 
  G_MAX_B_MIN, G_MAX_R_MIN, 
  B_MAX_R_MIN, B_MAX_G_MIN 
} MinMaxState;


/* Multiply the HLS-space lightness and saturation of the color by the
   given multiple, k - based on the way gtk does shading, but independently
   coded. Should give better relief colors for many cases than the old
   fvwm algorithm. */

/* FIXMS: This can probably be optimized more, examine later. */

void 
color_mult (unsigned short *red, 
	    unsigned short *green, 
	    unsigned short *blue, double k)
{
  if (*red == *green && *red == *blue) {
    double temp;
    /* A shade of gray */
    temp = k * (double) (*red);
    if (temp > SCALE) {
      temp = SCALE;
    }
    *red = (unsigned short)(temp);
    *green = *red;
    *blue = *red;
  } else {
    /* Non-zero saturation */
    double r, g, b;
    double min, max;
    double a, l, s;
    double delta;
    double middle;
    MinMaxState min_max_state;

    r = (double) *red;
    g = (double) *green;
    b = (double) *blue;

    if (r > g) {
      if (r > b) {
	max = r;
	if (g < b) {
	  min = g;
	  min_max_state = R_MAX_G_MIN;
	  a = b - g;
	} else {
	  min = b;
	  min_max_state = R_MAX_B_MIN;
	  a = g - b;
	}
      } else {
	max = b;
	min = g;
	min_max_state = B_MAX_G_MIN;
	a = r - g;
      }
    } else {
      if (g > b) {
	max = g;
	if (b < r) {
	  min = b;
	  min_max_state = G_MAX_R_MIN;
	  a = r - b;
	} else {
	  min = r;
	  min_max_state = G_MAX_B_MIN;
	  a = b - r; 
	}
      } else {
	max = b;
	min = r;
	min_max_state = B_MAX_R_MIN;
	a = g - r;
      }
    }
    
    delta = max - min;
    a = a / delta;

    l = (max + min) / 2;
    if (l <= HALF_SCALE) {
      s = max + min;
    } else {
      s = 2.0 * SCALE - (max + min);
    }
    s = delta/s;
    
    l *= k;
    if (l > SCALE) {
      l = SCALE;
    }
    s *= k;
    if (s > 1.0) {
      s = 1.0;
    }

    if (l <= HALF_SCALE) {
      max = l * (1 + s);
    } else {
      max = s * SCALE + l - s * l; 
    }

    min = 2 * l - max;
    delta = max - min;
    middle = min + delta * a;

    switch (min_max_state) {
    case R_MAX_G_MIN:
      r = max;
      g = min;
      b = middle;
      break;
    case R_MAX_B_MIN:
      r = max;
      g = middle;
      b = min;
      break;
    case G_MAX_B_MIN:
      r = middle;
      g = max;
      b = min;
      break;
    case G_MAX_R_MIN:
      r = min;
      g = max;
      b = middle;
      break;
    case B_MAX_G_MIN:
      r = middle;
      g = min;
      b = max;
      break;
    case B_MAX_R_MIN:
      r = min;
      g = middle;
      b = max;
      break;
    }

    *red = (unsigned short) r;
    *green = (unsigned short) g;
    *blue = (unsigned short) b;    
  }
}
	   
SCM
adjust_brightness (SCM color, double factor) {
  XColor c;
  char cnamebuf[19];

  c.pixel = XCOLOR(color);
  
  XQueryColor (dpy, Scr.ScwmRoot.attr.colormap, &c);
  color_mult(&c.red, &c.green, &c.blue, factor);
  sprintf(cnamebuf, "rgb:%.4hx/%.4hx/%.4hx", c.red, c.green, c.blue);
  cnamebuf[19]=0;

  return make_color(gh_str02scm(cnamebuf));
}


SCWM_PROC (make_relief_color, "make-relief-color", 2, 0, 0,
           (SCM color, SCM factor))
{
  double f;

  VALIDATE_COLOR (color, s_make_relief_color, 1);

  if (gh_number_p(factor) || ((f=gh_scm2double(factor)) < 0.0)) {
    scm_wrong_type_arg(s_make_relief_color, 1, factor);
  }

  return adjust_brightness(color, f);
}



/* Shadow and relief computation will now be similar to the way gtk
   does it; for now, the hilight and shadow factors may be set
   globally so people can experiment, but this is an unsatisfying
   solution for the long term, it needs to be settable for everything
   that takes a color that it will then use to construct shadows and
   hilights, and further, it should recompute these. For now, we'll
   trigger a recompute of everything we know about by setting all
   colors to themesleves.
   
   Perhaps explictly specifying the relief colors should be allowed
   eventually, and there should be a scheme-visible
   color brightness adjusting procedure. */


static void reset_decor_relief()
{
  ScwmDecor *fl;
  ScwmWindow *psw;

  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  if (fl->HiColors.bg != SCM_UNDEFINED) {
    set_hilight_background_x(fl->HiColors.bg);
  }

  for (psw=Scr.ScwmRoot.next; psw != NULL; psw=psw->next) {
    if (psw->fl == fl) {
      set_window_background_x(psw->BackColor, psw->schwin);
    }
  }
}


static void reset_menu_relief()
{
  /* FIXMS: Other menu colors? */

  if (Scr.MenuColors.bg != SCM_UNDEFINED) {
    set_menu_background_x(Scr.MenuColors.bg);
  }

}


SCWM_PROC (set_hilight_factor_x, "set-hilight-factor!", 1, 0, 0,
           (SCM factor))
{
  double f;
  ScwmDecor *fl;

  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  if (gh_number_p(factor) || ((f=gh_scm2double(factor)) < 0.0)) {
    scm_wrong_type_arg(s_set_hilight_factor_x, 1, factor);
  }

  fl->hilight_factor = f;

  /* Redraw hilights. */
  reset_decor_relief();
  return SCM_UNSPECIFIED;
}

SCWM_PROC (hilight_factor, "hilight-factor", 0, 0, 0,
           ())
{
  ScwmDecor *fl;

  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  return (gh_double2scm(fl->hilight_factor));
}


SCWM_PROC (set_shadow_factor_x, "set-shadow-factor!", 1, 0, 0,
           (SCM factor))
{
  double f;
  ScwmDecor *fl;

  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  if (gh_number_p(factor) || ((f=gh_scm2double(factor)) < 0.0)) {
    scm_wrong_type_arg(s_set_shadow_factor_x, 1, factor);
  }

  fl->shadow_factor = f;

  /* Redraw shadows. */
  reset_decor_relief();
  return SCM_UNSPECIFIED;
}


SCWM_PROC (shadow_factor, "shadow-factor", 0, 0, 0,
           ())
{
  ScwmDecor *fl;

  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  return (gh_double2scm(fl->shadow_factor));
}


double menu_hilight_factor_val = 1.2;
double menu_shadow_factor_val = 0.5;


SCWM_PROC (set_menu_hilight_factor_x, "set-menu-hilight-factor!", 1, 0, 0,
           (SCM factor))
{
  double f;
  if (gh_number_p(factor) || ((f=gh_scm2double(factor)) < 0.0)) {
    scm_wrong_type_arg(s_set_menu_hilight_factor_x, 1, factor);
  }

  menu_hilight_factor_val = f;

  /* Redraw hilights. */
  reset_menu_relief();
  return SCM_UNSPECIFIED;
}

SCWM_PROC (menu_hilight_factor, "menu-hilight-factor", 0, 0, 0,
           ())
{
  return (gh_double2scm(menu_hilight_factor_val));
}


SCWM_PROC (set_menu_shadow_factor_x, "set-menu-shadow-factor!", 1, 0, 0,
           (SCM factor))
{
  double f;
  if (gh_number_p(factor) || ((f=gh_scm2double(factor)) < 0.0)) {
    scm_wrong_type_arg(s_set_menu_shadow_factor_x, 1, factor);
  }

  menu_shadow_factor_val = f;

  /* Redraw shadows. */
  reset_menu_relief();
  return SCM_UNSPECIFIED;
}

SCWM_PROC (menu_shadow_factor, "menu-shadow-factor", 0, 0, 0,
           (SCM factor))
{
  return (gh_double2scm(menu_shadow_factor_val));
}


/* FIXMS: does this belong in util.c or something? 
   note, this function is fvwm-derived.*/

void 
redraw_hilight_window()
{
  ScwmWindow *hilight;
  
  if ((Scr.flags & WindowsCaptured) && (Scr.Hilite != NULL)) {
    hilight = Scr.Hilite;
    SetBorder(Scr.Hilite, False, True, True, None);
    SetBorder(hilight, True, True, True, None);
  }
}

/* FIXMS: Need to protect color objects in the below! */

SCWM_PROC (set_hilight_foreground_x, "set-hilight-foreground!", 1, 0, 0,
           (SCM fg) )
{ 
  ScwmDecor *fl;

  VALIDATE_COLOR(fg, s_set_hilight_foreground_x, 1);

  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  if (Scr.d_depth > 2) {
    fl->HiColors.fg = fg;
  } else {
    fl->HiColors.fg = BLACK_COLOR;
  }

  redraw_hilight_window();
  return (SCM_UNSPECIFIED);
}


/* FIXMS: eventually add option to pass background relief, shadow? */

/* FIXMS: the more I do this, the more I wish we had a nice GC
   abstraction. */

SCWM_PROC (set_hilight_background_x, "set-hilight-background!", 1, 0, 0,
           (SCM bg))
{
  XGCValues gcv;
  unsigned long gcm;
  ScwmDecor *fl;

  VALIDATE_COLOR(bg, s_set_hilight_background_x, 1);

  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  if (Scr.d_depth > 2) {
    fl->HiColors.bg = bg;
    fl->HiRelief.bg = adjust_brightness(bg,fl->shadow_factor);
    fl->HiRelief.fg = adjust_brightness(bg,fl->hilight_factor);
  } else {
    fl->HiColors.bg = WHITE_COLOR;
    fl->HiRelief.bg = BLACK_COLOR;
    fl->HiRelief.fg = WHITE_COLOR;
  }

  /* This gc-setting cruft is from fvwm. Just marking what code still
     has questionable license conditions. */

  gcm = GCFunction | GCPlaneMask | GCGraphicsExposures | GCLineWidth | GCForeground |
    GCBackground;
  gcv.foreground = XCOLOR(fl->HiRelief.fg);
  gcv.background = XCOLOR(fl->HiRelief.bg);
  gcv.fill_style = FillSolid;
  gcv.plane_mask = AllPlanes;
  gcv.function = GXcopy;
  gcv.graphics_exposures = False;
  gcv.line_width = 0;
  if (fl->HiReliefGC != NULL) {
    XFreeGC(dpy, fl->HiReliefGC);
  }
  fl->HiReliefGC = XCreateGC(dpy, Scr.Root, gcm, &gcv);
  gcv.foreground = XCOLOR(fl->HiRelief.bg);
  gcv.background = XCOLOR(fl->HiRelief.fg);
  if (fl->HiShadowGC != NULL) {
    XFreeGC(dpy, fl->HiShadowGC);
  }
  fl->HiShadowGC = XCreateGC(dpy, Scr.Root, gcm, &gcv);

  redraw_hilight_window();

  return (SCM_UNSPECIFIED);
}



SCWM_PROC (set_menu_foreground_x, "set-menu-foreground!", 1, 0, 0,
           (SCM fg) )
{ 
  XGCValues gcv;
  unsigned long gcm;

  VALIDATE_COLOR(fg, s_set_menu_foreground_x, 1);

  if (Scr.d_depth > 2) {
    Scr.MenuColors.fg = fg;
  } else {
    Scr.MenuColors.fg = BLACK_COLOR;
  }

  gh_vector_set_x(protected_colors,SCM_MAKINUM(0),Scr.MenuColors.fg);

  gcm = GCForeground;
  gcv.foreground = XCOLOR(Scr.MenuColors.fg);
  XChangeGC(dpy, Scr.MenuGC, gcm, &gcv);

  return (SCM_UNSPECIFIED);
}


SCWM_PROC (set_menu_background_x, "set-menu-background!", 1, 0, 0,
           (SCM bg) )
{ 
  XGCValues gcv;
  unsigned long gcm;

  VALIDATE_COLOR(bg, s_set_menu_background_x, 1);

  if (Scr.d_depth > 2) {
    Scr.MenuColors.bg = bg;
    Scr.MenuRelief.bg = adjust_brightness (bg, menu_shadow_factor_val);
    Scr.MenuRelief.fg = adjust_brightness (bg, menu_hilight_factor_val);
    Scr.MenuStippleColors.bg = Scr.MenuColors.bg;
  } else {
    Scr.MenuColors.bg = WHITE_COLOR;
    Scr.MenuRelief.bg = BLACK_COLOR;
    Scr.MenuRelief.fg = WHITE_COLOR;
    Scr.MenuStippleColors.bg = WHITE_COLOR;
  }

  gh_vector_set_x(protected_colors,SCM_MAKINUM(1),Scr.MenuColors.bg);
  gh_vector_set_x(protected_colors,SCM_MAKINUM(2),Scr.MenuRelief.fg);
  gh_vector_set_x(protected_colors,SCM_MAKINUM(3),Scr.MenuRelief.bg);
  gh_vector_set_x(protected_colors,SCM_MAKINUM(4),Scr.MenuStippleColors.bg);


  gcm = GCForeground | GCBackground;
  gcv.foreground = XCOLOR(Scr.MenuRelief.fg);
  gcv.background = XCOLOR(Scr.MenuRelief.bg);
  XChangeGC(dpy, Scr.MenuReliefGC, gcm, &gcv);
  gcv.foreground = XCOLOR(Scr.MenuRelief.bg);
  gcv.background = XCOLOR(Scr.MenuRelief.fg);
  XChangeGC(dpy, Scr.MenuShadowGC, gcm, &gcv);
  gcm = GCBackground;
  gcv.background = XCOLOR(Scr.MenuColors.bg);
  XChangeGC(dpy, Scr.MenuGC, gcm, &gcv);

  
  gcv.background = Scr.MenuStippleColors.bg;
  if (Scr.d_depth < 2) {
    gcm = GCBackground;
  } else {  
    gcm = GCBackground;
  }

  XChangeGC(dpy, Scr.MenuStippleGC, gcm, &gcv);

  return (SCM_UNSPECIFIED);
}


SCWM_PROC (set_menu_stipple_x, "set-menu-stipple!", 1, 0, 0,
           (SCM st) )
{
  XGCValues gcv;
  unsigned long gcm;

  VALIDATE_COLOR(st, s_set_menu_stipple_x, 1);

  if (Scr.d_depth > 2) {
    Scr.MenuStippleColors.fg = st;
  } else {
    Scr.MenuStippleColors.fg = BLACK_COLOR;
  }

  gh_vector_set_x(protected_colors,SCM_MAKINUM(5),Scr.MenuStippleColors.fg);

  gcv.foreground = Scr.MenuStippleColors.fg;
  if (Scr.d_depth < 2) {
    gcm = GCForeground | GCStipple | GCFillStyle;
    gcv.fill_style = FillStippled;
    gcv.stipple = Scr.gray_bitmap;
  } else {  
    gcm = GCForeground;
    gcv.foreground = Scr.MenuStippleColors.fg;
  }
  XChangeGC(dpy, Scr.MenuStippleGC, gcm, &gcv);

  return (SCM_UNSPECIFIED);

}

static scm_smobfuns color_smobfuns =
{
  &mark_color,
  &free_color,
  &print_color,
  0
};

void 
init_color()
{
  color_hash_table = 
    scm_make_weak_value_hash_table (SCM_MAKINUM(COLOR_HASH_SIZE));
  scm_protect_object(color_hash_table);

  protected_colors = scm_make_vector (SCM_MAKINUM(6), SCM_EOL, SCM_BOOL_F);
  scm_protect_object(protected_colors);

  str_black=gh_str02scm("black");
  scm_protect_object(str_black);
  str_white=gh_str02scm("white");
  scm_protect_object(str_white);

  /* XXX XXX remaining to fix for the color object migration: 
     fix all the Scheme code */

  scm_tc16_scwm_color = scm_newsmob (&color_smobfuns);
#ifndef SCM_MAGIC_SNARFER
#include "color.x"
#endif
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */

