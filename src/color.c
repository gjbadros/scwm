/* $Id$
 * Copyright (C) 1997, 1998, 1999, 2000 Maciej Stachowiak and Greg J. Badros
 * 
 * This module is derived from code by Rob Nation 
 * Copyright 1993, Robert Nation
 *     You may use this code for any purpose, as long as the original
 *     copyright remains in the source code and all documentation
 */

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include <guile/gh.h>
#include <libguile.h>
#include <X11/Xproto.h>
#include <X11/Xatom.h>
#include <X11/Xlib.h>
#include <float.h>

#define COLOR_IMPLEMENTATION
#include "color.h"

#include "scwm.h"
#include "screen.h"
#include "errors.h"
#include "borders.h"
#include "decor.h"
#include "guile-compat.h"
#include "module-interface.h"

#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif

#define COLOR_HASH_SIZE 20


static SCM color_hash_table = SCM_UNDEFINED;
static SCM protected_colors = SCM_UNDEFINED;

/**CONCEPT: Colors
  Colors are first-class objects. However, anywhere that a color is
taken as an argument, a string containing an X color specification
will also be accepted, and will be automatically converted to the
proper color object. Using the same color specifier string more than
once is not inefficient, as caching ensures that color objects are
shared.
*/

/**CONCEPT: Shadow and Highlight Factors
  Many decorations are supposed to look "three-dimensional".
To implement this, the decorations use three colors: the specified
decoration color, a brighter "highlight" color, and a darker "shadow"
color.  For "raised" decorations, the top and left edges are drawn in
the highlight color, and the bottom and right edges are drawn in the
shadow color; this is reversed for "sunken" decorations.

The highlight and shadow colors are computed from the decoration color
using the highlight and shadow factors.  The highlight factor should be
a floating-point number greater than 1.  If the highlight factor is
1, then the highlight color is the same as the decoration color;
the larger the highlight factor, the brighter the highlight color.
The shadow factor should be a floating-point number between 0 and 1.  If 
the shadow factor is 1, then the shadow color is the same as the
decoration color; the smaller the shadow factor, the darker the
shadow color.

(It is actually possible to give a highlight factor which is less than
1 (which makes the highlight color darker than the decoration color)
and a shadow factor which is greater than 1 (which makes the shadow
color brighter than the decoration color); the effect is to reverse
"raised" and "sunken" elements throughout the user interface.)

*/

SCM_SYMBOL (sym_name,"name");
SCM_SYMBOL (sym_pixel,"pixel");

SCM
mark_color(SCM obj)
{
  GC_MARK_SCM_IF_SET(COLORNAME(obj));
  return SCM_BOOL_F;
}

size_t 
free_color(SCM obj)
{
  scwm_color *sc=COLOR(obj);
  if (!sc->borrowed) {
    XFreeColors(dpy, Scr.ScwmRoot.attr.colormap, &sc->pixel, 1, 0);
  }
  FREE(sc);

  return 0;
}

int 
print_color(SCM obj, SCM port, scm_print_state *ARG_IGNORE(pstate))
{
  scm_puts("#<color ", port);
  scm_write(COLORNAME(obj), port);
  scm_putc('>', port);
  return 1;
}



SCM_DEFINE(color_p, "color?", 1, 0, 0, 
          (SCM obj),
"Returns #t if OBJ is a color object, otherwise #f.")
#define FUNC_NAME s_color_p
{
  return SCM_BOOL_FromBool(COLOR_P(obj));
}
#undef FUNC_NAME

/* MS:FIXME:: Should we extend this to return r, g and b values? Should we
   perhaps even store those in the color structure? */

SCM_DEFINE(color_properties, "color-properties", 1, 0, 0,
          (SCM color),
"Return an association list giving some properties of COLOR.\n\
Currently defined properties are 'name, the string name of the\n\
color, and 'pixel, the X pixel value it uses.")
#define FUNC_NAME s_color_properties
{
  VALIDATE_ARG_COLOR(1,color);

  return gh_list(gh_cons(sym_name, COLORNAME(color)),
		 gh_cons(sym_pixel, gh_int2scm(XCOLOR(color))),
		 SCM_UNDEFINED);
}
#undef FUNC_NAME

#undef ABS
#define ABS(x) (((x) < 0)? -(x): (x))

/* divisors are to do some simple (and fast) perceptual weighting
   (green matters less than red which matters less than blue) */
#define COLOR_DISTANCE(r1, g1, b1, r2, g2, b2) \
  ( ABS((g1)-(g2))/4 + ABS((r1)-(r2))/2 + ABS((b1)-(b2)) )

/* Return True on success, false on failure */
Bool
ClosestColor(XColor *pcolor)
{
  /* just deal with <=8 bit display
     as that's all I have right now,
     and that's where we need it most, anyway */
  if (Scr.d_depth > 8)
    return False;

  { /* scope */
    XColor cols[256];
    int i = 0;
    /* the desired color we are matching against */
    unsigned short 
      mr = pcolor->red,
      mg = pcolor->green,
      mb = pcolor->blue;
    int ccols = 1 << Scr.d_depth;
    /* track closest match distance in mindist, index into cols in icolBest */
    double mindist = DBL_MAX;
    int icolBest = -1;
    for (i = 0; i < ccols; ++i ) {
      cols[i].pixel = i;
      cols[i].flags = DoRed | DoGreen | DoBlue;
    }
    XQueryColors(dpy,Scr.ScwmRoot.attr.colormap, cols, ccols);
    for (i = 0; i < ccols; ++i) {
      unsigned short 
        r = cols[i].red,
        g = cols[i].green,
        b = cols[i].blue;
      double dist = COLOR_DISTANCE(mr,mg,mb,r,g,b);
      if (dist < mindist) {
        mindist = dist;
        icolBest = i;
      }
    }
    DBUG((WARN,"ClosestColor","Got match with error = %f",mindist));
    *pcolor = cols[icolBest];
    return True;
  }
}

SCM
ScmMakeColor(const char *cn, int *perror_status)
{
  SCM answer;
  scwm_color *sc;
  XColor color;
  Bool fBorrowedColor = False;
  if (perror_status)
    *perror_status = 0;

  color.pixel = 0;

  if (!XParseColor(dpy, Scr.ScwmRoot.attr.colormap, cn, &color)) {
    if (perror_status)
      *perror_status = 1; /* cannot parse */
    return SCM_BOOL_F;
  } else if (!XAllocColor(dpy, Scr.ScwmRoot.attr.colormap, &color)) {
    if (perror_status)
      *perror_status = 2; /* cannot alloc */
    if (ClosestColor(&color)) {
#ifdef WARN_CLOSE_MATCH /* GJB:FIXME:: need warnings to be run-time configurable */
      scwm_msg(WARN,"ScmMakeColor","Allocated a close match for: %s",cn);
#endif
      if (perror_status)
        *perror_status = 3; /* got close match */
      fBorrowedColor = True;    /* we are not responsible for freeing this color! */
    } else {
      scwm_msg(WARN,"ScmMakeColor","Cannot allocate color or a close match: %s -- using black",cn);
      return BLACK_COLOR;
    }
  }

  sc = NEW(scwm_color);
  sc->pixel = color.pixel;
  sc->name = gh_str02scm((char *)cn); /* GJB:FIXME:CONST */
  sc->borrowed = fBorrowedColor;

  SCWM_NEWCELL_SMOB(answer, scm_tc16_scwm_color, sc);

  /* GJB:FIXME:MS: why scm_string_copy? */
  scm_hash_set_x (color_hash_table, scm_string_copy(sc->name), answer);
  return answer;
}

/* Not as inefficient as it looks - after the first time, this just
   amounts to a hash lookup. */

SCM_DEFINE(make_color, "make-color", 1, 0, 0,
          (SCM cname),
"Return the color object corresponding to the X color specifier CNAME.\n\
If CNAME is not a valid X color name, or cannot be\n\
allocated, an error results.")
#define FUNC_NAME s_make_color
{
  SCM answer;
  char *cn;
  int error_status;

  VALIDATE_ARG_STR_NEWCOPY(1,cname,cn);

  answer = scm_hash_ref(color_hash_table, cname, SCM_BOOL_F);

  if (answer!=SCM_BOOL_F) {
    return answer;
  }

  answer = ScmMakeColor(cn,&error_status);
  switch (error_status) {
  case 1:
    gh_free(cn);
    scm_misc_error(FUNC_NAME,"Cannot parse color!",SCM_EOL);
    break;
  case 2:
    /* answer is now BLACK_COLOR, but we could do something
       smarter, perhaps */;
    break;
  }
  gh_free(cn);
  return answer;
}
#undef FUNC_NAME


SCM_DEFINE(clear_color_cache_entry, "clear-color-cache-entry", 1, 0, 0,
          (SCM name),
"Colors are cached by name. It is remotely possible that the\n\
meaning of a particular string as a color will change in your X\n\
server, if you try hard enough. For this unlikely eventuality,\n\
`clear-color-cache-entry' is provided - it removes the color\n\
associated with NAME from the color cache")
#define FUNC_NAME s_clear_color_cache_entry
{
  scm_hash_remove_x(color_hash_table, name);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



/* The highlight/shadow stuff should maybe be in a separate relief.c? */

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

/* MS:FIXME:: This can probably be optimized more, examine later. */

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

    if (r >= g) {
      if (r >= b) {
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
      if (g >= b) {
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
adjust_brightness(SCM color, double factor) {
  XColor c;
  char cnamebuf[19];

  c.pixel = XCOLOR(color);
  
  XQueryColor (dpy, Scr.ScwmRoot.attr.colormap, &c);
  color_mult(&c.red, &c.green, &c.blue, factor);
  sprintf(cnamebuf, "rgb:%.4hx/%.4hx/%.4hx", c.red, c.green, c.blue);
  cnamebuf[19]=0;

  return ScmMakeColor(cnamebuf, NULL);
}

Pixel
adjust_pixel_brightness(Pixel pixel, double factor)
{
  XColor c;
  c.pixel = pixel;
  
  XQueryColor (dpy, Scr.ScwmRoot.attr.colormap, &c);
  color_mult(&c.red, &c.green, &c.blue, factor);
  XAllocColor (dpy, Scr.ScwmRoot.attr.colormap, &c);

  return c.pixel;
}


SCM
invert_color(SCM color) {
  XColor c;
  char cnamebuf[19];

  c.pixel = XCOLOR(color);
  
  XQueryColor (dpy, Scr.ScwmRoot.attr.colormap, &c);
  sprintf(cnamebuf, "rgb:%.4hx/%.4hx/%.4hx", 
          0xffff-c.red, 0xffff-c.green, 0xffff-c.blue);
  cnamebuf[19]=0;

  return ScmMakeColor(cnamebuf, NULL);
}


/* Defined in error.c */
extern SCM sym_scwm_error;

static void 
nocolor(const char *note, const char *name)
{
  scwm_msg(ERR, "nocolor", "cannot %s color %s", note, name);
}

/*
 * Allocates a linear color gradient (veliaa@rpi.edu)
 * GJB:FIXME:: this should use color objects, not strings
 */
static Pixel *
AllocLinearGradient(char *s_from, char *s_to, int npixels)
{
  Pixel *pixels;
  XColor from, to, c;
  int r, dr, g, dg, b, db;
  int i = 0, got_all = 1;

  if (npixels < 1)
    return NULL;
  if (!s_from || !XParseColor(dpy, Scr.ScwmRoot.attr.colormap, s_from, &from)) {
    nocolor("parse", s_from);
    return NULL;
  }
  if (!s_to || !XParseColor(dpy, Scr.ScwmRoot.attr.colormap, s_to, &to)) {
    nocolor("parse", s_to);
    return NULL;
  }
  c = from;
  r = from.red;
  dr = (to.red - from.red) / npixels;
  g = from.green;
  dg = (to.green - from.green) / npixels;
  b = from.blue;
  db = (to.blue - from.blue) / npixels;
  pixels = NEWC(npixels,Pixel);
  c.flags = DoRed | DoGreen | DoBlue;
  for (; i < npixels; ++i) {
    if (!XAllocColor(dpy, Scr.ScwmRoot.attr.colormap, &c))
      got_all = 0;
    pixels[i] = c.pixel;
    c.red = (unsigned short) (r += dr);
    c.green = (unsigned short) (g += dg);
    c.blue = (unsigned short) (b += db);
  }
  if (!got_all) {
    char s[256];
    sprintf(s, "gradient %s to %s", s_from, s_to);
    nocolor("alloc", s);
    return NULL;
  }
  return pixels;
}


/*
 * Allocates a nonlinear color gradient (veliaa@rpi.edu)
 */
Pixel *
AllocNonlinearGradient(char *s_colors[], int clen[],
		       int nsegs, int npixels)
{
  Pixel *pixels = NEWC(npixels,Pixel);
  int i = 0, curpixel = 0, perc = 0;

  if (nsegs < 1) {
    scwm_msg(ERR, "AllocNonlinearGradient",
	     "must specify at least one segment");
    FREE(pixels);
    return NULL;
  }
  for (; i < npixels; i++)
    pixels[i] = 0;

  for (i = 0; (i < nsegs) && (curpixel < npixels) && (perc <= 100); ++i) {
    Pixel *p;
    int j = 0, n = clen[i] * npixels / 100;

    p = AllocLinearGradient(s_colors[i], s_colors[i + 1], n);
    if (!p) {
      scwm_msg(ERR, "AllocNonlinearGradient",
	       "could not allocate gradient");
      FREEC(pixels);
      return NULL;
    }
    for (; j < n; ++j)
      pixels[curpixel + j] = p[j];
    perc += clen[i];
    curpixel += n;
    FREEC(p);
  }
  for (i = curpixel; i < npixels; ++i)
    pixels[i] = pixels[i - 1];
  return pixels;
}


SCM_DEFINE(make_relief_color, "make-relief-color", 2, 0, 0,
          (SCM color, SCM factor),
"Convert a color into a new color appropriate for a relief.\n\
Multiplies the luminosity and saturation of COLOR by the\n\
positive floating point number FACTOR. Using a FACTOR smaller than 1\n\
will result in a dimmer color, suitable for use as a darker\n\
relief. Using a factor greater than 1 will result in a brighter color\n\
which is suitable for use as a highlight.")
#define FUNC_NAME s_make_relief_color
{
  double f;

  VALIDATE_ARG_COLOR(1,color);
  VALIDATE_ARG_DBL_MIN_COPY(2,factor,0.0,f);

  return adjust_brightness(color, f);
}
#undef FUNC_NAME


SCM_DEFINE(make_reversed_color, "make-reversed-color", 1, 0, 0,
          (SCM color),
"Return a new color that is opposite COLOR.\n\
Note that the returned color will not necessarily contrast with\n\
COLOR; (make-reversed-color \"gray50\") is almost indistinguishable\n\
from \"gray50\".")
#define FUNC_NAME s_make_reversed_color
{
  VALIDATE_ARG_COLOR(1,color);

  return invert_color(color);
}
#undef FUNC_NAME



/* Shadow and relief computation will now be similar to the way gtk
   does it; for now, the highlight and shadow factors may be set
   globally so people can experiment, but this is an unsatisfying
   solution for the long term, it needs to be settable for everything
   that takes a color that it will then use to construct shadows and
   highlights, and further, it should recompute these. For now, we'll
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
    set_highlight_background_x(fl->HiColors.bg);
  }

  for (psw=Scr.ScwmRoot.next; psw != NULL; psw=psw->next) {
    if (psw->fl == fl) {
      set_window_background_x(psw->BackColor, SCM_FROM_PSW(psw));
    }
  }
}

SCM_DEFINE(set_highlight_factor_x, "set-highlight-factor!", 1, 0, 0,
          (SCM factor),
"Use FACTOR to generate highlight colors for the current decor.\n\
FACTOR is a positive floating point number.")
#define FUNC_NAME s_set_highlight_factor_x
{
  double f;
  ScwmDecor *fl;

  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  VALIDATE_ARG_DBL_MIN_COPY(1,factor,0.0,f);

  fl->highlight_factor = f;

  /* Redraw highlights. */
  reset_decor_relief();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE(highlight_factor, "highlight-factor", 0, 0, 0,
          (),
"Return the current highlight factor.")
#define FUNC_NAME s_highlight_factor
{
  ScwmDecor *fl;

  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  return (gh_double2scm(fl->highlight_factor));
}
#undef FUNC_NAME



SCM_DEFINE(set_shadow_factor_x, "set-shadow-factor!", 1, 0, 0,
          (SCM factor),
"Use FACTOR to generate shadow colors in the current decor. \n\
FACTOR is a positive floating point number")
#define FUNC_NAME s_set_shadow_factor_x
{
  double f;
  ScwmDecor *fl;

  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  VALIDATE_ARG_DBL_MIN_COPY(1,factor,0.0,f);

  fl->shadow_factor = f;

  /* Redraw shadows. */
  reset_decor_relief();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE(shadow_factor, "shadow-factor", 0, 0, 0,
          (),
"Return the current shadow factor.")
#define FUNC_NAME s_shadow_factor
{
  ScwmDecor *fl;

  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  return (gh_double2scm(fl->shadow_factor));
}
#undef FUNC_NAME

/* FIXJTL: these should go away */
SCM_DEFINE(set_menu_highlight_factor_x, "set-menu-highlight-factor!", 1, 0, 0,
          (SCM factor),
"Use FACTOR to generate highlight colors for menus. \n\
FACTOR is a positive floating point number")
#define FUNC_NAME s_set_menu_highlight_factor_x
{
  double f;
  VALIDATE_ARG_DBL_MIN_COPY(1,factor,0.0,f);
  menu_highlight_factor_val = f;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE(menu_highlight_factor, "menu-highlight-factor", 0, 0, 0,
          (),
"Return the current menu highlight factor.")
#define FUNC_NAME s_menu_highlight_factor
{
  return (gh_double2scm(menu_highlight_factor_val));
}
#undef FUNC_NAME


SCM_DEFINE(set_menu_shadow_factor_x, "set-menu-shadow-factor!", 1, 0, 0,
          (SCM factor),
"Use FACTOR to generate shadow colors for menus. \n\
FACTOR is a positive floating point number")
#define FUNC_NAME s_set_menu_shadow_factor_x
{
  double f;
  VALIDATE_ARG_DBL_MIN_COPY(1,factor,0.0,f);
  menu_shadow_factor_val = f;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE(menu_shadow_factor, "menu-shadow-factor", 0, 0, 0,
          (),
"Return the current menu shadow factor.")
#define FUNC_NAME s_menu_shadow_factor
{
  return (gh_double2scm(menu_shadow_factor_val));
}
#undef FUNC_NAME


static void 
redraw_highlight_window()
{
  if (Scr.fWindowsCaptured && (Scr.Hilite != NULL)) {
    BroadcastConfig(M_CONFIGURE_WINDOW, Scr.Hilite);
    SetBorderX(Scr.Hilite, True, True, True, None, True);
  }
}

/* MS:FIXME:: Need to protect color objects in the below! */

SCM_DEFINE(set_highlight_foreground_x, "set-highlight-foreground!", 1, 0, 0,
          (SCM fg),
"Use FG for the foreground color of a window with the input focus.\n\
Applies to the current decor. This is used only for windows that don't\n\
have their own foreground color.")
#define FUNC_NAME s_set_highlight_foreground_x
{ 
  ScwmDecor *fl;

  VALIDATE_ARG_COLOR(1,fg);

  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  if (Scr.d_depth > 2) {
    fl->HiColors.fg = fg;
  } else {
    fl->HiColors.fg = BLACK_COLOR;
  }

  redraw_highlight_window();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (highlight_foreground, "highlight-foreground", 0, 0, 0,
           (),
"Return the default foreground color for windows with the input focus.\n\
Applies to the current decor. This is used only for windows that don't\n\
have their own foreground color.")
#define FUNC_NAME s_highlight_foreground
{ 
  ScwmDecor *fl;
  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  return (fl->HiColors.fg);
}
#undef FUNC_NAME


/* MS:FIXME:: eventually add option to pass background relief, shadow? */

/* MS:FIXME:: the more I do this, the more I wish we had a nice GC
   abstraction. */

SCM_DEFINE(set_highlight_background_x, "set-highlight-background!", 1, 0, 0,
          (SCM bg),
"Use BG as the background color for a window with the input focus.\n\
Applies to the current decor. This is used only for windows that don't\n\
have their own background color.")
#define FUNC_NAME s_set_highlight_background_x
{
  XGCValues gcv;
  unsigned long gcm;
  ScwmDecor *fl;

  VALIDATE_ARG_COLOR(1,bg);

  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  if (Scr.d_depth > 2) {
    fl->HiColors.bg = bg;
    fl->HiRelief.bg = adjust_brightness(bg,fl->shadow_factor);
    fl->HiRelief.fg = adjust_brightness(bg,fl->highlight_factor);
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

  redraw_highlight_window();

  return (SCM_UNSPECIFIED);
}
#undef FUNC_NAME


SCM_DEFINE (highlight_background, "highlight-background", 0, 0, 0,
           (),
"Return the default background color for windows with the input focus.\n\
Applies to the current decor. This is used only for windows that don't\n\
have their own background color.")
#define FUNC_NAME s_highlight_background
{ 
  ScwmDecor *fl;
  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  return (fl->HiColors.bg);
}
#undef FUNC_NAME


SCM_DEFINE(set_not_menu_foreground_x, "set-not-menu-foreground!", 1, 0, 0,
          (SCM fg),
"Use FG as the default foreground color for icons, titlebars, etc.")
#define FUNC_NAME s_set_not_menu_foreground_x
{ 
  VALIDATE_ARG_COLOR(1,fg);

  if (Scr.d_depth > 2) {
    Scr.NotMenuColors.fg = fg;
  } else {
    Scr.NotMenuColors.fg = BLACK_COLOR;
  }

  gh_vector_set_x(protected_colors,SCM_MAKINUM(0),Scr.NotMenuColors.fg);

  return (SCM_UNSPECIFIED);
}
#undef FUNC_NAME

SCM_DEFINE (not_menu_foreground, "not-menu-foreground", 0, 0, 0,
           (),
"Return the default foreground color for icons, titlebars, etc.")
#define FUNC_NAME s_not_menu_foreground
{ 
  return (Scr.NotMenuColors.fg);
}
#undef FUNC_NAME

SCM_DEFINE(set_not_menu_background_x, "set-not-menu-background!", 1, 0, 0,
          (SCM bg),
"Use BG as the default background color for icons, window frames, etc.")
#define FUNC_NAME s_set_not_menu_background_x
{ 
  VALIDATE_ARG_COLOR(1,bg);

  if (Scr.d_depth > 2) {
    Scr.NotMenuColors.bg = bg;
    Scr.NotMenuRelief.bg = adjust_brightness (bg, menu_shadow_factor_val);
    Scr.NotMenuRelief.fg = adjust_brightness (bg, menu_highlight_factor_val);
  } else {
    Scr.NotMenuColors.bg = WHITE_COLOR;
    Scr.NotMenuRelief.bg = BLACK_COLOR;
    Scr.NotMenuRelief.fg = WHITE_COLOR;
  }

  gh_vector_set_x(protected_colors,SCM_MAKINUM(1),Scr.NotMenuColors.bg);
  gh_vector_set_x(protected_colors,SCM_MAKINUM(2),Scr.NotMenuRelief.fg);
  gh_vector_set_x(protected_colors,SCM_MAKINUM(3),Scr.NotMenuRelief.bg);

  return (SCM_UNSPECIFIED);
}
#undef FUNC_NAME


SCM_DEFINE (not_menu_background, "not-menu-background", 0, 0, 0,
           (),
"Return the default background color for icons, window frames, etc.")
#define FUNC_NAME s_not_menu_background
{ 
  return (Scr.NotMenuColors.bg);
}
#undef FUNC_NAME

MAKE_SMOBFUNS(color);

void 
init_color()
{
  REGISTER_SCWMSMOBFUNS(color);

  color_hash_table = 
    scm_make_weak_value_hash_table (SCM_MAKINUM(COLOR_HASH_SIZE));
  scm_protect_object(color_hash_table);

  protected_colors = gh_make_vector (SCM_MAKINUM(4), SCM_BOOL_F);
  scm_protect_object(protected_colors);

  scm_protect_object(str_black=gh_str02scm("black"));
  scm_protect_object(str_white=gh_str02scm("white"));

#ifndef SCM_MAGIC_SNARFER
#include "color.x"
#endif
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta
 */
