/* $Id$
 * overlay-plane.c
 * By Greg J. Badros -- 11-Oct-1998
 */

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include <X11/Xlib.h>
#include <X11/Xresource.h>
#include <X11/Xutil.h>
#include <X11/extensions/shape.h>

#include <guile/gh.h>
#include "guile-compat.h"
#include <libguile/dynl.h>

#include "scwm.h"
#include "screen.h"
#include "xmisc.h"

#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif

/* Display and screen_num are used as arguments to nearly every Xlib routine, 
 * so it simplifies routine calls to declare them global.  If there were 
 * additional source files, these variables would be declared extern in
 * them. */
extern int get_colors();
static Bool fHaveOverlayPlane = False;
int screen_num = 0;

/* pixel values */
unsigned long foreground, background_pixel, overlay_pixel_1, overlay_pixel_2;
/* passed back from get_color */
unsigned long overlay_plane_mask;

static GC overlay_gc, clear_gc;

SCM_DEFINE (has_overlay_plane_p, "has-overlay-plane?", 0, 0, 0,
           (),
"Return #t if the current screen has an overlay plane, #f otherwise.")
#define FUNC_NAME s_has_overlay_plane_p
{
  return scm_from_bool(fHaveOverlayPlane);
}
#undef FUNC_NAME

static void
get_GC_clear(Window win, GC *gc)
{
  XGCValues gcv;
  unsigned long gcm = GCFunction | GCPlaneMask | GCSubwindowMode;
  gcv.function = GXclear;
  gcv.plane_mask = overlay_plane_mask;
  gcv.subwindow_mode = IncludeInferiors;

  *gc = XCreateGC(dpy, win, gcm, &gcv);
}

static void
get_GC_overlay(Window win, GC *gc)
{
  XGCValues gcv;
  unsigned long gcm = ( GCFunction | GCLineWidth | 
                        GCForeground | GCBackground |
                        GCPlaneMask | GCSubwindowMode );
  gcv.function = GXcopy;
  gcv.line_width = 2;
  gcv.foreground = overlay_pixel_1;
  gcv.background = overlay_pixel_2;
  gcv.plane_mask = overlay_plane_mask;
  gcv.subwindow_mode = IncludeInferiors;

  *gc = XCreateGC(dpy, win, gcm, &gcv);
}

static void
place_overlay(Window win, int width, int height)
{
  int x, y;
  x = 0;
  y = 0;
  XFillRectangle(dpy, win, overlay_gc, x, y, width, height);
}


static void
remove_overlay(Window win)
{
  /* this clears the overlay plane set in the gc */
  XSetForeground(dpy, clear_gc, 0);
  XFillRectangle(dpy, win, clear_gc, 0, 0, 10000, 10000);
}



SCM_DEFINE (draw_overlay_plane, "draw-overlay-plane", 0, 0, 0,
           (),
"Draw a box in the overlay plane.\n\
This is just for demonstration and testing purposes for now.")
#define FUNC_NAME s_draw_overlay_plane
{
  if (fHaveOverlayPlane) {
    place_overlay(Scr.Root, 200, 200);
  }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (hide_overlay_plane, "hide-overlay-plane", 0, 0, 0,
           (),
"Draw a box in the overlay plane.\n\
This is just for demonstration and testing purposes for now.")
#define FUNC_NAME s_hide_overlay_plane
{
  if (fHaveOverlayPlane) {
    remove_overlay(Scr.Root);
  }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



static void
init_overlay_plane()
{

  fHaveOverlayPlane = get_colors();
  if (fHaveOverlayPlane) {
    get_GC_overlay(Scr.Root, &overlay_gc);
    get_GC_clear(Scr.Root, &clear_gc);
  }

#ifndef SCM_MAGIC_SNARFER
#include "overlay-plane.x"
#endif
}

void scm_init_app_scwm_overlay_plane_module()
{
  scm_register_module_xxx("app scwm overlay-plane", init_overlay_plane);
}

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
