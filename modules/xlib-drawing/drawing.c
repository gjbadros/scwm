/* $Id$
 * (C) 1999 Jeffrey Nichols and Greg J. Badros
 */ 

/*
 * This module is derived from code by Rob Nation 
 * Copyright 1993, Robert Nation
 *     You may use this code for any purpose, as long as the original
 *     copyright remains in the source code and all documentation
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <X11/X.h>
#include <X11/Intrinsic.h>
#include <X11/keysym.h>

#include "drawing.h"
#include "scwm.h"
#include "screen.h"
#include "xmisc.h"

static GC DrawingGC;

static 
Bool xlib_point_pair_p(SCM point)
{
  if (!gh_pair_p(point))
    return False;
  return (gh_number_p(gh_car(point)) && gh_number_p(gh_cdr(point)));
}

/* assumes top_left is an xlib_point_pair */
static
void xlib_point_pair_get_values(SCM p, int *piX, int *piY)
{
  *piX = gh_scm2int(gh_car(p));
  *piY = gh_scm2int(gh_cdr(p));
}


/* MSFIX: Can't easily be a color w/o overlay planes-- needs to be really 
   fast to erase */
SCWM_PROC(xlib_set_drawing_mask_x, "xlib-set-drawing-mask!", 1, 0, 0,
          (SCM value))
     /** Set the drawing mask used by the xlib-* primitives.
VALUE is XORed with the background when dragging non-opaque move or
resize frames. VALUE should be an integer. */
#define FUNC_NAME s_xlib_set_drawing_mask_x
{
  XGCValues gcv;
  unsigned long gcm;

  SCM_REDEFER_INTS;

  if (!gh_number_p(value)) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 1, value);
  }
  gcm = GCFunction | GCLineWidth | GCForeground | GCSubwindowMode;
  gcv.function = GXxor;
  gcv.line_width = 0;
  gcv.foreground = gh_scm2long(value);
  gcv.subwindow_mode = IncludeInferiors;
  if (NULL != DrawingGC) {
    XFreeGC(dpy, DrawingGC);
  }
  DrawingGC = XCreateGC(dpy, Scr.Root, gcm, &gcv);
  SCM_REALLOW_INTS;
  return (value);
}
#undef FUNC_NAME


SCWM_PROC(xlib_draw_rectangle_x, "xlib-draw-rectangle!", 3, 0, 0,
	  (SCM top_left, SCM width, SCM height))
     /** Draws a rectangle to the screen using the Xlib call XDrawRectangle.
TOP-LEFT is the upper left point of the rectangle.  The rectangle is of size
WIDTH by HEIGHT.
TOP-LEFT is a point pair: (X . Y). */
#define FUNC_NAME s_xlib_draw_rectangle_x
{
  int iX, iY, iWidth, iHeight;

  SCM_REDEFER_INTS;

  if (!xlib_point_pair_p(top_left)) {
    scm_wrong_type_arg(FUNC_NAME, 1, top_left);
  }
  xlib_point_pair_get_values(top_left, &iX, &iY);

  if (!gh_number_p(width)) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 2, width);
  }
  iWidth = gh_scm2int(width);

  if (iWidth < 0) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 2, width);
  }

  if (!gh_number_p(height)) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 3, height);
  }
  iHeight = gh_scm2int(height);

  if (iHeight < 0) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 3, height);
  }

  SCM_REALLOW_INTS;

  XDrawRectangle(dpy, Scr.Root, DrawingGC, iX, iY, iWidth, iHeight);

  return SCM_UNDEFINED;
}
#undef FUNC_NAME


SCWM_PROC(xlib_draw_line_x, "xlib-draw-line!", 2, 0, 0,
	  (SCM p1, SCM p2))
     /** Draws a line using the Xlib call XDrawLine.
The line is drawn from P1 to P2.
Both P1 and P2 are pairs (X . Y) representing a point*/
#define FUNC_NAME s_xlib_draw_line_x
{
  int iX1, iY1, iX2, iY2;

  if (!xlib_point_pair_p(p1)) {
    scm_wrong_type_arg(FUNC_NAME, 1, p1);
  }
  xlib_point_pair_get_values(p1, &iX1, &iY1);

  if (!xlib_point_pair_p(p2)) {
    scm_wrong_type_arg(FUNC_NAME, 2, p2);
  }
  xlib_point_pair_get_values(p2, &iX2, &iY2);

  XDrawLine(dpy, Scr.Root, DrawingGC, iX1, iY1, iX2, iY2);

  return SCM_UNDEFINED;
}
#undef FUNC_NAME


SCWM_PROC(xlib_draw_arc_x, "xlib-draw-arc!", 5, 0, 0,
	  (SCM top_left, SCM width, SCM height, SCM angle1, SCM angle2))
     /** Draws a arc to the screen using the Xlib call XDrawArc.  
The arc is specified in terms of a rectangle, in which it is wholly
enclosed.  TOP-LEFT is a point pair for the upper left corner of the rectangle.  The
rectangle is of size WIDTH by HEIGHT.  The arc is drawn from ANGLE1 to
ANGLE2. Angles are specified in degrees (0.0 to 360.0).*/
#define FUNC_NAME s_xlib_draw_arc_x
{
  int iX, iY, iWidth, iHeight;
  double nAngle1, nAngle2;
  

  SCM_REDEFER_INTS;
  
  if (!xlib_point_pair_p(top_left)) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 1, top_left);
  }
  
  xlib_point_pair_get_values(top_left, &iX, &iY);

  if (!gh_number_p(width)) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 2, width);
  }
  iWidth = gh_scm2int(width);
  if (iWidth < 0) {
    gh_allow_ints();
    scm_misc_error(FUNC_NAME,"WIDTH must be non-negative",SCM_EOL);
  }

  if (!gh_number_p(height)) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 3, height);
  }
  iHeight = gh_scm2int(height);
  if (iHeight < 0) {
    gh_allow_ints();
    scm_misc_error(FUNC_NAME,"HEIGHT must be non-negative",SCM_EOL);
  }

  if (!gh_number_p(angle1)) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 4, angle1);
  }
  nAngle1 = gh_scm2double(angle1);

  if (!gh_number_p(angle2)) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 5, angle2);
  }
  nAngle2 = gh_scm2double(angle2);

  SCM_REALLOW_INTS;

  XDrawArc(dpy, Scr.Root, DrawingGC, iX, iY, iWidth, iHeight, 
	   (int) (nAngle1 * 64), (int) (nAngle2 * 64));

  return SCM_UNDEFINED;
}
#undef FUNC_NAME


SCWM_PROC(xlib_set_line_width_x, "xlib-set-line-width!", 1, 0, 0,
	  (SCM width))
     /** Sets the line width of the DrawingGC to WIDTH. */
#define FUNC_NAME s_xlib_set_line_width_x
{
  int iWidth;

  SCM_REDEFER_INTS;

  if ( !UNSET_SCM(width) ) {
    if (!gh_number_p(width)) {
      gh_allow_ints();
      scm_wrong_type_arg(FUNC_NAME, 1, width);
    }
    iWidth = gh_scm2int(width);
    if (iWidth < 0) {
      gh_allow_ints();
      scm_wrong_type_arg(FUNC_NAME, 1, width);
    }
  }

  SCM_REALLOW_INTS;

  XSetLineAttributes(dpy, DrawingGC, iWidth, LineSolid, CapButt, JoinMiter);

  return SCM_UNDEFINED;
}
#undef FUNC_NAME


SCWM_PROC(xlib_set_fill_style_x, "xlib-set-fill-style!", 1, 0, 0,
	  (SCM style))
     /** Sets the fill style of the DrawingGC to STYLE. */
#define FUNC_NAME s_xlib_set_fill_style_x
{
  int iStyle;

  SCM_REDEFER_INTS;

  if (!gh_number_p(style)) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 1, style);
  }
  iStyle = gh_scm2int(style);
  if (!(iStyle == FillSolid || iStyle == FillStippled ||
        iStyle == FillTiled || iStyle == FillOpaqueStippled)) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 1, style);
  }

  gh_allow_ints();

  XSetFillStyle(dpy, DrawingGC, iStyle);

  return SCM_UNDEFINED;
}
#undef FUNC_NAME


static void
init_drawing_gcs()
{
  XGCValues gcv;
  unsigned long gcm = GCFunction | GCLineWidth | GCForeground | GCSubwindowMode;
  gcv.function = GXxor;
  gcv.line_width = 0;
  gcv.foreground = 37; /* randomish */
  gcv.subwindow_mode = IncludeInferiors;
  DrawingGC = XCreateGC(dpy, Scr.Root, gcm, &gcv);
}

static void
init_drawing()
{
#ifndef SCM_MAGIC_SNARFER
#include "drawing.x"
#endif
  init_drawing_gcs();
}

void scm_init_app_scwm_xlib_drawing_module()
{
  scm_register_module_xxx("app scwm xlib-drawing", init_drawing);
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
