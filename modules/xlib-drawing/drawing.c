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


SCWM_PROC(xlib_draw_rectangle_x, "xlib-draw-rectangle!", 4, 0, 0,
	  (SCM x, SCM y, SCM width, SCM height))
     /** Draws a rectangle to the screen using the Xlib call XDrawRectangle.
( X, Y ) is the upper left point of the rectangle.  The rectangle is of size
WIDTH by HEIGHT. */
#define FUNC_NAME s_xlib_draw_rectangle_x
{
  int iX, iY, iWidth, iHeight;

  SCM_REDEFER_INTS;

  if (!gh_number_p(x)) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 1, x);
  }
  iX = gh_scm2int(x);

  if (!gh_number_p(y)) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 2, y);
  }
  iY = gh_scm2int(y);

  if (!gh_number_p(width)) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 3, width);
  }
  iWidth = gh_scm2int(width);

  if (iWidth < 0) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 3, width);
  }

  if (!gh_number_p(height)) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 4, height);
  }
  iHeight = gh_scm2int(height);

  if (iHeight < 0) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 4, height);
  }

  SCM_REALLOW_INTS;

  XDrawRectangle(dpy, Scr.Root, DrawingGC, iX, iY, iWidth, iHeight);

  return SCM_UNDEFINED;
}
#undef FUNC_NAME


SCWM_PROC(xlib_draw_line_x, "xlib-draw-line!", 4, 0, 0,
	  (SCM x1, SCM y1, SCM x2, SCM y2))
     /** Draws a line using the Xlib call XDrawLine.
The line is drawn from ( X1, Y1 ) to ( X2, Y2 ).  */
#define FUNC_NAME s_xlib_draw_line_x
{
  int iX1, iY1, iX2, iY2;

  SCM_REDEFER_INTS;

  if (!gh_number_p(x1)) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 1, x1);
  }
  iX1 = gh_scm2int(x1);

  if (!gh_number_p(y1)) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 2, y1);
  }
  iY1 = gh_scm2int(y1);

  if (!gh_number_p(x2)) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 3, x2);
  }
  iX2 = gh_scm2int(x2);

  if (!gh_number_p(y2)) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 4, y2);
  }
  iY2 = gh_scm2int(y2);

  SCM_REALLOW_INTS;

  XDrawLine(dpy, Scr.Root, DrawingGC, iX1, iY1, iX2, iY2);

  return SCM_UNDEFINED;
}
#undef FUNC_NAME


SCWM_PROC(xlib_draw_arc_x, "xlib-draw-arc!", 6, 0, 0,
	  (SCM x, SCM y, SCM width, SCM height, SCM angle1, SCM angle2))
     /** Draws a arc to the screen using the Xlib call XDrawArc.  The arc is
specified in terms of a rectangle, in which it is wholly enclosed.
( X, Y ) is at the upper left corner of the rectangle.  The rectangle is of 
size WIDTH by HEIGHT.  The arc is drawn from ANGLE1 to ANGLE2. Angles are
specified in degrees (0.0 to 360.0).*/
#define FUNC_NAME s_xlib_draw_arc_x
{
  int iX, iY, iWidth, iHeight;
  double nAngle1, nAngle2;

  SCM_REDEFER_INTS;
  
  if (!gh_number_p(x)) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 1, x);
  }
  iX = gh_scm2int(x);

  if (!gh_number_p(y)) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 2, y);
  }
  iY = gh_scm2int(y);

  if (!gh_number_p(width)) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 3, width);
  }
  iWidth = gh_scm2int(width);
  if (iWidth < 0) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 3, width);
  }

  if (!gh_number_p(height)) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 4, height);
  }
  iHeight = gh_scm2int(height);
  if (iHeight < 0) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 4, height);
  }

  if (!gh_number_p(angle1)) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 5, angle1);
  }
  nAngle1 = gh_scm2double(angle1);

  if (!gh_number_p(angle2)) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 5, angle2);
  }
  nAngle2 = gh_scm2double(angle2);

  SCM_REALLOW_INTS;

  XDrawArc(dpy, Scr.Root, DrawingGC, iX, iY, iWidth, iHeight, 
	   (int) nAngle1 * 64, (int) nAngle2 * 64);

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
