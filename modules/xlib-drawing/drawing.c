/* $Id$
 * Copyright (C) 1999, 2000 Jeffrey Nichols and Greg J. Badros
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
 */

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include <stdio.h>
#include <X11/X.h>
#include <X11/Intrinsic.h>
#include <X11/keysym.h>

#include "drawing.h"
#include "scwm.h"
#include "guile-compat.h"
#include "screen.h"
#include "xmisc.h"
#include "errors.h"

static GC DrawingGC;

#define VALIDATE_ARG_XLIBPP_COPY(pos, scm, x, y) \
  do { \
     if (!xlib_point_pair_p(scm)) SCWM_WRONG_TYPE_ARG(pos,scm); \
     else xlib_point_pair_get_values(scm, &x, &y); \
  } while (0)

static 
Bool xlib_point_pair_p(SCM point)
{
  if (!scm_to_bool(scm_pair_p(point)))
    return False;
  return scm_is_number(scm_car(point)) && scm_is_number(scm_cdr(point));
}

/* assumes top_left is an xlib_point_pair */
static
void xlib_point_pair_get_values(SCM p, int *piX, int *piY)
{
  *piX = scm_to_int(scm_car(p));
  *piY = scm_to_int(scm_cdr(p));
}


/* MSFIX: Can't easily be a color w/o overlay planes-- needs to be really 
   fast to erase */
SCM_DEFINE(xlib_set_drawing_mask_x, "xlib-set-drawing-mask!", 1, 0, 0,
          (SCM value),
"Set the drawing mask used by the xlib-* primitives.\n\
VALUE is XORed with the background when dragging non-opaque move or\n\
resize frames. VALUE should be an integer.")
#define FUNC_NAME s_xlib_set_drawing_mask_x
{
  XGCValues gcv;
  unsigned long gcm;
  int val;

  VALIDATE_ARG_INT_MIN_COPY(1,value,0,val);

  gcm = GCFunction | GCForeground;
  gcv.function = GXxor;
  gcv.foreground = val;
  gcv.subwindow_mode = IncludeInferiors;
  XChangeGC(dpy, DrawingGC, gcm, &gcv);
  return (value);
}
#undef FUNC_NAME


SCM_DEFINE(xlib_draw_rectangle_x, "xlib-draw-rectangle!", 3, 0, 0,
	  (SCM top_left, SCM width, SCM height),
"Draws a rectangle to the screen using the Xlib call XDrawRectangle.\n\
TOP-LEFT is the upper left point of the rectangle.  The rectangle is of size\n\
WIDTH by HEIGHT.\n\
TOP-LEFT is a point pair: (X . Y).")
#define FUNC_NAME s_xlib_draw_rectangle_x
{
  int iX, iY, iWidth, iHeight;

  VALIDATE_ARG_XLIBPP_COPY(1,top_left,iX,iY);
  VALIDATE_ARG_INT_MIN_COPY(2,width,0,iWidth);
  VALIDATE_ARG_INT_MIN_COPY(3,height,0,iHeight);

  XDrawRectangle(dpy, Scr.Root, DrawingGC, iX, iY, iWidth, iHeight);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE(xlib_draw_line_x, "xlib-draw-line!", 2, 0, 0,
	  (SCM p1, SCM p2),
"Draws a line using the Xlib call XDrawLine.\n\
The line is drawn from P1 to P2.\n\
Both P1 and P2 are pairs (X . Y) representing points.")
#define FUNC_NAME s_xlib_draw_line_x
{
  int iX1, iY1, iX2, iY2;

  VALIDATE_ARG_XLIBPP_COPY(1,p1,iX1,iY1);
  VALIDATE_ARG_XLIBPP_COPY(2,p2,iX2,iY2);

  XDrawLine(dpy, Scr.Root, DrawingGC, iX1, iY1, iX2, iY2);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE(xlib_draw_arc_x, "xlib-draw-arc!", 5, 0, 0,
	  (SCM top_left, SCM width, SCM height, SCM angle1, SCM angle2),
"Draws a arc to the screen using the Xlib call XDrawArc.\n\
The arc is specified in terms of a rectangle, in which it is wholly\n\
enclosed.  TOP-LEFT is a point pair for the upper left corner of the rectangle.  The\n\
rectangle is of size WIDTH by HEIGHT.  The arc is drawn from ANGLE1 to\n\
ANGLE2. Angles are specified in degrees (0.0 to 360.0).")
#define FUNC_NAME s_xlib_draw_arc_x
{
  int iX, iY, iWidth, iHeight;
  double nAngle1, nAngle2;
  
  VALIDATE_ARG_XLIBPP_COPY(1,top_left,iX,iY);
  VALIDATE_ARG_INT_MIN_COPY(2,width,0,iWidth);
  VALIDATE_ARG_INT_MIN_COPY(3,height,0,iHeight);
  VALIDATE_ARG_DBL_COPY(4,angle1,nAngle1);
  VALIDATE_ARG_DBL_COPY(5,angle2,nAngle2);

  XDrawArc(dpy, Scr.Root, DrawingGC, iX, iY, iWidth, iHeight, 
	   (int) (nAngle1 * 64), (int) (nAngle2 * 64));

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_SYMBOL(sym_solid,"solid");
SCWM_SYMBOL(sym_on_off_dash,"on-off-dash");
SCWM_SYMBOL(sym_double_dash,"double-dash");

SCM_DEFINE(xlib_set_line_attributes_x, "xlib-set-line-attributes!", 1, 1, 0,
	  (SCM width, SCM style),
"Sets the line width of the DrawingGC to WIDTH and style to STYLE.\n\
One of 'solid (default), 'on-off-dash, or 'double-dash should\n\
be given as STYLE.")
#define FUNC_NAME s_xlib_set_line_attributes_x
{
  int iWidth;
  int iStyle = 0;
  /* Be sure we registered these symbols */
  assert(sym_solid && sym_on_off_dash && sym_double_dash);
  VALIDATE_ARG_INT_COPY_USE_DEF(1,width,iWidth,0);
  if (UNSET_SCM(style)) iStyle = LineSolid; /* default */
  else if (sym_solid == style) iStyle = LineSolid;
  else if (sym_on_off_dash == style) iStyle = LineOnOffDash;
  else if (sym_double_dash == style) iStyle = LineDoubleDash;
  else {
    scwm_error(FUNC_NAME,"STYLE must be one of 'solid 'on-off-dash 'double-dash.");
  }
  XSetLineAttributes(dpy, DrawingGC, iWidth, iStyle, CapButt, JoinMiter);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE(xlib_set_fill_style_x, "xlib-set-fill-style!", 1, 0, 0,
	  (SCM style),
"Sets the fill style of the DrawingGC to STYLE.\n\
One of FillSolid (0), FillTiled (1), FillStippled (2), or FillOpaqueStippled (3)\n\
should be given as STYLE.")
#define FUNC_NAME s_xlib_set_fill_style_x
{
  int iStyle;
  VALIDATE_ARG_INT_RANGE_COPY(1,style,FillSolid,FillOpaqueStippled,iStyle);

  XSetFillStyle(dpy, DrawingGC, iStyle);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


static void
init_drawing_gc()
{
  XGCValues gcv;
  unsigned long gcm = GCFunction | GCLineWidth | GCForeground | GCSubwindowMode;
  int Mscreen = DefaultScreen(dpy);
  int planes = DisplayPlanes(dpy,Mscreen);
  gcv.function = GXxor;
  gcv.line_width = 0;
  gcv.foreground = ((long) pow(2,planes)) - 1;
  gcv.subwindow_mode = IncludeInferiors;
  DrawingGC = XCreateGC(dpy, Scr.Root, gcm, &gcv);
}

static void
init_drawing()
{
#ifndef SCM_MAGIC_SNARFER
#include "drawing.x"
#endif
  init_drawing_gc();
}

void scm_init_app_scwm_xlib_drawing_module()
{
  scm_register_module_xxx("app scwm xlib-drawing", init_drawing);
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
