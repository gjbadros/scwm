/* $Id$
 *
 * Copyright (C) 1997, 1998, 1999, 2000 Maciej Stachowiak and Greg J. Badros
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

/****************************************************************************
 * This module has been significantly modified by Maciej Stachowiak.
 * It may be used under the terms of the fvwm copyright (see COPYING.FVWM).
 * Changes Copyright 1997, 1998, Maciej Stachowiak and Greg J. Badros
 ****************************************************************************/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <guile/gh.h>
#include "guile-compat.h"
#include <ctype.h>
#include <unistd.h>

#include "deskpage.h"

#include "scwm.h"
#include "screen.h"
#include "module-interface.h"
#include "virtual.h"
#include "callbacks.h"

#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif

SCWM_HOOK(desk_size_change_hook,"desk-size-change-hook", 2,
"This hook is invoked whenever the desk size is changed.  It is
called with two arguments, both integers, which are the width and
height of the new desk size in screens.");

/**CONCEPT: Desks 

  Multiple virtual desktops are supported. A virtual desktop may be
bigger than the physical screen, in which case the current viewport on
the desktop may be moved around, as described in the Viewports entry.
Desks are identified by integers. There is currently an arbitrary
limit on the number of desks, but it should be much higher than anyone
will ever need. You can change the current desk with
`set-current-desk!'; find out what the current desk is with
`current-desk'; and set the desk a window is on with
`set-window-desk!'.
*/

SCWM_PROC(set_current_desk_x, "set-current-desk!", 1, 0, 0,
          (SCM desk),
"Change the current desk to DESK. DESK should be an integer\n\
small enough to fit in one machine word.")
#define FUNC_NAME s_set_current_desk_x
{
  int d;
  VALIDATE_ARG_INT_MIN_COPY(1,desk,0,d);

  changeDesks(0, d);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(current_desk, "current-desk", 0, 0, 0,
          (),
"Returns the integer identifying the current desk.")
#define FUNC_NAME s_current_desk
{
  return SCM_MAKINUM(Scr.CurrentDesk);
}
#undef FUNC_NAME


/**CONCEPT: Viewports 
  The current viewport is the area of the current desk that may be
seen on the physical screen. Since a desk can be larger than the
physical screen size, the viewport can move around the desk.

  Viewports give rise to two concepts of coordinates.  A viewport
coordinate is relative to the current viewport (i.e., it is the 
coordinate you actually see on the screen).  A virtual coordinate
is relative to the origin of the current desk.
*/

SCWM_PROC(set_viewport_position_x, "set-viewport-position!", 2, 0, 0,
          (SCM x, SCM y),
"Position the upper left corner of the viewport at coordinates X, Y.\n\
X and Y are given in pixels.  Does not affect the current desk.")
#define FUNC_NAME s_set_viewport_position_x
{
  int cx, cy;
  VALIDATE_ARG_INT_COPY(1,x,cx);
  VALIDATE_ARG_INT_COPY(2,y,cy);

  MoveViewport(cx,cy);
  return (SCM_UNSPECIFIED);
}
#undef FUNC_NAME

SCWM_PROC(viewport_position, "viewport-position", 0, 0, 0,
          (),
"Returns the current position of the viewport in pixels.\n\
The returned value is a list of the x and y positions.")
#define FUNC_NAME s_viewport_position
{
  return gh_list(SCM_MAKINUM(Scr.Vx),
                 SCM_MAKINUM(Scr.Vy),
                 SCM_UNDEFINED);
}
#undef FUNC_NAME


SCWM_PROC(set_edge_x_scroll_x, "set-edge-x-scroll!", 1, 0, 0,
          (SCM pixels),
"Set the horizontal edge scroll increment to PIXELS.\n\
The horizontal edge scroll setting is the amount by which the viewport\n\
will scroll when the mouse hits the left or right edge. Use `%x' to\n\
convert from a percent of screen size to pixels.")
#define FUNC_NAME s_set_edge_x_scroll_x
{
  int pix;
  VALIDATE_ARG_INT_MIN_COPY(1,pixels,0,pix);

  Scr.EdgeScrollX = pix;
  checkPanFrames();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(edge_x_scroll, "edge-x-scroll", 0, 0, 0,
          (),
"Return the horizontal edge scroll increment as set by `set-edge-x-scroll!'.")
#define FUNC_NAME s_edge_x_scroll
{
  return (gh_int2scm(Scr.EdgeScrollX));
}
#undef FUNC_NAME


SCWM_PROC(set_edge_y_scroll_x, "set-edge-y-scroll!", 1, 0, 0,
          (SCM pixels),
"Set the vertical edge scroll increment to PIXELS.\n\
The vertical edge scroll setting is the amount by which the viewport\n\
will scroll when the mouse hits the top or bottom edge. Use `%y' to\n\
convert from a percent of screen size to pixels.")
#define FUNC_NAME s_set_edge_y_scroll_x
{
  int pix;
  VALIDATE_ARG_INT_MIN_COPY(1,pixels,0,pix);

  Scr.EdgeScrollY = pix;
  checkPanFrames();

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(edge_y_scroll, "edge-y-scroll", 0, 0, 0,
          (),
"Return the vertical edge scroll increment as set by `set-edge-y-scroll!'.")
#define FUNC_NAME s_edge_y_scroll
{
  return (gh_int2scm(Scr.EdgeScrollY));
}
#undef FUNC_NAME



SCWM_PROC(set_edge_x_wrap_x, "set-edge-x-wrap!", 1, 0, 0,
          (SCM flag),
"Set whether to wrap pointer around horizontal edges.\n\
If the boolean value FLAG is #t, the pointer will wrap from the right\n\
edge of the desktop to the left of the display as it moves off the\n\
right edge, and vice-versa. See also `set-edge-y-wrap!'")
#define FUNC_NAME s_set_edge_x_wrap_x
{
  VALIDATE_ARG_BOOL_COPY(1,flag,Scr.fEdgeWrapX);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(edge_x_wrap, "edge-x-wrap", 0, 0, 0,
          (),
"Return the current horizonatal edge wrap setting as set by `set-edge-x-wrap!'.")
#define FUNC_NAME s_edge_x_wrap
{
  return SCM_BOOL_FromBool(Scr.fEdgeWrapX);
}
#undef FUNC_NAME


SCWM_PROC(set_edge_y_wrap_x, "set-edge-y-wrap!", 1, 0, 0,
          (SCM flag),
"Set whether to wrap pointer around vertical edges.\n\
If the boolean value FLAG is #t, the pointer will wrap from the bottom\n\
edge of the desktop to the top of the display as it moves off the very\n\
bottom edge, and vice-versa. See also `set-edge-x-wrap!'")
#define FUNC_NAME s_set_edge_y_wrap_x
{
  VALIDATE_ARG_BOOL_COPY(1,flag,Scr.fEdgeWrapY);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(edge_y_wrap, "edge-y-wrap", 0, 0, 0,
          (),
"Return the current vertical edge wrap setting as set by `set-edge-y-wrap!'.")
#define FUNC_NAME s_edge_y_wrap
{
  return SCM_BOOL_FromBool(Scr.fEdgeWrapY);
}
#undef FUNC_NAME


SCWM_PROC(set_edge_scroll_delay_x, "set-edge-scroll-delay!", 1, 0, 0,
          (SCM ms),
"Set the edge scroll delay to MS milliseconds.\n\
When the mouse pointer hits the edge of the screen, it must stay there\n\
for at least the edge scroll delay amount before the desktop will be\n\
scrolled. If this parameter is #f, the viewport will not scroll at all\n\
at the screen edge.")
#define FUNC_NAME s_set_edge_scroll_delay_x
{
  VALIDATE_ARG_INT_COPY_USE_DEF(1,ms,Scr.ScrollResistance,-1);
  if (Scr.ScrollResistance >= 10000) { 
    scwm_msg(WARN, FUNC_NAME, "Possible deprecated use of "
             "`set-edge-scroll-delay!' detected. Give #f rather than ms "
             ">= 10000 to prohibit scrolling.");
  }
  return SCM_UNSPECIFIED; 
}
#undef FUNC_NAME


SCWM_PROC(edge_scroll_delay, "edge-scroll-delay", 0, 0, 0,
          (),
"Return the edge scroll delay (in ms) as set by `set-edge-scroll-delay!'.")
#define FUNC_NAME s_edge_scroll_delay
{
  return gh_int2scm(Scr.ScrollResistance);
}
#undef FUNC_NAME




SCWM_PROC(set_edge_move_threshold_x, "set-edge-move-threshold!", 1, 0, 0,
          (SCM pixels),
"Set the edge move threshold to PIXELS.\n\
Attempts to move a window so that it is off the edge of the screen by\n\
fewer than PIXELS pixels will leave the window entirely onscreen.")
#define FUNC_NAME s_set_edge_move_threshold_x
{
  int pix;
  VALIDATE_ARG_INT_MIN_COPY(1,pixels,0,pix);

  Scr.MoveResistance = pix;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(edge_move_threshold, "edge-move-threshold", 0, 0, 0,
          (),
"Return the edge move threshold as set by `set-edge-move-threshold!'.")
#define FUNC_NAME s_edge_move_threshold
{
  return gh_int2scm(Scr.MoveResistance);
}
#undef FUNC_NAME



SCWM_PROC(set_desk_size_x, "set-desk-size!", 2, 0, 0,
          (SCM width, SCM height),
"Sets the desk size to WIDTH, HEIGHT.\n\
Both numbers are given in units of the physical screen size.  For\n\
example <informalexample><programlisting>(set-desk-size! 3 3)\n\
</programlisting></informalexample> creates a virtual world 9 times the\n\
size of the physical display.")
#define FUNC_NAME s_set_desk_size_x
{
  int w, h;
  VALIDATE_ARG_INT_MIN_COPY(1,width,1,w);
  VALIDATE_ARG_INT_MIN_COPY(2,height,1,h);
  Scr.VxMax = w;
  Scr.VyMax = h;
  Scr.VxMax = Scr.VxMax * Scr.DisplayWidth - Scr.DisplayWidth;
  Scr.VyMax = Scr.VyMax * Scr.DisplayHeight - Scr.DisplayHeight;
  if (Scr.VxMax < 0)
    Scr.VxMax = 0;
  if (Scr.VyMax < 0)
    Scr.VyMax = 0;
  Broadcast(M_NEW_PAGE, 5, Scr.Vx, Scr.Vy, Scr.CurrentDesk, Scr.VxMax, Scr.VyMax, 0, 0);

  checkPanFrames();


  call2_hooks(desk_size_change_hook,
              gh_ulong2scm(Scr.VxMax / Scr.DisplayWidth + 1),
              gh_ulong2scm(Scr.VyMax / Scr.DisplayHeight + 1));
                 

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(desk_size, "desk-size", 0, 0, 0,
          (),
"Returns the size of the current desk.\n\
The returned value is in units of the physical screen size, as a list\n\
of the width and the height.")
#define FUNC_NAME s_desk_size
{
  return gh_list(SCM_MAKINUM((int) (Scr.VxMax / Scr.DisplayWidth + 1)),
                 SCM_MAKINUM((int) (Scr.VyMax / Scr.DisplayHeight + 1)),
                 SCM_UNDEFINED);
}
#undef FUNC_NAME


SCWM_PROC(display_size, "display-size", 0, 0, 0,
          (),
"Returns the size of the physical screen in pixels.\n\
The return value is list of the width and the height. The\n\
width is the `car', the height is the `cadr' of the returned list. \n\
See also the variables \"display-width\" and \"display-height\".")
#define FUNC_NAME s_display_size
{
  return gh_list(SCM_MAKINUM(Scr.DisplayWidth),
                 SCM_MAKINUM(Scr.DisplayHeight),
                 SCM_UNDEFINED);
}
#undef FUNC_NAME




void
init_deskpage()
{

#ifndef SCM_MAGIC_SNARFER
#include "deskpage.x"
#endif
}

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

