/* $Id$
 *
 *      Copyright (C) 1997-1998, Maciej Stachowiak and Greg J. Badros
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
#include <ctype.h>
#include <unistd.h>

#include "deskpage.h"

#include "scwm.h"
#include "screen.h"
#include "module-interface.h"
#include "virtual.h"

#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif

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
          (SCM desk))
     /** Change the current desk to DESK. DESK should be an integer
small enough to fit in one machine word. */
#define FUNC_NAME s_set_current_desk_x
{
  if (!gh_number_p(desk)) {
    scm_wrong_type_arg(FUNC_NAME, 1, desk);
  }
  /* XXX - should do something useful if desk is out of range. */
  changeDesks(0, gh_scm2int(desk));

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(current_desk, "current-desk", 0, 0, 0,
          ())
     /** Returns the integer identifying the current desk. */
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
          (SCM x, SCM y))
     /** Position the upper left corner of the viewport at coordinates X, Y.
X and Y are given in pixels.  Does not affect the current desk. */
#define FUNC_NAME s_set_viewport_position_x
{
  if (!gh_number_p(x)) {
    scm_wrong_type_arg(FUNC_NAME, 1, x);
  }
  if (!gh_number_p(y)) {
    scm_wrong_type_arg(FUNC_NAME, 2, y);
  }
  MoveViewport(gh_scm2int(x), gh_scm2int(y), True);

  return (SCM_UNSPECIFIED);
}
#undef FUNC_NAME

SCWM_PROC(viewport_position, "viewport-position", 0, 0, 0,
          ())
     /** Returns the current position of the viewport in pixels.
The returned value is a list of the x and y positions. */
#define FUNC_NAME s_viewport_position
{
  return gh_list(SCM_MAKINUM(Scr.Vx),
                 SCM_MAKINUM(Scr.Vy),
                 SCM_UNDEFINED);
}
#undef FUNC_NAME


SCWM_PROC(set_edge_x_scroll_x, "set-edge-x-scroll!", 1, 0, 0,
          (SCM pixels))
     /** Set the horizontal edge scroll increment to PIXELS.
The horizontal edge scroll setting is the amount by which the viewport
will scroll when the mouse hits the left or right edge. Use `%x' to
convert from a percent of screen size to pixels. */
#define FUNC_NAME s_set_edge_x_scroll_x
{
  if (!gh_number_p(pixels)) {
    scm_wrong_type_arg(FUNC_NAME, 1, pixels);
  }

  Scr.EdgeScrollX = gh_scm2int(pixels);
  checkPanFrames();

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(edge_x_scroll, "edge-x-scroll", 0, 0, 0,
          ())
     /** Return the horizontal edge scroll increment as set by `set-edge-x-scroll!'. */
#define FUNC_NAME s_edge_x_scroll
{
  return (gh_int2scm(Scr.EdgeScrollX));
}
#undef FUNC_NAME


SCWM_PROC(set_edge_y_scroll_x, "set-edge-y-scroll!", 1, 0, 0,
          (SCM pixels))
     /** Set the vertical edge scroll increment to PIXELS.
The vertical edge scroll setting is the amount by which the viewport
will scroll when the mouse hits the top or bottom edge. Use `%y' to
convert from a percent of screen size to pixels. */
#define FUNC_NAME s_set_edge_y_scroll_x
{
  if (!gh_number_p(pixels)) {
    scm_wrong_type_arg(FUNC_NAME, 1, pixels);
  }

  Scr.EdgeScrollY = gh_scm2int(pixels);
  checkPanFrames();

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(edge_y_scroll, "edge-y-scroll", 0, 0, 0,
          ())
     /** Return the vertical edge scroll increment as set by `set-edge-y-scroll!'. */
#define FUNC_NAME s_edge_y_scroll
{
  return (gh_int2scm(Scr.EdgeScrollY));
}
#undef FUNC_NAME



SCWM_PROC(set_edge_x_wrap_x, "set-edge-x-wrap!", 1, 0, 0,
          (SCM flag))
     /** Set whether to wrap pointer around horizontal edges.
If the boolean value FLAG is #t, the pointer will wrap from the right
edge of the desktop to the left of the display as it moves off the
right edge, and vice-versa. See also `set-edge-y-wrap!' */
#define FUNC_NAME s_set_edge_x_wrap_x
{
  COPY_BOOL_OR_ERROR(Scr.fEdgeWrapX, flag, 1, FUNC_NAME);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(edge_x_wrap, "edge-x-wrap", 0, 0, 0,
          ())
     /** Return the current horizonatal edge wrap setting as set by `set-edge-x-wrap!'. */
#define FUNC_NAME s_edge_x_wrap
{
  return SCM_BOOL_FromBool(Scr.fEdgeWrapX);
}
#undef FUNC_NAME


SCWM_PROC(set_edge_y_wrap_x, "set-edge-y-wrap!", 1, 0, 0,
          (SCM flag))
     /** Set whether to wrap pointer around vertical edges.
If the boolean value FLAG is #t, the pointer will wrap from the bottom
edge of the desktop to the top of the display as it moves off the very
bottom edge, and vice-versa. See also `set-edge-x-wrap!' */
#define FUNC_NAME s_set_edge_y_wrap_x
{
  COPY_BOOL_OR_ERROR(Scr.fEdgeWrapY, flag, 1, FUNC_NAME);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(edge_y_wrap, "edge-y-wrap", 0, 0, 0,
          ())
     /** Return the current vertical edge wrap setting as set by `set-edge-y-wrap!'. */
#define FUNC_NAME s_edge_y_wrap
{
  return SCM_BOOL_FromBool(Scr.fEdgeWrapY);
}
#undef FUNC_NAME


SCWM_PROC(set_edge_scroll_delay_x, "set-edge-scroll-delay!", 1, 0, 0,
          (SCM usec))
     /** Set the edge scroll delay to USEC microseconds.
When the mouse pointer hits the edge of the screen, it must stay there
for at least the edge scroll delay amount before the desktop will be
scrolled. If this parameter is #f, the viewport will not scroll at all
at the screen edge. */
#define FUNC_NAME s_set_edge_scroll_delay_x
{
  if (usec == SCM_BOOL_F)
    Scr.ScrollResistance = -1;
  else if (!gh_number_p(usec)) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 1, usec);
  } else {
    Scr.ScrollResistance = gh_scm2int(usec);
    if (Scr.ScrollResistance >= 10000) { 
      Scr.ScrollResistance = -1;
      scwm_msg(WARN, FUNC_NAME, "Possible deprecated use of "
	       "`set-edge-scroll-delay!' detected. Give #f rather than usec "
	       ">= 10000 to prohibit scrolling.");
    }
  }
  return SCM_UNSPECIFIED; 
}
#undef FUNC_NAME


SCWM_PROC(edge_scroll_delay, "edge-scroll-delay", 0, 0, 0,
          ())
     /** Return the edge scroll delay as set by `set-edge-scroll-delay!'. */
#define FUNC_NAME s_edge_scroll_delay
{
  return gh_int2scm(Scr.ScrollResistance);
}
#undef FUNC_NAME




SCWM_PROC(set_edge_move_threshold_x, "set-edge-move-threshold!", 1, 0, 0,
          (SCM pixels))
	  /**  Set the edge move threshold to PIXELS.
This is the number of pixels past the edge of the screen that a window
must be moved before it will really move past the edge. */
#define FUNC_NAME s_set_edge_move_threshold_x
{
  if (!gh_number_p(pixels)) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 1, pixels);
  }

  Scr.MoveResistance = gh_scm2int(pixels);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(edge_move_threshold, "edge-move-threshold", 0, 0, 0,
          ())
     /** Return the edge move threshold as set by `set-edge-move-threshold!'. */
#define FUNC_NAME s_edge_move_threshold
{
  return gh_int2scm(Scr.MoveResistance);
}
#undef FUNC_NAME



SCWM_PROC(set_desk_size_x, "set-desk-size!", 2, 0, 0,
          (SCM width, SCM height))
     /** Sets the desk size to WIDTH, HEIGHT.
Both numbers are given in units of the physical screen size.  For
example <informalexample><programlisting>(set-desk-size 3 3)
</programlisting></informalexample> createsa virtual world 9 times the
size of the physical display. */
#define FUNC_NAME s_set_desk_size_x
{
  SCM_REDEFER_INTS;

  if (!gh_number_p(width)) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 1, width);
  }
  if (!gh_number_p(height)) {
    gh_allow_ints();
    scm_wrong_type_arg(FUNC_NAME, 2, height);
  }
  Scr.VxMax = gh_scm2int(width);
  Scr.VyMax = gh_scm2int(height);
  Scr.VxMax = Scr.VxMax * Scr.DisplayWidth - Scr.DisplayWidth;
  Scr.VyMax = Scr.VyMax * Scr.DisplayHeight - Scr.DisplayHeight;
  if (Scr.VxMax < 0)
    Scr.VxMax = 0;
  if (Scr.VyMax < 0)
    Scr.VyMax = 0;
  Broadcast(M_NEW_PAGE, 5, Scr.Vx, Scr.Vy, Scr.CurrentDesk, Scr.VxMax, Scr.VyMax, 0, 0);

  checkPanFrames();

  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(desk_size, "desk-size", 0, 0, 0,
          ())
     /** Returns the size of the current desk.
The returned value is in units of the physical screen size, as a list
of the width and the height. */
#define FUNC_NAME s_desk_size
{
  return gh_list(SCM_MAKINUM((int) (Scr.VxMax / Scr.DisplayWidth + 1)),
                 SCM_MAKINUM((int) (Scr.VyMax / Scr.DisplayHeight + 1)),
                 SCM_UNDEFINED);
}
#undef FUNC_NAME


SCWM_PROC(display_size, "display-size", 0, 0, 0,
          ())
     /** Returns the size of the physical screen in pixels.
The return value is list of the width and the height. The
width is the `car', the height is the `cadr' of the returned list. 
See also the variables "display-width" and "display-height". */
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
