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
 * Changes Copyright 1997, Maciej stachowiak
 ****************************************************************************/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <guile/gh.h>
#include <ctype.h>
#include <unistd.h>

#include "scwm.h"
#include "screen.h"
#include "deskpage.h"
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
{ SCM_REDEFER_INTS;

  if (!gh_number_p(desk)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg(FUNC_NAME, 1, desk);
  }
  /* XXX - should do something useful if desk is out of range. */
  changeDesks(0, gh_scm2int(desk));

  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/**CONCEPT: Viewports 
  The current viewport is the area of the current desk that may be
seen on the physical screen. Since a desk can be larger than the
physical screen size, the viewport can move around the desk.
*/

SCWM_PROC(set_viewport_position_x, "set-viewport-position!", 2, 0, 0,
          (SCM x, SCM y))
     /** Position the upper left corner of the viewport at coordinates X, Y.
X and Y are given in pixels.  Does not affect the current desk. */
#define FUNC_NAME s_set_viewport_position_x
{
  SCM_REDEFER_INTS;
  if (!gh_number_p(x)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg(FUNC_NAME, 1, x);
  }
  if (!gh_number_p(y)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg(FUNC_NAME, 2, y);
  }
  MoveViewport(gh_scm2int(x), gh_scm2int(y), True);
  SCM_REALLOW_INTS;
  return (SCM_UNSPECIFIED);
}
#undef FUNC_NAME


SCWM_PROC(set_edge_scroll_x, "set-edge-scroll!", 2, 0, 0,
          (SCM sx, SCM sy))
     /** Set the edge scroll amount in pixels.
The edge scroll setting is the amount by which the viewport will scroll
when the mouse hits the edge. SX gives the amount at a time to scroll
horizontally, while SY gives the amount to scroll vertically. Use `%x'
and `%y' to convert from a percent of screen size to pixels. */
#define FUNC_NAME s_set_edge_scroll_x
{
  SCM_REDEFER_INTS;
  if (!gh_number_p(sx)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg(FUNC_NAME, 1, sx);
  }
  if (!gh_number_p(sy)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg(FUNC_NAME, 2, sy);
  }
  Scr.EdgeScrollX = gh_scm2int(sx);
  Scr.EdgeScrollY = gh_scm2int(sy);
  checkPanFrames();

  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(set_edge_wrap_x, "set-edge-wrap!", 2, 0, 0,
          (SCM wx, SCM wy))
     /** Set whether to wrap pointer around edges.
If WX is #t, the pointer will wrap from the right edge of the
display to the left of the display as it moves off the right edge,
and vice-versa.  WY indicates whether the analogous vertical
wraparound is in effect */
#define FUNC_NAME s_set_edge_wrap_x
{
  SCM_REDEFER_INTS;
  if (!gh_boolean_p(wx)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg(FUNC_NAME, 1, wx);
  }
  if (!gh_boolean_p(wy)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg(FUNC_NAME, 2, wy);
  }
  if (wx == SCM_BOOL_T) {
    Scr.flags |= EdgeWrapX;
  } else {
    Scr.flags &= ~EdgeWrapX;
  }
  if (wy == SCM_BOOL_T) {
    Scr.flags |= EdgeWrapY;
  } else {
    Scr.flags &= ~EdgeWrapY;
  }

  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/* FIXMS: this should probably be split into two procedures. */
SCWM_PROC(set_edge_resistance_x, "set-edge-resistance!", 2, 0, 0,
          (SCM sr, SCM mr))
     /** Set the edge resistance parameters.
Sets two parameters indicating how much resistance should be
offered when scrolling things past the edge, in two different
senses. SR is an amount in microseconds that indicates how long the
mouse pointer must stay at the edge of the screen before the viewport
scrolls. If this parameter is greater than 10,000, the viewport will
not scroll at all at the screen edge (FIXMS: that's a bogus way to
indicate that.) MR is an amount in pixels that indicates how many
pixels past the edge of the screen a window must be moved before it
will really go past the edge. */
#define FUNC_NAME s_set_edge_resistance_x
{
  SCM_REDEFER_INTS;

  if (!gh_number_p(sr)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg(FUNC_NAME, 1, sr);
  }
  if (!gh_number_p(mr)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg(FUNC_NAME, 2, mr);
  }
  Scr.ScrollResistance = gh_scm2int(sr);
  Scr.MoveResistance = gh_scm2int(mr);

  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(set_desk_size_x, "set-desk-size!", 2, 0, 0,
          (SCM width, SCM height))
     /** Sets the desk size to WIDTH, HEIGHT.
Both numbers are given in units of the physical screen size.
For example <code>(set-desk-size 3 3)</code> creates a
virtual world 9 times the size of the physical display. */
#define FUNC_NAME s_set_desk_size_x
{
  SCM_REDEFER_INTS;

  if (!gh_number_p(width)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg(FUNC_NAME, 1, width);
  }
  if (!gh_number_p(height)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg(FUNC_NAME, 2, height);
  }
  Scr.VxMax = gh_scm2int(width);
  Scr.VyMax = gh_scm2int(height);
  Scr.VxMax = Scr.VxMax * Scr.MyDisplayWidth - Scr.MyDisplayWidth;
  Scr.VyMax = Scr.VyMax * Scr.MyDisplayHeight - Scr.MyDisplayHeight;
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


SCWM_PROC(display_size, "display-size", 0, 0, 0,
          ())
     /** Returns the size of the physical screen in pixels.
The return value is list of the width and the height. The
width is the `car', the height is the `cadr' of the returned list. */
#define FUNC_NAME s_display_size
{
  return scm_listify(SCM_MAKINUM(Scr.MyDisplayWidth),
		     SCM_MAKINUM(Scr.MyDisplayHeight),
		     SCM_UNDEFINED);
}
#undef FUNC_NAME


SCWM_PROC(desk_size, "desk-size", 0, 0, 0,
          ())
     /** Returns the size of the current desk.
The returned value is in units of the physical screen size, as a list
of the width and the height. */
#define FUNC_NAME s_desk_size
{
  return scm_listify(SCM_MAKINUM((int) (Scr.VxMax / Scr.MyDisplayWidth + 1)),
                     SCM_MAKINUM((int) (Scr.VyMax / Scr.MyDisplayHeight + 1)),
		     SCM_UNDEFINED);
}
#undef FUNC_NAME


SCWM_PROC(viewport_position, "viewport-position", 0, 0, 0,
          ())
     /** Returns the current position of the viewport in pixels.
The returned value is a list of the x and y positions. */
#define FUNC_NAME s_viewport_position
{
  return scm_listify(SCM_MAKINUM(Scr.Vx),
		     SCM_MAKINUM(Scr.Vy),
		     SCM_UNDEFINED);
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
