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

#include <guile/gh.h>
#include <config.h>

#include <ctype.h>
#include <unistd.h>

#include "scwm.h"
#include "misc.h"
#include "screen.h"
#include "deskpage.h"
#include "module-interface.h"
#include "virtual.h"
#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif


SCM_PROC(s_set_current_desk_x, "set-current-desk!", 1, 0, 0,  set_current_desk_x);

SCM 
set_current_desk_x(SCM sx)
{
  SCM_REDEFER_INTS;

  if (!gh_number_p(sx)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("set-current-desk!", 1, sx);
  }
  /* XXX - should do something useful if desk is out of range. */
  changeDesks(0, gh_scm2int(sx));

  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_set_viewport_position_x, "set-viewport-position!", 2, 0, 0,  set_viewport_position_x);

SCM 
set_viewport_position_x(SCM sx, SCM sy)
{
  SCM_REDEFER_INTS;
  if (!gh_number_p(sx)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("set-viewport-position!", 1, sx);
  }
  if (!gh_number_p(sy)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("set-viewport-position!", 2, sy);
  }
  MoveViewport(gh_scm2int(sx), gh_scm2int(sy), True);
  SCM_REALLOW_INTS;
  return (SCM_UNSPECIFIED);
}


SCM_PROC(s_set_edge_scroll_x, "set-edge-scroll!", 2, 0, 0,  set_edge_scroll_x);

SCM 
set_edge_scroll_x(SCM sx, SCM sy)
{
  SCM_REDEFER_INTS;
  if (!gh_number_p(sx)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("set-edge-scroll!", 1, sx);
  }
  if (!gh_number_p(sy)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("set-edge-scroll!", 2, sy);
  }
  Scr.EdgeScrollX = gh_scm2int(sx);
  Scr.EdgeScrollY = gh_scm2int(sy);
  checkPanFrames();

  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_set_edge_wrap_x, "set-edge-wrap!", 2, 0, 0,  set_edge_wrap_x);

SCM 
set_edge_wrap_x(SCM sx, SCM sy)
{
  SCM_REDEFER_INTS;
  if (!gh_boolean_p(sx)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("set-edge-wrap!", 1, sx);
  }
  if (!gh_boolean_p(sy)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("set-edge-wrap!", 2, sy);
  }
  if (sx == SCM_BOOL_T) {
    Scr.flags |= EdgeWrapX;
  } else {
    Scr.flags &= ~EdgeWrapX;
  }
  if (sy == SCM_BOOL_T) {
    Scr.flags |= EdgeWrapY;
  } else {
    Scr.flags &= ~EdgeWrapY;
  }

  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_set_edge_resistance_x, "set-edge-resistance!", 2, 0, 0,  set_edge_resistance_x);

SCM 
set_edge_resistance_x(SCM sr, SCM mr)
{
  SCM_REDEFER_INTS;

  if (!gh_number_p(sr)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("set-edge-resistence!", 1, sr);
  }
  if (!gh_number_p(mr)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("set-edge-resistance!", 2, mr);
  }
  Scr.ScrollResistance = gh_scm2int(sr);
  Scr.MoveResistance = gh_scm2int(mr);

  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_set_desk_size_x, "set-desk-size!", 2, 0, 0,  set_desk_size_x);

SCM 
set_desk_size_x(SCM sx, SCM sy)
{
  SCM_REDEFER_INTS;

  if (!gh_number_p(sx)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("set-desk-size!", 1, sx);
  }
  if (!gh_number_p(sy)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("set-desk-size!", 2, sy);
  }
  Scr.VxMax = gh_scm2int(sx);
  Scr.VyMax = gh_scm2int(sy);
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


SCM_PROC(s_display_size, "display-size", 0, 0, 0,  display_size);

SCM 
display_size()
{
  return scm_listify(SCM_MAKINUM(Scr.MyDisplayWidth),
		     SCM_MAKINUM(Scr.MyDisplayHeight),
		     SCM_UNDEFINED);
}


SCM_PROC(s_desk_size, "desk-size", 0, 0, 0,  desk_size);

SCM 
desk_size()
{
  return scm_listify(SCM_MAKINUM((int) (Scr.VxMax / Scr.MyDisplayWidth + 1)),
                     SCM_MAKINUM((int) (Scr.VyMax / Scr.MyDisplayHeight + 1)),
		     SCM_UNDEFINED);
}


SCM_PROC(s_viewport_position, "viewport-position", 0, 0, 0,  viewport_position);

SCM 
viewport_position()
{
  return scm_listify(SCM_MAKINUM(Scr.Vx),
		     SCM_MAKINUM(Scr.Vy),
		     SCM_UNDEFINED);
}


SCM_PROC(s_current_desk, "current-desk", 0, 0, 0,  current_desk);

SCM 
current_desk()
{
  return SCM_MAKINUM(Scr.CurrentDesk);
}

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
