/* $Id$
 * Copyright (C) 1997-1998, Maciej Stachowiak and Greg J. Badros
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

#ifndef DESKPAGE_H__
#define DESKPAGE_H__

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

void init_deskpage();

SCM set_current_desk_x(SCM sx);
SCM set_viewport_position_x(SCM sx, SCM sy);
SCM set_edge_scroll_x(SCM sx, SCM sy);
SCM set_edge_wrap_x(SCM sx, SCM sy);
SCM set_edge_resistance_x(SCM sr, SCM mr);
SCM set_desk_size_x(SCM sx, SCM sy);
SCM display_size();
SCM desk_size();
SCM viewport_position();
SCM current_desk();

#endif	/* DESKPAGE_H__ */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
