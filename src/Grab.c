/* $Id$ */
/* This module is based on code distributed with fvwm with no
 * copyright notice. Thus, the code it is based on should be in
 * the public domain.
 *
 * It may be used or distributed under either the FVWM license 
 * (see COPYING.fvwm) or the GNU General Public License (see COPYING.GPL and
 * the description below)
 * Copyright 1997, Maciej Stachowiak and Greg J Badros
 ****************************************************************************/
/*      Copyright (C) 1997, Maciej Stachowiak
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
 * As a special exception, this file may alternatively be distributed under 
 * the fvwm license (see COPYING.FVWM).
 *
 */

#include <stdio.h>
#include <X11/Xlib.h>

static int xgrabcount = 0;

void 
XGrabServer_withSemaphore(Display * disp)
{
  if (xgrabcount == 0) {
    XGrabServer(disp);
  }
  ++xgrabcount;
}

void 
XUngrabServer_withSemaphore(Display * disp)
{
  if (--xgrabcount < 0) {	/* should never happen */
    fprintf(stderr,"%s: too many ungrabs!\n",__FUNCTION__);
    xgrabcount = 0;
  }
  if (xgrabcount == 0) {
    XUngrabServer(disp);
  }
}

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
