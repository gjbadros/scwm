/* $Id$ */
/*
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


/***************************************************************************
 *
 * ICCCM Client Messages - Section 4.2.8 of the ICCCM dictates that all
 * client messages will have the following form:
 *
 *     event type	ClientMessage
 *     message type	XA_WM_PROTOCOLS
 *     window		tmp->w
 *     format		32
 *     data[0]		message atom
 *     data[1]		time stamp
 *
 ****************************************************************************/

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>

#include "ICCCM.h"
#include "scwm.h"
#include "screen.h"

extern SCM sym_root_window;
extern Time lastTimestamp;

Atom XA_WM_PROTOCOLS = None;

void 
send_clientmessage(Display * disp, Window w, Atom a, Time timestamp)
{
  XClientMessageEvent ev;

  ev.type = ClientMessage;
  ev.window = w;
  ev.message_type = XA_WM_PROTOCOLS;
  ev.format = 32;
  ev.data.l[0] = a;
  ev.data.l[1] = timestamp;
  XSendEvent(disp, w, False, 0L, (XEvent *) & ev);
}

SCWM_PROC(send_client_message, "send-client-message", 2, 0, 1,
          (SCM win, SCM atom, SCM data),
"Send WIN the message \"ATOM DATA\".\n\
WIN can be 'root-window or an X window identification number.\n\
Useful for supporting other WMs module communication protocols. \n\
ATOM is the X11 atom representing the message type (e.g.,\n\
\"XA_WM_PROTOCOLS\") and DATA is up to 4 32-bit integers of data.\n\
for the message. DATA will be the used to create the message data, \n\
and the lastTimestamp will be appended as the last integer in\n\
the data message.")
#define FUNC_NAME s_send_client_message
{
  Window w;
  long at;
  long mask = 0L;
  XClientMessageEvent ev;
  int i = 0;

  memset(&ev, 0, sizeof(ev));

  VALIDATE_ARG_WIN_ROOTSYM_OR_NUM_COPY(1,win,w);
  VALIDATE_ARG_INT_COPY(2,atom,at);
  if (gh_length(data) > 4) {
    scm_misc_error(FUNC_NAME,"There can be no more than 4 data elements",SCM_EOL);
  }
  
  /* Use SubstructureRedirectMask for root window messages */
  if (w == Scr.Root) mask = SubstructureRedirectMask;

  ev.type = ClientMessage;
  ev.window = w;
  ev.message_type = at;
  ev.format = 32;
  for (i = 0;  SCM_EOL != data && i<5; ++i) {
    long v;
    VALIDATE_ARG_INT_COPY(3+i,gh_car(data),v);
    ev.data.l[i] = v;
    data = gh_cdr(data);
  }
  ev.data.l[i] = lastTimestamp;
  XSendEvent(dpy, w, False, mask, (XEvent *) &ev);
  return SCM_UNSPECIFIED;
}

void 
init_ICCCM()
{
#ifndef SCM_MAGIC_SNARFER
#include "ICCCM.x"
#endif
}



/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

