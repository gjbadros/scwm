/* $Id$ */
/*
 *      Copyright (C) 1997, Maciej Stachowiak and Greg J. Badros
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
#include <config.h>
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

SCWM_PROC(send_client_message, "send-client-message", 2, 1, 0,
          (SCM win, SCM atom, SCM x))
     /** Send WIN the message "ATOM X".
WIN can be 'root-window or an X window identification number.
Useful for supporting other WMs module communication protocols. 
ATOM and X are both 32-bit integers. */
#define FUNC_NAME s_send_client_message
{
  XClientMessageEvent ev;
  long mask = 0L;
  Window w;

  memset(&ev, 0, sizeof(ev));

  if (win == sym_root_window) {
    w = Scr.Root;
    /* use SubstructureRedirectMask if sending to root window */
    mask = SubstructureRedirectMask;
  } else if (WINDOWP(win)) {
    w = PSWFROMSCMWIN(win)->w;
  } else if (gh_number_p(win)) {
    w = gh_scm2long(win);
  } else {
    scm_wrong_type_arg(FUNC_NAME, 1, win);
  }

  if (!gh_number_p(atom)) {
    scm_wrong_type_arg(FUNC_NAME, 1, win);
  }

  if (!gh_number_p(x)) {
    scm_wrong_type_arg(FUNC_NAME, 1, win);
  }

  ev.type = ClientMessage;
  ev.window = w;
  ev.message_type = gh_scm2long(atom);
  ev.format = 32;
  ev.data.l[0] = gh_scm2long(x);
  ev.data.l[1] = lastTimestamp;
  XSendEvent(dpy, w, False, mask, (XEvent *) &ev);
  return SCM_UNDEFINED;
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
