/* $Id$
 * Copyright (C) 1997-1999, Maciej Stachowiak and Greg J. Badros
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

#include <X11/Xatom.h>
#include <X11/X.h>
#include <X11/Xlib.h>
#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include "scwmexec.h"

static Atom XA_SCWMEXEC_LISTENER;
static Atom XA_SCWMEXEC_REQUEST;
static Atom XA_SCWMEXEC_REQWIN;
static Atom XA_SCWMEXEC_REPLY;
static Atom XA_SCWMEXEC_NOTIFY;
static Atom XA_SCWMEXEC_OUTPUT;
static Atom XA_SCWMEXEC_ERROR;


Window scwmexec_init(Display *dpy) 
{
  Window root;
  Atom type_ret;
  int form_ret;
  unsigned long nitems;
  unsigned long bytes_after;
  unsigned char *prop;

  /* intern our atoms */
  XA_SCWMEXEC_LISTENER=XInternAtom(dpy,"SCWMEXEC_LISTENER", False);
  XA_SCWMEXEC_REQWIN=XInternAtom(dpy,"SCWMEXEC_REQWIN", False);
  XA_SCWMEXEC_REQUEST=XInternAtom(dpy,"SCWMEXEC_REQUEST", False);
  XA_SCWMEXEC_REPLY=XInternAtom(dpy,"SCWMEXEC_REPLY", False);
  XA_SCWMEXEC_NOTIFY=XInternAtom(dpy,"SCWMEXEC_NOTIFY", False);
  XA_SCWMEXEC_ERROR=XInternAtom(dpy,"SCWMEXEC_ERROR", False);
  XA_SCWMEXEC_OUTPUT=XInternAtom(dpy,"SCWMEXEC_OUTPUT", False);

  root=DefaultRootWindow(dpy);

  nitems=1;
  if (XGetWindowProperty(dpy, root, XA_SCWMEXEC_LISTENER,
			 0,1, False, AnyPropertyType, 
			 &type_ret, &form_ret, &nitems, &bytes_after,
			 &prop) != Success || prop == NULL || nitems==0) {
    if (prop!=NULL) {
      XFree(prop);
    }
    return (None);
  }

  XFree(prop);
  return(XCreateSimpleWindow(dpy, root, 3, 4, 2, 2, 1, 0, 0));
}

char *scwmexec_exec(Display *dpy, Window w, char *req)
{
  char *result, *out, *err;
  result = scwmexec_exec_full(dpy, w, req, &out, &err);
  XFree (out);
  XFree (err);
  return result;
}

Bool FPropertyNotifyOnWindow(Display *dpy, XEvent *ev, Window *w)
{
  if (ev->type==PropertyNotify && ev->xproperty.window== *w) {
    return True;
  } else {
    return False;
  }
}

typedef Bool (*PredicateFn)();

char *scwmexec_exec_full(Display *dpy, Window w, char *req,
			 char **output, char **error)
{
  Atom type_ret;
  int form_ret;
  unsigned long nitems;
  unsigned long bytes_after;
  unsigned char *prop;
  Window root=DefaultRootWindow(dpy);
  XEvent ev;
  int got_reply = 0;
  int got_output = 0; 
  int got_error = 0;

  /* X event handling - wait for XA_SCWMEXEC_REPLY on w 
     This needs to be before the ChangeProperty, otherwise
     there is a race condition. --09/15/98 gjb*/
  XSelectInput(dpy,w,PropertyChangeMask);

  XChangeProperty(dpy, w, XA_SCWMEXEC_REQUEST, XA_STRING,
		  8, PropModeReplace, req, strlen(req)+1);

  XChangeProperty(dpy, root, XA_SCWMEXEC_REQWIN, 1,
		  32, PropModeAppend, (unsigned char *) &w, 1);

  do {
    XIfEvent (dpy, &ev, (PredicateFn) FPropertyNotifyOnWindow, (XPointer) &w);
    if (ev.xproperty.state == PropertyNewValue) {
      if (ev.xproperty.atom == XA_SCWMEXEC_REPLY) {
	got_reply = 1;
      } else if (ev.xproperty.atom == XA_SCWMEXEC_OUTPUT) {
	got_output = 1;
      } else if (ev.xproperty.atom == XA_SCWMEXEC_ERROR) {
	got_error = 1;
      }
    }
#ifdef DEBUG_REPLIES
    fprintf(stderr, "Got {reply,output,error} = {%d,%d,%d}\n",
            got_reply, got_output, got_error);
#endif
  } while (!got_reply || !got_output || !got_error);

  *error=NULL;
  *output=NULL;

  /* FIXMS: Grabbing these in delete mode loses massively for some
     reason.  Need to find out why. The properties really should be
     deleted.*/
  XGetWindowProperty(dpy, w, XA_SCWMEXEC_OUTPUT,
		     0,0, False, AnyPropertyType, 
		     &type_ret, &form_ret, &nitems, &bytes_after,
		     (unsigned char **) output);
  XGetWindowProperty(dpy, w, XA_SCWMEXEC_OUTPUT,
		     0, (bytes_after / 4) + ((bytes_after % 4) ? 1 : 0), 
		     True, type_ret, 
		     &type_ret, &form_ret, &nitems, &bytes_after,
		     (unsigned char **) output);

  XGetWindowProperty(dpy, w, XA_SCWMEXEC_ERROR,
		     0,0, False, AnyPropertyType, 
		     &type_ret, &form_ret, &nitems, &bytes_after,
		     (unsigned char **) error);
  XGetWindowProperty(dpy, w, XA_SCWMEXEC_ERROR,
		     0, (bytes_after / 4) + ((bytes_after % 4) ? 1 : 0), 
		     True, type_ret, 
		     &type_ret, &form_ret, &nitems, &bytes_after,
		     (unsigned char **) error);

  XGetWindowProperty(dpy, w, XA_SCWMEXEC_REPLY,
		     0,0, False, AnyPropertyType, 
		     &type_ret, &form_ret, &nitems, &bytes_after,
		     &prop);
  XGetWindowProperty(dpy, w, XA_SCWMEXEC_REPLY,
		     0, (bytes_after / 4) + ((bytes_after % 4) ? 1 : 0), 
		     True, type_ret, 
		     &type_ret, &form_ret, &nitems, &bytes_after,
		     &prop);

  return (char *) prop;
}
