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

char *scwmexec_exec(Display *dpy, Window w, unsigned char *req)
{
  unsigned char *result, *out, *err;
  scwmexec_exec_full(dpy, w, req, &out, &err);
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

char *scwmexec_exec_full(Display *dpy, Window w, unsigned char *req,
			 unsigned char **output, unsigned char **error)
{
  Atom type_ret;
  int form_ret;
  unsigned long nitems;
  unsigned long bytes_after;
  unsigned char *prop;
  int done = 0;
  Window root=DefaultRootWindow(dpy);
  XEvent ev;
  int got_reply = 0;
  int got_output = 0; 
  int got_error = 0;

  XChangeProperty(dpy, w, XA_SCWMEXEC_REQUEST, XA_STRING,
		  8, PropModeReplace, req, strlen(req)+1);

  XChangeProperty(dpy, root, XA_SCWMEXEC_REQWIN, 1,
		  32, PropModeAppend, (unsigned char *) &w, 1);

  /* X event handling - wait for XA_SCWMEXEC_REPLY on w */
  XSelectInput(dpy,w,PropertyChangeMask);


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
  } while (!got_reply || !got_output || !got_error);

  *error=NULL;
  *output=NULL;

  /* FIXMS: Grabbing these in delete mode loses massively for some
     reason.  Need to find out why. The properties really should be
     deleted.*/
  XGetWindowProperty(dpy, w, XA_SCWMEXEC_OUTPUT,
		     0,0, False, AnyPropertyType, 
		     &type_ret, &form_ret, &nitems, &bytes_after,
		     output);
  XGetWindowProperty(dpy, w, XA_SCWMEXEC_OUTPUT,
		     0, (bytes_after / 4) + ((bytes_after % 4) ? 1 : 0), 
		     True, type_ret, 
		     &type_ret, &form_ret, &nitems, &bytes_after,
		     output);

  XGetWindowProperty(dpy, w, XA_SCWMEXEC_ERROR,
		     0,0, False, AnyPropertyType, 
		     &type_ret, &form_ret, &nitems, &bytes_after,
		     error);
  XGetWindowProperty(dpy, w, XA_SCWMEXEC_ERROR,
		     0, (bytes_after / 4) + ((bytes_after % 4) ? 1 : 0), 
		     True, type_ret, 
		     &type_ret, &form_ret, &nitems, &bytes_after,
		     error);

  XGetWindowProperty(dpy, w, XA_SCWMEXEC_REPLY,
		     0,0, False, AnyPropertyType, 
		     &type_ret, &form_ret, &nitems, &bytes_after,
		     &prop);
  XGetWindowProperty(dpy, w, XA_SCWMEXEC_REPLY,
		     0, (bytes_after / 4) + ((bytes_after % 4) ? 1 : 0), 
		     True, type_ret, 
		     &type_ret, &form_ret, &nitems, &bytes_after,
		     &prop);

  return (prop);
}
