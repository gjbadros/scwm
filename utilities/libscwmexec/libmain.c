#include <X11/Xatom.h>
#include <X11/X.h>
#include <X11/Xlib.h>
#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>

static Atom XA_SCWMEXEC_LISTENER;
static Atom XA_SCWMEXEC_REQUEST;
static Atom XA_SCWMEXEC_REQWIN;
static Atom XA_SCWMEXEC_REPLY;
static Atom XA_SCWMEXEC_NOTIFY;

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

  root=DefaultRootWindow(dpy);

  if (XGetWindowProperty(dpy, root, XA_SCWMEXEC_LISTENER,
			 0,1, False, AnyPropertyType, 
			 &type_ret, &form_ret, &nitems, &bytes_after,
			 &prop) != Success) {
    return (None);
  }

  return(XCreateSimpleWindow(dpy, root, 3, 4, 2, 2, 1, 0, 0));
}

char *scwmexec_exec(Display *dpy, Window w, char *req)
{
  Atom type_ret;
  int form_ret;
  unsigned long nitems;
  unsigned long bytes_after;
  unsigned char *prop;
  int done = 0;
  Window root=DefaultRootWindow(dpy);
  XEvent ev;

  XChangeProperty(dpy, w, XA_SCWMEXEC_REQUEST, XA_STRING,
		  8, PropModeReplace, req, strlen(req)+1);

  XChangeProperty(dpy, root, XA_SCWMEXEC_REQWIN, 1,
		  32, PropModeReplace, &w, 1);

  XSelectInput(dpy, w, PropertyChangeMask);
  /* X event loop - wait for XA_SCWMEXEC_REPLY on w */
  while(!done) {
    XWindowEvent(dpy,w,PropertyChangeMask,&ev);
    switch (ev.type) {
    case PropertyNotify:
      if (ev.xproperty.window== w
	  && ev.xproperty.atom==XA_SCWMEXEC_REPLY) {
	done=1;
      }
      break;
    default:
      break;
    }
  }

  XGetWindowProperty(dpy, w, XA_SCWMEXEC_REPLY,
		     0,0, False, AnyPropertyType, 
		     &type_ret, &form_ret, &nitems, &bytes_after,
		     &prop);

  XGetWindowProperty(dpy, w, XA_SCWMEXEC_REPLY,
		     0, bytes_after*4, False, type_ret, 
		     &type_ret, &form_ret, &nitems, &bytes_after,
		     &prop);
  
  return (prop);
}
