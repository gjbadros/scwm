/* $Id$
 * scwm_set_pid_property.c
 * (C) 1999 Toby Sargeant and Greg J. Badros
 * 20-April-1999
 * See bottom for compilation command, then do:

LD_PRELOAD=/full/path/to/scwm_set_pid_property.so xlogo

and then you can access the PID of the xlogo process from the window
property SCWM_RUNNING_PID.

From Scwm, this is as easy as:

(use-scwm-modules xprop-extras)
(window-pid WINDOW)

!!!
 */

#include <X11/X.h>

#include <dlfcn.h>
#include <X11/X.h>
#include <X11/Xlib.h>
#include <unistd.h>
#ifdef DEBUG
#include <stdio.h>
#endif

#define PROPERTY_NAME "SCWM_RUNNING_PID"

static void *pfXCreateWindow = NULL;
static void *pfXCreateSimpleWindow = NULL;
static void *dlhX11 = NULL;
pid_t mypid;
static Atom XA_PID;

_init() {
  unsetenv("LD_PRELOAD");
  dlhX11=dlopen("libX11.so",RTLD_GLOBAL);
  pfXCreateWindow=dlsym(dlhX11,"XCreateWindow");
  pfXCreateSimpleWindow=dlsym(dlhX11,"XCreateSimpleWindow");
  mypid = getpid();
}

typedef Window (*XCType)(Display *display, Window, int, int, unsigned int,
                         unsigned int, unsigned int, int, unsigned int, Visual *, unsigned
                         long, XSetWindowAttributes *);

typedef Window (*XCSType)(Display *display, Window, int, int, unsigned int,
                          unsigned int, unsigned int, unsigned long, unsigned long);

Window XCreateWindow(Display *display,
                     Window parent,
                     int x,
                     int y,
                     unsigned int width,
                     unsigned int height,
                     unsigned int border_width,
                     int depth,
                     unsigned int class,
                     Visual *visual,
                     unsigned long valuemask,
                     XSetWindowAttributes *attributes) 
{
  Window w = ((XCType)pfXCreateWindow)(display, parent,
                                       x, y, width, height, border_width,
                                       depth, class, visual, valuemask, attributes);
  if (parent==DefaultRootWindow(display)) {
#ifdef DEBUG
    fprintf(stderr,"called XCreateWindow, got %ld\n",w);
#endif
    XA_PID = XInternAtom(display,PROPERTY_NAME, False);
    XChangeProperty(display, w,
                    XA_PID, 1,
                    32, PropModeReplace,
                    (unsigned char *) &mypid, 1);
  }
  return w;
}

Window XCreateSimpleWindow(Display *display,
                           Window parent,
                           int x,
                           int y,
                           unsigned int width,
                           unsigned int height,
                           unsigned int border_width,
                           unsigned long border,
                           unsigned long background)
{
  Window w = ((XCSType)pfXCreateSimpleWindow)(display, parent,
                                              x, y, width, height, 
                                              border_width, border, background);
  if (parent==DefaultRootWindow(display)) {
#ifdef DEBUG
    fprintf(stderr,"called XCreateSimpleWindow, got %ld\n",w);
#endif
    XA_PID = XInternAtom(display,PROPERTY_NAME, False);
    XChangeProperty(display, w,
                    XA_PID, 1,
                    32, PropModeReplace,
                    (unsigned char *) &mypid, 1);
  }
  return w;
}


/* Local variables: */
/* compile-command: "gcc -shared -fpic -c scwm_set_pid_property.c; ld -shared scwm_set_pid_property.o -o scwm_set_pid_property.so  -ldl -L/usr/X11R6/lib -lX11" */
