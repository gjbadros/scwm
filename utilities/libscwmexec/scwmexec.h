#ifndef SCWMEXEC_H
#define SCWMEXEC_H

#include <X11/Xatom.h>
#include <X11/X.h>
#include <X11/Xlib.h>

Window scwmexec_init(Display *dpy);
char *scwmexec_exec(Display *dpy, Window w, unsigned char *req);
char *scwmexec_exec_full(Display *dpy, Window w, unsigned char *req,
			 unsigned char **output, unsigned char **error);

#endif /* SCWMEXEC_H */
