/* $Id$
 * xrm.h 
 * X Resource manager primitives for scwm
 * (C) 1998 Greg J. Badros and Maciej Stachowiak
 */

#ifndef XRM_H__
#define XRM_H__

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <X11/Xlib.h>

void IntegrateNewResourceManagerProperty(Display *dpy);

#endif /* XRM_H__ */
