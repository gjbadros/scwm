/* $Id$
 * add_window.h
 * Copyright (C) 1997, 1998, 1999, 2000 Greg J. Badros and Maciej Stachowiak
 */


#ifndef ADD_WINDOW_H
#define ADD_WINDOW_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "window_fwd.h"

#undef EXTERN
#undef EXTERN_SET
#ifdef ADD_WINDOW_IMPLEMENTATION
#define EXTERN
#define EXTERN_SET(x,y) x = y
#else
#define EXTERN extern
#define EXTERN_SET(x,y) extern x
#endif

void SetScwmWindowPosition(ScwmWindow *psw, int x, int y, Bool fOpaque);
Bool SetScwmWindowGeometry(ScwmWindow *psw, int x, int y, int w, int h, Bool fOpaque);

ScwmWindow *AddWindow(Window w);
void DestroyScwmWindow(ScwmWindow *psw);

void GetWindowSizeHints(ScwmWindow *psw);

void FetchWmProtocols(ScwmWindow *psw);

#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

