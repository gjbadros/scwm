/* $Id$
 * add_window.h
 * Copyright (C) 1997-1999 Maciej Stachowiak and Greg J. Badros
 */


#ifndef ADD_WINDOW_H
#define ADD_WINDOW_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "window_fwd.h"

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

