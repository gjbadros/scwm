/* $Id$
 * add_window.h
 * (C) 1997, 1998 Maciej Stachowiak and Greg J. Badros
 */


#ifndef ADD_WINDOW_H
#define ADD_WINDOW_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "window_fwd.h"

#define ScwmWindow    struct ScwmWindow

void SetScwmWindowGeometry(ScwmWindow *psw, int x, int y, int w, int h);

ScwmWindow *AddWindow(Window w);

void GrabButtonWithModifiers(int button, int modifier, ScwmWindow *psw);

void UngrabButtonWithModifiers(int button, int modifier, ScwmWindow *psw);

void GetWindowSizeHints(ScwmWindow *psw);

void FetchWmProtocols(ScwmWindow *psw);

void GrabKeys(ScwmWindow *psw);

#undef ScwmWindow

#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
