/* $Id$
 * add_window.h
 */


#ifndef ADD_WINDOW_H
#define ADD_WINDOW_H

ScwmWindow *AddWindow(Window w);

void GrabButtonWithModifiers(int button, int modifier, ScwmWindow *sw);

void UngrabButtonWithModifiers(int button, int modifier, ScwmWindow *sw);

void GetWindowSizeHints(ScwmWindow * tmp);

#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
