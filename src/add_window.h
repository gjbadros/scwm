/* $Id$
 * add_window.h
 */


#ifndef ADD_WINDOW_H
#define ADD_WINDOW_H

ScwmWindow *AddWindow(Window w);

void GrabButtonWithModifiers(int button, int modifier, ScwmWindow *sw);

void UngrabButtonWithModifiers(int button, int modifier, ScwmWindow *sw);

void GetWindowSizeHints(ScwmWindow * tmp);

void FetchWmProtocols(ScwmWindow * tmp);

void GrabKeys(ScwmWindow * tmp_win);

void init_add_window();

#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
