/* $Id$
 * resize.h 
 * (C) 1997, 1998 Maciej Stachowiak and Greg J. Badros
 */

#ifndef RESIZE_H
#define RESIZE_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

void DoResize(int x_root, int y_root, ScwmWindow *psw);
void DisplaySize(ScwmWindow *psw, int width, int height, Bool Init);
void ConstrainSize(ScwmWindow *psw, int *widthp, int *heightp);
void MoveOutline(Window root, int x, int y, int width, int height);

#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
