/* $Id$
 * resize.h 
 */

#ifndef RESIZE_H
#define RESIZE_H

void DoResize(int x_root, int y_root, ScwmWindow *psw);
void DisplaySize(ScwmWindow *psw, int width, int height, Bool Init);
void ConstrainSize(ScwmWindow *psw, int *widthp, int *heightp);
void MoveOutline(Window root, int x, int y, int width, int height);

#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
