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
void RedrawOutlineAtNewPosition(Window root, int x, int y, int width, int height);
#define RemoveRubberbandOutline(root) do { RedrawOutlineAtNewPosition((root), 0,0,0,0); } while (0)

Window CreateMessageWindow(Pixel fg, Pixel bg, Bool fMWMLike);

/* used by add_window */
SCM interactive_resize(SCM win);

#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
