/* $Id$
 * resize.h 
 * (C) 1997, 1998 Maciej Stachowiak and Greg J. Badros
 */

#ifndef RESIZE_H
#define RESIZE_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

void ConstrainSize(ScwmWindow *psw, int xmotion, int ymotion, 
                   int *widthp, int *heightp);
void RedrawOutlineAtNewPosition(Window root, int x, int y, int width, int height);
#define RemoveRubberbandOutline(root) do { RedrawOutlineAtNewPosition((root), 0,0,0,0); } while (0)

Window CreateMessageWindow(Pixel fg, Pixel bg);

/* used by add_window */
SCM interactive_resize(SCM win);

void init_resize_gcs();

#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
