/* $Id$
 * resize.h 
 * Copyright (C) 1997-1999 Maciej Stachowiak and Greg J. Badros
 */

#ifndef RESIZE_H
#define RESIZE_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <X11/X.h>
#include <X11/Intrinsic.h>

#include "window_fwd.h"

void ConstrainSize(ScwmWindow *psw, int xmotion, int ymotion, 
                   int *widthp, int *heightp);
void RedrawOutlineAtNewPosition(int x, int y, int width, int height);
#define RemoveRubberbandOutline() do { RedrawOutlineAtNewPosition(0,0,0,0); } while (0)

Bool InteractiveResize(ScwmWindow *psw, Bool fOpaque,
                       int *pwidthReturn, int *pheightReturn);

void init_resize_gcs();

#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
