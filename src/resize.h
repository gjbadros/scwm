/* $Id$
 * resize.h 
 * Copyright (C) 1997, 1998, 1999, 2000 Greg J. Badros and Maciej Stachowiak
 */

#ifndef RESIZE_H
#define RESIZE_H

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include <X11/X.h>
#include <X11/Intrinsic.h>

#include "window_fwd.h"

void ComputeDeltaForResize(ScwmWindow *psw, int *pdx, int *pdy, int width, int height);
void ComputeDeltaForResizeWithOrigSize(ScwmWindow *psw, int *pdx, int *pdy, int width, int height, 
                                       int orig_width, int orig_height);

void ComputePositionForResize(ScwmWindow *psw, int *px, int *py, int width, int height);

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
/* vim:ts=8:sw=2:sta 
 */

