/* $Id$
 * virtual.h
 * (C) 1997, 1998 Maciej Stachowiak and Greg J. Badros
 */

#ifndef VIRTUAL_H_
#define VIRTUAL_H_

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifndef NON_VIRTUAL
void checkPanFrames();
void raisePanFrames();
void initPanFrames();
void changeDesks(int val1, int val2);
void MoveViewport(int newx, int newy, Bool grab);
void MoveViewport_internal(int newx, int newy, Bool grab);
void HandlePaging(int HorWarpSize, int VertWarpSize, int *xl, int *yt,
                  int *delta_x, int *delta_y, Bool Grab);
#endif

#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
