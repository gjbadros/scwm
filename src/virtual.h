/* $Id$
 * virtual.h
 * (C) 1997, 1998 Maciej Stachowiak and Greg J. Badros
 */

#ifndef VIRTUAL_H_
#define VIRTUAL_H_

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <X11/Intrinsic.h>

typedef enum {
  EDGE_NONE,
  EDGE_TOP,
  EDGE_LEFT,
  EDGE_RIGHT,
  EDGE_BOTTOM
} Edge;


void GenerateEdgeEvents();


#ifndef NON_VIRTUAL
void checkPanFrames();
void raisePanFrames();
void initPanFrames();
void changeDesks(int val1, int val2);
void MoveViewport(int newx, int newy);
void MoveViewport_internal(int newx, int newy);
void HandlePaging(int HorWarpSize, int VertWarpSize, int *xl, int *yt,
                  int *delta_x, int *delta_y, Bool Grab);
Bool FNeedsPaging(int HorWarpSize, int VertWarpSize, int xl, int yt);

#endif

#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
