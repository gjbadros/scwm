/* $Id$
 * virtual.h
 * Copyright (C) 1997, 1998, 1999, 2000 Greg J. Badros and Maciej Stachowiak
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

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

