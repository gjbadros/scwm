/* $Id$
 * virtual.h
 *
 */

#ifndef VIRTUAL_H_
#define VIRTUAL_H_


#ifndef NON_VIRTUAL
void checkPanFrames();
void raisePanFrames();
void initPanFrames();
void changeDesks(int val1, int val2);
void MoveViewport(int newx, int newy, Bool grab);
void HandlePaging(int HorWarpSize, int VertWarpSize, int *xl, int *yt,
                   int *delta_x, int *delta_y, Bool Grab);
#endif

#endif
