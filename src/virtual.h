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
#endif

#endif
