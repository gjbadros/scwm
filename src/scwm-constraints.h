/* $Id$
 * scwm-constraints.h
 *
 * (C) 1998 Greg J. Badros
 */

#ifndef SCWM_CONSTRAINTS_H__
#define SCWM_CONSTRAINTS_H__

#include <config.h>

#ifdef USE_CASSOWARY

#include "ClVariable.h"
#include "ClSimplexSolver.h"

typedef ClVariable ScwmClVariable;

#define FRAME_X(psw) ((psw)->frame_x.intValue())
#define FRAME_Y(psw) ((psw)->frame_y.intValue())
#define FRAME_WIDTH(psw) ((psw)->frame_width.intValue())
#define FRAME_HEIGHT(psw) ((psw)->frame_height.intValue())

/* defined in scwm.c for now */
extern ClSimplexSolver solver;

#else 
/* !USE_CASSOWARY code */

typedef int ScwmClVariable;

#define FRAME_X(psw) ((psw)->frame_x)
#define FRAME_Y(psw) ((psw)->frame_y)
#define FRAME_WIDTH(psw) ((psw)->frame_width)
#define FRAME_HEIGHT(psw) ((psw)->frame_height)

#endif


#endif /* SCWM_CONSTRAINTS_H__ */
