/* $Id$
 * scwm-constraints.h
 *
 * (C) 1998 Greg J. Badros
 */

#ifndef SCWM_CONSTRAINTS_H__
#define SCWM_CONSTRAINTS_H__

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef USE_CASSOWARY

#include "ClVariable.h"
#include "ClSimplexSolver.h"

struct ScwmWindow;
typedef struct ScwmWindow *PScwmWindow;
typedef const struct ScwmWindow *ConstPScwmWindow;

class ScwmClVariable : public ClVariable {
private: typedef ClVariable super;
public:
  ScwmClVariable() : ClVariable(), _psw(NULL) { }
  virtual void change_value(Number n);
  set_psw(ConstPScwmWindow psw) { _psw = (PScwmWindow) psw; }
private:
  PScwmWindow _psw;
};


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
