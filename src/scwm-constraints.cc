/* $Id$
 * scwm-constraints.h
 *
 * (C) 1998 Greg J. Badros
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <assert.h>
#include <vector.h>
#include "scwm-constraints.h"
#include "scwm-constraints.hpp"
#include "window.h"
#include "xmisc.h"
#include <strstream.h>
#include "ClVariable.h"
#include "ClSimplexSolver.h"

extern ClSimplexSolver *psolver;

void
CassowaryInitClVarsInPsw(ScwmWindow *psw)
{
  psw->pswci = new ScwmWindowConstraintInfo(psw);
}

void
CassowarySetCValuesAndSolve(ScwmWindow *psw)
{
  ScwmWindowConstraintInfo *pswci = psw->pswci;
  vector<double> v;
  if (psw->frame_x != pswci->_frame_x.intValue()) {
    psolver->addEditVar(pswci->_frame_x);
    v.push_back(psw->frame_x);
  }
  if (psw->frame_y != pswci->_frame_x.intValue()) {
    psolver->addEditVar(pswci->_frame_y);
    v.push_back(psw->frame_y);
  }
  if (psw->frame_width != pswci->_frame_width.intValue()) {
    psolver->addEditVar(pswci->_frame_width);
    v.push_back(psw->frame_width);
  }
  if (psw->frame_height != pswci->_frame_height.intValue()) {
    psolver->addEditVar(pswci->_frame_height);
    v.push_back(psw->frame_height);
  }
  psolver->resolve(v);
  psolver->endEdit();
}


void
CassowaryEditPosition(PScwmWindow psw)
{
  ScwmWindowConstraintInfo *pswci = psw->pswci;
  (*psolver)
    .addEditVar(pswci->_frame_x)
    .addEditVar(pswci->_frame_y)
    .beginEdit();
}


void 
SuggestMoveWindowTo(PScwmWindow psw, int x, int y)
{
  ScwmWindowConstraintInfo *pswci = psw->pswci;
  (*psolver)
    .suggestValue(pswci->_frame_x,x)
    .suggestValue(pswci->_frame_y,y)
    .resolve();
}

void 
EndEditPosition(PScwmWindow psw)
{
  psolver->endEdit();
}
