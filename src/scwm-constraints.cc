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

extern "C" {
#include "scwm-constraints.h"
#include "window.h"
#include "xmisc.h"
}

#include "scwm-constraints.hpp"
#include <strstream.h>
#include "ClVariable.h"
#include "ClSimplexSolver.h"

extern ClSimplexSolver *psolver;

extern "C" {

void
CassowaryInitClVarsInPsw(ScwmWindow *psw)
{
  psw->pswci = new ScwmWindowConstraintInfo(psw);
}

void
CassowaryNewWindow(ScwmWindow *psw)
{
  if (psolver) {
    psw->pswci->AddStays(psolver);
    psw->pswci->AddSizeConstraints(psolver);
  }
}


void
CassowarySetCValuesAndSolve(ScwmWindow *psw, int fSolve)
{
  ScwmWindowConstraintInfo *pswci = psw->pswci;

  if (!psolver || !fSolve) {
    // no solver attached or not supposed to solve, so just copy the values over
    pswci->_frame_x.set_value(psw->frame_x);
    pswci->_frame_y.set_value(psw->frame_y);
    pswci->_frame_width.set_value(psw->frame_width);
    pswci->_frame_height.set_value(psw->frame_height);
    return;
  }
    
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
  if (v.size() > 0) {
    psolver->resolve(v);
    psolver->endEdit();
  }
}


void
CassowaryEditPosition(PScwmWindow psw)
{
  if (!psolver)
    return;

  ScwmWindowConstraintInfo *pswci = psw->pswci;
  (*psolver)
    .addEditVar(pswci->_frame_x)
    .addEditVar(pswci->_frame_y)
    .beginEdit();
}

void
CassowaryEditSize(PScwmWindow psw)
{
  if (!psolver)
    return;

  ScwmWindowConstraintInfo *pswci = psw->pswci;
  (*psolver)
    .addEditVar(pswci->_frame_x)
    .addEditVar(pswci->_frame_y)
    .addEditVar(pswci->_frame_width)
    .addEditVar(pswci->_frame_height)
    .beginEdit();
}


void 
SuggestMoveWindowTo(PScwmWindow psw, int x, int y)
{
  if (!psolver) {
    if (x != FRAME_X(psw) || y != FRAME_Y(psw)) {
      FRAME_X(psw) = x; 
      FRAME_Y(psw) = y; 
      XMoveWindow(dpy, psw->frame, x, y); 
    }
    return;
  }
  ScwmWindowConstraintInfo *pswci = psw->pswci;

  /* do not bother with re-solve if nothing has changed */
  if (pswci->_frame_x.intValue() == x &&
      pswci->_frame_y.intValue() == y)
    return;

  (*psolver)
    .suggestValue(pswci->_frame_x,x)
    .suggestValue(pswci->_frame_y,y)
    .resolve();
}

void 
SuggestSizeWindowTo(PScwmWindow psw, int x, int y, int w, int h)
{
  if (!psolver) {
    SetScwmWindowGeometry(psw,x,y,w,h);
    return;
  }
  ScwmWindowConstraintInfo *pswci = psw->pswci;

  /* do not bother with re-solve if nothing has changed */
  if (pswci->_frame_x.intValue() == x &&
      pswci->_frame_y.intValue() == y &&
      pswci->_frame_width.intValue() == w &&
      pswci->_frame_height.intValue() == h)
    return;

  (*psolver)
    .suggestValue(pswci->_frame_x,x)
    .suggestValue(pswci->_frame_y,y)
    .suggestValue(pswci->_frame_width,w)
    .suggestValue(pswci->_frame_height,h)
    .resolve();

  DBUG(__FUNCTION__,
       "Now clv-width,height are %d x %d",
       pswci->_frame_width.intValue(),
       pswci->_frame_height.intValue());
}


void 
CassowaryEndEdit(PScwmWindow psw)
{
  if (!psolver)
    return;
  psolver->endEdit();
}

} // extern "C"
