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
#include "screen.h"
#include "virtual.h"
#include "xmisc.h"
}

#include "scwm-constraints.hpp"
#include "scwm-screen-ci.hpp"
#include <strstream.h>
#include "ClVariable.h"
#include "ClSimplexSolver.h"

extern ClSimplexSolver *psolver;

extern "C" {

void
CassowaryInitClVarsInPsw(ScwmWindow *psw)
{
  /* FIXGJB: this is intentionally not freed for now
     since the ClVs are still in a solver, perhaps */
  psw->pswci = new ScwmWindowConstraintInfo(psw);
}


void
CassowaryInitClVarsInPscreen(ScreenInfo *pscreen)
{
  pscreen->pssci = new ScwmScreenConstraintInfo(pscreen);
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
SuggestMoveWindowTo(PScwmWindow psw, int x, int y, Bool fOpaque)
{
  if (!psolver) {
    SetScwmWindowPosition(psw,x,y,fOpaque);
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
SuggestSizeWindowTo(PScwmWindow psw, int x, int y, int w, int h, Bool fOpaque)
{
  if (!psolver) {
    SetScwmWindowGeometry(psw,x,y,w,h,fOpaque);
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

  DBUG((DBG,__FUNCTION__,
        "Now clv-width,height are %d x %d",
        pswci->_frame_width.intValue(),
        pswci->_frame_height.intValue()));
}

void
CassowaryModifyOpaqueFlag(Bool *pfOpaque)
{
  *pfOpaque = True;
}

void 
ChangeVirtualPosition(int vx, int vy, Bool fGrab)
{
  if (!psolver) {
    MoveViewport_internal(vx, vy, fGrab);
    return;
  }

  ScwmScreenConstraintInfo *pssci = Scr.pssci;

  /* do not bother with re-solve if nothing has changed */
  if (pssci->_vx.intValue() == vx &&
      pssci->_vy.intValue() == vy)
    return;

  /* Separated for debugging purposes */
  (*psolver)
    .addEditVar(pssci->_vx)
    .addEditVar(pssci->_vy)
    .beginEdit();

  (*psolver)
    .suggestValue(pssci->_vx,vx)
    .suggestValue(pssci->_vy,vy);

  psolver->resolve();
  psolver->endEdit();

  MoveViewport_internal(pssci->_vx.intValue(),pssci->_vy.intValue(),fGrab);
}


/* Just pass a NULL window for ending an
   edit that does not involve a window 
   (e.g., the virtual position) */
void 
CassowaryEndEdit(PScwmWindow psw)
{
  if (!psolver) {
    return;
  }
  psolver->endEdit();
}

} // extern "C"
