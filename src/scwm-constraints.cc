/* $Id$
 * scwm-constraints.h
 *
 * Copyright (C) 1998, 1999, 2000 Greg J. Badros
 */

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
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
#include "Cl.h"

extern ClSimplexSolver *psolver;

extern "C" {

void
CassowaryInitClVarsInPsw(ScwmWindow *psw)
{
  /* GJB:FIXME:: these allocations need to 
     be freed when the solver is turned off or reset,
     but not when psw disappears, as other windows
     could be constrained through this window */
  psw->pswci = new ScwmWindowConstraintInfo(psw);
}


void
CassowaryInitClVarsInPscreen(ScreenInfo *pscreen)
{
  /* GJB:FIXME:: these allocations need to 
     be freed when the solver is turned off or reset */
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
CassowaryCloseWindow(ScwmWindow *)
{
#if 0 // GJB:FIXME:: need to write these after I decide if it's the right thing
  if (psolver) {
    psw->pswci->RemoveStays(psolver);
    psw->pswci->RemoveSizeConstraints(psolver);
  }
#endif
}


void
CassowarySetCValuesAndSolve(ScwmWindow *psw, int fSolve)
{
  ScwmWindowConstraintInfo *pswci = psw->pswci;

  if (!psolver || !fSolve) {
    // no solver attached or not supposed to solve, so just copy the values over
    pswci->_frame_x.SetValue(psw->frame_x);
    pswci->_frame_y.SetValue(psw->frame_y);
    pswci->_frame_width.SetValue(psw->frame_width);
    pswci->_frame_height.SetValue(psw->frame_height);
    MovePswToCurrentPosition(psw);
    return;
  }
    
  vector<double> v;
  if (psw->frame_x != pswci->_frame_x.IntValue()) {
    psolver->AddEditVar(pswci->_frame_x);
    v.push_back(psw->frame_x);
  }
  if (psw->frame_y != pswci->_frame_x.IntValue()) {
    psolver->AddEditVar(pswci->_frame_y);
    v.push_back(psw->frame_y);
  }
  if (psw->frame_width != pswci->_frame_width.IntValue()) {
    psolver->AddEditVar(pswci->_frame_width);
    v.push_back(psw->frame_width);
  }
  if (psw->frame_height != pswci->_frame_height.IntValue()) {
    psolver->AddEditVar(pswci->_frame_height);
    v.push_back(psw->frame_height);
  }
  if (v.size() > 0) {
    psolver->Resolve(v);
    psolver->EndEdit();
  }
}


void
CassowaryEditPosition(PScwmWindow psw)
{
  if (!psolver)
    return;

  ScwmWindowConstraintInfo *pswci = psw->pswci;
  DBUG((scwm_msg(INFO,"CassowaryEditPosition","Begin edit");))
  (*psolver)
    .AddEditVar(pswci->_frame_x)
    .AddEditVar(pswci->_frame_y)
    .BeginEdit();
}

void
CassowaryEditSize(PScwmWindow psw)
{
  if (!psolver)
    return;

  ScwmWindowConstraintInfo *pswci = psw->pswci;
  DBUG((scwm_msg(INFO,"CassowaryEditSize","Begin edit");))
  (*psolver)
    .AddEditVar(pswci->_frame_x)
    .AddEditVar(pswci->_frame_y)
    .AddEditVar(pswci->_frame_width)
    .AddEditVar(pswci->_frame_height)
    .BeginEdit();
}


/* x,y are virtual positions */
void 
SuggestMoveWindowTo(PScwmWindow psw, int x, int y, Bool fOpaque)
{
  if (!psolver) {
    SetScwmWindowPosition(psw,x,y,fOpaque);
    return;
  }
  ScwmWindowConstraintInfo *pswci = psw->pswci;

  /* do not bother with re-solve if nothing has changed */
  if (pswci->_frame_x.IntValue() == x &&
      pswci->_frame_y.IntValue() == y)
    return;

  try {
    (*psolver)
      .SuggestValue(pswci->_frame_x,x)
      .SuggestValue(pswci->_frame_y,y)
      .Resolve();
  }
  catch (const ExCLEditMisuse &e) {
    scwm_msg(ERR,"SuggestMoveWindowTo","Constraint solver edit misuse exception!");
  }
}


/* x,y are virtual positions */
Bool
SuggestSizeWindowTo(PScwmWindow psw, int x, int y, int w, int h, Bool fOpaque)
#define FUNC_NAME "SuggestSizeWindowTo"
{
  if (!psolver) {
    return SetScwmWindowGeometry(psw,x,y,w,h,fOpaque);
  }
  ScwmWindowConstraintInfo *pswci = psw->pswci;

  /* do not bother with re-solve if nothing has changed */
  if (pswci->_frame_x.IntValue() == x &&
      pswci->_frame_y.IntValue() == y &&
      pswci->_frame_width.IntValue() == w &&
      pswci->_frame_height.IntValue() == h)
    return False;

  (*psolver)
    .SuggestValue(pswci->_frame_x,x)
    .SuggestValue(pswci->_frame_y,y)
    .SuggestValue(pswci->_frame_width,w)
    .SuggestValue(pswci->_frame_height,h)
    .Resolve();

  DBUG((DBG,FUNC_NAME,
        "Now clv-width,height are %d x %d",
        pswci->_frame_width.IntValue(),
        pswci->_frame_height.IntValue()));
  return True;
}
#undef FUNC_NAME

void
CassowaryModifyOpaqueFlag(Bool *pfOpaque)
{
  *pfOpaque = True;
}

void 
ChangeVirtualPosition(int vx, int vy)
{
  if (!psolver) {
    MoveViewport_internal(vx, vy);
    return;
  }

  ScwmScreenConstraintInfo *pssci = Scr.pssci;

  /* do not bother with re-solve if nothing has changed */
  if (pssci->_vx.IntValue() == vx &&
      pssci->_vy.IntValue() == vy)
    return;

  /* Separated for debugging purposes */
  (*psolver)
    .AddEditVar(pssci->_vx)
    .AddEditVar(pssci->_vy)
    .BeginEdit();

  (*psolver)
    .SuggestValue(pssci->_vx,vx)
    .SuggestValue(pssci->_vy,vy);

  psolver->Resolve();
  psolver->EndEdit();

  MoveViewport_internal(pssci->_vx.IntValue(),pssci->_vy.IntValue());
}


/* Just pass a NULL window for ending an
   edit that does not involve a window 
   (e.g., the virtual position) */
void 
CassowaryEndEdit(PScwmWindow)
{
  if (!psolver) {
    return;
  }
  DBUG((scwm_msg(INFO,"CassowaryEndEdit","Ended edit");))
  psolver->EndEdit();
  // Below line needs cassowary-0.60 or newer, drop if using older version
  psolver->ResetStayConstants();
}

} // extern "C"
