/* $Id$
 * constraint-primitives.cc
 *
 * Temporary until I get the full general cassowary wrapped in guile nicely
 *
 * (C) 1998 Greg J. Badros
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef USE_CASSOWARY

extern "C" {
#include "scwm.h"
#include "window.h"
}

#include "constraint-primitives.h"
#include "scwm-constraints.h"
#include "scwm-constraints.hpp"
#include "ClLinearEquation.h"
#include "ClLinearInequality.h"
#include "ClSimplexSolver.h"
#include "../guile/cassowary_scm.hpp"
#include <strstream>
#include <list>


ClSimplexSolver *psolver;

static list<ScwmWindow *> rgpswDirty;

static void
ScwmClvChanged(ClVariable *pclv, ClSimplexSolver *)
{
  ScwmWindow *psw = static_cast<ScwmWindow *>(pclv->Pv());
  if (!psw) {
    DBUG(__FUNCTION__,"No struct ScwmWindow attached to var: %s", pclv->name().data());
    return;
  }
  rgpswDirty.push_back(psw);
}

static void
ScwmResolve(ClSimplexSolver *psolver)
{
  // go through the dirty windows and move them
  list<ScwmWindow *>::const_iterator it = rgpswDirty.begin();
  for ( ; it != rgpswDirty.end(); ++it ) {
    const ScwmWindow *psw = *it;
    psw->pswci->CopyStateToPswVars();
    MovePswToCurrentPosition(psw);
    ResizePswToCurrentSize(psw);
  }
  rgpswDirty.clear();
}


SCWM_PROC(add_stays_on_window, "add-stays-on-window", 1, 0, 0,
          (SCM win))
  /** Add stay constraint on all window dimensions
WIN is a window objects */
#define FUNC_NAME s_add_stays_on_window
{
  int iarg = 1;
  if (!WINDOWP(win)) {
    scm_wrong_type_arg(FUNC_NAME, iarg++, win);
  }
  
  ScwmWindow const *const psw = PSWFROMSCMWIN(win);
  ScwmWindowConstraintInfo *pswci = psw->pswci;
  pswci->AddStays(psolver);

  /* FIXGJB: need to add hook to remove the stays if win disappears */
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC (scwm_set_master_solver, "scwm-set-master-solver", 1, 0, 0,
           (SCM solver))
     /** Use SOLVER as the master solver for scwm */
#define FUNC_NAME s_scwm_set_master_solver
{
  int iarg = 1;
  if (!FIsClSimplexSolverScm(solver))
    scm_wrong_type_arg(FUNC_NAME,iarg++,solver);

  psolver = PsolverFromScm(solver);
  psolver->SetChangeClvCallback(ScwmClvChanged);
  psolver->SetResolveCallback(ScwmResolve);
  return SCM_UNDEFINED;
}
#undef FUNC_NAME


SCWM_PROC (window_clv_x, "window-clv-x", 1, 0, 0,
           (SCM window))
     /**
      */
#define FUNC_NAME s_window_clv_x
{
  if (!WINDOWP(window)) scm_wrong_type_arg(FUNC_NAME,1,window);
  ScwmWindow const *const psw = PSWFROMSCMWIN(window); assert(psw);
  ScwmWindowConstraintInfo const *const pswci = psw->pswci; assert(pswci);
  return pswci->_scmX;
}
#undef FUNC_NAME

SCWM_PROC (window_clv_y, "window-clv-y", 1, 0, 0,
           (SCM window))
     /**
      */
#define FUNC_NAME s_window_clv_y
{
  if (!WINDOWP(window)) scm_wrong_type_arg(FUNC_NAME,1,window);
  ScwmWindow const *const psw = PSWFROMSCMWIN(window); assert(psw);
  ScwmWindowConstraintInfo const *const pswci = psw->pswci; assert(pswci);
  return pswci->_scmY;
}
#undef FUNC_NAME

SCWM_PROC (window_clv_width, "window-clv-width", 1, 0, 0,
           (SCM window))
     /**
      */
#define FUNC_NAME s_window_clv_width
{
  if (!WINDOWP(window)) scm_wrong_type_arg(FUNC_NAME,1,window);
  ScwmWindow const *const psw = PSWFROMSCMWIN(window); assert(psw);
  ScwmWindowConstraintInfo const *const pswci = psw->pswci; assert(pswci);
  return pswci->_scmWidth;
}
#undef FUNC_NAME

SCWM_PROC (window_clv_height, "window-clv-height", 1, 0, 0,
           (SCM window))
     /**
      */
#define FUNC_NAME s_window_clv_height
{
  if (!WINDOWP(window)) scm_wrong_type_arg(FUNC_NAME,1,window);
  ScwmWindow const *const psw = PSWFROMSCMWIN(window); assert(psw);
  ScwmWindowConstraintInfo const *const pswci = psw->pswci; assert(pswci);
  return pswci->_scmHeight;
}
#undef FUNC_NAME


extern "C" {
void
init_constraint_primitives()
{
#ifndef SCM_MAGIC_SNARFER
#include "constraint-primitives.x"
#endif
}

}

#endif /* USE_CASSOWARY */
