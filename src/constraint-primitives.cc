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
#include "scwm-screen-ci.hpp"
#include "ClLinearEquation.h"
#include "ClLinearInequality.h"
#include "ClSimplexSolver.h"
#include "../guile/cassowary_scm.hpp"
#include "../guile/cassowary_scm.h"
#include "screen.h" /* to be able to add stays to all the windows */
#include <strstream>
#include <set>


ClSimplexSolver *psolver;

static set<ScwmWindow *> setpswDirty;

static void
ScwmClvChanged(ClVariable *pclv, ClSimplexSolver *)
{
  ScwmWindow *psw = static_cast<ScwmWindow *>(pclv->Pv());
  if (!psw) {
    DBUG(__FUNCTION__,"No struct ScwmWindow attached to var: %s", pclv->name().data());
    return;
  }
  if (!psolver) {
    return;
  }
  /* only gets inserted if not NULL */
  setpswDirty.insert(psw);
}

static void
ScwmResolve(ClSimplexSolver *psolver)
{
  // go through the dirty windows and move them
  set<ScwmWindow *>::const_iterator it = setpswDirty.begin();
  for ( ; it != setpswDirty.end(); ++it ) {
    bool fMoved, fResized;
    ScwmWindow *psw = *it;
    assert(psw);
    psw->pswci->CopyStateToPswVars(&fMoved, &fResized);
#ifndef NDEBUG
    if (fMoved && fResized) {
      DBUG(__FUNCTION__,"Move and resize of %s",psw->name);
    } else if (fMoved) {
      DBUG(__FUNCTION__,"Move of %s",psw->name);
    } else if (fResized) {
      DBUG(__FUNCTION__,"Resize of %s",psw->name);
    }
#endif
    /* resize subsumes a move, so check for it first */
    if (fResized) ResizePswToCurrentSize(psw);
    else if (fMoved) MovePswToCurrentPosition(psw);
  }
  setpswDirty.clear();
}


SCWM_PROC(add_stays_on_window, "add-stays-on-window", 1, 0, 0,
          (SCM win))
  /** Add stay constraint on all window dimensions.
WIN is a window object.  This is done automatically for all
current windows when a solver is made the master solver via
`scwm-set-master-solver' and is also done for all newly-created
windows.   Occasions for using this primitive are rare. */
#define FUNC_NAME s_add_stays_on_window
{
  int iarg = 1;
  if (!WINDOWP(win)) {
    scm_wrong_type_arg(FUNC_NAME, iarg++, win);
  }
  
  ScwmWindow const *const psw = PSWFROMSCMWIN(win);
  ScwmWindowConstraintInfo *pswci = psw->pswci;
  pswci->AddStays(psolver);
  pswci->AddSizeConstraints(psolver);

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
  /* empty the set of dirty windows, just in case */
  setpswDirty.clear();

  /* now add stay constriants on all existing windows */
  for (ScwmWindow *psw = Scr.ScwmRoot.next; NULL != psw; psw = psw->next) {
    CassowarySetCValuesAndSolve(psw,False);
    psw->pswci->AddStays(psolver);
    psw->pswci->AddSizeConstraints(psolver);
  }

  Scr.pssci->AddStays(psolver);

  psolver->SetChangeClvCallback(ScwmClvChanged);
  psolver->SetResolveCallback(ScwmResolve);
  return SCM_UNDEFINED;
}
#undef FUNC_NAME


SCWM_PROC (window_clv_xl, "window-clv-xl", 1, 0, 0,
           (SCM window))
     /** Return the cl-variable object for the left X coordinate of WINDOW.
      */
#define FUNC_NAME s_window_clv_xl
{
  if (!WINDOWP(window)) scm_wrong_type_arg(FUNC_NAME,1,window);
  ScwmWindow const *const psw = PSWFROMSCMWIN(window); assert(psw);
  ScwmWindowConstraintInfo const *const pswci = psw->pswci; assert(pswci);
  return pswci->_scmXL;
}
#undef FUNC_NAME

SCWM_PROC (window_clv_yt, "window-clv-yt", 1, 0, 0,
           (SCM window))
     /** Return the cl-variable object for the top Y coordinate of WINDOW.
      */
#define FUNC_NAME s_window_clv_yt
{
  if (!WINDOWP(window)) scm_wrong_type_arg(FUNC_NAME,1,window);
  ScwmWindow const *const psw = PSWFROMSCMWIN(window); assert(psw);
  ScwmWindowConstraintInfo const *const pswci = psw->pswci; assert(pswci);
  return pswci->_scmYT;
}
#undef FUNC_NAME

SCWM_PROC (window_clv_xr, "window-clv-xr", 1, 0, 0,
           (SCM window))
     /** Return the cl-expression object for the right X coordinate of WINDOW.
      */
#define FUNC_NAME s_window_clv_xr
{
  if (!WINDOWP(window)) scm_wrong_type_arg(FUNC_NAME,1,window);
  ScwmWindow const *const psw = PSWFROMSCMWIN(window); assert(psw);
  ScwmWindowConstraintInfo const *const pswci = psw->pswci; assert(pswci);
  return pswci->_scmXR;
}
#undef FUNC_NAME

SCWM_PROC (window_clv_yb, "window-clv-yb", 1, 0, 0,
           (SCM window))
     /** Return the cl-expression object for the bottom Y coordinate of WINDOW.
      */
#define FUNC_NAME s_window_clv_yb
{
  if (!WINDOWP(window)) scm_wrong_type_arg(FUNC_NAME,1,window);
  ScwmWindow const *const psw = PSWFROMSCMWIN(window); assert(psw);
  ScwmWindowConstraintInfo const *const pswci = psw->pswci; assert(pswci);
  return pswci->_scmYB;
}
#undef FUNC_NAME


SCWM_PROC (window_clv_width, "window-clv-width", 1, 0, 0,
           (SCM window))
     /** Return the cl-variable object for the width of WINDOW.
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
     /** Return the cl-variable object for the height of WINDOW.
      */
#define FUNC_NAME s_window_clv_height
{
  if (!WINDOWP(window)) scm_wrong_type_arg(FUNC_NAME,1,window);
  ScwmWindow const *const psw = PSWFROMSCMWIN(window); assert(psw);
  ScwmWindowConstraintInfo const *const pswci = psw->pswci; assert(pswci);
  return pswci->_scmHeight;
}
#undef FUNC_NAME


/*** Now the wrappers for the screen ClVariables */

SCWM_PROC (screen_clv_vx, "screen-clv-vx", 0, 0, 0,
           ())
     /** Return the cl-variable object for the virtual screen X coordinate.
*/
#define FUNC_NAME s_screen_clv_vx
{
  return Scr.pssci->_scmVx;
}
#undef FUNC_NAME


SCWM_PROC (screen_clv_vy, "screen-clv-vy", 0, 0, 0,
           ())
     /** Return the cl-variable object for the virtual screen Y coordinate.
*/
#define FUNC_NAME s_screen_clv_vy
{
  return Scr.pssci->_scmVx;
}
#undef FUNC_NAME

SCWM_PROC (screen_clv_pointerx, "screen-clv-pointerx", 0, 0, 0,
           ())
     /** Return the cl-variable object for the virtual screen X coordinate.
*/
#define FUNC_NAME s_screen_clv_pointerx
{
  return Scr.pssci->_scmVx;
}
#undef FUNC_NAME


SCWM_PROC (screen_clv_pointery, "screen-clv-pointery", 0, 0, 0,
           ())
     /** Return the cl-variable object for the virtual screen Y coordinate.
*/
#define FUNC_NAME s_screen_clv_pointery
{
  return Scr.pssci->_scmVx;
}
#undef FUNC_NAME

SCWM_PROC(cl_windows_of_constraint, "cl-windows-of-constraint", 1, 0, 0,
          (SCM cn))
  /** Return a list of window objects who have variables used by CN */
#define FUNC_NAME s_cl_windows_of_constraint
{
  int iarg = 1;
  if (!FIsClConstraintScm(cn))
    scm_wrong_type_arg(FUNC_NAME,iarg++,cn);

  ClConstraint *pcn = PcnFromScm(cn);

  SCM answer = SCM_EOL;
  set<ScwmWindow *> setpsw;

  ClLinearExpression expr = pcn->expression();
  const ClLinearExpression::ClVarToCoeffMap &mapclv = expr.terms();

  ClLinearExpression::ClVarToCoeffMap::const_iterator it = mapclv.begin();
  for ( ; it != mapclv.end(); ++it) {
    const ClAbstractVariable *pclav = (*it).first;
    const ClVariable *pclv = dynamic_cast<const ClVariable *>(pclav);
    if (!pclv)
      continue;
    ScwmWindow *psw = static_cast<ScwmWindow *>(pclv->Pv());
    if (psw)
      setpsw.insert(psw);
  }
  
  set<ScwmWindow *>::const_iterator itw = setpsw.begin();
  for ( ; itw != setpsw.end(); ++itw ) {
    ScwmWindow *psw = *itw;
    answer = gh_cons(psw->schwin,answer);
  }

  return answer;
}


extern "C" {
void
init_constraint_primitives()
{
  init_cassowary_scm();
#ifndef SCM_MAGIC_SNARFER
#include "constraint-primitives.x"
#endif
}

}

#endif /* USE_CASSOWARY */
