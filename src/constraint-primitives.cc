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

#include "scwm.h"
#include "constraint-primitives.h"
#include "scwm-constraints.h"
#include "scwm-constraints.hpp"
#include "window.h"
#include "ClLinearEquation.h"
#include "ClLinearInequality.h"
#include "ClSimplexSolver.h"
#include "../guile/cassowary_scm.hpp"
#include <strstream>

ClSimplexSolver *psolver;

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
  pswci->AddStays();

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
  return SCM_UNDEFINED;
}
#undef FUNC_NAME




// FIXGJB: below are obsoleted by general mechanism
SCWM_PROC(keep_tops_even, "keep-tops-even", 2, 0, 0,
          (SCM winA, SCM winB))
  /** Keep the tops of the frames of winA and winB at the same y position.
WINA and WINB are both window objects */
#define FUNC_NAME s_keep_tops_even
{
  int iarg = 1;
  if (!WINDOWP(winA)) {
    scm_wrong_type_arg(FUNC_NAME, iarg++, winA);
  }
  if (!WINDOWP(winB)) {
    scm_wrong_type_arg(FUNC_NAME, iarg++, winB);
  }
  
  ScwmWindow const *const pswA = PSWFROMSCMWIN(winA);
  ScwmWindow const *const pswB = PSWFROMSCMWIN(winB);
  const ClVariable &clvY_A = pswA->pswci->_frame_y;
  const ClVariable &clvY_B = pswB->pswci->_frame_y;
  psolver->addVar(clvY_A).addVar(clvY_B);

  ClLinearEquation eqY(clvY_A,clvY_B+0.0);
  strstream ss;
  ss << "Adding constraint: " << eqY << endl << ends;
  fprintf(stderr,"%s",ss.str());

  psolver->addConstraint(eqY);
  /* FIXGJB: need to add hook to remove the constraint if winA or winB
     disappears */
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(keep_to_left_of, "keep-to-left-of", 2, 0, 0,
          (SCM winA, SCM winB))
  /** Keep the winA to the left of winB
WINA and WINB are both window objects */
#define FUNC_NAME s_keep_to_left_of
{
  int iarg = 1;
  if (!WINDOWP(winA)) {
    scm_wrong_type_arg(FUNC_NAME, iarg++, winA);
  }
  if (!WINDOWP(winB)) {
    scm_wrong_type_arg(FUNC_NAME, iarg++, winB);
  }
  
  ScwmWindow const *const pswA = PSWFROMSCMWIN(winA);
  ScwmWindow const *const pswB = PSWFROMSCMWIN(winB);
  const ClVariable &clvX_A = pswA->pswci->_frame_x;
  const ClVariable &clvW_A = pswA->pswci->_frame_width;
  const ClVariable &clvX_B = pswB->pswci->_frame_x;
  const ClVariable &clvW_B = pswA->pswci->_frame_width;
  psolver->addVar(clvX_A).addVar(clvX_B);
  psolver->addVar(clvW_A).addVar(clvW_B);

  ClLinearInequality ineqX(clvX_A+clvW_A,cnLEQ,clvX_B);
  strstream ss;
  ss << "Adding constraint: " << ineqX << endl << ends;
  fprintf(stderr,"%s",ss.str());

  psolver->addConstraint(ineqX);
  /* FIXGJB: need to add hook to remove the constraint if winA or winB
     disappears */
  return SCM_UNSPECIFIED;
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
