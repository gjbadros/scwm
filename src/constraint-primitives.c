/* $Id$
 * constraint-primitives.cc
 *
 * Temporary until I get the full general cassowary wrapped in guile nicely
 *
 * (C) 1998 Greg J. Badros
 */

#include <config.h>

#ifdef USE_CASSOWARY

#include "scwm.h"
#include "constraint-primitives.h"
#include "scwm-constraints.h"
#include "window.h"
#include "ClLinearEquation.h"
#include "ClLinearInequality.h"
#include "ClSimplexSolver.h"
#include <strstream>

SCWM_PROC(add_stays_on_window, "add-stays-on-window", 1, 0, 0,
          (SCM win))
  /** Add stay constraint on all window dimensions
WIN is a window objects */
{
  int iarg = 1;
  if (!WINDOWP(win)) {
    scm_wrong_type_arg(s_add_stays_on_window, iarg++, win);
  }
  
  ScwmWindow const *const psw = PSWFROMWIN(win);
  const ClVariable &clvX = psw->frame_x;
  const ClVariable &clvY = psw->frame_y;
  const ClVariable &clvW = psw->frame_width;
  const ClVariable &clvH = psw->frame_height;
  solver
    .addPointStay(clvW,clvH,100)
    .addPointStay(clvX,clvY,1);

  /* FIXGJB: need to add hook to remove the stays if win disappears */
  return SCM_UNSPECIFIED;
}



SCWM_PROC(keep_tops_even, "keep-tops-even", 2, 0, 0,
          (SCM winA, SCM winB))
  /** Keep the tops of the frames of winA and winB at the same y position.
WINA and WINB are both window objects */
{
  int iarg = 1;
  if (!WINDOWP(winA)) {
    scm_wrong_type_arg(s_keep_tops_even, iarg++, winA);
  }
  if (!WINDOWP(winB)) {
    scm_wrong_type_arg(s_keep_tops_even, iarg++, winB);
  }
  
  ScwmWindow const *const pswA = PSWFROMWIN(winA);
  ScwmWindow const *const pswB = PSWFROMWIN(winB);
  const ClVariable &clvY_A = pswA->frame_y;
  const ClVariable &clvY_B = pswB->frame_y;
  solver.addVar(clvY_A).addVar(clvY_B);

  ClLinearEquation eqY(clvY_A,clvY_B+0.0);
  strstream ss;
  ss << "Adding constraint: " << eqY << endl << ends;
  fprintf(stderr,"%s",ss.str());

  solver.addConstraint(eqY);
  /* FIXGJB: need to add hook to remove the constraint if winA or winB
     disappears */
  return SCM_UNSPECIFIED;
}


SCWM_PROC(keep_to_left_of, "keep-to-left-of", 2, 0, 0,
          (SCM winA, SCM winB))
  /** Keep the winA to the left of winB
WINA and WINB are both window objects */
{
  int iarg = 1;
  if (!WINDOWP(winA)) {
    scm_wrong_type_arg(s_keep_to_left_of, iarg++, winA);
  }
  if (!WINDOWP(winB)) {
    scm_wrong_type_arg(s_keep_to_left_of, iarg++, winB);
  }
  
  ScwmWindow const *const pswA = PSWFROMWIN(winA);
  ScwmWindow const *const pswB = PSWFROMWIN(winB);
  const ClVariable &clvX_A = pswA->frame_x;
  const ClVariable &clvW_A = pswA->frame_width;
  const ClVariable &clvX_B = pswB->frame_x;
  const ClVariable &clvW_B = pswA->frame_width;
  solver.addVar(clvX_A).addVar(clvX_B);
  solver.addVar(clvW_A).addVar(clvW_B);

  ClLinearInequality ineqX(clvX_A+clvW_A,cnLEQ,clvX_B);
  strstream ss;
  ss << "Adding constraint: " << ineqX << endl << ends;
  fprintf(stderr,"%s",ss.str());

  solver.addConstraint(ineqX);
  /* FIXGJB: need to add hook to remove the constraint if winA or winB
     disappears */
  return SCM_UNSPECIFIED;
}


#if 0
/* not needed -- can do in scheme */
SCWM_PROC(resolve_and_reposition, "resolve-and-reposition", 0, 0, 0,
          ())
{
  
  return SCM_UNSPECIFIED;
}
#endif

void
init_constraint_primitives()
{
#ifndef SCM_MAGIC_SNARFER
#include "constraint-primitives.x"
#endif
}

#endif /* USE_CASSOWARY */
