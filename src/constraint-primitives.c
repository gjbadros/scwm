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
#include "ClSimplexSolver.h"
#include <strstream>

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
  ss << "Adding constraint: " << eqY << endl;
  ss.freeze();
  fprintf(stderr,"%s",ss.str());

  solver.addConstraint(eqY);
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
