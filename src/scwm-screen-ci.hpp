/* $Id$
 * scwm-screen-ci.hpp
 * (C) 1998 Greg J. Badros
 *
 * This header file contains the definition of the ScwmScreenConstraintInfo class
 * A SSCI is stored with each screen to hold the ClVariables associated with it.
 *
 */

#ifndef SCWM_SCREEN_CI_HPP__
#define SCWM_SCREEN_CI_HPP__


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <assert.h>
#include "scwm-constraints.h"
#include "screen.h"
#include "window.h"
#include "xmisc.h"
#include <strstream>
#include <string>
#include "ClVariable.h"
#include "ClSimplexSolver.h"
#include "../guile/cassowary_scm.hpp"

class ScwmScreenConstraintInfo {
public:
  ScwmScreenConstraintInfo(ScreenInfo *pscreen)
    {
      _vx.setName("vx");
      _vy.setName("vy");
      _pointerx.setName("px");
      _pointery.setName("py");
      _vx.setPv(pscreen);
      _vy.setPv(pscreen);
      _pointerx.setPv(pscreen);
      _pointery.setPv(pscreen);
      SCM_DEFER_INTS;
      scm_protect_object(_scmVx = ScmMakeClVariable(&_vx));
      scm_protect_object(_scmVy = ScmMakeClVariable(&_vy));
      scm_protect_object(_scmPointerX = ScmMakeClVariable(&_pointerx));
      scm_protect_object(_scmPointerY = ScmMakeClVariable(&_pointery));
      SCM_ALLOW_INTS;
    }

  ScreenInfo *Pscreen() const
    {
      return static_cast<ScreenInfo *>(_vx.Pv());
    }

  void CopyStateToPscreenVars(bool *pfVirtualMove) const
    {
      ScreenInfo *pscreen = Pscreen();
      assert(pscreen);
      *pfVirtualMove = false;

      { /* scope */
        int vx = _vx.intValue();
        int vy = _vy.intValue();
        if (pscreen->Vx != vx || pscreen->Vy != vy) {
          pscreen->Vx = vx;
          pscreen->Vy = vy;
          *pfVirtualMove = true;
        }
      }
    }

  ClVariable _vx;
  ClVariable _vy;
  ClVariable _pointerx;
  ClVariable _pointery;
  SCM _scmVx;
  SCM _scmVy;
  SCM _scmPointerX;
  SCM _scmPointerY;
};


#endif
