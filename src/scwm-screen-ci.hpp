/* $Id$
 * scwm-screen-ci.hpp
 * Copyright (C) 1998, 1999, 2000 Greg J. Badros
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
#include "Cl.h"
#include "cassowary_scm.hpp"

class ScwmScreenConstraintInfo {
public:
  ScwmScreenConstraintInfo(ScreenInfo *pscreen)
    {
      void *pvScr = PvFromScm(pscreen->schscreen);
      _vx.SetName("vx");
      _vy.SetName("vy");
      _pointerx.SetName("px");
      _pointery.SetName("py");
      _vx.SetPv(pvScr);
      _vy.SetPv(pvScr);
      _pointerx.SetPv(pvScr);
      _pointery.SetPv(pvScr);
      scwm_defer_ints();
      scm_protect_object(_scmVx = ScmMakeClVariable(&_vx));
      scm_protect_object(_scmVy = ScmMakeClVariable(&_vy));
      scm_protect_object(_scmPointerX = ScmMakeClVariable(&_pointerx));
      scm_protect_object(_scmPointerY = ScmMakeClVariable(&_pointery));
      scwm_allow_ints();
    }

#if 0
  /* GJB:FIXME:: will need to do something with this when supporting
     multiple screens better */
  ScreenInfo *Pscreen() const
    {
      return static_cast<ScreenInfo *>(_vx.Pv());
    }
#endif

  void
  AddStays(ClSimplexSolver *psolver)
    {
      psolver->AddStay(_vx,ClsMedium(),1.0);
      psolver->AddStay(_vy,ClsMedium(),1.0);
      ClLinearInequality *pminX = new ClLinearInequality(_vx,cnGEQ,0);
      ClLinearInequality *pminY = new ClLinearInequality(_vy,cnGEQ,0);
      ClLinearInequality *pmaxX = new ClLinearInequality(_vx,cnLEQ,Scr.VxMax);
      ClLinearInequality *pmaxY = new ClLinearInequality(_vy,cnLEQ,Scr.VyMax);
      (*psolver)
        .AddConstraint(*pminX)
        .AddConstraint(*pminY)
        .AddConstraint(*pmaxX)
        .AddConstraint(*pmaxY);
      /*     psolver->AddPointStay(_pointerx,_pointery); */
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
