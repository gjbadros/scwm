/* $Id$
 * scwm-constraints.hpp
 *
 * (C) 1998 Greg J. Badros
 */

#ifndef SCWM_CONSTRAINTS_HPP__
#define SCWM_CONSTRAINTS_HPP__


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <assert.h>
#include "scwm-constraints.h"
#include "window.h"
#include "xmisc.h"
#include <strstream.h>
#include "ClVariable.h"
#include "ClSimplexSolver.h"
#include "../guile/cassowary_scm.hpp"

class ScwmWindowConstraintInfo {
public:
  ScwmWindowConstraintInfo(ScwmWindow *psw)
    {
      if (psw->name != NoName) {
        int ich = strlen(psw->name);
        char *szNm = NEWC(ich+3,char);
        strcpy(szNm, psw->name);
        szNm[ich++] = '/';
        szNm[ich+1] = '\0';
        szNm[ich] = 'x'; _frame_x.setName(szNm);
        szNm[ich] = 'y'; _frame_y.setName(szNm);
        szNm[ich] = 'w'; _frame_width.setName(szNm);
        szNm[ich] = 'h'; _frame_height.setName(szNm);
      }
      _frame_x.setPv(psw);
      _frame_y.setPv(psw);
      _frame_width.setPv(psw);
      _frame_height.setPv(psw);
      SCM_DEFER_INTS;
      scm_protect_object(_scmX = ScmMakeClVariable(&_frame_x));
      scm_protect_object(_scmY = ScmMakeClVariable(&_frame_y));
      scm_protect_object(_scmWidth = ScmMakeClVariable(&_frame_width));
      scm_protect_object(_scmHeight = ScmMakeClVariable(&_frame_height));
      SCM_ALLOW_INTS;
    }

  void
  AddStays(ClSimplexSolver *psolver)
    {
      // FIXGJB: these weights should increase each time this is called
      psolver->addPointStay(_frame_width,_frame_height,100);
      psolver->addPointStay(_frame_x,_frame_y,1);
    }

  ScwmWindow *Psw() const
    {
      assert(_frame_x.Pv() == _frame_y.Pv());
      assert(_frame_x.Pv() == _frame_width.Pv());
      assert(_frame_x.Pv() == _frame_height.Pv());
      return static_cast<ScwmWindow *>(_frame_x.Pv());
    }

  void CopyStateToPswVars() const
    {
      ScwmWindow *psw = Psw();
      assert(psw);
      psw->frame_x = _frame_x.intValue();
      psw->frame_y = _frame_y.intValue();
      psw->frame_width = _frame_width.intValue();
      psw->frame_height = _frame_height.intValue();
    }

  ClVariable _frame_x;
  ClVariable _frame_y;
  ClVariable _frame_width;
  ClVariable _frame_height;
  SCM _scmX;
  SCM _scmY;
  SCM _scmWidth;
  SCM _scmHeight;
};


#endif
