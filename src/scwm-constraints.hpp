/* $Id$
 * scwm-constraints.hpp
 * (C) 1998 Greg J. Badros
 *
 * This header file contains the definition of the ScwmWindowConstraintInfo class
 * A SWCI is stored with each ClVariable so that when the ClVariable is changed,
 * the resolve callback (ScwmResolve) can get at the SWCI, which then permits
 * access to the other clvariables for the window, as well as the ScwmWindow *
 * itself (which in turn permits access to the Scheme-level and X11-level windows
 *
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
#include <strstream>
#include <string>
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
        _name = szNm;
        szNm[ich++] = '/';
        szNm[ich+1] = '\0';
        szNm[ich] = 'x'; _frame_x.setName(szNm);
        szNm[ich] = 'y'; _frame_y.setName(szNm);
        szNm[ich] = 'w'; _frame_width.setName(szNm);
        szNm[ich] = 'h'; _frame_height.setName(szNm);
        free(szNm);
      }
      _frame_x.setPv(psw);
      _frame_y.setPv(psw);
      _frame_width.setPv(psw);
      _frame_height.setPv(psw);
      gh_defer_ints();
      scm_protect_object(_scmXL = ScmMakeClVariable(&_frame_x));
      scm_protect_object(_scmYT = ScmMakeClVariable(&_frame_y));
      scm_protect_object(_scmWidth = ScmMakeClVariable(&_frame_width));
      scm_protect_object(_scmHeight = ScmMakeClVariable(&_frame_height));

      ClLinearExpression *pexprXR = new ClLinearExpression(_frame_x);
      pexprXR->addVariable(_frame_width);
      scm_protect_object(_scmXR = ScmMakeClLinearExpression(pexprXR));

      ClLinearExpression *pexprYB = new ClLinearExpression(_frame_y);
      pexprYB->addVariable(_frame_height);
      scm_protect_object(_scmYB = ScmMakeClLinearExpression(pexprYB));

      gh_allow_ints();
    }

  void
  AddStays(ClSimplexSolver *psolver)
    {
      DBUG((DBG,__FUNCTION__,"Adding stays for window %s: (%d,%d) %d x %d",
            Psw()->name, _frame_x.intValue(), _frame_y.intValue(),
            _frame_width.intValue(), _frame_height.intValue()));
      // FIXGJB: these weights should increase each time this is called
      psolver->addPointStay(_frame_width,_frame_height,100);
      psolver->addPointStay(_frame_x,_frame_y,1);
    }

  void
  AddSizeConstraints(ClSimplexSolver *psolver)
    {
      int minWidth, minHeight, maxWidth, maxHeight;
      ScwmWindow *psw = Psw();
      assert(psw);
      minWidth = psw->hints.min_width;
      minHeight = psw->hints.min_height;

      maxWidth = psw->hints.max_width;
      maxHeight = psw->hints.max_height;

      // Required constraints
      ClLinearInequality *pineqMinWidth = new ClLinearInequality(_frame_width,cnGEQ,minWidth);
      ClLinearInequality *pineqMaxWidth = new ClLinearInequality(_frame_width,cnLEQ,maxWidth);
      ClLinearInequality *pineqMinHeight = new ClLinearInequality(_frame_height,cnGEQ,minHeight);
      ClLinearInequality *pineqMaxHeight = new ClLinearInequality(_frame_height,cnLEQ,maxHeight);
      (*psolver)
        .addConstraint(*pineqMinWidth)
        .addConstraint(*pineqMaxWidth)
        .addConstraint(*pineqMinHeight)
        .addConstraint(*pineqMaxHeight);
    }

  ScwmWindow *Psw() const
    {
#ifndef NDEBUG
      if (_frame_x.Pv() != _frame_y.Pv() ||
          _frame_x.Pv() != _frame_width.Pv() ||
          _frame_x.Pv() != _frame_height.Pv()) {
        strstream ss;
        ss << "Bad Pv in variable of SWCI for window named " << _name << "; "
           << &_frame_x << ", " << &_frame_y << ", "
           << &_frame_width << ", " << &_frame_height << ends;
        scwm_msg(ERR,__FUNCTION__,ss.str());
        assert(False);
      }
#endif
      return static_cast<ScwmWindow *>(_frame_x.Pv());
    }

  void CopyStateToPswVars(bool *pfMoved, bool *pfResized) const
    {
      ScwmWindow *psw = Psw();
      assert(psw);
      *pfMoved = false;
      *pfResized = false;

      { /* scope */
        int x = _frame_x.intValue();
        int y = _frame_y.intValue();
        if (psw->frame_x != x || psw->frame_y != y) {
          psw->frame_x = x;
          psw->frame_y = y;
          *pfMoved = true;
        }
      }
      { /* scope */
        int w = _frame_width.intValue();
        int h = _frame_height.intValue();
        if (psw->frame_width != w || psw->frame_height != h) {
          DBUG((DBG,__FUNCTION__,"Resized from (%d x %d) to (%d x %d)",
                psw->frame_width,psw->frame_height, w,h));
          psw->frame_width = w,
          psw->frame_height = h;
          *pfResized = true;
        }
      }
    }

  string _name;
  ClVariable _frame_x;
  ClVariable _frame_y;
  ClVariable _frame_width;
  ClVariable _frame_height;
  SCM _scmXL, _scmXR;
  SCM _scmYT, _scmYB;
  SCM _scmWidth;
  SCM _scmHeight;
};


#endif
