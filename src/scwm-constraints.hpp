/* $Id$
 * scwm-constraints.hpp
 * Copyright (C) 1998, 1999, 2000 Greg J. Badros
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
#include "scwmconfig.h"
#endif

#include <assert.h>
#include "scwm-constraints.h"
#include "window.h"
#include "xmisc.h"
#include <strstream>
#include <string>
#include "Cl.h"
#include "cassowary_scm.hpp"

inline ScwmWindow *PswFromClvPv(void *pv) {
  if (!pv)
    return NULL;
  SCM obj = ScmFromPv(pv);
  if (!WINDOWP(obj))
    return NULL;
  return PSWFROMSCMWIN(obj);
}

inline ClVariable *PclvFromClvPv(void *pv) {
  if (!pv)
    return NULL;
  SCM obj = ScmFromPv(pv);
  if (!FIsClVariableScm(obj))
    return NULL;
  return PclvFromScm(obj);
}

class ScwmWindowConstraintInfo {
public:
  ScwmWindowConstraintInfo(ScwmWindow *psw)
    {
      void *pvWin = PvFromScm(SCM_FROM_PSW(psw));
      if (psw->name != NoName) {
        int ich = strlen(psw->name);
        char *szNm = NEWC(ich+3,char);
        strcpy(szNm, psw->name);
        _name = szNm;
        szNm[ich++] = '/';
        szNm[ich+1] = '\0';
        szNm[ich] = 'x'; _frame_x.SetName(szNm);
        szNm[ich] = 'y'; _frame_y.SetName(szNm);
        szNm[ich] = 'w'; _frame_width.SetName(szNm);
        szNm[ich] = 'h'; _frame_height.SetName(szNm);
        free(szNm);
      }
      _frame_x.SetPv(pvWin);
      _frame_y.SetPv(pvWin);
      _frame_width.SetPv(pvWin);
      _frame_height.SetPv(pvWin);
      scwm_defer_ints();
      scm_protect_object(_scmXL = ScmMakeClVariable(&_frame_x));
      scm_protect_object(_scmYT = ScmMakeClVariable(&_frame_y));
      scm_protect_object(_scmWidth = ScmMakeClVariable(&_frame_width));
      scm_protect_object(_scmHeight = ScmMakeClVariable(&_frame_height));

      ClLinearExpression *pexprXR = new ClLinearExpression(_frame_x);
      pexprXR->AddVariable(_frame_width);
      scm_protect_object(_scmXR = ScmMakeClLinearExpression(pexprXR));

      ClLinearExpression *pexprYB = new ClLinearExpression(_frame_y);
      pexprYB->AddVariable(_frame_height);
      scm_protect_object(_scmYB = ScmMakeClLinearExpression(pexprYB));

      scwm_allow_ints();
    }

  void
  AddStays(ClSimplexSolver *psolver)
#define FUNC_NAME "ScwmWindowConstraintInfo::AddStays"
    {
      static int last_weight = 1;
      DBUG((DBG,FUNC_NAME,"Adding stays for window %s: (%d,%d) %d x %d",
            Psw()->name, _frame_x.IntValue(), _frame_y.IntValue(),
            _frame_width.IntValue(), _frame_height.IntValue()));
      psolver->AddPointStay(_frame_width,_frame_height,ClsMedium(),last_weight);
      psolver->AddPointStay(_frame_x,_frame_y,ClsWeak(),last_weight);
      last_weight += 50;
    }
#undef FUNC_NAME

  void
  AddSizeConstraints(ClSimplexSolver *psolver)
    {
      int minWidth, minHeight, maxWidth, maxHeight;
      ScwmWindow *psw = Psw();
      assert(psw);
      // these sizes are client window sizes,
      // but the constraints are frame sizes -- 
      // correct for decorations just below
      minWidth = MinFrameWidth(psw);
      minHeight = MinFrameHeight(psw);

      maxWidth = MaxFrameWidth(psw);
      maxHeight = MaxFrameHeight(psw);
      
      // GJB:FIXME:: these constraints need to be affected by
      // later changes in the variables -- the decoration
      // geometry variables need to be ClVariables ultimately,
      // or the constraints need to be about client sizes instead
      // of frame sizes.

      // Required constraints
      ClLinearInequality *pineqMinWidth = new ClLinearInequality(_frame_width,cnGEQ,minWidth);
      ClLinearInequality *pineqMaxWidth = new ClLinearInequality(_frame_width,cnLEQ,maxWidth);
      ClLinearInequality *pineqMinHeight = new ClLinearInequality(_frame_height,cnGEQ,minHeight);
      ClLinearInequality *pineqMaxHeight = new ClLinearInequality(_frame_height,cnLEQ,maxHeight);
      (*psolver)
        .AddConstraint(*pineqMinWidth)
        .AddConstraint(*pineqMaxWidth)
        .AddConstraint(*pineqMinHeight)
        .AddConstraint(*pineqMaxHeight);
    }

  ScwmWindow *Psw() const
#define FUNC_NAME "ScwmWindowConstraintInfo::Psw"
    {
#ifndef NDEBUG
      if (_frame_x.Pv() != _frame_y.Pv() ||
          _frame_x.Pv() != _frame_width.Pv() ||
          _frame_x.Pv() != _frame_height.Pv()) {
        strstream ss;
        ss << "Bad Pv in variable of SWCI for window named " << _name << "; "
           << &_frame_x << ", " << &_frame_y << ", "
           << &_frame_width << ", " << &_frame_height << ends;
        scwm_msg(ERR,FUNC_NAME,ss.str());
        assert(False);
      }
#endif
      return PswFromClvPv(_frame_x.Pv());
    }
#undef FUNC_NAME

  void CopyStateToPswVars(bool *pfMoved, bool *pfResized) const
#define FUNC_NAME "ScwmWindowConstraintInfo::CopyStateToPswVars"
    {
      ScwmWindow *psw = Psw();
      assert(psw);
      *pfMoved = false;
      *pfResized = false;

      { /* scope */
        int x = _frame_x.IntValue();
        int y = _frame_y.IntValue();
        if (psw->frame_x != x || psw->frame_y != y) {
          psw->frame_x = x;
          psw->frame_y = y;
          *pfMoved = true;
        }
      }
      { /* scope */
        int w = _frame_width.IntValue();
        int h = _frame_height.IntValue();
        if (psw->frame_width != w || psw->frame_height != h) {
          DBUG((DBG,FUNC_NAME,"Resized from (%d x %d) to (%d x %d)",
                psw->frame_width,psw->frame_height, w,h));
          psw->frame_width = w,
          psw->frame_height = h;
          *pfResized = true;
        }
      }
    }
#undef FUNC_NAME

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
