//// $Id$
//// Skeleton to use for creating a new type for GUILE
//// Copyright (C) 1998-1999, By Greg J. Badros
// SCMTYPE: %[SCMTYPE (e.g., cl_variable -- a C identifier)? %]
// SCM object name: %[SCM object name (e.g., cl-variable -- a Scheme id)? %]
// Wrapped type name: %[Wrapped Type Name (e.g., ClVariable)? %]
// Hungarian Tag: %[Hungarian tag name (e.g, clv)? %]
// By %U
// %d

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <guile/gh.h>

#include <assert.h>
#include "scwm.h"

#define %F_IMPLEMENTATION
#include "%b.h"

#ifdef __cplusplus
extern "C" {
#endif

#define NEW(x) ((x *) safemalloc(sizeof(x)))
#define NEWC(c,x) ((x *) safemalloc((c)*sizeof(x)))
#define FREE(x) free(x)

inline SCM SCM_BOOL_FromF(bool f) { return (f? SCM_BOOL_T: SCM_BOOL_F); }
inline bool FUnsetSCM(SCM scm) { return (scm == SCM_UNDEFINED || scm == SCM_BOOL_F); }


//// %3 wrapper
#undef SCMTYPEID
#define SCMTYPEID scm_tc16_%1

long SCMTYPEID;

inline bool FIs%3Scm(SCM scm) 
{ return SCM_NIMP(scm) && SCM_CAR(scm) == (SCM) SCMTYPEID; }

inline %3 *P%4FromScm(SCM scm)
{ return (%3 *)(SCM_CDR(scm)); }

SCM
mark_%1(SCM scm)
{
  SCM_SETGC8MARK(scm);
  return SCM_BOOL_F;
}

size_t
free_%1(SCM scm)
{
  %3 *p%4 = P%4FromScm(scm);
  delete p%4;
  return 0;
}

int
print_%1(SCM scm, SCM port, scm_print_state *pstate)
{
  strstream ss;
  %3 *p%4 = P%4FromScm(scm);
  ss << "#<%2" << *p%4 << ">" << ends;
  scm_puts(ss.str(), port);
  return 1;
}

SCWM_PROC (%1_p, "%2?", 1, 0, 0,
           (SCM scm))
#define FUNC_NAME s_%1_p
{
  return SCM_BOOL_FromF(FIs%3Scm(scm));
}
#undef FUNC_NAME

SCWM_PROC (make_%1, "make-%2", 0, 0, 0,
           ())
#define FUNC_NAME s_make_%1
{
  int iarg = 1;

%@
#if 0 // FIXME
  if (!FUnsetSCM(scm)) {
    if ( ) {
      scm_wrong_type_arg(FUNC_NAME, iarg++, scm);
    } else {
      sz = gh_scm2newstr(scm,NULL);
      p = PFromScm(scm);
    }
  }
#endif

  %3 *p%4 = new %3();

  SCM answer;

  SCM_DEFER_INTS;
  SCM_NEWCELL(answer);
  SCM_SETCAR(answer, (SCM) SCMTYPEID);
  SCM_SETCDR(answer, (SCM) p%4);
  SCM_ALLOW_INTS;

  return answer;
}
#undef FUNC_NAME


#if 0 // PRIMITIVE FUNCTION TEMPLATE
SCWM_PROC (, , 0, 0, 0,
           ())
#define FUNC_NAME s_
{
  return SCM_UNDEFINED;
}
#undef FUNC_NAME
#endif


MAKE_SMOBFUNS(%1);

void
init_%b()
{
  REGISTER_SCWMSMOBFUNS(%1);

#ifndef SCM_MAGIC_SNARFER
#include "%b.x"
#endif
}

#ifdef __cplusplus
} // extern "C"
#endif
