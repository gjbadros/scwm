//// $Id$
//// Skeleton to use for creating a new type for GUILE
//// (C) 1998, By Greg J. Badros -- 18 July 1998
// X = SCMTYPE (e.g., cl_variable)
// P = SCMTYPE for primitive names (e.g., cl-variable)
// Y = Type name (e.g., ClVariable)
// H = Hungarian tag name (e.g, clv)

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <guile/gh.h>

#include <iostream.h>
#include <assert.h>
#include <strstream.h>
#include "scwm-snarf.h"

#define CASSOWARY_SCM_IMPLEMENTATION
#include "cassowary_scm.h"

#ifdef __cplusplus
extern "C" {
#endif

#define NEW(x) ((x *) safemalloc(sizeof(x)))
#define NEWC(c,x) ((x *) safemalloc((c)*sizeof(x)))
#define FREE(x) free(x)

inline SCM SCM_BOOL_FromF(bool f) { return (f? SCM_BOOL_T: SCM_BOOL_F); }
inline bool FUnsetSCM(SCM scm) { return (scm == SCM_UNDEFINED || scm == SCM_BOOL_F); }

#undef SCMTYPEID
#define SCMTYPEID scm_tc16_XXXXXXXX

long SCMTYPEID;

inline bool FIsTTTTTTTTSCM(SCM scm) 
{ return SCM_NIMP(scm) && SCM_CAR(scm) == (SCM) SCMTYPEID; }

inline TTTTTTTT *PHHHHHHHHFromScm(SCM scm)
{ return (TTTTTTTT *)(SCM_CDR(scm)); }

SCM
mark_XXXXXXXX(SCM scm)
{
  SCM_SETGC8MARK(scm);
  return SCM_BOOL_F;
}

size_t
free_XXXXXXXX(SCM scm)
{
  TTTTTTTT *pHHHHHHHH = PHHHHHHHHFromScm(scm);
  delete pHHHHHHHH;
  return 0;
}

int
print_XXXXXXXX(SCM scm, SCM port, scm_print_state *pstate)
{
  strstream ss;
  TTTTTTTT *pHHHHHHHH = PHHHHHHHHFromScm(scm);
  ss << "#<PPPPPPPP" << *pHHHHHHHH << ">" << ends;
  scm_puts(ss.str(), port);
  return 1;
}

SCWM_PROC (XXXXXXXX_p, "PPPPPPPP?", 1, 0, 0,
           (SCM scm))
#define FUNC_NAME s_XXXXXXXX_p
{
  return SCM_BOOL_FromF(FIsTTTTTTTTSCM(scm));
}
#undef FUNC_NAME

SCWM_PROC (make_XXXXXXXX, "make-PPPPPPPP", 0, 0, 0,
           ())
#define FUNC_NAME s_make_XXXXXXXX
{
  int iarg = 1;

#if 0
  if (!FUnsetSCM(scm)) {
    if ( ) {
      scm_wrong_type_arg(FUNC_NAME, iarg++, scm);
    } else {
      sz = gh_scm2newstr(scm,NULL);
      p = PFromScm(scm);
    }
  }
#endif

  TTTTTTTT *pHHHHHHHH = new TTTTTTTT();

  SCM answer;

  SCM_DEFER_INTS;
  SCM_NEWCELL(answer);
  SCM_SETCAR(answer, (SCM) SCMTYPEID);
  SCM_SETCDR(answer, (SCM) pHHHHHHHH);
  SCM_ALLOW_INTS;

  return answer;
}
#undef FUNC_NAME

#ifdef __cplusplus
} // extern "C"
#endif
