/* $Id$
 * scwm-guile.h
 * Copyright (C) 1999, 2000 Greg J. Badros <gjb@cs.washington.edu>
 */

#ifndef SCWM_GUILE_H__
#define SCWM_GUILE_H__

#undef SCWM_INLINE
#ifndef __GNUC__
#define SCWM_INLINE
#define SCWM_STATIC_INLINE static
#else
#define SCWM_INLINE __inline__
#define SCWM_STATIC_INLINE static __inline__
#endif

#include <guile/gh.h>

#define SCWM_NEWCELL_SMOB(ANSWER,ID,PSMOB) \
   do { \
     SCM_NEWCELL((ANSWER)); \
     SCM_SETCDR((ANSWER),(SCM) (PSMOB)); \
     SCM_SETCAR((ANSWER),(ID)); \
   } while (0)

#define DEREF_IF_SYMBOL(x) do { if (gh_symbol_p((x))) { \
                                   (x) = scm_symbol_binding(SCM_BOOL_F,(x)); \
                                } } while (0)

#define DYNAMIC_PROCEDURE_P(x) (gh_procedure_p((x)) || \
				(gh_symbol_p((x)) && \
				 gh_procedure_p(scm_symbol_binding(SCM_BOOL_F,(x)))))

#define PROCEDURE_OR_SYMBOL_P(x) (gh_procedure_p((x)) || gh_symbol_p((x)))

#define RESTP_SCM 1


#define scwm_ptr2scm(p) gh_long2scm((long)(p))

#define SCM_BOOL_FromBool(x) ((x)? SCM_BOOL_T: SCM_BOOL_F)


SCWM_INLINE 
static void scwm_defer_ints() {
  SCM_REDEFER_INTS;
}

SCWM_INLINE 
static void scwm_allow_ints() {
  SCM_REALLOW_INTS;
}

#endif
