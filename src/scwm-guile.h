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

#define SCWM_NEWCELL_SMOB(ANSWER,ID,PSMOB) SCM_NEWSMOB(ANSWER, ID, PSMOB)

#define DEREF_IF_SYMBOL(x) do { if (scm_is_symbol(x)) { \
                                   (x) = scm_variable_ref(scm_lookup(x)); \
                                } } while (0)

#define DYNAMIC_PROCEDURE_P(x) (scm_to_bool(scm_procedure_p(x)) || \
				(scm_is_symbol(x) && \
				 scm_to_bool(scm_procedure_p(scm_variable_ref(scm_lookup(x))))))

#define PROCEDURE_OR_SYMBOL_P(x) (scm_to_bool(scm_procedure_p(x)) || scm_is_symbol(x))

#define RESTP_SCM 1


#define scwm_ptr2scm(p) scm_from_long((long)(p))

SCWM_INLINE 
static void scwm_defer_ints() {
    // SCM_REDEFER_INTS;
}

SCWM_INLINE 
static void scwm_allow_ints() {
    // SCM_REALLOW_INTS;
}

#endif
