/* $Id$
 * decor.h
 * Copyright (C) 1997, 1998, 1999, 2000 Maciej Stachowiak and Greg J. Badros
 */


#ifndef DECOR_H
#define DECOR_H

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include <libguile.h>

#include "scwm.h"
#include "screen.h"

#undef EXTERN
#undef EXTERN_SET
#ifdef DECOR_IMPLEMENTATION
#define EXTERN
#define EXTERN_SET(x,y) x = y
#else
#define EXTERN extern
#define EXTERN_SET(x,y) extern x
#endif


typedef struct {
  ScwmDecor *sd;
  int refcnt;
} scwm_decor;

EXTERN_SET(ScwmDecor *last_decor,NULL);
EXTERN_SET(ScwmDecor *cur_decor,NULL);

EXTERN long scm_tc16_scwm_decor;

#define DECORP(X) (SCM_SMOB_PREDICATE(scm_tc16_scwm_decor, X))
#define DECOR(X)  ((scwm_decor *)SCM_SMOB_DATA(X))
#define SCWMDECOR(X) (DECOR(X)->sd)

/* These macros should only be used in two places, though,
   setting the current decor that commands are redirected to, and
   setting the default decor. Other than that, the mark/sweep
   algorithm should be sufficient. */

#define DECORREF(X) do {if (DECOR(X)->refcnt == 0) {DECOR(X)->refcnt++; scm_gc_protect_object(X);}} while (0)
#define DECORUNREF(X) do {int x = --(DECOR(X)->refcnt); if (x == 0) scm_gc_unprotect_object(X); else if (x < 0) DECOR(X)->refcnt = 0; } while (0)

SCM decor2scm(ScwmDecor * fl);

#define VALIDATE_ARG_DECOR(pos,arg,cvar) \
  do { if (!DECORP(arg)) scm_wrong_type_arg(FUNC_NAME,pos,arg); } while (0)

#define VALIDATE_ARG_DECOR_COPY(pos,arg,cvar) \
  do { if (!DECORP(arg)) scm_wrong_type_arg(FUNC_NAME,pos,arg); \
       else cvar = SCWMDECOR(arg); } while (0)

#define VALIDATE_ARG_DECOR_COPY_USE_NULL(pos,arg,cvar) \
  do { if (UNSET_SCM(arg)) cvar = NULL; \
       else if (!DECORP(arg)) scm_wrong_type_arg(FUNC_NAME,pos,arg); \
       else cvar = SCWMDECOR(arg); } while (0)


#endif /* DECOR_H */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

