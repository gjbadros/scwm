/* $Id$
 * decor.h
 * (C) 1997, 1998 Maciej Stachowiak and Greg J. Badros
 */


#ifndef DECOR_H
#define DECOR_H

#ifdef HAVE_CONFIG_H
#include <config.h>
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

#define DECORP(X) (SCM_NIMP(X) && SCM_CAR(X) == (SCM)scm_tc16_scwm_decor)
#define DECOR(X)  ((scwm_decor *)SCM_CDR(X))
#define SCWMDECOR(X) (((scwm_decor *)SCM_CDR(X))->sd)

/* This is broken if you DECORUNREF something more times than you
   DECORREF it. These should only be used in two places, though,
   setting the current decor that commands are redirected to, and
   setting the default decor. Other than that, the mark/sweep
   algorithm should be sufficient. */

#define DECORREF(X) do {if (DECOR(X)->refcnt == 0) {DECOR(X)->refcnt++; scm_protect_object(X);}} while (0)
#define DECORUNREF(X) do {int x = --DECOR(X)->refcnt; if (x == 0) scm_unprotect_object(X); else if (x < 0) DECOR(X)->refcnt = 0; } while (0)


size_t free_decor(SCM obj);
int print_decor(SCM obj, SCM port, scm_print_state * pstate);
SCM mark_decor(SCM obj);

SCM decor2scm(ScwmDecor * fl);
SCM make_decor(SCM name);
SCM default_decor();
SCM set_current_decor_x(SCM decor);
SCM current_decor();
SCM set_window_decor_x(SCM decor, SCM win);

void init_decor();

#endif /* DECOR_H */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
