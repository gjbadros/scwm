#ifndef DECOR_H
#define DECOR_H

#include <libguile.h>
#include "scwm.h"
#include "screen.h"

typedef struct {
  ScwmDecor *sd;
  int refcnt;
} scwm_decor;

extern long scm_tc16_scwm_decor;

#define DECORP(X) (SCM_CAR(X) == (SCM)scm_tc16_scwm_decor)
#define DECOR(X)  ((scwm_decor *)SCM_CDR(X))
#define SCWMDECOR(X) (((scwm_decor *)SCM_CDR(X))->sd)

/* This is broken if you DECORUNREF something more times than you
   DECORREF it. These should only be used in two places, though,
   setting the current decor that commands are redirected to, and
   setting the default decor. Other than that, the mark/sweep
   algorithm should be sufficient. */

#define DECORREF(X) {if (!DECOR(X)->refcnt++) {scm_protect_object(X);}} 
#define DECORUNREF(X) {if (DECOR(X)->refcnt--) {scm_unprotect_object(X);}} 


size_t free_decor (SCM obj);
int print_decor (SCM obj, SCM port, scm_print_state *pstate);

SCM decor2scm(ScwmDecor *fl);
SCM make_decor (SCM name);
SCM default_decor ();
SCM set_current_decor_x (SCM decor);
SCM current_decor();
SCM set_window_decor_x (SCM decor, SCM win);

#endif /* DECOR_H */

