/* $Id$ */
#ifndef CALLBACKS_H
#define CALLBACKS_H

#include <guile/gh.h>

SCM scwm_safe_apply (SCM proc, SCM args);
SCM scwm_safe_call0 (SCM thunk);
SCM scwm_safe_call1 (SCM proc, SCM arg);
SCM safe_load (SCM fname);
SCM scwm_safe_load (char *filename);
SCM scwm_safe_eval_str (char *string);

void init_callbacks();

#endif /* CALLBACKS_H */
