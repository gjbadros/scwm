/* $Id$ */
#ifndef CALLBACKS_H
#define CALLBACKS_H

#include <guile/gh.h>

SCM scwm_handle_error (void *handler_data, SCM tag, SCM throw_args);
SCM scwm_safe_apply_message_only (SCM proc, SCM args);

/* Individual callbacks. */

SCM scwm_safe_apply (SCM proc, SCM args);
SCM scwm_safe_call0 (SCM thunk);
SCM scwm_safe_call1 (SCM proc, SCM arg);
SCM safe_load (SCM fname);
SCM scwm_safe_load (char *filename);
SCM scwm_safe_eval_str (char *string);

/* Hooks. */

#define DEFINE_HOOK(var, name) var=scm_sysintern(name, SCM_EOL)

SCM call0_hooks (SCM hook);
SCM call1_hooks (SCM hook_type, SCM arg);
SCM apply_hooks (SCM hook_type, SCM args);
SCM apply_hooks_message_only (SCM hook_type, SCM args);

/* Timer hooks. */

SCM add_timer_hook_x(SCM usec, SCM proc);
SCM remove_timer_hook_x(SCM handle);

long shortest_timer_timeout();
void update_timer_hooks();
void run_timed_out_timers();

/* Input hooks. */

SCM add_input_hook_x (SCM fd, SCM proc);
SCM remove_timer_hook_x(SCM handle);

void add_hook_fds_to_set(fd_set *in_fdset, int *fd_width);
void force_new_input_hooks();
void run_input_hooks(fd_set *in_fdset);

/* Initialization. */

void init_callbacks();

#endif /* CALLBACKS_H */
