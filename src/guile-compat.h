#ifndef GUILE_COMPAT_H
#define GUILE_COMPAT_H

#include <config.h>

#ifndef HAVE_SCM_PUTS
#define scm_putc(x,y) scm_gen_putc(x,y)
#define scm_puts(x,y) scm_gen_puts(scm_regular_port,x,y)
#endif

#ifndef HAVE_GH_LENGTH
#define gh_length gh_list_length
#endif /* HAVE_GH_LENGTH */

#ifndef HAVE_SCM_INTERNAL_SELECT
#define scm_internal_select select
#endif

SCM scm_parse_path (char *path, SCM tail);

#ifndef HAVE_GH_VECTOR_SET_X
#define gh_vector_set_x gh_vset
#endif

#ifndef HAVE_GH_VECTOR_REF
#define gh_vector_ref gh_vref
#endif

#ifdef HAVE_SCM_THE_LAST_STACK_FLUID
#define DEREF_LAST_STACK scm_fluid_ref(SCM_CDR(scm_the_last_stack_fluid))
#define SET_LAST_STACK(X) scm_fluid_set_x (SCM_CDR (scm_the_last_stack_fluid), (X))

#else
#define DEREF_LAST_STACK SCM_CDR(scm_the_last_stack_var)
#define SET_LAST_STACK(X) SCM_SETCDR(scm_the_last_stack_var, (X))
#endif

#ifndef SCM_EOF_OBJECT_P
#define SCM_EOF_OBJECT_P(x) ((x) == SCM_EOF_VAL)
#endif

#endif GUILE_COMPAT_H





