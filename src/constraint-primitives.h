/* $Id$
 * constraint-primitives.h
 *
 * Temporary until I get the full general cassowary wrapped in guile nicely
 *
 * (C) 1998 Greg J. Badros
 */

#ifndef CONSTRAINT_PRIMITIVES_H__
#define CONSTRAINT_PRIMITIVES_H__

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* FIXGJB: this should get into scm_init_funcs conditionally,
   and shouldn't need to be here */
void init_constraint_primitives();

#ifdef __cplusplus
}
#endif

#endif /* CONSTRAINT_PRIMITIVES_H__ */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
