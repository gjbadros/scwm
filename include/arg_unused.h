/* $Id$
 * (C) 1999, 2000 Greg J. Badros
 */

#ifndef ARG_UNUSED_H__
#define ARG_UNUSED_H__


/* Can mark unused formals as ARG_IGNORE or ARG_UNUSED and avoid warning;
   uses a gcc feature, but C++ also can do this by just
   not giving a formal name.
   ARG_IGNORE is for arguments that really won't be used.
   whereas ARG_UNUSED just comments that the argument is not used at present
     and might be worth revisiting to see if we can generalize the code
     to use it. (Or if alternate implementations might use the variable) */
#ifdef __GNUC__
/* do not use the variable name as given-- paste an
   "_unused" on to the end so we force an error if
   it is used. */
#define ARG_IGNORE(x) x ## _ignore __attribute__ ((unused))
#define ARG_UNUSED(x) x ## _unused __attribute__ ((unused))
#elif defined(__cplusplus)
#define ARG_IGNORE(x) /* empty */
#define ARG_UNUSED(x) /* empty */
#else
#define ARG_IGNORE(x) x ## _ignore
#define ARG_UNUSED(x) x ## _unused
#endif


#endif
