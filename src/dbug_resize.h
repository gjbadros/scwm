/* $Id$
 * dbug_resize.h 
 * Greg Badros 
 * <gjb@cs.washington.edu> 
 * Seattle, WA  USA  {FIX: use env_vars}
 * http://www.cs.washington.edu/homes/gjb
 */

#ifndef DBUG_RESIZE_H__
#define DBUG_RESIZE_H__

#ifdef SCWM_DEBUG_RESIZE_MSGS
#  define DBUG_RESIZE(X) scwm_msg X
#else
#  define DBUG_RESIZE(X)		/* no messages */
#endif

#endif /* DBUG_RESIZE_H__ */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta */

