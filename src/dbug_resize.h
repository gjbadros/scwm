/* $Id$
 * dbug_resize.h 
 * Greg Badros 
 * <gjb@cs.washington.edu> 
 * Seattle, WA  USA  {FIX: use env_vars}
 * http://www.cs.washington.edu/homes/gjb
 */

#ifndef DBUG_RESIZE_H__
#define DBUG_RESIZE_H__


#ifdef __GNUC__
#  ifdef SCWM_DEBUG_RESIZE_MSGS
#    define DBUG_RESIZE(x,y...) scwm_msg(DBG,x,## y)
#  else
#    define DBUG_RESIZE(x,y...)		/* no messages */
#  endif
#else
/* Not GNUC, so no varargs macros */
#  ifdef SCWM_DEBUG_RESIZE_MSGS
#    define DBUG_RESIZE(x,y) scwm_msg(DBG,x,y)
#    define DBUG_RESIZE(x,y,a) scwm_msg(DBG,x,y,a)
#    define DBUG_RESIZE(x,y,a,b) scwm_msg(DBG,x,y,a,b)
#    define DBUG_RESIZE(x,y,a,b,c) scwm_msg(DBG,x,y,a,b,c)
#    define DBUG_RESIZE(x,y,a,b,c,d) scwm_msg(DBG,x,y,a,b,c,d)
#    define DBUG_RESIZE(x,y,a,b,c,d,e) scwm_msg(DBG,x,y,a,b,c,d,e)
#  else
#    define DBUG_RESIZE(x,y)
#    define DBUG_RESIZE(x,y,a)
#    define DBUG_RESIZE(x,y,a,b)
#    define DBUG_RESIZE(x,y,a,b,c)
#    define DBUG_RESIZE(x,y,a,b,c,d)
#    define DBUG_RESIZE(x,y,a,b,c,d,e)
#  endif
#endif


#endif /* DBUG_RESIZE_H__ */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
