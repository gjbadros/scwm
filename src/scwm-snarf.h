/* $Id$
 * scwm-snarf.h
 * (C) 1998 Maciej Stachowiak and Greg J. Badros
 */

#ifndef SCWM_SNARF_H__
#define SCWM_SNARF_H__

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <libguile/snarf.h>

/* Can extract comment post-cpp using, e.g.:
gcc -DHAVE_CONFIG_H -I. -I. -I../include -I/usr/X11R6/include -I/uns/include -E -C -DSCWM_EXTRACT_COMMENTS binding.c
*/

#ifndef SCWM_EXTRACT_COMMENTS
#ifndef SCM_MAGIC_SNARFER
#define SCWM_PROC(fname,primname, req, opt, var, ARGLIST) \
	SCM_PROC(s_ ## fname, primname, req, opt, var, fname); \
SCM fname ARGLIST
#else
#define SCWM_PROC(fname,primname, req, opt, var, ARGLIST) \
	SCM_PROC(s_ ## fname, primname, req, opt, var, fname);
#endif
#endif

#define SCWM_SYMBOL(cname,scheme_name) SCM_SYMBOL(cname,scheme_name)
#define SCWM_GLOBAL_SYMBOL(cname,scheme_name) SCM_GLOBAL_SYMBOL(cname,scheme_name)

#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
