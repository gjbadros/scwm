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

/* do not define this macro if we are extracting comments since
   the macro name is used as a lexical cue to the extractor */

#ifndef SCM_MAGIC_SNARFER
#define SCWM_HOOK(var, name, args) \
static SCM var
#else

#if 0 /* GJB:FIXME:: new style hooks are not yet supported */
#define SCWM_HOOK(var, name, args) \
%%%     do { var = scm_make_named_hook(name,args); } while (0)
#else
#define SCWM_HOOK(var, name, args) \
%%%     do { var = scm_sysintern(name, SCM_EOL); } while (0)
#endif

#endif

#ifndef SCM_MAGIC_SNARFER
#define SCWM_GLOBAL_HOOK(var, name, args) \
SCM var
#else

#if 0 /* GJB:FIXME:: new style hooks are not yet supported */
#define SCWM_GLOBAL_HOOK(var, name, args) \
%%%     do { var = scm_make_named_hook(name,args); } while (0)
#else
#define SCWM_GLOBAL_HOOK(var, name, args) \
%%%     do { var = scm_sysintern(name, SCM_EOL); } while (0)
#endif

#endif


#define SCWM_SYMBOL(cname,scheme_name) SCM_SYMBOL(cname,scheme_name)
#define SCWM_GLOBAL_SYMBOL(cname,scheme_name) SCM_GLOBAL_SYMBOL(cname,scheme_name)

#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
