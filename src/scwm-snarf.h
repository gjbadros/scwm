/* $Id$
 * scwm-snarf.h
 * Copyright (C) 1998, 1999, 2000  Greg J. Badros and Maciej Stachowiak 
 */

#ifndef SCWM_SNARF_H__
#define SCWM_SNARF_H__

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include <libguile/snarf.h>
#include "winprop.h"

/* Can extract comment post-cpp using, e.g.:
gcc -DHAVE_CONFIG_H -I. -I. -I../include -I/usr/X11R6/include -I/uns/include -E -C -DSCWM_EXTRACT_COMMENTS binding.c
*/

#if defined(__cplusplus) || defined(GUILE_CPLUSPLUS_SNARF)
SCM scwm_make_gsubr(const char *name, int req, int opt, int rst, SCM (*fcn)(...), char *szArgList);
#else
SCM scwm_make_gsubr(const char *name, int req, int opt, int rst, SCM (*fcn)(), char *szArgList);
#endif

#ifndef SCWM_EXTRACT_COMMENTS
#ifndef SCM_MAGIC_SNARFER
#define SCWM_PROC(fname,primname, req, opt, var, ARGLIST, docstring) \
	SCM_PROC(s_ ## fname, primname, req, opt, var, fname); \
SCM fname ARGLIST

#define SCWM_IPROC(fname,primname, req, opt, var, ARGLIST, intspec, docstring) \
	SCM_PROC(s_ ## fname, primname, req, opt, var, fname); \
SCM fname ARGLIST
#else
#if defined(__cplusplus) || defined(GUILE_CPLUSPLUS_SNARF)
#define SCWM_PROC(fname,primname, req, opt, var, ARGLIST, docstring) \
%%%	scwm_make_gsubr(s_ ## fname, req, opt, var, (SCM (*)(...))fname, %/%/ #ARGLIST); \
$$$ primname #ARGLIST req opt var @@@ docstring @!!!
#define SCWM_IPROC(fname,primname, req, opt, var, ARGLIST, intspec, docstring) \
%%%	scwm_make_igsubr(s_ ## fname, req, opt, var, (SCM (*)(...))fname, intspec, %/%/ #ARGLIST); \
$$$ primname #ARGLIST req opt var @@@ docstring @!!!
#else
#define SCWM_PROC(fname,primname, req, opt, var, ARGLIST, docstring) \
%%%	scwm_make_gsubr(s_ ## fname, req, opt, var, (SCM (*)())   fname, %/%/ #ARGLIST); \
$$$ primname #ARGLIST req opt var @@@ docstring @!!!
#define SCWM_IPROC(fname,primname, req, opt, var, ARGLIST, intspec, docstring) \
%%%	scwm_make_igsubr(s_ ## fname, req, opt, var, (SCM (*)())   fname, intspec, %/%/ #ARGLIST); \
$$$ primname #ARGLIST req opt var @@@ docstring @!!!
#endif
#endif
#endif

/* do not define this macro if we are extracting comments since
   the macro name is used as a lexical cue to the extractor */

#ifndef SCM_MAGIC_SNARFER
#define SCWM_HOOK(var, name, args, docstring) \
static SCM var
#else

#ifdef HAVE_SCM_MAKE_HOOK
#define SCWM_HOOK(var, name, args, docstring) \
%%%     do { var = scm_make_named_hook(name,args); } while (0)
#else
#define SCWM_HOOK(var, name, args, docstring) \
%%%     do { var = scm_sysintern(name, SCM_EOL); } while (0)
#endif

#endif

#ifndef SCM_MAGIC_SNARFER
#define SCWM_GLOBAL_HOOK(var, name, args, docstring) \
SCM var
#else

#if HAVE_SCM_MAKE_HOOK
#define SCWM_GLOBAL_HOOK(var, name, args, docstring) \
%%%     do { var = scm_make_named_hook(name,args); } while (0)
#else
#define SCWM_GLOBAL_HOOK(var, name, args, docstring) \
%%%     do { var = scm_sysintern(name, SCM_EOL); } while (0)
#endif

#endif


#ifndef SCM_MAGIC_SNARFER
#define SCWM_PROPERTY_HANDLER(var, sym, getter, setter) \
static scwm_property_handler var = { &setter, &getter }
#else
#define SCWM_PROPERTY_HANDLER(var, sym, getter, setter) \
%%%     set_property_handler (sym, &var)
#endif


#define SCWM_SYMBOL(cname,scheme_name) SCM_SYMBOL(cname,scheme_name)
#define SCWM_GLOBAL_SYMBOL(cname,scheme_name) SCM_GLOBAL_SYMBOL(cname,scheme_name)

#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

