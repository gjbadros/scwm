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

#if defined(GUILE_CPLUSPLUS_SNARF)
SCM scwm_make_gsubr(const char *name, int req, int opt, int rst, SCM (*fcn)(...), char *szArgList);
#else
SCM scwm_make_gsubr(const char *name, int req, int opt, int rst, SCM (*fcn)(), char *szArgList);
#endif

/* do not define this macro if we are extracting comments since
   the macro name is used as a lexical cue to the extractor */

#ifndef SCM_MAGIC_SNARFER
#define SCWM_HOOK(var, name, args, docstring) \
static SCM var
#else

#ifndef HAVE_SCM_MAKE_HOOK
#define SCWM_HOOK(var, name, args, docstring) \
SCM__I     do { var = scm_sysintern(name, SCM_EOL); } while (0)
#else
#ifdef HAVE_SCM_CREATE_HOOK
#define SCWM_HOOK(var, name, args, docstring) \
SCM__I     do { var = scm_create_hook(name,args); } while (0)
#else
#define SCWM_HOOK(var, name, args, docstring) \
SCM__I     do { var = scm_make_named_hook(name,args); } while (0)
#endif
#endif

#endif

#ifndef SCM_MAGIC_SNARFER
#define SCWM_GLOBAL_HOOK(var, name, args, docstring) \
SCM var
#else

#ifndef HAVE_SCM_MAKE_HOOK
#define SCWM_GLOBAL_HOOK(var, name, args, docstring) \
SCM__I     do { var = scm_sysintern(name, SCM_EOL); } while (0)
#else
#ifdef HAVE_SCM_CREATE_HOOK
#define SCWM_GLOBAL_HOOK(var, name, args, docstring) \
SCM__I     do { var = scm_create_hook(name,args); } while (0)
#else
#define SCWM_GLOBAL_HOOK(var, name, args, docstring) \
SCM__I     do { var = scm_make_named_hook(name,args); } while (0)
#endif
#endif

#endif

/* GJB:FIXME:: need to signify interactive procedures somehow */
#define SCWM_IPROC(x1,x2,x3,x4,x5,x6,x7,x8) SCM_DEFINE(x1,x2,x3,x4,x5,x6,x8)

#ifndef SCM_MAGIC_SNARFER
#define SCWM_PROPERTY_HANDLER(var, sym, getter, setter) \
static scwm_property_handler var = { &setter, &getter }
#else
#define SCWM_PROPERTY_HANDLER(var, sym, getter, setter) \
SCM__I     set_property_handler (sym, &var)
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

