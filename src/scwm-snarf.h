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

#define SCWM_HOOK(var, name, args, docstring)\
SCM_SNARF_HERE(\
static SCM var\
)\
SCM_SNARF_INIT(\
var = scm_permanent_object(scm_make_hook(scm_from_int(args)));\
scm_c_define(name, var)\
)

#define SCWM_GLOBAL_HOOK(var, name, args, docstring)\
SCM_SNARF_HERE(\
SCM var\
)\
SCM_SNARF_INIT(\
var = scm_permanent_object(scm_make_hook(scm_from_int(args)));\
scm_c_define(name, var)\
)


/* GJB:FIXME:: need to signify interactive procedures somehow */
#define SCWM_IPROC(FNAME, PRIMNAME, REQ, OPT, VAR, ARGLIST, ISPEC, DOC) \
SCM_SNARF_HERE(static const char s_##FNAME[]=PRIMNAME;\
SCM FNAME ARGLIST\
)\
SCM_SNARF_INIT(\
scwm_make_igsubr(s_##FNAME, REQ, OPT, VAR, \
                 (SCM_FUNC_CAST_ARBITRARY_ARGS) FNAME, ISPEC, NULL);\
)

#if 0
#define SCM_DEFINE(FNAME, PRIMNAME, REQ, OPT, VAR, ARGLIST, DOCSTRING) \
SCM_SNARF_HERE(\
static const char s_ ## FNAME [] = PRIMNAME; \
SCM FNAME ARGLIST\
)\
SCM_SNARF_INIT(\
scm_c_define_gsubr (s_ ## FNAME, REQ, OPT, VAR, \
                    (SCM_FUNC_CAST_ARBITRARY_ARGS) FNAME); \
)\
SCM_SNARF_DOCS(primitive, FNAME, PRIMNAME, ARGLIST, REQ, OPT, VAR, DOCSTRING)
#endif

#ifndef SCM_MAGIC_SNARFER
#define SCWM_PROPERTY_HANDLER(var, sym, getter, setter) \
static scwm_property_handler var = { &setter, &getter }
#else
#define SCWM_PROPERTY_HANDLER(var, sym, getter, setter) \
SCM_SNARF_INIT(\
set_property_handler (sym, &var)\
)
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

