/* $id$
 * scwm-snarf.h
 * (C) 1998 Maciej Stachowiak and Greg J. Badros
 */

#ifndef SCWM_SNARF_H__
#define SCWM_SNARF_H__

#ifndef SCM_MAGIC_SNARFER
#define SCWM_PROC(fname,primname, req, opt, var, ARGLIST) \
	SCM_PROC(s_ ## fname, primname, req, opt, var, fname); \
SCM fname ARGLIST
#else
#define SCWM_PROC(fname,primname, req, opt, var, ARGLIST) \
	SCM_PROC(s_ ## fname, primname, req, opt, var, fname);
#endif

#endif
