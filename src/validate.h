/* $Id$
 * validate.h
 * Copyright (C) 1997-1999, Greg J. Badros and Maciej Stachowiak
 *
 * This contains the argument validation macros for standard
 * guile types.  Other argument validation procedures
 * appear in the type-defining header file; e.g., for validating
 * windows, see windows.h, for colors, see color.h.
 *
 * All validation procedures should look something like this
 *
 * VALIDATE_ARG_type_action_USE_default
 *          ^^^^ optional -- macros w/o this use 1 as position parameter
 *                   ^^^^^^^ optional -- COPY/INVERT currently exist
 *                                       for moving scm into C variable
 *                         ^^^^^^^^^^^^ optional -- default is CONTEXT/T/F/DEF
 *
 * "optional" is not meant to imply that all versions of the VALIDATE
 * macro exist-- only those commonly used.
 *
 * All such macros use the value of FUNC_NAME when reporting errors
 * if the function name is passed in an argument (as opposed to being
 * statically determined by where the VALIDATE macro invocation appears)
 * then users of the VALIDATE_ macros should do something like:

#define FUNC_NAME func_name_formal_parameter
VALIDATE_...
VALIDATE_...
#undef FUNC_NAME

 The arguments to the macro correspond to the sub-parts of the macro name.
 ARG is the argument position number (e.g., 1, 2, etc.)
 type is the actual SCM object formal parameter name (e.g., window)
 action has an argument that is the target of the action (a C lvalue)
 default is either implicit (as for T/F [true/false]) or needs a value argument.

 If a default value is permitted, this means to use that value if
 the scheme object is SCM_UNDEFINED or SCM_BOOL_F -- validate macros uses
 the UNSET_SCM(x) macro to test for this cases.

 */

#ifndef VALIDATE_H__
#define VALIDATE_H__


/* Use implied FUNC_NAME (cascaded macro) */
#define SCWM_WRONG_TYPE_ARG(pos,formal) \
   do { scm_wrong_type_arg(FUNC_NAME, pos, formal); } while (0)

/* Sample Usage:
  VALIDATE_ARG_BOOL_COPY(1,modified_p?,fModified);

NOTE: Assignments to the cvar in the error handling
      branch of the _COPY macros are to quiet compiler
      warnings about possibly unitialized variables.
*/
#define VALIDATE_ARG_BOOL_COPY(pos,scm,f) \
  do { \
  if (scm == SCM_BOOL_T) f = True; \
  else if (scm == SCM_BOOL_F) f = False; \
  else { f = False; scm_wrong_type_arg(FUNC_NAME,pos,scm); } \
  } while (0)

#define VALIDATE_ARG_BOOL_COPY_USE_T(pos,scm,f) \
  do { \
  if (scm == SCM_BOOL_T || scm == SCM_UNDEFINED) f = True; \
  else if (scm == SCM_BOOL_F) f = False; \
  else { f = False; scm_wrong_type_arg(FUNC_NAME,pos,scm); } \
  } while (0)

#define VALIDATE_ARG_BOOL_COPY_USE_F(pos,scm,f) \
  do { \
  if (scm == SCM_BOOL_T) f = True; \
  else if (scm == SCM_BOOL_F || scm == SCM_UNDEFINED) f = False; \
  else {f = False; scm_wrong_type_arg(FUNC_NAME,pos,scm); } \
  } while (0)


#define VALIDATE_ARG_BOOL_INVERT(pos,scm,f) \
  do { \
  if (scm == SCM_BOOL_F) f = True; \
  else if (scm == SCM_BOOL_T) f = False; \
  else { f = False; scm_wrong_type_arg(FUNC_NAME,pos,scm); } \
  } while (0)

/* range is [low,high]; i.e., low and high are both okay values */
#define VALIDATE_ARG_INT_RANGE_COPY(pos,scm,low,high,cvar) \
  do { \
  if (!gh_number_p(scm)) scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  cvar = gh_scm2long(scm); \
  if (cvar < low || cvar > high) \
     scm_misc_error(FUNC_NAME,"Argument %S must be in [%S,%S]", \
                    gh_list(gh_int2scm(pos),gh_int2scm(low),gh_int2scm(high),SCM_UNDEFINED)); \
  } while (0)

#define VALIDATE_ARG_INT_MIN_COPY(pos,scm,low,cvar) \
  do { \
  if (!gh_number_p(scm)) scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  cvar = gh_scm2long(scm); \
  if (cvar < low) scm_misc_error(FUNC_NAME,"Argument %S must be greater than %S", \
                                 gh_list(gh_int2scm(pos),gh_int2scm(low),SCM_UNDEFINED)); \
  } while (0)

#define VALIDATE_ARG_INT_MIN(pos,scm,low) \
  do { \
  int cvar; \
  if (!gh_number_p(scm)) scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  cvar = gh_scm2long(scm); \
  if (cvar < low) scm_misc_error(FUNC_NAME,"Argument %S must be greater than %S", \
                                 gh_list(gh_int2scm(pos),gh_int2scm(low),SCM_UNDEFINED)); \
  } while (0)

#define VALIDATE_ARG_INT_MAX_COPY(pos,scm,high,cvar) \
  do { \
  if (!gh_number_p(scm)) scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  cvar = gh_scm2long(scm); \
  if (cvar > high) scm_misc_error(FUNC_NAME,"Argument %S must be less than %S", \
                                  gh_list(gh_int2scm(pos),gh_int2scm(high),SCM_UNDEFINED)); \
  } while (0)

#define VALIDATE_ARG_INT_OR_UNDEF(pos,x) \
  do { \
    if (!UNSET_SCM(x) && !gh_number_p(x)) SCWM_WRONG_TYPE_ARG(pos, x); \
  } while (0)


/* Sample Usage:
  VALIDATE_ARG_INT_COPY_USE_DEF(1,pixels,cpixMoveAmount,10);
  [default to setting cpixMoveAmount to 10 if pixels is not set */
#define VALIDATE_ARG_INT_COPY_USE_DEF(pos,scm,cvar,val) \
  do { \
  if (UNSET_SCM(scm)) cvar = val; \
  else if (gh_number_p(scm)) cvar = gh_scm2int(scm); \
  else { cvar = 0; scm_wrong_type_arg(FUNC_NAME,pos,scm); } \
  } while (0)


#define VALIDATE_ARG_INT_COPY(pos,scm,cvar) \
  do { \
  if (gh_number_p(scm)) cvar = gh_scm2int(scm); \
  else scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  } while (0)

#define VALIDATE_ARG_WINID_COPY(pos,scm,cvar) \
  do { \
  if (gh_number_p(scm)) cvar = (Window) gh_scm2ulong(scm); \
  else scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  } while (0)


#define VALIDATE_ARG_DBL_MIN_COPY(pos,scm,low,cvar) \
  do { \
  if (gh_number_p(scm)) cvar = gh_scm2double(scm); \
  else scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  if (cvar < low) scm_misc_error(FUNC_NAME,"Argument %S must be greater than %S", \
                                 gh_list(gh_int2scm(pos),gh_int2scm(low),SCM_UNDEFINED)); \
  } while (0)


#define VALIDATE_ARG_DBL_COPY(pos,scm,cvar) \
  do { \
  if (gh_number_p(scm)) cvar = gh_scm2double(scm); \
  else scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  } while (0)

#define VALIDATE_ARG_DBL_COPY_USE_DEF(pos,scm,cvar,val) \
  do { \
  if (UNSET_SCM(scm)) cvar = val; \
  else if (gh_number_p(scm)) cvar = gh_scm2double(scm); \
  else scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  } while (0)

#define VALIDATE_ARG_LIST(pos,scm) \
  do { \
  if (!gh_list_p(scm)) scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  } while (0)

#define VALIDATE_ARG_LISTNONEMPTY(pos,scm) \
  do { \
  if (!gh_list_p(scm)) scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  else if (!gh_pair_p(scm)) scm_misc_error(FUNC_NAME,"List must be non-empty.",SCM_EOL); \
  } while (0)

#define VALIDATE_ARG_SYM(pos,scm) \
  do { \
  if (!gh_symbol_p(scm)) scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  } while (0)

#define VALIDATE_ARG_SYM_USE_DEF(pos,scm,def) \
  do { \
  if (UNSET_SCM(scm)) scm = def; \
  else if (!gh_symbol_p(scm)) scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  } while (0)


#define VALIDATE_ARG_STR(pos,scm) \
  do { \
  if (!gh_string_p(scm)) scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  } while (0)

#define VALIDATE_ARG_STR_NEWCOPY(pos,scm,pch) \
  do { \
  if (gh_string_p(scm)) pch = gh_scm2newstr(scm,NULL); \
  else { pch = NULL; scm_wrong_type_arg(FUNC_NAME,pos,scm); } \
  } while (0)

#define VALIDATE_ARG_STR_NEWCOPY_LEN(pos,scm,pch,len) \
  do { \
  if (gh_string_p(scm)) pch = gh_scm2newstr(scm,&len); \
  else { pch = NULL; scm_wrong_type_arg(FUNC_NAME,pos,scm); } \
  } while (0)

#define VALIDATE_ARG_STR_NEWCOPY_USE_NULL(pos,scm,pch) \
  do { \
  if (UNSET_SCM(scm)) pch = NULL; \
  else if (gh_string_p(scm)) pch = gh_scm2newstr(scm,NULL); \
  else { pch = NULL; scm_wrong_type_arg(FUNC_NAME,pos,scm); } \
  } while (0)


#define VALIDATE_ARG_PROC(pos,scm) \
  do { \
  if (!gh_procedure_p(scm)) scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  } while (0)

/* we use UNSET_SCM instead of just testing for == SCM_UNDEFINED
   so SCM_BOOL_F is okay -- this does do an extra assignment, though */
#define VALIDATE_ARG_PROC_USE_F(pos,scm) \
  do { \
  if (UNSET_SCM(scm)) scm = SCM_BOOL_F; \
  else if (!gh_procedure_p(scm)) scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  } while (0)

#define VALIDATE_ARG_PROC_OR_SYM_USE_F(pos,scm) \
  do { \
  if (UNSET_SCM(scm)) scm = SCM_BOOL_F; \
  else if (!PROCEDURE_OR_SYMBOL_P(scm)) scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  } while (0)


#endif
