/* acconfig.h --- documentation for local preprocessor symbols defined
   by configure.
   Greg J. Badros <gjb@cs.washington.edu>
   Maciej Stachowiak <mstachow@alum.mit.edu>
   */

/* Package and version macros defined by automake */
#undef PACKAGE 
#undef VERSION 

/* Define this if your Xext library supports the X extension (wether
   your server does or not will be determined at runtime */
#undef HAVE_SHAPE

/* Define this if you have libXpm */
#undef HAVE_LIBXPM

/* Define this if you have libXmu */
#undef HAVE_LIBXMU

/* Define this if you have libXtst, the XTest X server extension */
#undef HAVE_XTEST

/* Define this if you have libSM and libIce for session manager support */
#undef HAVE_LIBSM_LIBICE

/* Define this if you have the readline library */
#undef HAVE_READLINE

/* Define this if your readline also has add_history() */
#undef HAVE_HISTORY

/* Define this if your libguile has a scm_eval_string that is safe against
   re-entry by continuations. This should be true of snapshots newer than
   970928.  */
#undef HAVE_SAFE_SCM_EVAL_STRING

/* Define this if your libguile exports scm_puts, meaning that
   scm_gen_puts should no longer be used. This should be true of
   snapshots newer than 971014.  */
#undef HAVE_SCM_PUTS

/* Define this if your libguile has gh_vector_ref instead of gh_vref,
   meaning that gh_vref should no longer be used. This should be
   true of snapshots newer than 971012.  */
#undef HAVE_GH_VECTOR_REF

/* Define this if your libguile has gh_vector_set_x instead of gh_vset,
   meaning that gh_vset should no longer be used. This should be
   true of snapshots newer than 971020.  */
#undef HAVE_GH_VECTOR_SET_X

/* Define this if your libguile has readline support. This should be
   true of snapshots newer than 971023.  */
#undef HAVE_SCM_READLINE

/* Define this if your libguile has gh_length and not
   gh_list_length. This should be true of snapshots newer than 970915.  */
#undef HAVE_GH_LENGTH

/* Define this if your libguile has scm_parse_path.  */
#undef HAVE_SCM_PARSE_PATH

/* Define this if your libguile has scm_internal_select.  */
#undef HAVE_SCM_INTERNAL_SELECT

/* Define this if your libguile has scm_the_last_stack_fluid instead
   of scm_the_last_stack_var.  */
#undef HAVE_SCM_THE_LAST_STACK_FLUID

/* Define this if your libguile has scm_internal_cwdr.  */
#undef HAVE_SCM_INTERNAL_CWDR

/* Define this if your libguile has scm_internal_stack_catch.  */
#undef HAVE_SCM_INTERNAL_STACK_CATCH

/* Define this if your libguile has scm_internal_parse_path,
   which should be used instead of scm_parse_path from C.  */
#undef HAVE_SCM_INTERNAL_PARSE_PATH

/* Define this if your libguile has a scm_make_vector, which needs
   three arguments. This should be true only of older versions. */
#undef HAVE_SCM_MAKE_VECTOR_3_ARGS

/* Define this if your libguile has scm_load_startup_files,
   which means the hack to get boot-9.scm to be loaded is unnecessary
   and even dangerous. */
#undef HAVE_SCM_LOAD_STARTUP_FILES

/* Define this if your libguile has scm_make_hook, indicating
   C-level support for hooks. */
#undef HAVE_SCM_MAKE_HOOK

/* Define this if your libguile has scm_create_hook, which replaces
   the deprecated scm_make_named_hook. */
#undef HAVE_SCM_CREATE_HOOK

/* Define this if your libguile has the scm_hook_empty_p primitive */
#undef HAVE_SCM_HOOK_EMPTY_P

/* Define this if your libguile has scm_make_smob_type_mfpe,
   the new-style SMOB interface (>= guile-1.3.2 */
#undef HAVE_SCM_MAKE_SMOB_TYPE_MFPE


/* Define this if your libguile has scm_strport_to_string
   (added sometime after guile-1.3) */
#undef HAVE_SCM_STRPORT_TO_STRING

/* Define this if your system has a <getopt.h> header file. */
#undef HAVE_GETOPT_H

/* Define this if your system has a <sys/select.h> header file. */
#undef HAVE_SYS_SELECT_H

/* Define this if your system has the getopt_long function. */
#undef HAVE_GETOPT_LONG

/* Define this if your system has the usleep function. */
#undef HAVE_USLEEP

/* Define this if your system has the setlinebuf function. */
#undef HAVE_SETLINEBUF

/* Define this if your system has the setvbuf function. */
#undef HAVE_SETVBUF

/* Define this if you want multibyte support. */
#undef I18N

/* Define this if you want to compile with X_LOCALE define. */
#undef X_LOCALE

/* Define this and use C++ compiler if you want to use constraint solver */
#undef USE_CASSOWARY

/* Define this if you want to use libproplist */
#undef USE_PROPLIST

/* Define tihs if you want to turn debug support off for cassowary */
#undef CL_NO_TRACE

/* Define this if you want to use imlib pixmap/bitmap support */
#undef USE_IMLIB

/* Define this if you have IBM ViaVoice installed with its smapi.h header */
#undef HAVE_VIAVOICE

/* Define this if you want the --dump option */
#undef ENABLE_DUMP
