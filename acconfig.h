/* acconfig.h --- documentation for local preprocessor symbols defined
   by configure.
   Maciej Stachowiak <mstachow@mit.edu>
   */

/* Package and version macros defined by automake */
#undef PACKAGE 
#undef VERSION 

/* Define this if your Xext library supports the X extension (wether
   your server does or not will be determined at runtime */
#undef HAVE_SHAPE

/* Define this if you have libXpm */
#undef HAVE_LIBXPM

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

/* Define this if your libguile has scm_parse_path.  */
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

/* Define this if your system has a <getopt.h> header file. */
#undef HAVE_GETOPT_H

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

/* Define this and use C++ compiler if you want to use constraint solver */
#undef USE_CASSOWARY

/* Define tihs if you want to turn debug support off for cassowary */
#undef CL_NO_TRACE
