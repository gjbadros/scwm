/* acconfig.h --- documentation for local preprocessor symbols defined
   by configure.
   Maciej Stachowiak <mstachow@mit.edu>
   */

/* Define this if your libguile has a scm_eval_string that is safe against
   re-entry by continuations. This should be true of snapshots newer than
   970928.
 */
#undef HAVE_SAFE_SCM_EVAL_STRING

/* Define this if your libguile exports scm_puts, meaning that
   scm_gen_puts should no longer be used. This should be true of
   snapshots newer than 971014.  */
#undef HAVE_SCM_PUTS

/* Define this if your libguile has gh_vector_ref instead of gh_vref,
   meaning that gh_vref should no longer be used. This should be
   true of snapshots newer than 971012.  */
#undef HAVE_GH_VECTOR_REF

/* Define this if your libguile has readline support. This should be
   true of snapshots newer than 971023.  */
#undef HAVE_SCM_READLINE


