/* acconfig.h --- documentation for local preprocessor symbols defined
   by configure.
   Maciej Stachowiak <mstachow@mit.edu>
   */

/* Define this if your libguile has a scm_eval_string that is safe against
   re-entry by continuations. This should be true of snapshots newer than
   970928.
 */
#undef HAVE_SAFE_SCM_EVAL_STRING
