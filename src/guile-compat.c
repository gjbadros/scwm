#include <guile/gh.h>
#include <config.h>
#include "guile-compat.h"

#ifndef HAVE_SCM_PARSE_PATH
SCM
scm_parse_path (char *path, SCM tail)
{
  if (path && path[0] != '\0')
    {
      char *scan, *elt_end;

      /* Scan backwards from the end of the string, to help
         construct the list in the right order.  */
      scan = elt_end = path + strlen (path);
      do {
        /* Scan back to the beginning of the current element.  */
        do scan--;
        while (scan >= path && *scan != ':');
        tail = scm_cons (scm_makfromstr (scan + 1, elt_end - (scan + 1), 0),
                         tail);
        elt_end = scan;
      } while (scan >= path);
    }

  return tail;
}
#endif /* HAVE_SCM_PARSE_PATH */

#ifndef HAVE_SCM_INTERNAL_STACK_CATCH
/* scm_internal_stack_catch
   Use this one if you want debugging information to be stored in
   scm_the_last_stack_{fluid,var} on error. */

static SCM
ss_handler (void *data, SCM tag, SCM throw_args)
{
  /* Save the stack */
  SET_LAST_STACK(scm_make_stack (scm_cons (SCM_BOOL_T, SCM_EOL)));
  /* Throw the error */
  return scm_throw (tag, throw_args);
}

struct cwss_data
{
  SCM tag;
  scm_catch_body_t body;
  void *data;
};

static SCM
cwss_body (void *data)
{
  struct cwss_data *d = data;
  return scm_internal_lazy_catch (d->tag, d->body, d->data, ss_handler, NULL);
}

SCM
scm_internal_stack_catch (SCM tag,
			  scm_catch_body_t body,
			  void *body_data,
			  scm_catch_handler_t handler,
			  void *handler_data)
{
  struct cwss_data d;
  d.tag = tag;
  d.body = body;
  d.data = body_data;
  return scm_internal_catch (tag, cwss_body, &d, handler, handler_data);
}

#endif /* HAVE_SCM_INTERNAL_STACK_CATCH */


