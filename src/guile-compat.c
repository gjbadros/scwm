/* $Id$ */
/*
 *      Copyright (C) 1997, Maciej Stachowiak and Greg J. Badros
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.GPL.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <guile/gh.h>
#include "guile-compat.h"

#ifdef __cplusplus
extern "C" {
#endif


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
        tail = gh_cons(scm_makfromstr (scan + 1, elt_end - (scan + 1), 0),
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
  SET_LAST_STACK(scm_make_stack (gh_cons(SCM_BOOL_T, SCM_EOL)));
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

#ifdef __cplusplus
}
#endif

