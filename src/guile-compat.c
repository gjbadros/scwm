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



