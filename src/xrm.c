/* $Id$
 * xrm.c
 * X Resource manager primitives for scwm
 * (C) 1998 Greg J. Badros and Maciej Stachowiak
 */


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <X11/Xlib.h>
#include <unistd.h>
#include <guile/gh.h>
#include <signal.h>
#include "scwm.h"

extern XrmDatabase db;

SCWM_PROC (X_resource_put, "X-resource-put", 2, 0, 0,
           (SCM resource, SCM value))
     /** Stores string VALUE as X resource RESOURCE (also a string).
Later, the value can be retrieved using `X-resource-get'. */
#define FUNC_NAME s_X_resource_put
{
  int iarg = 1;

  if (!gh_string_p(resource))
    scm_wrong_type_arg(FUNC_NAME, iarg++, resource);
  if (!gh_string_p(value))
    scm_wrong_type_arg(FUNC_NAME, iarg++, value);

  { /* scope */
    char *szSpecifier = gh_scm2newstr(resource,NULL);
    char *szValue = gh_scm2newstr(value,NULL);
    XrmPutStringResource(&db,szSpecifier,szValue);
    FREE(szSpecifier);
    FREE(szValue);
  }
  return SCM_UNDEFINED;
}
#undef FUNC_NAME


SCWM_PROC (X_resource_get, "X-resource-get", 1, 1, 0,
           (SCM name, SCM xclass))
     /** Get X resource specified by NAME and XCLASS from Xrm database.
Both NAME and XCLASS are strings, as is the returned value.  If
XCLASS is omitted, it defaults to the same string as NAME.
If there is no resource under the given key, #f is returned. */
#define FUNC_NAME s_X_resource_get
{
  int iarg = 1;
  SCM answer = SCM_BOOL_F;

  if (!gh_string_p(name))
    scm_wrong_type_arg(FUNC_NAME, iarg++, name);
  if (!UNSET_SCM(xclass) && !gh_string_p(xclass))
    scm_wrong_type_arg(FUNC_NAME, iarg++, xclass);

  { /* scope */
    char *szName = gh_scm2newstr(name,NULL);
    char *szClass = !UNSET_SCM(xclass)?gh_scm2newstr(xclass,NULL):strdup(szName);
    char *szType;
    XrmValue ret;
    if (XrmGetResource(db,szName,szClass,&szType,&ret)) {
      answer = gh_str02scm(ret.addr);
    }
    FREE(szName);
    FREE(szClass);
  }
  return answer;
}
#undef FUNC_NAME


void
init_xrm()
{
#ifndef SCM_MAGIC_SNARFER
#include "xrm.x"
#endif
}

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
