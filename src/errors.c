
#include <guile/gh.h>
#include "errors.h"

static char *scwm_errors[] = {
  "",
  "Unable to load 'fixed' font.",
  "Unable to parse color.",
  "Unable to allocate color.",
  "Bad binding specifier.",
  "Justification must be \'left, \'right or \'center.",
  "Window no longer valid.",
  "Bad height argument; must be from 5 to 256.",
  "No binding contexts specified.",
  "Invalid binding context.",
  "Colormap focus must be \'focus or \'mouse."
  "Bad menu entry specifier."
};


void scwm_error(char *subr,int err) {
    scm_error(gh_symbol2scm("scwm-error"),subr,"%s",
	      gh_list(gh_str02scm(scwm_errors[err]),SCM_UNDEFINED),
	      gh_list(gh_int2scm(err),SCM_UNDEFINED));
}


