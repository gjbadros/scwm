/* $Id$ */

#ifndef PATHS_H
#define PATHS_H

#include <guile/gh.h>

#undef EXTERN
#undef EXTERN_SET
#ifdef PATHS_H_IMPLEMENTATION
#define EXTERN
#define EXTERN_SET(x,y) x = y
#else
#define EXTERN extern
#define EXTERN_SET(x,y) extern x
#endif

EXTERN_SET(char *szImagePath,SCWM_ICONDIR);

SCM set_image_path_x(SCM newpath);

#endif	/* PATHS_H */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
