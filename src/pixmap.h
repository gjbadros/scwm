/* $Id$
 * pixmap.h of SCWM
 */

#ifndef PIXMAP_H
#define PIXMAP_H

#include <X11/xpm.h>

#undef EXTERN
#undef EXTERN_SET
#ifdef PIXMAP_IMPLEMENTATION
#define EXTERN
#define EXTERN_SET(x,y) x = y
#else
#define EXTERN extern
#define EXTERN_SET(x,y) extern x
#endif

#define PIXMAPP(X) (SCM_CAR(X) == (SCM)scm_tc16_scwm_pixmap)
#define PIXMAP(X)  ((scwm_pixmap *)SCM_CDR(X))

EXTERN long scm_tc16_scwm_pixmap;


SCM mark_pixmap(SCM obj);
size_t free_pixmap(SCM obj);
int print_pixmap(SCM obj, SCM port, scm_print_state * pstate);
SCM pixmap_p(SCM obj);
SCM make_pixmap(SCM pixmap_filename);


#endif

