/* $Id$
 * face.h
 * Copyright (C) 1997, 1998 Maciej Stachowiak and Greg J. Badros
 */

#ifndef FACE_H
#define FACE_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <libguile.h>

#include "scwm.h"
#include "screen.h"

typedef struct {
  ButtonFace *bf;
} scwm_face;

extern long scm_tc16_scwm_face;

#define FACEP(X) (SCM_NIMP(X) && (gh_car(X) == (SCM)scm_tc16_scwm_face))
#define FACE(X)  ((scwm_face *)gh_cdr(X))
#define BUTTONFACE(X) (((scwm_face *)gh_cdr(X))->bf)

extern SCM default_titlebar_face;
extern SCM default_border_face;
extern SCM default_lbutton_face[5];
extern SCM default_rbutton_face[5];

#endif /* FACE_H */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
