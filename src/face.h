/* $Id$
 * face.h
 * Copyright (C) 1997, 1998, 1999, 2000 Greg J. Badros and Maciej Stachowiak
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

#define VALIDATE_ARG_FACE(pos,scm) \
  do { \
  if (!FACEP(scm)) scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  } while (0)

#define VALIDATE_ARG_FACE_COPY(pos,scm,cvar) \
  do { \
  if (!FACEP(scm)) scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  else cvar = FACE(scm); \
  } while (0)

#define VALIDATE_ARG_FACE_USE_DEF(pos,scm,val) \
  do { \
  if (UNSET_SCM(scm)) scm = val; \
  else if (!FACEP(scm)) scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  } while (0)


#define VALIDATE_ARG_BUTTONFACE_COPY(pos,scm,cvar) \
  do { \
  if (!FACEP(scm)) scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  else cvar = BUTTONFACE(scm); \
  } while (0)


#endif /* FACE_H */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

