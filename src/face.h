#ifndef FACE_H
#define FACE_H

#include <libguile.h>
#include "scwm.h"

typedef struct {
  ButtonFace *bf;
} scwm_face;

extern long scm_tc16_scwm_face;

#define FACEP(X) ((X) && (SCM_CAR(X) == (SCM)scm_tc16_scwm_face))
#define FACE(X)  ((scwm_face *)SCM_CDR(X))
#define BUTTONFACE(X) (((scwm_face *)SCM_CDR(X))->bf)



size_t free_face(SCM obj);
int print_face(SCM obj, SCM port, scm_print_state * pstate);

void init_face();

extern SCM default_titlebar_face;
extern SCM default_border_face;
extern SCM default_lbutton_face[5];
extern SCM default_rbutton_face[5];

SCM make_face(SCM flags, SCM specs);

#endif /* FACE_H */
