#ifndef PICTURE_H
#define PICTURE_H

#include <guile/gh.h>
#include <X11/xpm.h>

#undef EXTERN
#undef EXTERN_SET
#ifdef PICTURE_IMPLEMENTATION
#define EXTERN
#define EXTERN_SET(x,y) x = y
#else
#define EXTERN extern
#define EXTERN_SET(x,y) extern x
#endif

/* FIXGJB: maybe paths.c should own this */
EXTERN char *szPicturePath;

/***********************************************************************
 * Stuff for dealing w/ pictures (built-in types are pixmaps & bitmaps)
 ***********************************************************************/
typedef struct Picture_tag
{
  struct Picture_tag *next;
  char *name;
  int cchName;			/* used for faster cache lookup */
  Pixmap picture;
  Pixmap mask;
  unsigned int depth;
  unsigned int width;
  unsigned int height;
  unsigned int count;
  Bool valid;
} Picture;


typedef struct {
  Picture *pic;
  int valid;
} scwm_picture;

void InitPictureCMap(Display*,Window);
Picture *CachePicture(Display*,Window,char *szPath,char *name);
void DestroyPicture(Display*,Picture*);


#define PICTURE_P(X) (SCM_CAR(X) == (SCM)scm_tc16_scwm_picture)
#define PICTURE(X)  ((scwm_picture *)SCM_CDR(X))

EXTERN long scm_tc16_scwm_picture;

/* Scheme object interface to Picture-s */

size_t free_picture(SCM obj);
int print_picture(SCM obj, SCM port, scm_print_state * pstate);
SCM picture_p(SCM obj);
SCM make_picture(SCM picture_filename);

#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
