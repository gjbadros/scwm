/* $Id$
 * Picture.c
 * By Greg J. Badros -- Nov 8, 1997
 * Loosely descended from code by Robert Nation, 1993, for fvwm
 * May be distributed under the terms below.
 */
/****************************************************************************
 * This module is all original code 
 * by Rob Nation 
 * Copyright 1993, Robert Nation
 *     You may use this code for any purpose, as long as the original
 *     copyright remains in the source code and all documentation
 ****************************************************************************/


#define PICTURE_IMPLEMENTATION

#include <config.h>

#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <X11/keysym.h>
#include <sys/types.h>
#include <sys/time.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include <guile/gh.h>
#include "scwm.h"
#include "screen.h"
#include "Picture.h"
#include "system.h"

static Picture *PictureList = NULL;
static Colormap PictureCMap;


void 
InitPictureCMap(Display * dpy, Window Root)
{
  XWindowAttributes root_attr;

  XGetWindowAttributes(dpy, Root, &root_attr);
  PictureCMap = root_attr.colormap;
}

/* LoadPicture:  get a picture name "path" and return the picture object
   Note: will make a copy of path to store in the
   cache's list of pictures */
Picture *
LoadPicture(Display * dpy, Window Root, char *path)
{
  int l;
  Picture *p;

  XpmAttributes xpm_attributes;


  p = safemalloc(sizeof(Picture));
  p->count = 1;
  p->name = strdup(path);
  p->cchName = strlen(path);
  p->next = NULL;

  /* FIXGJB: should check extension and call appropriate loader */
  /* don't try calling each loader waiting for a success! */

  /* Try to load it as an X Pixmap first */
  xpm_attributes.colormap = PictureCMap;
  /* FIXGJB: the closeness should be configurable */
  xpm_attributes.closeness = 40000;	/* Allow for "similar" colors */
  xpm_attributes.valuemask =
    XpmSize | XpmReturnPixels | XpmColormap | XpmCloseness;

  if (XpmReadFileToPixmap(dpy, Root, path, &p->picture, &p->mask, &xpm_attributes)
      == XpmSuccess) {
    p->width = xpm_attributes.width;
    p->height = xpm_attributes.height;
    p->depth = DefaultDepthOfScreen(DefaultScreenOfDisplay(dpy));
    return p;
  }

  /* If no XPM support, or XPM loading failed, try bitmap */
  if (XReadBitmapFile(dpy, Root, path, &p->width, &p->height, &p->picture, &l, &l)
      == BitmapSuccess) {
    p->depth = 0;
    p->mask = None;
    return p;
  }
  free(p->name);
  free(p);
  return NULL;
}

/* This is the fvwm2 (old code's) main interface to getting a pixmap or 
   bitmap */
Picture *
CachePicture(Display * dpy, Window Root, char *szPath, char *name)
{
  Picture *p = PictureList;
  char *path;
  int ich; 
  int cchPath;

  /* First find the full pathname */
  if (!(path = findFile(name, szPath, R_OK)))
    return NULL;

  cchPath = strlen(path);

  /* See if the picture is already cached */
  while (p) {
    /* first check length */
    if (cchPath != p->cchName) {
      goto NEXT;
    }
    /* start from end since that's more likely to differ */
    ich = cchPath;
    while (ich >= 0 && path[ich] == p->name[ich])
      ich--;
    if (ich < 0) {		/* We have found a picture with the wanted name */
      p->count++;		/* Increment its reference count */
      return p;
    }
NEXT:
    p = p->next;
  }

  /* Not previously cached, have to load it ourself. Put it first in list */
  p = LoadPicture(dpy, Root, path);
  if (p) {
    p->next = PictureList;
    PictureList = p;
  }
  free(path);
  return p;
}


void 
DestroyPicture(Display * dpy, Picture * p)
{
  Picture *q = PictureList;

  if (!p)			/* bag out if NULL */
    return;
  if (--(p->count) > 0)		/* Remove a weight, still too heavy? */
    return;

  /* Let it fly */
  if (p->name != NULL)
    free(p->name);
  if (p->picture != None)
    XFreePixmap(dpy, p->picture);
  if (p->mask != None)
    XFreePixmap(dpy, p->mask);

  /* Link it out of the list (it might not be there) */
  if (p == q)			/* in head? simple */
    PictureList = p->next;
  else {
    while (q && q->next != p)	/* fast forward until end or found */
      q = q->next;
    if (q)			/* not end? means we found it in there, possibly at end */
      q->next = p->next;	/* link around it */
  }
  free(p);
}


/* Scheme Object Interface to Picture-s */
/* FIXGJB: add a dimensions, depth accessor function */

size_t 
free_picture(SCM obj)
{
  scwm_picture *spic = PICTURE(obj);
  DestroyPicture(dpy,spic->pic);
  free(spic);
  return (0);
}

int 
print_picture(SCM obj, SCM port, scm_print_state * pstate)
{
  scm_puts("#<picture ", port);
  if (PICTURE_P(obj)) {
    scm_write(gh_str02scm(PICTURE(obj)->pic->name), port);
  } else {
    scm_puts("(invalid)", port);
  }
  scm_putc('>', port);

  return 1;
}

SCM 
picture_p(SCM obj)
{
  return ((SCM_NIMP(obj) && PICTURE_P(obj)) ? SCM_BOOL_T : SCM_BOOL_F);
}

inline SCM
pic2scm(Picture *pic)
{
  SCM answer;
  scwm_picture *scmpic;

  scmpic = safemalloc(sizeof(scwm_picture));
  scmpic->valid = (pic? True: False);
  scmpic->pic = pic;

  SCM_DEFER_INTS;
  SCM_NEWCELL(answer);
  SCM_SETCAR(answer, scm_tc16_scwm_picture);
  SCM_SETCDR(answer, (SCM) scmpic);
  SCM_ALLOW_INTS;

  return answer;
}

SCM 
make_picture(SCM picture_filename)
{
  char *szName = NULL;
  int len;
  Picture *pic;

  if (gh_string_p(picture_filename)) {
    szName = gh_scm2newstr(picture_filename, &len);
  } else {
    scm_wrong_type_arg(__FUNCTION__, 1, picture_filename);
    /* FIXNOWGJB: return SCM_UNDEFINED; */
  }

  pic = CachePicture(dpy,Scr.Root,szPicturePath,szName);
  free(szName);

  if (!pic) {
    scwm_msg(ERR,__FUNCTION__,"Could not load pixmap %s",szName);
    return SCM_UNDEFINED;
  }
  return pic2scm(pic);
}

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
