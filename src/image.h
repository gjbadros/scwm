/* $Id$ */
/*
 * Copyright (C) 1997-1999 Maciej Stachowiak and Greg J. Badros
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.GPL.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 *
 */

#ifndef IMAGE_H
#define IMAGE_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <unistd.h>
#include <X11/Intrinsic.h>

#include <guile/gh.h>

#undef EXTERN
#undef EXTERN_SET
#ifdef IMAGE_IMPLEMENTATION
#define EXTERN
#define EXTERN_SET(x,y) x = y
#else
#define EXTERN extern
#define EXTERN_SET(x,y) extern x
#endif

#ifdef USE_IMLIB
#include <Imlib.h>
#endif

/* GJB:FIXME:MS:: It'd be really nice to get full pathname of
   the picture into the image object for debugging of scwmrc-s;
   then this could go back in, too, though I imagine it's
   rarely used --gjb 11/28/97  */
typedef struct
{
  SCM full_name;
  Pixmap image;
  Pixmap mask;
#ifdef USE_IMLIB
  ImlibImage *im;
#endif
  int foreign;
  unsigned int depth;
  unsigned int width;
  unsigned int height;
} scwm_image;


#define IMAGE_P(X) (SCM_NIMP(X) && gh_car(X) == (SCM)scm_tc16_scwm_image)
#define IMAGE(X)  ((scwm_image *)gh_cdr(X))
#define SAFE_IMAGE(X)  (IMAGE_P((X))? IMAGE((X)) : NULL)
#define DYNAMIC_IMAGE_P(X) (gh_symbol_p(X)? \
                            IMAGE_P(scm_symbol_binding(SCM_BOOL_F,(X))) : \
                            IMAGE_P(X))
#define IMAGE_OR_SYMBOL_P(X) (IMAGE_P(X) || gh_symbol_p(X))
#define DYNAMIC_SAFE_IMAGE(X) (gh_symbol_p(X)? \
                               SAFE_IMAGE(scm_symbol_binding(SCM_BOOL_F,(X))):\
                               SAFE_IMAGE(X))

EXTERN long scm_tc16_scwm_image;

SCM make_image_from_pixmap(char *szDescription,
			   Pixmap image, Pixmap mask, 
			   int width, int height, int depth);
SCM make_image(SCM name);

void init_image_colormap();

#endif /* IMAGE_H */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
