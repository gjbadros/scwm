/* $Id$ */
/*
 * Copyright (C) 1997, 1998, 1999, 2000 Greg J. Badros and Maciej Stachowiak
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

#include <libguile.h>

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include <unistd.h>
#include <X11/Intrinsic.h>

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


#define IMAGE_P(X) (SCM_SMOB_PREDICATE(scm_tc16_scwm_scwmimage, X))
#define IMAGE(X)  ((scwm_image *)SCM_SMOB_DATA(X))
#define SAFE_IMAGE(X)  (IMAGE_P(X)? IMAGE(X) : NULL)
#define DYNAMIC_IMAGE_P(X) (scm_is_symbol(X)? \
                            IMAGE_P(scm_variable_ref(scm_lookup(X))) : \
                            IMAGE_P(X))
#define IMAGE_OR_SYMBOL_P(X) (IMAGE_P(X) || scm_is_symbol(X))
#define DYNAMIC_SAFE_IMAGE(X) (scm_is_symbol(X)? \
                               SAFE_IMAGE(scm_variable_ref(scm_lookup(X))):\
                               SAFE_IMAGE(X))

EXTERN long scm_tc16_scwm_scwmimage;

SCM make_image_from_pixmap(char *szDescription,
			   Pixmap image, Pixmap mask, 
			   int width, int height, int depth);
SCM make_image(SCM name);
char *SzNewImageShortName(scwm_image *psimg);

void init_image_colormap();

#define VALIDATE_ARG_IMAGE(pos,scm) \
  do { \
  if (!IMAGE_P(scm)) scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  } while (0)

#define VALIDATE_ARG_IMAGE_COPY(pos,scm,cvar) \
  do { \
  if (!IMAGE_P(scm)) scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  else cvar = IMAGE(scm); \
  } while (0)


/* we use UNSET_SCM instead of just testing for == SCM_UNDEFINED
   so SCM_BOOL_F is okay -- this does do an extra assignment, though */
#define VALIDATE_ARG_IMAGE_USE_F(pos,scm) \
  do { \
  if (UNSET_SCM(scm)) scm = SCM_BOOL_F; \
  else if (!IMAGE_P(scm)) scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  } while (0)

#define VALIDATE_ARG_IMAGE_OR_SYM_USE_F(pos,scm) \
  do { \
  if (UNSET_SCM(scm)) scm = SCM_BOOL_F; \
  else if (!IMAGE_OR_SYMBOL_P(scm)) scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  } while (0)

#define VALIDATE_ARG_IMAGE_OR_SYM(pos,scm) \
  do { \
  if (!IMAGE_OR_SYMBOL_P(scm)) scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  } while (0)

#define VALIDATE_ARG_IMAGE_OR_STRING(pos,scm) \
  do { \
  if (scm_is_string(scm)) scm = make_image(scm); \
  if (!IMAGE_P(scm)) scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  } while (0)

#define VALIDATE_ARG_IMAGE_OR_STRING_OR_F(pos,scm) \
  do { \
  if (scm_is_string(scm)) scm = make_image(scm); \
  if (!IMAGE_P(scm) && SCM_BOOL_F != scm) scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  } while (0)


#endif /* IMAGE_H */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

