/* $Id$ */
/****************************************************************************
 * This module is all original code 
 * by Maciej Stachowiak and Greg J. Badros.
 * It may be used or distributed under either the FVWM license 
 * (see COPYING.fvwm) or the GNU General Public License (see COPYING.GPL and
 * the description below)
 * Copyright 1997, Maciej Stachowiak and Greg J. Badros.
 ****************************************************************************/
/*      Copyright (C) 1997, Maciej Stachowiak

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
 * As a special exception, this file may alternatively be distributed under 
 * the fvwm license (see COPYING.FVWM).
 *
 */

#ifndef IMAGE_H
#define IMAGE_H

#undef EXTERN
#undef EXTERN_SET
#ifdef IMAGE_IMPLEMENTATION
#define EXTERN
#define EXTERN_SET(x,y) x = y
#else
#define EXTERN extern
#define EXTERN_SET(x,y) extern x
#endif

typedef struct
{
  SCM name;
  Pixmap image;
  Pixmap mask;
  unsigned int depth;
  unsigned int width;
  unsigned int height;
} scwm_image;


#define IMAGE_P(X) (SCM_CAR(X) == (SCM)scm_tc16_scwm_image)
#define IMAGE(X)  ((scwm_image *)SCM_CDR(X))

EXTERN long scm_tc16_scwm_image;

/* Scheme object interface to Picture-s */

size_t free_image(SCM obj);
int print_image(SCM obj, SCM port, scm_print_state * pstate);
SCM mark_image(SCM obj);

SCM image_p(SCM obj);
SCM make_image(SCM picture_filename);

SCM image_p(SCM obj);

SCM make_empty_image(SCM name);

SCM load_bitmap(SCM full_path);
SCM load_pixmap(SCM full_path);
SCM register_image_loader(SCM extension, SCM proc);
SCM unregister_image_loader(SCM extension);
SCM load_image(SCM name);
SCM make_image(SCM name);

void init_image_colormap();
void init_image();

#endif IMAGE_H
