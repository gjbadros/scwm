/* $Id$ */
/****************************************************************************
 * This module is all original code 
 * by Maciej Stachowiak and Greg J. Badros.
 * It may be used or distributed under either the FVWM license 
 * (see COPYING.fvwm) or the GNU General Public License (see COPYING.GPL and
 * the description below)
 * Copyright 1997, Maciej Stachowiak and Greg J. Badros
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

#include <config.h>

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/time.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include <guile/gh.h>
#include "scwm.h"
#include "screen.h"
#include "system.h"

#define IMAGE_IMPLEMENTATION

#include "image.h"

static Colormap ImageColorMap;

/* FIXMS: arbitrary choices for these sizes - I need to check out if
 * there is a good way to have hash tables grow automatically. 
 */

#define IMAGE_HASH_SIZE 41
#define IMAGE_LOADER_HASH_SIZE 11

/*
 * A weak value hash table from image name strings to image objects.
 * The hash table will maintain associations as long as long as the
 * value (the image object) is otherwise gc-protected, but will remove
 * the association and let the object be freed if no other references
 * are held anywhere.
 */

/*
 * FIXMS: This architecture prevents new images from shadowing old
 * ones during a single invocation if the old image is currently in
 * core, but this can be fixed by adding some of the info from stat()
 * on the image file to the image struct.  
 */

static SCM image_hash_table = SCM_UNDEFINED;

/*
 * The hash table of image loader procedures. This one is not
 * weak; we don't want image loader procedures to be free-able until
 * and unless they get unregistered.
 */

static SCM image_loader_hash_table = SCM_UNDEFINED;

/* The location of the value of the image-load-path variable. */

static SCM *loc_image_load_path;

/* used by C code to create image objects for images not loaded from
   files, e.g. app-provided icon bitmaps or window. */

size_t 
free_image(SCM obj)
{
  scwm_image *si = IMAGE(obj);

  if (si->image != None) {
    XFreePixmap(dpy, si->image);
  }
  if (si->mask != None) {
    XFreePixmap(dpy, si->mask);
  }

  free(si);
  return (0);
}

int 
print_image(SCM obj, SCM port, scm_print_state * pstate)
{
  scm_puts("#<image ", port);
  scm_write(IMAGE(obj)->name, port);
  scm_putc('>', port);
  return 1;
}

SCM
mark_image(SCM obj)
{
  if (SCM_GC8MARKP (obj)) {
    return SCM_BOOL_F;
  }
  SCM_SETGC8MARK(obj);
  return IMAGE(obj)->name;  
}


SCM_PROC (s_image_p, "image?-new", 1, 0, 0, image_p);

SCM 
image_p(SCM obj)
{
  return ((SCM_NIMP(obj) && IMAGE_P(obj)) ? SCM_BOOL_T : SCM_BOOL_F);
}


SCM
make_empty_image(SCM name)
{
  SCM result;
  scwm_image *ci;

  ci = safemalloc(sizeof(scwm_image));
  ci->name = name;
  ci->image = None;
  ci->mask = None;

  SCM_DEFER_INTS;
  SCM_NEWCELL(result);
  SCM_SETCAR(result, scm_tc16_scwm_image);
  SCM_SETCDR(result, (SCM) ci);
  SCM_ALLOW_INTS;

  return result;
}

/* These get exported as Scheme procedures on their own, but are also
   registered as image loaders for the specified types of images. It's
   done this way to provide for extending the available image loaders
   with ones written in either C or Scheme. For instance, I can
   envision a Scheme loader function for the .gz suffix which
   decompresses the file into /tmp, calls make-image on that, and then
   deletes the image in /tmp so you can keep all of your images gzipped. 
   */

SCM_PROC (s_load_xbm, "load-xbm", 1, 0, 0, load_xbm);

SCM
load_xbm (SCM full_path)
{
  SCM result;
  scwm_image *ci;
  int ignore;
  char *c_path;
  int xbm_return;

  result=make_empty_image(full_path);
  ci=IMAGE(result);

  c_path=gh_scm2newstr(full_path,&ignore);

  xbm_return=XReadBitmapFile(dpy, Scr.Root, c_path, 
		      &ci->width, &ci->height, &ci->image, 
		      &ignore, &ignore);
  free(c_path);
  if (xbm_return == BitmapSuccess) {
    ci->depth = 0;
    ci->mask = None;
    return result;
  } else {
    /* warn that the image could not be loaded, then drop the result
       on the floor and let GC clean it up. */
    return SCM_BOOL_F;
  }
}


SCM_PROC (s_load_xpm, "load-xpm", 1, 0, 0, load_xpm);

SCM
load_xpm (SCM full_path)
{
  SCM result;
  scwm_image *ci;
  int ignore;
  char *c_path;
  XpmAttributes xpm_attributes;
  int xpm_return;

  result=make_empty_image(full_path);
  ci=IMAGE(result);

  c_path=gh_scm2newstr(full_path,&ignore);

  xpm_attributes.colormap = ImageColorMap;

  /* FIXMS: the Xpm manual suggests that a closeness of 40000 is
     usually best. 655535 will allow any color to match, thus
     preventing failure solely due to color allocation problems. 0
     presumably requires an exact match. This should be configurable
     to some degree, but shouldn't be too high a priority. */

  xpm_attributes.closeness = 40000;	
  xpm_attributes.valuemask =
    XpmSize | XpmReturnPixels | XpmColormap | XpmCloseness;

  xpm_return = XpmReadFileToPixmap(dpy, Scr.Root, 
				   c_path, &ci->image, &ci->mask, 
				   &xpm_attributes);
  free(c_path);
  if (xpm_return  == XpmSuccess) {
    ci->width = xpm_attributes.width;
    ci->height = xpm_attributes.height;
    ci->depth = xpm_attributes.depth; /* the Xpm manual implies this should
					work, if not, fall back to the
					code below. */
      /* DefaultDepthOfScreen(DefaultScreenOfDisplay(dpy)); */
    return result;
  } else {
    /* warn that the image could not be loaded, then drop the result
       on the floor and let GC clean it up. */
    return SCM_BOOL_F;
  }
}

SCM_PROC (s_register_image_loader, "register-image-loader", 2, 0, 0, register_image_loader);

SCM
register_image_loader(SCM extension, SCM proc)
{
  if (!gh_string_p(extension)) {
    scm_wrong_type_arg(s_register_image_loader, 1, extension);
  }
  
  /* Sadly, there is no way to test the arity of a procedure. */
  if (!gh_procedure_p(proc)) {
    scm_wrong_type_arg(s_register_image_loader, 1, proc);
  }

  scm_hash_set_x(image_loader_hash_table, extension, proc);

  return SCM_UNSPECIFIED;
}

SCM_PROC (s_unregister_image_loader, "unregister-image-loader", 1, 0, 0, unregister_image_loader);

SCM
unregister_image_loader(SCM extension)
{
  if (!gh_string_p(extension)) {
    scm_wrong_type_arg(s_unregister_image_loader, 1, extension);
  }

  scm_hash_remove_x(image_loader_hash_table, extension);

  return SCM_UNSPECIFIED;
}

SCM
load_image(SCM name)
{
  SCM loader;
  SCM result;
  SCM full_name, extension;
  char *c_name, *c_fname, *c_ext;
  int length;
  int max_path_len = 0;

  if (!gh_string_p(name)) {
    scm_wrong_type_arg("load-image", 1, name);
  }

  c_name = gh_scm2newstr(name, &length);
  
  if (length > 0 && c_name[0]=='/') {
    /* absolute path */
    if(access(c_name, R_OK)==0) {
      c_fname=c_name;
      c_name=NULL;
    } else {
      free(c_name);
      return(SCM_BOOL_F);
    }
  } else {
    SCM p;

    /* relative path */ 
    if (!gh_list_p(*loc_image_load_path)) {
      /* Warning, image-load-path is not a list. */
      return SCM_BOOL_F;
    }
    
    /* traverse the path list to compute the max buffer size we will
       need. */
    for(p = *loc_image_load_path; p != SCM_EOL; p = SCM_CDR(p)) {
      SCM elt = SCM_CAR(p);
      if (!gh_string_p(elt)) {
	/* Warning, non-string in image-load-path */
	return SCM_BOOL_F;
      } else {
	int l=SCM_ROLENGTH(elt);
	max_path_len= (l > max_path_len) ? l : max_path_len;
      }
    }
	  
    c_fname=(char *) safemalloc(sizeof(char) *(max_path_len + 1 /* '/' */
					       + length + 1 /* 0 */ ));
    
    /* Try every possible path */
    for(p = *loc_image_load_path; p != SCM_EOL; p = SCM_CDR(p)) {
      SCM elt = SCM_CAR(p);
      int path_len = SCM_ROLENGTH(elt);
      memcpy(c_fname, SCM_ROCHARS(elt), path_len);
      c_fname[path_len] = '/';
      memcpy(&(c_fname[path_len+1]), c_name, length);
      c_fname[path_len+length+1] = 0;
      if(access(c_fname, R_OK)==0) {
	break;
      }
    }
    
    if (p==SCM_EOL) {
      /* warn that the file is not found. */
      free(c_name);
      free(c_fname);
      return SCM_BOOL_F;
    }

    free(c_name);
  }

  c_ext=strrchr(c_fname,'.');

  /* Construct a scheme string for the full name. */
  full_name=gh_str02scm(c_fname);
  /* Similarly for the extension. */
  extension=gh_str02scm(c_ext != NULL ? c_ext : "");
  free(c_fname);

  /* Find the right loader based on the extension. */
  loader=scm_hash_ref(image_loader_hash_table, extension, SCM_BOOL_F);

  if (loader==SCM_BOOL_F) {
    /* Warn: unknown imge type. */
    return SCM_BOOL_F;
  }
  
  result=gh_call1(loader,full_name);

  if (SCM_NIMP(result) && IMAGE_P(result)) {
    IMAGE(result)->name=name;
    return result;
  }

  if (result != SCM_BOOL_F) {
    /* Warn: invalid return value from image loader. */
  }

  return SCM_BOOL_F;
}

SCM_PROC (s_make_image, "make-image-new", 1, 0, 0, make_image);

/* FIXMS: maybe add an optional arg to explicitly specify the type,
   rather than guessing from the extension? */

SCM
make_image(SCM name)
{
  SCM result;

  if (!gh_string_p(name)) {
    scm_wrong_type_arg(s_make_image,1,name);
  }

  /* First check the hash table for this image.  */
  result=scm_hash_ref(image_hash_table,name, SCM_BOOL_F);
  
  if (result != SCM_BOOL_F) {
    /* FIXMS: should really check for up-to-date-ness here */
    return(result);
  }

  /* OK, it wasn't in the hash table so we need to load it.
   */

  result=load_image(name);
  
  if (result == SCM_BOOL_F) {
    /* the load failed; warn and return #f */
    return SCM_BOOL_F;
  }

  scm_hash_set_x(image_hash_table, name, result);

  return result;
}


static scm_smobfuns image_smobfuns =
{
  &mark_image,
  &free_image,
  &print_image,
  0
};


void 
init_image_colormap() 
{
  XWindowAttributes root_attributes;
  XGetWindowAttributes(dpy, Scr.Root, &root_attributes);
  ImageColorMap = root_attributes.colormap;
}

void init_image() 
{
  SCM val_load_xbm, val_load_xpm;


  /* Include registration of procedures and other things. */
# include "image.x"

  /* Register the image type. */
  scm_tc16_scwm_image = scm_newsmob(&image_smobfuns);

  /* Initialize the image cache table. */
  image_hash_table = 
    scm_make_weak_value_hash_table (SCM_MAKINUM(IMAGE_HASH_SIZE));
  scm_protect_object(image_hash_table);

  /* Initialize the loader hash table. */
  image_loader_hash_table = 
    scm_make_vector (SCM_MAKINUM(IMAGE_LOADER_HASH_SIZE), SCM_EOL, 
		     SCM_BOOL_F);
  scm_protect_object(image_loader_hash_table);

  /* Register the standard loaders. */

  val_load_xbm = gh_lookup("load-xbm");
  val_load_xpm = gh_lookup("load-xpm");

  register_image_loader (gh_str02scm(""), val_load_xbm);
  register_image_loader (gh_str02scm(".icon"), val_load_xbm);
  register_image_loader (gh_str02scm(".bitmap"), val_load_xbm);
  register_image_loader (gh_str02scm(".xbm"), val_load_xbm);
  register_image_loader (gh_str02scm(".xpm"), val_load_xpm);

  /* Make the image-load-path Scheme variable easily accessible from C,
     and load it with a nice default value. */
  loc_image_load_path = SCM_CDRLOC
    (scm_sysintern("image-load-path", 
		   gh_eval_str("\'"SCWM_IMAGE_LOAD_PATH)));
}
