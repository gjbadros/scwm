/* $Id$ */
/*
 * Copyright (C) 1997, 1998, Maciej Stachowiak and Greg J. Badros
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

#include <config.h>
#include <X11/Intrinsic.h>
#include <X11/xpm.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/stat.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include <guile/gh.h>
#include "scwm.h"
#include "screen.h"
#include "system.h"
#include "callbacks.h"
#include "scwmpaths.h"
#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif


#define IMAGE_IMPLEMENTATION

#include "image.h"
#include "guile-compat.h"


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

static SCM *loc_image_not_found_hook;

static SCM str_default;
static SCM str_empty;

SCM_SYMBOL (sym_filename,"filename");
SCM_SYMBOL (sym_width,"width");
SCM_SYMBOL (sym_height,"height");
SCM_SYMBOL (sym_depth,"depth");

size_t 
free_image(SCM obj)
{
  scwm_image *si = IMAGE(obj);

  if (!si->foreign) {
    if (si->image != None) {
      XFreePixmap(dpy, si->image);
    }
    if (si->mask != None) {
      XFreePixmap(dpy, si->mask);
    }
  }

  free(si);
  return (0);
}

int 
print_image(SCM obj, SCM port, scm_print_state * pstate)
{
  scm_puts("#<image ", port);
  scm_write(IMAGE(obj)->full_name, port);
  scm_putc('>', port);
  return 1;
}

SCM
mark_image(SCM obj)
{
  SCM_SETGC8MARK(obj);
  scm_gc_mark (IMAGE(obj)->full_name);
  return SCM_BOOL_F;
}

SCWM_PROC (image_p, "image?", 1, 0, 0,
           (SCM obj))
{
  return SCM_BOOL_FromBool(IMAGE_P(obj));
}



SCWM_PROC (image_properties, "image-properties", 1, 0, 0,
           (SCM image))
{
  scwm_image *psimg = SAFE_IMAGE(image);
  if (!psimg) {
    scm_wrong_type_arg(s_image_properties, 1, image);
  } 
  return gh_list(gh_cons(sym_filename,psimg->full_name),
		 gh_cons(sym_width,gh_int2scm(psimg->width)),
		 gh_cons(sym_height,gh_int2scm(psimg->height)),
		 gh_cons(sym_depth,gh_int2scm(psimg->depth)),
		 SCM_UNDEFINED);
  /* FIXGJB: why SCM_UNDEFINED -- GH_EOL? SCM_EOL does not work! */
  /* MS: because SCM_EOL is the empty list, which is a valid list item
     - you can have '() as a member of a list. However, SCM_UNDEFINED
     should never be part of a valid Scheme object. */
}


SCM
make_empty_image(SCM name)
{
  SCM result;
  scwm_image *ci;

  ci = (scwm_image *) safemalloc(sizeof(scwm_image));
  ci->full_name = name;
  ci->image = None;
  ci->mask = None;
  ci->height = 0;
  ci->width = 0;
  ci->depth = 0;
  ci->foreign = 0;

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
   with ones written in either C or Scheme. Scheme-based loaders could
   call external conversion programs for instance. */

SCWM_PROC (load_xbm, "load-xbm", 1, 0, 0,
           (SCM full_path))
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
  if (xbm_return == BitmapSuccess) {
    ci->depth = 0;
    ci->mask = None;
  } else {
    /* warn that the image could not be loaded, then drop the result
       on the floor and let GC clean it up. */
    scwm_msg(WARN,s_load_xbm,"Could not load bitmap `%s'",c_path);
    result = SCM_BOOL_F;
  }
  free(c_path);
  return result;
}


SCWM_PROC (load_xpm, "load-xpm", 1, 0, 0,
           (SCM full_path))
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
  if (xpm_return  == XpmSuccess) {
    ci->width = xpm_attributes.width;
    ci->height = xpm_attributes.height;
    ci->depth = DefaultDepthOfScreen(DefaultScreenOfDisplay(dpy));
  } else {
    /* warn that the image could not be loaded, then drop the result
       on the floor and let GC clean it up. */
    scwm_msg(WARN,s_load_xpm,"Could not load pixmap `%s'",c_path);
    result = SCM_BOOL_F;
  }
  free(c_path);
  return result;
}


SCWM_PROC (register_image_loader, "register-image-loader", 2, 0, 0,
           (SCM extension, SCM proc))
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

SCWM_PROC (unregister_image_loader, "unregister-image-loader", 1, 0, 0,
           (SCM extension))
{
  if (!gh_string_p(extension)) {
    scm_wrong_type_arg(s_unregister_image_loader, 1, extension);
  }

  scm_hash_remove_x(image_loader_hash_table, extension);

  return SCM_UNSPECIFIED;
}

SCM
InvokeHook1(SCM proc, SCM arg1)
{
  if (proc != SCM_BOOL_F && gh_procedure_p(proc)) {
    return scwm_safe_call1(proc, arg1);
  }
  return SCM_BOOL_F;
}


SCM
path_expand_image_fname(SCM name)
{
  char *c_name, *c_fname;
  int length;
  SCM result;

  if (!gh_string_p(name)) {
    scm_wrong_type_arg(__FUNCTION__, 1, name);
  }

  c_name = gh_scm2newstr(name, &length);

  if ((length > 0 && c_name[0]=='/') ||
      (length > 1 && c_name[0]=='.' &&
       (c_name[1]=='/' || (length > 2 && c_name[1]=='.' 
			   && c_name[2]=='/')))) {
    /* absolute path */

    if(access(c_name, R_OK)==0) {
      c_fname=c_name;
    } else {
      free(c_name);
      return(SCM_BOOL_F);
    }
  } else {
    SCM p;
    int max_path_len= 0;

    /* relative path */ 
    if (!gh_list_p(*loc_image_load_path)) {
      /* Warning, image-load-path is not a list. */
      return SCM_BOOL_F;
    }
    
    /* traverse the path list to compute the max buffer size we will
       need. */
    /* MSFIX: FIXGJB: ideally, we'd like to do this only after 
     *loc_image_load_path changes */
    /* GJBFIX: I don't think there is a way to know that... */

    for (p = *loc_image_load_path; p != SCM_EOL; p = SCM_CDR(p)) {
      SCM elt = SCM_CAR(p);
      if (!gh_string_p(elt)) {
	/* Warning, non-string in image-load-path */
	scwm_msg(WARN,__FUNCTION__,"Non-string in image-load-path");
 	return SCM_BOOL_F; /* Why bail? just skip it--gjb 11/28/97  */
	/* Assuming path is list of strings simplifies code below. */
      } else {
	int l=SCM_ROLENGTH(elt);
	max_path_len= (l > max_path_len) ? l : max_path_len;
      }
    }
    
    /* Add 2, one for the '/', one for the final NULL */
    c_fname = (char *) safemalloc(sizeof(char) *(max_path_len + length + 2));
    
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
      scwm_msg(WARN,__FUNCTION__,"Image file was not found: `%s'",c_name);
      InvokeHook1(*loc_image_not_found_hook,gh_str02scm(c_name));
      free(c_name);
      free(c_fname);
      return SCM_BOOL_F;
    }
  }

  result = gh_str02scm(c_fname);
  free(c_name);
  free(c_fname);
  return result;
}


SCM
get_image_loader(SCM name)
{
  char *c_name;
  char *c_ext;
  int length;
  SCM result = SCM_BOOL_F;
  
  c_name=gh_scm2newstr(name,&length);

  c_ext = strchr (c_name,'.');
  if (c_ext==NULL) {
    scm_hash_ref (image_loader_hash_table, str_empty, SCM_BOOL_F);  
  } else {
    while (c_ext != NULL && result == SCM_BOOL_F) {
      result = scm_hash_ref (image_loader_hash_table, 
			     gh_str02scm(c_ext), SCM_BOOL_F);
      c_ext = strchr (c_ext+1,'.');
    }
  }

  free (c_name);
  if (result == SCM_BOOL_F) {
    result = scm_hash_ref(image_loader_hash_table, str_default,
			  SCM_BOOL_F);
  }
  return result;
}


SCWM_PROC (make_image, "make-image", 1, 0, 0,
           (SCM name))
{
  SCM result;
  SCM full_path;
  SCM loader;

  if (!gh_string_p(name)) {
    scm_wrong_type_arg(s_make_image,1,name);
  }

  /* First check the hash table for this image.  */
  result=scm_hash_ref(image_hash_table, name, SCM_BOOL_F);
  if (result != SCM_BOOL_F) {
    /* FIXMS: should really check for up-to-date-ness here */
    /* Never mind, let's just rely on the cache clear stuff */
    return(result);
  }

  /* OK, it wasn't in the hash table - we need to expand the filename.
   */
  full_path = path_expand_image_fname(name);

  if (full_path==SCM_BOOL_F) {
    return (SCM_BOOL_F);
  }

  /* Check the cache again */
  result=scm_hash_ref(image_hash_table, full_path, SCM_BOOL_F);
  if (result != SCM_BOOL_F) {
    scm_hash_set_x(image_hash_table, name, result);
    return result;
  }

  /* still not there, find the right loader, and load it up. */
  loader = get_image_loader(name);
  if (loader != SCM_BOOL_F) {
    result = scwm_safe_call1(loader, full_path);
  }

  if (result == SCM_BOOL_F) {
    /* the load failed; try the default loader */
    loader = scm_hash_ref(image_loader_hash_table, str_default, SCM_BOOL_F);
    if (loader != SCM_BOOL_F) {
      result = scwm_safe_call1(loader, full_path);
    }
    if (result == SCM_BOOL_F) {
      /* Still failed, return #f and possibly warn. */
      return SCM_BOOL_F;
    }
  }

  IMAGE(result)->full_name=full_path;

  /* Put it in the hash table under both the full name and the given name. */
  scm_hash_set_x(image_hash_table, name, result);
  scm_hash_set_x(image_hash_table, full_path, result);

  return result;
}


SCWM_PROC (clear_image_cache_entry, "clear-image-cache-entry", 1, 0, 0,
           (SCM name))
{
  scm_hash_remove_x(image_hash_table, name);
  return SCM_UNSPECIFIED;
}


SCM
make_image_from_pixmap(char *szDescription,
		       Pixmap image, Pixmap mask, 
		       int width, int height, int depth)
{
  SCM result = make_empty_image(gh_str02scm(szDescription));
  scwm_image *ci = IMAGE(result);
  ci->image = image;
  ci->mask = mask;
  ci->width = width;
  ci->height = height;
  ci->depth = depth;

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

  /* Save a convenient Scheme "default" string */
  str_default=gh_str02scm("default");
  scm_protect_object(str_default);

  /* Do the same for "" */
  str_empty=gh_str02scm("");
  scm_protect_object(str_empty);

  /* Include registration of procedures and other things. */
#ifndef SCM_MAGIC_SNARFER
# include "image.x"
#endif

  /* Register the image type. */
  scm_tc16_scwm_image = scm_newsmob(&image_smobfuns);

  /* Initialize the image cache table. */
  image_hash_table = 
    scm_make_weak_value_hash_table (SCM_MAKINUM(IMAGE_HASH_SIZE));
  scm_protect_object(image_hash_table);

  /* Initialize the loader hash table. */
  image_loader_hash_table = 
    scm_make_vector (SCM_MAKINUM(IMAGE_LOADER_HASH_SIZE), SCM_EOL);
  scm_protect_object(image_loader_hash_table);

  /* Register the standard loaders. */

  val_load_xbm = gh_lookup("load-xbm");
  val_load_xpm = gh_lookup("load-xpm");

  register_image_loader (str_empty, val_load_xbm);
  register_image_loader (gh_str02scm(".icon"), val_load_xbm);
  register_image_loader (gh_str02scm(".bitmap"), val_load_xbm);
  register_image_loader (gh_str02scm(".xbm"), val_load_xbm);
  register_image_loader (gh_str02scm(".xpm"), val_load_xpm);
  register_image_loader (gh_str02scm(".xpm.gz"), val_load_xpm);
  register_image_loader (str_default, val_load_xbm);

  /* Make the image-load-path Scheme variable easily accessible from C,
     and load it with a nice default value. */
  loc_image_load_path = SCM_CDRLOC
    (scm_sysintern("image-load-path", 
		   gh_eval_str("\'"SCWM_IMAGE_LOAD_PATH)));
  loc_image_not_found_hook = SCM_CDRLOC
    (scm_sysintern("image-not-found-hook", SCM_BOOL_F));
}

