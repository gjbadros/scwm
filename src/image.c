/* $Id$
 * Copyright (C) 1997-1999, Maciej Stachowiak and Greg J. Badros
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <X11/Intrinsic.h>
#include <X11/xpm.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include <guile/gh.h>

#define IMAGE_IMPLEMENTATION
#include "image.h"

#include "scwm.h"
#include "screen.h"
#include "callbacks.h"
#include "scwmpaths.h"
#include "guile-compat.h"

#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif

#ifdef USE_IMLIB
ImlibData *imlib_data;
#endif


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

static SCM *pscm_image_load_path;

SCWM_HOOK(image_not_found_hook, "image-not-found-hook", 1);
  /** Called with image name as a string when not found. */
  


static SCM str_default;
static SCM str_empty;

SCM_SYMBOL (sym_filename,"filename");
SCM_SYMBOL (sym_width,"width");
SCM_SYMBOL (sym_height,"height");
SCM_SYMBOL (sym_depth,"depth");
SCM_SYMBOL (sym_pixmap,"pixmap");
SCM_SYMBOL (sym_mask,"mask");

/**CONCEPT: Images 
  Images are first-class objects. However, anywhere that an image is
taken as an argument, a string containing a filename will also be
accepted, and will be automatically converted to the proper image
object by loading the image. Using the same image filename more than
once is not inefficient, as caching ensures that image objects are
shared.
*/


size_t 
free_image(SCM obj)
{
  scwm_image *si = IMAGE(obj);
  if (!si)
    return 0;
#ifdef USE_IMLIB 
  if (si->image != None)
    Imlib_free_pixmap(imlib_data, si->image); 
  if (si->im)
    Imlib_destroy_image(imlib_data, si->im); 
#else   
  if (!si->foreign) {
    if (si->image != None) {
      XFreePixmap(dpy, si->image);
    }
    if (si->mask != None) {
      XFreePixmap(dpy, si->mask);
    }
  }
#endif
  FREE(si);
  return (0);
}

int 
print_image(SCM obj, SCM port, scm_print_state *ARG_IGNORE(pstate))
{
  scm_puts("#<image ", port);
  scm_write(IMAGE(obj)->full_name, port);
  scm_putc('>', port);
  return 1;
}

SCM
mark_image(SCM obj)
{
  GC_MARK_SCM_IF_SET(IMAGE(obj)->full_name);
  return SCM_BOOL_F;
}

SCWM_PROC(image_p, "image?", 1, 0, 0,
           (SCM obj))
     /** Returns #t if OBJ is an image object, otherwise #f. */
#define FUNC_NAME s_image_p
{
  return SCM_BOOL_FromBool(IMAGE_P(obj));
}
#undef FUNC_NAME


SCWM_PROC(image_properties, "image-properties", 1, 0, 0,
           (SCM image))
     /** Return an association list giving some properties of IMAGE.
Currently defined properties are 'filename, the fully expanded
pathname of the image, 'width, its width, 'height, its height, and
depth, its color depth. 
*/
#define FUNC_NAME s_image_properties
{
  scwm_image *psimg = SAFE_IMAGE(image);
  if (!psimg) {
    SCWM_WRONG_TYPE_ARG(1, image);
  } 
  return gh_list(gh_cons(sym_filename,psimg->full_name),
		 gh_cons(sym_width,gh_int2scm(psimg->width)),
		 gh_cons(sym_height,gh_int2scm(psimg->height)),
		 gh_cons(sym_depth,gh_int2scm(psimg->depth)),
		 gh_cons(sym_pixmap,gh_int2scm(psimg->image)),
		 gh_cons(sym_mask,gh_int2scm(psimg->mask)),
		 SCM_UNDEFINED);
}
#undef FUNC_NAME


SCM
make_empty_image(SCM name)
{
  SCM result;
  scwm_image *ci;

  ci = NEW(scwm_image);
  ci->full_name = name;
#ifdef USE_IMLIB
  ci->im = 0;
#endif  
  ci->image = None;
  ci->mask = None;
  ci->height = 0;
  ci->width = 0;
  ci->depth = 0;
  ci->foreign = 0;

  gh_defer_ints();
  SCWM_NEWCELL_SMOB(result, scm_tc16_scwm_image, ci);
  gh_allow_ints();

  return result;
}

/**CONCEPT: Image Loaders 
  Different loaders are available for various images types. `load-xbm'
and `load-xpm' load X bitmaps and X pixmaps respectively. The user may
register other image loaders using the extension or the special string
"default" for the loader to be tried for an image that cannot be
loaded any other way.
*/

#ifndef USE_IMLIB
SCWM_PROC(load_xbm, "load-xbm", 1, 0, 0,
           (SCM full_path))
     /** Load an X Bitmap file identified by the pathname FULL-PATH. */
#define FUNC_NAME s_load_xbm
{
  SCM result;
  scwm_image *ci;
  int ignore;
  char *c_path;
  int xbm_return;

  result=make_empty_image(full_path);
  ci=IMAGE(result);
  
  c_path = gh_scm2newstr(full_path,&ignore);

  xbm_return=XReadBitmapFile(dpy, Scr.Root, c_path, 
		      &ci->width, &ci->height, &ci->image, 
		      &ignore, &ignore);
  if (xbm_return == BitmapSuccess) {
    ci->depth = 0;
    ci->mask = None;
  } else {
    /* warn that the image could not be loaded, then drop the result
       on the floor and let GC clean it up. */
    scwm_msg(WARN,FUNC_NAME,"Could not load bitmap `%s'",c_path);
    result = SCM_BOOL_F;
  }
  gh_free(c_path);
  return result;
}
#undef FUNC_NAME


SCWM_PROC(load_xpm, "load-xpm", 1, 0, 0,
           (SCM full_path))
     /** Load an X Pixmap file identified by the pathname FULL-PATH. */
#define FUNC_NAME s_load_xpm
{
  SCM result;
  scwm_image *ci;
  int ignore;
  char *c_path;
  XpmAttributes xpm_attributes;
  int xpm_return;

  result=make_empty_image(full_path);
  ci=IMAGE(result);

  c_path = gh_scm2newstr(full_path,&ignore);

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
    scwm_msg(WARN,FUNC_NAME,"Could not load pixmap `%s'",c_path);
    result = SCM_BOOL_F;
  }
  gh_free(c_path);
  return result;
}
#undef FUNC_NAME
#else /* USE_IMLIB */

SCWM_PROC(load_imlib_image, "load-imlib-image", 1, 0, 0,
           (SCM full_path))
     /** Load an image file using imlib identified by the pathname FULL-PATH. */
#define FUNC_NAME s_load_imlib_image
{
  SCM result;
  scwm_image *ci;
  int ignore;
  char *c_path;

  result=make_empty_image(full_path);
  ci=IMAGE(result);

  c_path = gh_scm2newstr(full_path,&ignore);

  ci->im = Imlib_load_image(imlib_data, c_path);
  if(ci->im == 0)
  {
    scwm_msg(WARN, FUNC_NAME, "Could not load(with imlib) image `%s'", c_path);
    result = SCM_BOOL_F;
  }
  else
  {
    ci->width = ci->im->rgb_width;
    ci->height = ci->im->rgb_height;
    
    if(!Imlib_render(imlib_data, ci->im, ci->width, ci->height))
    {
      scwm_msg(WARN, FUNC_NAME, "Imlib unable to render pixmaps from "
               "image `%s'", c_path);
      result = SCM_BOOL_F;
    }
    else
    {
      ci->image = Imlib_move_image(imlib_data, ci->im);
      ci->mask = Imlib_move_mask(imlib_data, ci->im);
      ci->depth = imlib_data->x.depth;
    }
  }

  gh_free(c_path);
  return result;
  
}
#undef FUNC_NAME

#endif /* USE_IMLIB */


SCWM_PROC(register_image_loader, "register-image-loader", 2, 0, 0,
           (SCM extension, SCM proc))
     /** Register PROC as the loader to use for images ending in EXTENSION.
EXTENSION must be a string beginning with a period, the
empty string (for files with no extension), or the string "default"
(for files that no other image loader succeeds in loading). PROC will
be called with the full pathname of the image and should return an
image object, or #f if it succeeds. */
#define FUNC_NAME s_register_image_loader
{
  if (!gh_string_p(extension)) {
    SCWM_WRONG_TYPE_ARG(1, extension);
  }
  
  /* Sadly, there is no way to test the arity of a procedure. */
  /* MSFIX: Yes there is, but let's not use it yet. */
  if (!gh_procedure_p(proc)) {
    SCWM_WRONG_TYPE_ARG(2, proc);
  }

  scm_hash_set_x(image_loader_hash_table, extension, proc);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(unregister_image_loader, "unregister-image-loader", 1, 0, 0,
           (SCM extension))
     /** Unregister the loader, if any, for images ending in EXTENSION.
EXTENSION must be a string beginning with a period, the
empty string (for files with no extension), or the string "default"
(for files that no other image loader succeeds in loading). */
#define FUNC_NAME s_unregister_image_loader
{
  if (!gh_string_p(extension)) {
    SCWM_WRONG_TYPE_ARG(1, extension);
  }

  scm_hash_remove_x(image_loader_hash_table, extension);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/*SCWM_VALIDATE: name*/
SCM
path_expand_image_fname(SCM name, const char *func_name)
#define FUNC_NAME func_name
{
  char *c_name, *c_fname;
  int length;
  SCM result;

  VALIDATE_ARG_STR_NEWCOPY_LEN(1,name,c_name,length);

  if ((length > 0 && c_name[0]=='/') ||
      (length > 1 && c_name[0]=='.' &&
       (c_name[1]=='/' || (length > 2 && c_name[1]=='.' 
			   && c_name[2]=='/')))) {
    /* absolute path */

    if (access(c_name, R_OK)==0) {
      c_fname = c_name;  /* alias the two names */
    } else {
      FREE(c_name);
      return SCM_BOOL_F;
    }
  } else {
    SCM p;
    int max_path_len= 0;

    /* relative path */ 
    if (!gh_list_p(*pscm_image_load_path)) {
      /* Warning, image-load-path is not a list. */
      return SCM_BOOL_F;
    }
    
    /* traverse the path list to compute the max buffer size we will
       need. */
    /* GJB:FIXME:: ideally, we'd like to do this only after 
     *pscm_image_load_path changes -- maybe we could compare
     against a hash of the old value before redoing this work */

    for (p = *pscm_image_load_path; p != SCM_EOL; p = gh_cdr(p)) {
      SCM elt = gh_car(p);
      if (!gh_string_p(elt)) {
	/* Warning, non-string in image-load-path */
	scwm_msg(WARN,FUNC_NAME,"Non-string in image-load-path");
 	return SCM_BOOL_F; /* Why bail? just skip it--gjb 11/28/97  */
	/* Assuming path is list of strings simplifies code below. */
      } else {
	int l=SCM_ROLENGTH(elt);
	max_path_len= (l > max_path_len) ? l : max_path_len;
      }
    }
    
    /* Add 2, one for the '/', one for the final NULL */
    c_fname = NEWC(max_path_len + length + 2,char);
    
    /* Try every possible path */
    for(p = *pscm_image_load_path; p != SCM_EOL; p = gh_cdr(p)) {
      SCM elt = gh_car(p);
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
      scwm_msg(WARN,FUNC_NAME,"Image file was not found: `%s'",c_name);

      call1_hooks(image_not_found_hook,gh_str02scm(c_name));
      FREE(c_name);
      FREEC(c_fname);
      return SCM_BOOL_F;
    }
  }

  result = gh_str02scm(c_fname);
  /* c_fname and c_name may be aliased from assignment above --
     be sure to free only once */
  if (c_fname != c_name)
    FREE(c_fname);
  FREE(c_name);
  return result;
}
#undef FUNC_NAME

SCM
get_image_loader(SCM name)
{
  char *c_name;
  char *c_ext;
  int length;
  SCM result = SCM_BOOL_F;
  
  c_name = gh_scm2newstr(name,&length);

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

  gh_free(c_name);
  c_name = NULL;

  if (result == SCM_BOOL_F) {
    result = scm_hash_ref(image_loader_hash_table, str_default,
			  SCM_BOOL_F);
  }
  return result;
}


SCWM_PROC(make_image, "make-image", 1, 0, 0,
           (SCM name))
     /** Loads an image from the file NAME.
To load the image, the appropriate image loaders will be invoked as
needed. If NAME starts with "/", "./" or "../", it is treated as a
fully qualified pathname; otherwise, the image path is searched for an
appropriate file. */
#define FUNC_NAME s_make_image
{
  SCM result;
  SCM full_path;
  SCM loader;

  if (!gh_string_p(name)) {
    SCWM_WRONG_TYPE_ARG(1,name);
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
  full_path = path_expand_image_fname(name,FUNC_NAME);

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
#undef FUNC_NAME


SCWM_PROC(clear_image_cache_entry, "clear-image-cache-entry", 1, 0, 0,
           (SCM name))
     /** Images are cached by both name and full pathname. It is
remotely possible that the file that should be used for a particular
name will change, for example if you alter the image file or change
your image path. For this unlikely eventuality,
`clear-image-cache-entry' is provided - it removes the image
associated with NAME from the image cache.*/
#define FUNC_NAME s_clear_image_cache_entry
{
  scm_hash_remove_x(image_hash_table, name);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


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


void 
init_image_colormap() 
{
  XWindowAttributes root_attributes;
  XGetWindowAttributes(dpy, Scr.Root, &root_attributes);
  ImageColorMap = root_attributes.colormap;
}

MAKE_SMOBFUNS(image);

void init_image() 
{
#ifdef USE_IMLIB
  SCM val_load_imlib_image;
#else /* !USE_IMLIB */  
  SCM val_load_xbm, val_load_xpm;
#endif
  
  REGISTER_SCWMSMOBFUNS(image);

  /* Save a convenient Scheme "default" string */
  scm_permanent_object(str_default=gh_str02scm("default"));

  /* Do the same for "" */
  scm_permanent_object(str_empty=gh_str02scm(""));

  /* Include registration of procedures and other things. */
#ifndef SCM_MAGIC_SNARFER
# include "image.x"
#endif

  /* Initialize the image cache table. */
  image_hash_table = 
    scm_make_weak_value_hash_table (SCM_MAKINUM(IMAGE_HASH_SIZE));
  scm_permanent_object(image_hash_table);

  /* Initialize the loader hash table. */
  image_loader_hash_table = 
    gh_make_vector (SCM_MAKINUM(IMAGE_LOADER_HASH_SIZE), SCM_EOL);
  scm_permanent_object(image_loader_hash_table);

  /* Register the standard loaders. */
#ifdef USE_IMLIB

  /* init imlib */
  imlib_data = Imlib_init(dpy);
  
  val_load_imlib_image = gh_lookup("load-imlib-image");
  if(val_load_imlib_image == SCM_UNDEFINED)
  {
    scwm_msg(ERR,"init_image","load-imlib-image not defined -- "
             "probable build error\n consider 'rm *.x' and rebuild");
    abort();
  }

  register_image_loader (str_empty, val_load_imlib_image);
  register_image_loader (gh_str02scm("default"), val_load_imlib_image);

#else /* !USE_IMLIB */  
  val_load_xbm = gh_lookup("load-xbm");
  val_load_xpm = gh_lookup("load-xpm");

  if (val_load_xpm == SCM_UNDEFINED ||
      val_load_xbm == SCM_UNDEFINED) {
    scwm_msg(ERR,"init_image","load-xbm, load-xpm not defined -- probable build error\n\
consider 'rm *.x' and rebuild");
    abort();
  }

  register_image_loader (str_empty, val_load_xbm);
  register_image_loader (gh_str02scm(".icon"), val_load_xbm);
  register_image_loader (gh_str02scm(".bitmap"), val_load_xbm);
  register_image_loader (gh_str02scm(".xbm"), val_load_xbm);
  register_image_loader (gh_str02scm(".xpm"), val_load_xpm);
  register_image_loader (gh_str02scm(".xpm.gz"), val_load_xpm);
  register_image_loader (str_default, val_load_xbm);
#endif /* USE_IMLIB */
  
  /* Make the image-load-path Scheme variable easily accessible from C,
     and load it with a nice default value. */
  
  SCWM_VAR_INIT(image_load_path,"image-load-path", gh_eval_str("\'"SCWM_IMAGE_LOAD_PATH));
  /** List of strings of directories in which to look for image files. */

}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

