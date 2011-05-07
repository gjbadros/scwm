/* $Id$
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

#include <libguile.h>

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <X11/Intrinsic.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#ifdef HAVE_LIBXPM
#include <X11/xpm.h>
#endif

#define IMAGE_IMPLEMENTATION
#include "image.h"

#include "scwm.h"
#include "screen.h"
#include "callbacks.h"
#include "scwmpaths.h"
#include "guile-compat.h"
#include "xmisc.h"

#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif

#ifdef USE_IMLIB
ImlibData *imlib_data;
#endif


static Colormap ImageColorMap;

/* MS:FIXME:: arbitrary choices for these sizes - I need to check out if
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
 * MS:FIXME:: This architecture prevents new images from shadowing old
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

/* Make the image-load-path Scheme variable easily accessible from C,
   and load it with a nice default value. */
  
SCM_VARIABLE_INIT(scm_image_load_path,"image-load-path", scm_c_eval_string("\'"SCWM_IMAGE_LOAD_PATH));
/** List of strings of directories in which to look for image files. */


SCWM_HOOK(image_not_found_hook, "image-not-found-hook", 1,
"Called with image name as a string when not found.");

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

SCM_GLOBAL_SMOB(scm_tc16_scwm_scwmimage, "scwm-image", 0);

SCM_SMOB_FREE(scm_tc16_scwm_scwmimage, free_scwmimage, obj)
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
  scm_gc_free(si, sizeof (scwm_image), "scwm-image");
  return 0;
}

SCM_SMOB_PRINT(scm_tc16_scwm_scwmimage, print_scwmimage, obj, port, pstate)
{
  scm_puts("#<image ", port);
  scm_write(IMAGE(obj)->full_name, port);
  scm_putc('>', port);
  return 1;
}

SCM_SMOB_MARK(scm_tc16_scwm_scwmimage, mark_scwmimage, obj)
{
  GC_MARK_SCM_IF_SET(IMAGE(obj)->full_name);
  return SCM_BOOL_F;
}

SCM_DEFINE(image_p, "image?", 1, 0, 0,
          (SCM obj),
"Returns #t if OBJ is an image object, otherwise #f.")
#define FUNC_NAME s_image_p
{
  return scm_from_bool(IMAGE_P(obj));
}
#undef FUNC_NAME


SCM_DEFINE(image_properties, "image-properties", 1, 0, 0,
          (SCM image),
"Return an association list giving some properties of IMAGE.\n\n"
"Currently defined properties are 'filename, the fully expanded\n"
"pathname of the image, 'width, its width, 'height, its height, and\n"
"'depth, its color depth. ")
#define FUNC_NAME s_image_properties
{
  scwm_image *psimg;
  VALIDATE_ARG_IMAGE_COPY(1,image,psimg);
  return scm_list_n(scm_cons(sym_filename, psimg->full_name),
		    scm_cons(sym_width,    scm_from_int(psimg->width)),
		    scm_cons(sym_height,   scm_from_int(psimg->height)),
		    scm_cons(sym_depth,    scm_from_int(psimg->depth)),
		    scm_cons(sym_pixmap,   scm_from_int(psimg->image)),
		    scm_cons(sym_mask,     scm_from_int(psimg->mask)),
		    SCM_UNDEFINED);
}
#undef FUNC_NAME

/* Free returned value using FREE */
char *
SzNewImageShortName(scwm_image *psimg)
{
  char *sz = scm_to_locale_string(psimg->full_name);
  char *pch = strrchr(sz,'/');
  char *szAnswer;
  if (!pch) pch = sz;
  else pch++;
  szAnswer = strdup(pch);
  free(sz);
  return szAnswer;
}

SCM_DEFINE(image_short_name, "image-short-name", 1, 0, 0,
          (SCM image),
"Return the short name of IMAGE. \n\n"
"Use `image-properties' to access other properties of IMAGE\n"
"including its full name.")
#define FUNC_NAME s_image_short_name
{
  scwm_image *psimg;
  SCM answer;
  char *sz;
  VALIDATE_ARG_IMAGE_COPY(1,image,psimg);
  sz = SzNewImageShortName(psimg);
  answer = scm_from_locale_string(sz);
  free(sz);
  return answer;
}  
#undef FUNC_NAME



SCM_DEFINE(image_size, "image-size", 1, 0, 0,
          (SCM image),
"Return the size of IMAGE as a list (width height).")
#define FUNC_NAME s_image_size
{
  scwm_image *psimg;
  VALIDATE_ARG_IMAGE_COPY(1,image,psimg);
  return scm_list_n(scm_from_int(psimg->width),scm_from_int(psimg->height),SCM_UNDEFINED);
}
#undef FUNC_NAME


SCM
make_empty_image(SCM name)
{
  scwm_image *ci;

  ci = scm_gc_malloc(sizeof (scwm_image), "scwm-image");
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

  SCM_RETURN_NEWSMOB(scm_tc16_scwm_scwmimage, ci);
}

/**CONCEPT: Image Loaders 
  Different loaders are available for various images types. `load-xbm'
and `load-xpm' load X bitmaps and X pixmaps respectively. The user may
register other image loaders using the extension or the special string
"default" for the loader to be tried for an image that cannot be
loaded any other way.
*/

#ifndef USE_IMLIB
SCM_DEFINE(load_xbm, "load-xbm", 1, 0, 0,
          (SCM full_path),
"Load an X Bitmap file identified by the pathname FULL-PATH.")
#define FUNC_NAME s_load_xbm
{
  SCM result;
  scwm_image *ci;
  int ignore;
  char *c_path;
  int xbm_return;

  result=make_empty_image(full_path);
  ci=IMAGE(result);

  c_path = scm_to_locale_string(full_path);

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
  free(c_path);
  return result;
}
#undef FUNC_NAME

#ifdef HAVE_LIBXPM
SCM_DEFINE(load_xpm, "load-xpm", 1, 0, 0,
          (SCM full_path),
"Load an X Pixmap file identified by the pathname FULL-PATH.")
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

  c_path = scm_to_locale_string(full_path);

  xpm_attributes.colormap = ImageColorMap;

  /* MS:FIXME:: the Xpm manual suggests that a closeness of 40000 is
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
  free(c_path);
  return result;
}
#undef FUNC_NAME

#endif /* HAVE_LIBXPM */
#else /* USE_IMLIB */

Bool
MakeScwmImageFromImlibImage(scwm_image *ci,ImlibImage *pimg)
{
  ci->im = pimg;
  ci->width = pimg->rgb_width;
  ci->height = pimg->rgb_height;
  
  if (!Imlib_render(imlib_data, pimg, ci->width, ci->height)) {
    return False;
  }
  else {
    ci->image = Imlib_move_image(imlib_data, pimg);
    ci->mask = Imlib_move_mask(imlib_data, pimg);
    ci->depth = imlib_data->x.depth;
  }
  return True;
}


SCM_DEFINE(load_imlib_image, "load-imlib-image", 1, 0, 0,
          (SCM full_path),
"Load an image file using imlib identified by the pathname FULL-PATH.")
#define FUNC_NAME s_load_imlib_image
{
  SCM result;
  ImlibImage *pimg = NULL;
  scwm_image *ci;
  char *c_path;

  result=make_empty_image(full_path);
  ci=IMAGE(result);

  c_path = scm_to_locale_string(full_path);

  pimg = Imlib_load_image(imlib_data, c_path);
  if (!pimg) {
    scwm_msg(WARN, FUNC_NAME, "load-imlib-image could not load image `%s'", c_path);
    result = SCM_BOOL_F;
  } else {
    if (!MakeScwmImageFromImlibImage(ci,pimg)) {
      scwm_msg(WARN, FUNC_NAME, "Imlib unable to render pixmaps from "
               "image `%s'", c_path);
    }
  }

  free(c_path);
  return result;
}
#undef FUNC_NAME

#endif /* USE_IMLIB */


SCM_DEFINE(register_image_loader, "register-image-loader", 2, 0, 0,
          (SCM extension, SCM proc),
"Register PROC as the loader to use for images ending in EXTENSION.\n\n"
"EXTENSION must be a string beginning with a period, the\n"
"empty string (for files with no extension), or the string \"default\"\n"
"(for files that no other image loader succeeds in loading). PROC will\n"
"be called with the full pathname of the image and should return an\n"
"image object, or #f if it succeeds.")
#define FUNC_NAME s_register_image_loader
{
  VALIDATE_ARG_STR(1,extension);
  
  /* GJB:FIXME:MS: Why not check arity of procedure? */
  VALIDATE_ARG_PROC(2,proc);

  scm_hash_set_x(image_loader_hash_table, extension, proc);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE(unregister_image_loader, "unregister-image-loader", 1, 0, 0,
          (SCM extension),
"Unregister the loader, if any, for images ending in EXTENSION.\n\n"
"EXTENSION must be a string beginning with a period, the\n"
"empty string (for files with no extension), or the string \"default\"\n"
"(for files that no other image loader succeeds in loading).")
#define FUNC_NAME s_unregister_image_loader
{
  VALIDATE_ARG_STR(1,extension);

  scm_hash_remove_x(image_loader_hash_table, extension);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM
get_image_loader(SCM name)
{
  char *c_name;
  char *c_ext;
  SCM result = SCM_BOOL_F;
  
  c_name = scm_to_locale_string(name);

  c_ext = strchr (c_name,'.');
  if (c_ext==NULL) {
    scm_hash_ref (image_loader_hash_table, str_empty, SCM_BOOL_F);  
  } else {
    while (c_ext != NULL && scm_is_false(result)) {
      result = scm_hash_ref (image_loader_hash_table, 
			     scm_from_locale_string(c_ext), SCM_BOOL_F);
      c_ext = strchr (c_ext+1,'.');
    }
  }

  free(c_name);
  c_name = NULL;

  if (scm_is_false(result)) {
    result = scm_hash_ref(image_loader_hash_table, str_default,
			  SCM_BOOL_F);
  }
  return result;
}

/* SRL:FIXME:: Sometimes crashes scwm if can't find pixmap. 
 * Use (title-style #:pixmap "a036") to trigger error. */
SCM_DEFINE(make_image, "make-image", 1, 0, 0,
          (SCM name),
"Loads an image from the file NAME.\n\n"
"To load the image, the appropriate image loaders will be invoked as\n"
"needed. If NAME starts with \"/\", \"./\" or \"../\", it is treated as a\n"
"fully qualified pathname; otherwise, the image path is searched for an\n"
"appropriate file.")
#define FUNC_NAME s_make_image
{
  SCM result;
  SCM full_path;
  SCM loader;

  VALIDATE_ARG_STR(1,name);

  /* First check the hash table for this image.  */
  result=scm_hash_ref(image_hash_table, name, SCM_BOOL_F);
  if (scm_is_true(result)) {
    /* MS:FIXME:: should really check for up-to-date-ness here */
    /* Never mind, let's just rely on the cache clear stuff */
    return result;
  }

  /* OK, it wasn't in the hash table - we need to expand the filename.
   */
  full_path = scm_search_path(scm_variable_ref(scm_image_load_path),
			      name, SCM_UNDEFINED);
  if (scm_is_false(full_path)) {
    return SCM_BOOL_F;
  }

  /* Check the cache again */
  result=scm_hash_ref(image_hash_table, full_path, SCM_BOOL_F);
  if (scm_is_true(result)) {
    scm_hash_set_x(image_hash_table, name, result);
    return result;
  }

  /* still not there, find the right loader, and load it up. */
  loader = get_image_loader(name);

  if (scm_is_true(loader)) {
    result = scwm_safe_call1(loader, full_path);
  }

  if (scm_is_false(result)) {
    /* the load failed; try the default loader */
    loader = scm_hash_ref(image_loader_hash_table, str_default, SCM_BOOL_F);
    if (scm_is_true(loader)) {
      result = scwm_safe_call1(loader, full_path);
    }
    if (scm_is_false(result)) {
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


/* GJB:FIXME:: make name == #t do a clear of all entries (ie. reset hash table). */
SCM_DEFINE(clear_image_cache_entry, "clear-image-cache-entry", 1, 0, 0,
          (SCM name),
"Remove the image associated with NAME from the image cache.\n\n"
"Images are cached by both name and full pathname. It is\n"
"remotely possible that the file that should be used for a particular\n"
"name will change, for example if you alter the image file or change\n"
"your image path. For this unlikely eventuality,\n"
"`clear-image-cache-entry' is provided")
#define FUNC_NAME s_clear_image_cache_entry
{
  scm_hash_remove_x(image_hash_table, name);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


#ifdef USE_IMLIB
SCM_DEFINE(window_to_image,"window->image", 1, 4, 0,
          (SCM win, SCM x_offset, SCM y_offset, SCM width, SCM height),
"Return an image with the contents of window WIN.\n\n"
"WIN can be a window id (as a long), a window object, or\n"
"the symbol 'root-window. Captures the rectangle of the window\n"
"at X-OFFSET, Y-OFFSET with width WIDTH and height HEIGHT.")
#define FUNC_NAME s_window_to_image
{
  Window w;
  int x, y, wd, ht;
  int win_width, win_height;
  VALIDATE_ARG_WIN_ROOTSYM_OR_NUM_COPY(1,win,w);
  VALIDATE_ARG_INT_COPY_USE_DEF(2,x_offset,x,0);
  VALIDATE_ARG_INT_COPY_USE_DEF(3,y_offset,y,0);
  VALIDATE_ARG_INT_COPY_USE_DEF(4,width,wd,-1);
  VALIDATE_ARG_INT_COPY_USE_DEF(5,height,ht,-1);
  FXGetWindowSize(w,&win_width,&win_height);
  if (wd < 0) wd = win_width - x;
  if (ht < 0) ht = win_height - y;
  { /* scope */
    ImlibImage *pimg = Imlib_create_image_from_drawable(imlib_data,
                                                        w, None, x, y, wd, ht);
    SCM result = make_empty_image(scm_from_locale_string("by window->image"));
    scwm_image *ci = IMAGE(result);
    if (!MakeScwmImageFromImlibImage(ci,pimg)) {
      scwm_msg(WARN, FUNC_NAME, "Imlib unable to render pixmaps from window");
    }
    return result;
  }
}
#undef FUNC_NAME


SCM_DEFINE (clone_scaled_image, "clone-scaled-image", 3, 0, 0,
           (SCM image, SCM width, SCM height),
"Returns a copy of IMAGE scaled to have dimensions WIDTH by HEIGHT. \n\n"
"See also `clone-resized-image' from the background module.")
#define FUNC_NAME s_clone_scaled_image
{
  int w, h;
  VALIDATE_ARG_IMAGE(1,image);
  VALIDATE_ARG_INT_COPY(2,width,w);
  VALIDATE_ARG_INT_COPY(3,height,h);

  { /* scope */
    ImlibImage *pimgSource = IMAGE(image)->im;
    ImlibImage *pimgNew = Imlib_clone_scaled_image(imlib_data,pimgSource,w,h);
    SCM result = make_empty_image(scm_from_locale_string("by window->image"));
    scwm_image *ci = IMAGE(result);
    if (!MakeScwmImageFromImlibImage(ci,pimgNew)) {
      scwm_msg(WARN, FUNC_NAME, "Imlib unable to render pixmap");
    }
    return result;
  }
}
#undef FUNC_NAME


#endif

SCM
make_image_from_pixmap(char *szDescription,
		       Pixmap image, Pixmap mask, 
		       int width, int height, int depth)
{
  SCM result = make_empty_image(scm_from_locale_string(szDescription));
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

void init_image() 
{
#ifdef USE_IMLIB
  SCM val_load_imlib_image;
#else /* !USE_IMLIB */  
  SCM val_load_xbm, val_load_xpm;
#endif

  /* Save a convenient Scheme "default" string */
  scm_permanent_object(str_default=scm_from_locale_string("default"));

  /* Do the same for "" */
  scm_permanent_object(str_empty=scm_from_locale_string(""));

  /* Include registration of procedures and other things. */
# include "image.x"

  /* Initialize the image cache table. */
  image_hash_table = 
    scm_make_weak_value_hash_table (scm_from_int(IMAGE_HASH_SIZE));
  scm_permanent_object(image_hash_table);

  /* Initialize the loader hash table. */
  image_loader_hash_table = 
    scm_c_make_hash_table(IMAGE_LOADER_HASH_SIZE);
  scm_permanent_object(image_loader_hash_table);

  /* Register the standard loaders. */
#ifdef USE_IMLIB
  { /* scope */
    Screen *screen = ScreenOfDisplay(dpy, Scr.screen);
    Visual *v = DefaultVisualOfScreen(screen);
    ImlibInitParams imlib_params;
    imlib_params.flags = PARAMS_VISUALID;
    imlib_params.visualid = XVisualIDFromVisual(v);

    /* init imlib */
    imlib_data = Imlib_init_with_params(dpy, &imlib_params);
    
    /* DPS:FIX  will throw error if undefined */
    val_load_imlib_image = scm_variable_ref(scm_c_lookup("load-imlib-image"));
    if(val_load_imlib_image == SCM_UNDEFINED) {
      scwm_msg(ERR,"init_image","load-imlib-image not defined -- "
               "probable build error\n consider 'rm *.x' and rebuild");
      abort();
    }

    register_image_loader (str_empty, val_load_imlib_image);
    register_image_loader (scm_from_locale_string("default"), val_load_imlib_image);
  }
#else /* !USE_IMLIB */  

  /* DPS:FIX   will throw error if undefined */
  val_load_xbm = scm_variable_ref(scm_c_lookup("load-xbm"));
  val_load_xpm = scm_variable_ref(scm_c_lookup("load-xpm"));

  if (val_load_xbm == SCM_UNDEFINED) {
    scwm_msg(ERR,"init_image","load-xbm not defined -- probable build error\n\
consider 'rm *.x' and rebuild");
    abort();
  }

  register_image_loader (str_empty, val_load_xbm);
  register_image_loader (scm_from_locale_string(".icon"), val_load_xbm);
  register_image_loader (scm_from_locale_string(".bitmap"), val_load_xbm);
  register_image_loader (scm_from_locale_string(".xbm"), val_load_xbm);
  if (!UNSET_SCM(val_load_xpm)) {
    register_image_loader (scm_from_locale_string(".xpm"), val_load_xpm);
    register_image_loader (scm_from_locale_string(".xpm.gz"), val_load_xpm);
    register_image_loader (str_default, val_load_xbm);
  }
#endif /* USE_IMLIB */
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

