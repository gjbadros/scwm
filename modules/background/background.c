/* $Id$
 * Copyright (C) 1999, Maciej Stachowiak, Greg J. Badros
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

#include <X11/Xlib.h>

#include <guile/gh.h>

#include "scwm.h"
#include "image.h"
#include "color.h"
#include "guile-compat.h"
#include "screen.h"

static Atom atom_XROOTPMAP_ID;
static Atom atom_XROOTCOLOR_PIXEL;
static Atom atom_XSETROOT_ID;


SCM_SYMBOL(sym_centered, "centered");
SCM_SYMBOL(sym_tiled, "tiled");

static SCM root_bg_color = SCM_BOOL_F;
static SCM root_image = SCM_BOOL_F;

/* MS:FIXME:MS: Add docs! */

SCWM_PROC(set_background_color_x, "set-background-color!", 1, 0, 0,
	  (SCM color))
     /** Sets the color of the root window to COLOR. */
#define FUNC_NAME s_set_background_color_x
{
  Pixmap dummy = None;

  VALIDATE_ARG_COLOR(1,color);

  XSetWindowBackground(dpy, Scr.Root, XCOLOR(color));
  XClearWindow(dpy, Scr.Root);

  XChangeProperty(dpy, Scr.Root, atom_XROOTCOLOR_PIXEL, XA_CARDINAL, 32, PropModeReplace,
		  (char *)&(XCOLOR(color)), 1);
  XChangeProperty(dpy, Scr.Root, atom_XROOTPMAP_ID, XA_PIXMAP, 32, PropModeReplace,
		  (char *)&dummy, 1);

  root_bg_color = color;

  /* MS:FIXME:: Should set _XSETROOT_ID to a fake one-pixel pixmap, just delete for now. */
  XDeleteProperty(dpy, Scr.Root, atom_XSETROOT_ID);


  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME 

SCWM_PROC(clone_resized_image, "clone-resized-image", 3, 1, 0,
	  (SCM image, SCM width, SCM height, SCM bgcolor))
     /** Makes a new image from IMAGE of the given WIDTH and HEIGHT.
It does not scale IMAGE.  If the resized image is smaller than the
original, it is cropped; if larger, the extra space in the new image
is filled with BGCOLOR. See also `clone-scaled-image'.*/
#define FUNC_NAME s_clone_resized_image
{
  int nw;
  int nh;
  SCM img;

  VALIDATE_ARG_IMAGE_OR_STRING(1,image);
  VALIDATE_ARG_INT_MIN_COPY(2,width,1,nw);
  VALIDATE_ARG_INT_MIN_COPY(3,height,1,nh);

  if (IMAGE(img)->width==nw && IMAGE(img)->height==nh) {
    return img;
  }
  
  { /* scope */
    SCM retval;
    int ox, oy, nx, ny, nnw, nnh;
    XGCValues gcv;
    GC gc;
    Pixmap pix = IMAGE(img)->image;
    Pixmap bit = IMAGE(img)->mask;
    
    Pixmap npix = XCreatePixmap(dpy, Scr.Root, nw, nh, Scr.d_depth);

    if (IMAGE(img)->height <= nh) {
      oy = 0;
      ny = (nh - IMAGE(img)->height)/2;
      nnh = IMAGE(img)->height;
    } else {
      oy = (IMAGE(img)->height - nh)/2;
      ny = 0;
      nnh = nh;   
    }

    if (IMAGE(img)->width <= nw) {
      ox = 0;
      nx = (nw - IMAGE(img)->width)/2;
      nnw = IMAGE(img)->width;
    } else {
      ox = (IMAGE(img)->width - nw)/2;
      nx = 0;
      nnw = nw;   
    }
    
    /* Create the GC and stuff */

    VALIDATE_ARG_COLOR_COPY_USE_BLACK(4,bgcolor,gcv.foreground);

    gc=XCreateGC(dpy, Scr.Root, GCForeground, &gcv);
		 
    XFillRectangle(dpy, npix, gc, 0, 0, nw+1, nh+1);

    if (bit!=None) {
      /* Add appropriate mask */
      gcv.clip_mask = bit;
      gcv.clip_x_origin = nx;
      gcv.clip_y_origin = ny;
      
      XChangeGC(dpy, gc, GCClipMask | GCClipXOrigin | GCClipYOrigin,
		&gcv);
    }

    XCopyArea(dpy, pix, npix, gc, ox, oy, nnw, nnh, nx, ny);


    /* MS:FIXME:: Use a better name */
    retval=make_image_from_pixmap("(resized image)", npix, None, nw,
				  nh, Scr.d_depth);

    return retval;
  }    
}
#undef FUNC_NAME 


SCWM_PROC(set_background_image_x, "set-background-image!", 1, 1, 0,
	  (SCM image, SCM style))
     /** Sets the background of the root window to be IMAGE.
STYLE can be either 'centered or 'tiled. */
#define FUNC_NAME s_set_background_image_x
{
  int dummy = 0;

  VALIDATE_ARG_IMAGE_OR_STRING(1,image);
 
  if (style==SCM_UNDEFINED) {
    style = sym_centered;
  }

  if (!(style == sym_tiled || style == sym_centered)) {
    SCWM_WRONG_TYPE_ARG(2,style);
  }

  if (style==sym_centered) {
    image = clone_resized_image(image, gh_ulong2scm(Scr.DisplayWidth),
                                gh_ulong2scm(Scr.DisplayHeight),
                                root_bg_color);
  };

  XSetWindowBackgroundPixmap(dpy, Scr.Root, IMAGE(image)->image);
  XClearWindow(dpy, Scr.Root);

  root_image = image;

  XChangeProperty(dpy, Scr.Root, atom_XROOTCOLOR_PIXEL, XA_CARDINAL, 32, PropModeReplace,
		  (char *)&dummy, 1);
  XChangeProperty(dpy, Scr.Root, atom_XROOTPMAP_ID, XA_PIXMAP, 32, PropModeReplace,
		  (char *)&(IMAGE(image)->image), 1);
  XChangeProperty(dpy, Scr.Root, atom_XSETROOT_ID, XA_PIXMAP, 32, PropModeReplace,
		  (char *)&(IMAGE(image)->image), 1);

  
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME 


SCWM_PROC(reset_background_x, "reset-background!", 0, 0, 0,
	  ())
     /** Resets the root window to the default "weave". */
#define FUNC_NAME s_reset_background_x
{
  XSetWindowBackgroundPixmap(dpy, Scr.Root, (Pixmap) None);
  XClearWindow(dpy, Scr.Root);
  root_bg_color = SCM_BOOL_F;
  root_image = SCM_BOOL_F;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME 


static
void
init_background()
{

  atom_XROOTPMAP_ID = XInternAtom (dpy, "_XROOTPMAP_ID", False);
  atom_XROOTCOLOR_PIXEL = XInternAtom (dpy,"_XROOTCOLOR_PIXEL", False);
  atom_XSETROOT_ID = XInternAtom (dpy, "_XSETROOT_ID", False);
  scm_permanent_object(root_image);
  scm_permanent_object(root_bg_color);

#ifndef SCM_MAGIC_SNARFER
 #include "background.x"
#endif
}

void scm_init_app_scwm_background_module()
{
  scm_register_module_xxx("app scwm background", init_background);
}

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
