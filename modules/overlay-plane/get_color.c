/* $Id$
 * Greg Badros <gjb@cs.washington.edu> 
 * Seattle, WA  USA  {FIX: use env_vars}
 * http://www.cs.washington.edu/homes/gjb
 */

/*
 * Copyright 1989, 1990, 1991 O'Reilly and Associates, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * online documentation for any purpose is hereby granted without 
 * fee, provided that the above copyright notice appear in all copies 
 * and that both that copyright notice and this permission notice 
 * appear in supporting documentation, and that the name of O'Reilly 
 * and Associates, Inc. not be used in advertising or publicity 
 * pertaining to distribution of the software without specific, 
 * written prior permission.  O'Reilly and Associates, Inc. makes no 
 * representations about the suitability of this software for any 
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 * O'REILLY AND ASSOCIATES, INC. DISCLAIMS ALL WARRANTIES WITH REGARD 
 * TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF 
 * MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL O'REILLY AND ASSOCIATES, INC.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Adrian Nye of O'Reilly and Associates, Inc.
 */

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <stdio.h>

extern Display *dpy;
extern int screen_num;
extern unsigned long foreground, background_pixel, overlay_pixel_1, overlay_pixel_2;
extern unsigned long overlay_plane_mask;

#define MAX_COLORS 2
#define MAX_PLANES 1
#define MAX_CELLS 4       /* MAX_COLORS * 2 ^ MAX_PLANES */

#if 0
static char *visual_class[] = {
  "StaticGray",
  "GrayScale",
  "StaticColor",
  "PseudoColor",
  "TrueColor",
  "DirectColor"
};
#endif

/* Return True if we can overlay, False otherwise */
Bool
get_colors()
{
  int default_depth;
  static char *name[] = {"Red", "Yellow", "Green", "Green"};
  XColor exact_defs[MAX_CELLS];
  Colormap default_cmap;
  int ncolors = 4;
  unsigned long plane_masks[MAX_PLANES];
  unsigned long colors[MAX_COLORS];
  int i;
  XVisualInfo visual_info;

  default_depth = DefaultDepth(dpy, screen_num);
  default_cmap   = DefaultColormap(dpy, screen_num);
  if (default_depth == 1) {
    /* must be StaticGray, use black and white */
    background_pixel = WhitePixel(dpy, screen_num);
    foreground = BlackPixel(dpy, screen_num);
    fprintf(stderr,"using black and white\n");
    return False;
  }

  if (!XMatchVisualInfo(dpy, screen_num, default_depth, PseudoColor, &visual_info)) {
    if (!XMatchVisualInfo(dpy, screen_num, default_depth, DirectColor, &visual_info)) {
      /* No PseudoColor or TrueColor visual available at default_depth.
       * Some applications might try for a GrayScale visual 
       * here if they can use gray to advantage, before 
       * giving up and using black and white.
       */
      background_pixel = WhitePixel(dpy, screen_num);
      foreground = BlackPixel(dpy, screen_num);
      fprintf(stderr,"using black and white\n");
      return False;
    }
  }

  /* got PseudoColor or TrueColor visual at default depth */

  /* The visual we found is not necessarily the 
   * default visual, and therefore it is not necessarily
   * the one we used to create our window.  However,
   * we now know for sure that color is supported, so the
   * following code will work (or fail in a controlled way).
   */

  if (XAllocColorCells (dpy, default_cmap, False, plane_masks, 1, colors, 2) == 0) {
    /* Can't get enough read/write cells to overlay.
     * Try at least to get three colors. */
    if (XAllocColorCells (dpy, default_cmap, False, plane_masks, 0, colors, 3) == 0) {
      /* Can't even get that.  Give up and
       * use black and white */
      background_pixel = WhitePixel(dpy, screen_num);
      foreground = BlackPixel(dpy, screen_num);
      fprintf(stderr,"using black and white\n");
      return False;
    }
    else {
      ncolors = 3;
    }
  }
      	
  /* allocated three or four colorcells succesfully,
   * now set their colors - three and four
   * are set to the same RGB values */
  for (i = 0; i < ncolors; i++)
    {
      if (!XParseColor (dpy, default_cmap, name[i], &exact_defs[i])) {
        fprintf(stderr, "color name %s not in database", name[i]);
        return False;
      }
      /* this needed before calling XStoreColors */
      exact_defs[i].flags = DoRed | DoGreen | DoBlue;
    }
  fprintf(stderr,"got RGB values\n");

  /* set pixel value in struct to the allocated ones */
  exact_defs[0].pixel = colors[0];
  exact_defs[1].pixel = colors[1];
  exact_defs[2].pixel = colors[0] | plane_masks[0];
  exact_defs[3].pixel = colors[1] | plane_masks[0];

  /* this sets the color of the read/write cells */
  XStoreColors (dpy, default_cmap, exact_defs, ncolors);
  fprintf(stderr,"stored colors\n");

  background_pixel = exact_defs[0].pixel;
  foreground = exact_defs[1].pixel;
  fprintf(stderr,"set f and g\n");
  if (ncolors == 4) {
    overlay_pixel_1 = exact_defs[2].pixel;
    overlay_pixel_2 = exact_defs[3].pixel;
    overlay_plane_mask = plane_masks[0];
    fprintf(stderr,"overlay_pixel_{1,2} = %ld, %ld; overlay_plane_mask = %ld\n",
            overlay_pixel_1,overlay_pixel_2,overlay_plane_mask);
    return True;
  } else {
    /* this must be used as a normal color, not overlay */
    overlay_pixel_1 = exact_defs[2].pixel;
    fprintf(stderr,"cannot do overlays\n");
    return False;
  }
}
