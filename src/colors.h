/* $Id$
 * colors.h
 * (C) 1997, 1998 Maciej Stachowiak and Greg J. Badros
 */

#ifndef COLORS_H
#define COLORS_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <X11/Intrinsic.h>

Pixel *AllocNonlinearGradient(char *s_colors[], int clen[], 
			      int nsegs, int npixels);

#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
