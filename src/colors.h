/* $Id$
 * colors.h
 */

#ifndef COLORS_H
#define COLORS_H

void CreateGCs(void);
Pixel *AllocNonlinearGradient(char *s_colors[], int clen[], 
			      int nsegs, int npixels);

#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
