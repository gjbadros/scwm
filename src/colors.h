/* $Id$
 * colors.h
 */

#ifndef COLORS_H
#define COLORS_H

Pixel GetShadow(Pixel background);
Pixel GetHilite(Pixel background);
void CreateGCs(void);
Pixel *AllocNonlinearGradient(char *s_colors[], int clen[],
			      int nsegs, int npixels);

#endif
