/* $Id$ */
/*
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
 * As a special exception, this file may alternatively be distributed under 
 * the fvwm license (see COPYING.FVWM).
 *
 */

#ifndef SYSTEM_H
#define SYSTEM_H

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

void *safemalloc(int length);
char *xgetcwd(char *, int);

/* GJB:FIXME:: must handle a couple realloc-s too */

#if defined(USE_CASSOWARY) && defined(__cplusplus)
#define NEWCPP(x) (new x)
#define NEWCPPC(c,x) (new x[c])
#define FREECPP(x) (delete x)
#define FREECPPC(x) (delete [] x)
#else
#define NEWCPP(x) ((x *) safemalloc(sizeof(x)))
#define NEWCPPC(c,x) ((x *) safemalloc((c)*sizeof(x)))
#define FREECPP(x) free(x)
#define FREECPPC(x) free(x)
#endif

/* pair gh_free with gh_scm2newstr, etc. */
#ifndef gh_free
#define gh_free(x) free(x)
#endif

#ifndef DEBUG_C_ALLOC
#define NEW(x) ((x *) safemalloc(sizeof(x)))
#define NEWC(c,x) ((x *) safemalloc((c)*sizeof(x)))
#define FREE(x) free(x)
#define FREEC(x) free(x)
#else
#define NEW(x) ((x *) \
  ({ void *_p = safemalloc(sizeof(x)); \
     fprintf(stderr,"NEW : %s: %d -- %x (%d)\n",__FILE__,__LINE__,_p,sizeof(x)); \
     _p; }))
#define NEWC(c,x) ((x *) \
  ({ void *_p = safemalloc((c)*sizeof(x)); \
     fprintf(stderr,"NEWC : %s: %d -- %x (%d,%d)\n",__FILE__,__LINE__,_p,c,sizeof(x)); \
     _p; }))
#define FREE(x) ({ fprintf(stderr,"FREE: %s: %d -- %x\n",__FILE__,__LINE__,x); \
                   free(x); })
#define FREEC(x) ({ fprintf(stderr,"FREEC: %s: %d -- %x\n",__FILE__,__LINE__,x); \
                    free(x); })
#endif

#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

