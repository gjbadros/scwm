/* $Id$
 *
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

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/time.h>
#include <sys/types.h>
#include <assert.h>

#include "system.h"

#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif

/*
 * safemalloc - mallocs specified space or exits if there's a problem
 *
 * You should use NEW and FREE, or NEWC (like calloc) and FREEC
 * see system.h for the macro definitions
 */
void *
safemalloc(int length)
{
  void *ptr;

  assert(length >= 1);

  ptr = calloc(1,length);
  if (ptr == NULL) {
    fprintf(stderr, "safemalloc: calloc of %d bytes failed. Exiting\n", length);
    exit(1);
  }
  return ptr;
}

/*
 * xgetcwd - like getcwd, but reserves new space if buffer is NULL
 *	     size is ignored in that case
 */
char *
xgetcwd(char *buffer, int size)
{
  char *value;

  if (!buffer) {
    size = 100;
    buffer = NEWC(size, char);
  }

  while (1) {
    value = getcwd (buffer, size);
    if (value)
      return buffer;
    size *= 2;
    FREEC(buffer);
    buffer = NEWC(size, char);
  }
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

