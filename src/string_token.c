/* This module is based on code distributed with fvwm with no
 * copyright notice. Thus, the code it is based on should be in
 * the public domain.
 *
 * It may be used or distributed under either the FVWM license 
 * (see COPYING.fvwm) or the GNU General Public License (see COPYING.GPL and
 * the description below)
 * Copyright 1997, Maciej Stachowiak and Greg J Badros
 ****************************************************************************/
/*      Copyright (C) 1997, Maciej Stachowiak
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


#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "system.h"

/* FIXGJB: strcasecmp and strncasecmp should be autoconf tested for,
   and versions should be linked in if they don't exist */
#ifndef HAVE_STRCASECMP
int 
strcasecmp(char *s1, char *s2)
{
  int c1, c2;
  int n, n2;

  n = strlen(s1);
  n2 = strlen(s2);
  if (n != n2)
    return 1;

  for (;;) {
    c1 = *s1;
    c2 = *s2;
    if (!c1 || !c2)
      return (c1 - c2);
    if (isupper(c1))
      c1 = 'a' - 1 + (c1 & 31);
    if (isupper(c2))
      c2 = 'a' - 1 + (c2 & 31);
    if (c1 != c2)
      return (c1 - c2);
    n--, s1++, s2++;
  }
}
#endif /* !HAVE_STRCASECMP */

#ifndef HAVE_STRNCASECMP
int 
strncasecmp(char *s1, char *s2, int n)
{
  register int c1, c2;

  for (;;) {
    if (!n)
      return (0);
    c1 = *s1, c2 = *s2;
    if (!c1 || !c2)
      return (c1 - c2);
    if (isupper(c1))
      c1 = 'a' - 1 + (c1 & 31);
    if (isupper(c2))
      c2 = 'a' - 1 + (c2 & 31);
    if (c1 != c2)
      return (c1 - c2);
    n--, s1++, s2++;
  }
}
#endif /* !HAVE_STRNCASECMP */

/****************************************************************************
 * 
 * Copies a string into a new, malloc'ed string
 * Strips leading spaces and trailing spaces and new lines
 * FIXGJB: only used in menus.c now -- can be removed --gjb 11/28/97 
 ****************************************************************************/
char *
stripcpy(char *source)
{
  char *tmp, *ptr;
  int len;

  if (source == NULL)
    return NULL;

  while (isspace(*source))
    source++;
  len = strlen(source);
  tmp = source + len - 1;
  while (((isspace(*tmp)) || (*tmp == '\n')) && (tmp >= source)) {
    tmp--;
    len--;
  }
  ptr = safemalloc(len + 1);
  strncpy(ptr, source, len);
  ptr[len] = 0;
  return ptr;
}

/* IchIgnoreCaseInSz(sz,ch)
 * Return the offset of ch in sz, ignoring case
 */
int
IchIgnoreCaseInSz(const char *sz, char ch)
{
  int ich = 0;
  ch = tolower(ch);
  while (*sz && tolower(*sz++) != ch) ich++;
  if (*sz == '\0') ich = -1;
  return ich;
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */


