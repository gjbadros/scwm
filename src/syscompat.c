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
 */

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include <stdlib.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/types.h>
#if HAVE_UNAME
#include <sys/utsname.h>
#endif

#include "syscompat.h"


#ifndef HAVE_GETHOSTNAME
/* define gethostname() by using uname() */
int 
gethostname(char *client, size_t length)
{
#ifdef HAVE_UNAME
  struct utsname sysname;

  uname(&sysname);
  strncpy(client, sysname.nodename, length);
  return 1;
#else /* !HAVE_UNAME */
  *client = NULL;
  return -1;
#endif /* HAVE_UNAME */
}
#endif /* !HAVE_GETHOSTNAME */


#ifndef HAVE_USLEEP
int 
usleep(unsigned long n)
{
  struct timeval value;

  if (n <= 0)
    return;

  value.tv_usec = n % 1000000;
  value.tv_sec = n / 1000000;

  (void) select(1, 0, 0, 0, &value);
}
#endif /* !HAVE_USLEEP */

#ifndef HAVE_SETENV
int
setenv(const char *name, const char *value, int overwrite)
{
  int cch = strlen(name)+strlen(value)+2;
  int answer;
  char *szEval = malloc(cch*sizeof(char));
  sprintf(szEval,"%s=%s",name,value);
  answer = putenv(szEval);
  free(szEval);
  return answer;
}
#endif


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



/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

