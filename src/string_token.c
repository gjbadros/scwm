#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "system.h"

/* FIXGJB: strcasecmp and strncasecmp should be autoconf tested for,
   and versions should be linked in if they don't exist */
#ifdef MISSING_STRCASECMP
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
#endif

/****************************************************************************
 * 
 * Copies a string into a new, malloc'ed string
 * Strips leading spaces and trailing spaces and new lines
 *
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
