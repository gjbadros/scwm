#include <unistd.h>
#include <sys/time.h>
#include <sys/types.h>
#include <config.h>
#include "syscompat.h"

#if HAVE_UNAME
#include <sys/utsname.h>
#endif

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
void 
usleep(unsigned long n)
{
  struct timeval value;

  if (n <= 0)
    return;

  value.tv_usec = n % 1000;
  value.tv_sec = n / 1000;

  (void) select(1, 0, 0, 0, &value);
}
#endif /* !HAVE_USLEEP */

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


