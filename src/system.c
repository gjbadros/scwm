/* $Id$ */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/time.h>
#include <sys/types.h>

#if defined ___AIX || defined _AIX || defined __QNX__ || defined ___AIXV3 || defined AIXV3 || defined _SEQUENT_
#include <sys/select.h>
#endif

#include <config.h>

/* FIXMS looking at this file, it's clear we need to do autoconf tests
   for a lot of this stuff. */

#if HAVE_UNAME
#include <sys/utsname.h>
#endif

#ifndef HAVE_GETHOSTNAME
/* define gethostname() by using uname() */
int 
gethostname(char *client, size_t length)
{
  struct utsname sysname;

#ifdef HAVE_UNAME
  uname(&sysname);
  strncpy(client, sysname.nodename, length);
  return 1;
#else /* do not even have uname */
  *client = 0;
  return -1;
#endif 
}
#endif

int 
GetFdWidth(void)
{
  int system_says;
#ifdef HAVE_SYSCONF
  system_says = sysconf(_SC_OPEN_MAX);
#else
  system_says = getdtablesize();
#endif
  if (system_says > 2048)
    return 2048;
  return system_says;
}


/***********************************************************************
 *
 *  Procedure:
 *	safemalloc - mallocs specified space or exits if there's a 
 *		     problem
 *
 ***********************************************************************/
void *
safemalloc(int length)
{
  char *ptr;

  if (length <= 0)
    length = 1;

  ptr = malloc(length);
  if (ptr == NULL) {
    fprintf(stderr, "malloc of %d bytes failed. Exiting\n", length);
    exit(1);
  }
  return ptr;
}

/**************************************************************************
 * 
 * Sleep for n milliseconds using select
 *
 *************************************************************************/

void 
sleep_ms(int n)
{
  struct timeval value;

  if (n <= 0)
    return;

  value.tv_usec = n % 1000;
  value.tv_sec = n / 1000;

  (void) select(1, 0, 0, 0, &value);
}



