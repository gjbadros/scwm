/* $Id$ */
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
#ifdef HAVE_USLEEP
  usleep((unsigned long) n);
#else /* !HAVE_USLEEP */
  struct timeval value;

  if (n <= 0)
    return;

  value.tv_usec = n % 1000;
  value.tv_sec = n / 1000;

  (void) select(1, 0, 0, 0, &value);
#endif /* HAVE_USLEEP */
}



/****************************************************************************
 *
 * Find the specified file (or file.gz) somewhere along the given path.
 *
 * There is a possible race condition here:  We check the file and later
 * do something with it.  By then, the file might not be accessible.
 * Oh well.
 * FIXGJB: race conditions here!
 * FIXGJB: This function is not used --gjb 12/04/97 
 * Note that the returned char * must be free-ed
 ****************************************************************************/
char *
findFile(char *szName, char *szPathlist, int type)
{
  char *path = NULL;
  char *dir_end = NULL;
  int cchName = 0;
  int cchPathlist = 0;

  if (szName != NULL)
    cchName = strlen(szName);

  if (szPathlist != NULL)
    cchPathlist = strlen(szPathlist);

  path = safemalloc(cchName + cchPathlist + 10);
  *path = '\0';

  if (*szName == '/') {
    /* No search if szName begins with a slash */
    strcpy(path, szName);
    return path;
  }
  if ((szPathlist == NULL) || (*szPathlist == '\0')) {
    /* No search if szPathlist is empty */
    strcpy(path, szName);
    return path;
  }
  /* Search each element of the szPathlist for the szName file */
  while ((szPathlist) && (*szPathlist)) {
    dir_end = strchr(szPathlist, ':');
    if (dir_end != NULL) {
      strncpy(path, szPathlist, dir_end - szPathlist);
      path[dir_end - szPathlist] = 0;
    } else
      strcpy(path, szPathlist);

    strcat(path, "/");
    strcat(path, szName);
    if (access(path, type) == 0)
      return path;
    strcat(path, ".gz");
    if (access(path, type) == 0)
      return path;

    /* Point to next element of the path */
    if (dir_end == NULL)
      szPathlist = NULL;
    else
      szPathlist = dir_end + 1;
  }
  /* Hmm, couldn't find the file.  Return NULL */
  free(path);
  return NULL;
}

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
