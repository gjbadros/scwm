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
 */

#ifndef SYSCOMPAT_H
#define SYSCOMPAT_H

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include <sys/wait.h>
#include <unistd.h>

#ifdef HAVE_WAITPID
#define ReapChildren()  while ((waitpid(-1, NULL, WNOHANG)) > 0)
#else /* !HAVE_WAITPID */
#define ReapChildren()  while ((wait3(NULL, WNOHANG, NULL)) > 0)
#endif /* !HAVE_WAITPID */

#ifndef HAVE_GETHOSTNAME
int gethostname(char *client, size_t length);
#endif

#ifndef HAVE_USLEEP
int usleep(unsigned long n);
#endif

#ifndef HAVE_STRCASECMP
int  strcasecmp(char *s1, char *s2);
#endif /* !HAVE_STRCASECMP */

#ifndef HAVE_STRNCASECMP
int  strncasecmp(char *s1, char *s2, int n);
#endif /* !HAVE_STRNCASECMP */


#ifndef HAVE_SETLINEBUF
  #ifdef HAVE_SETVBUF
    #define setlinebuf(stream) setvbuf((stream), NULL, _IOLBF, BUFSIZ)
  #else /* HAVE_SETVBUF */
    #define setlinebuf(stream) setbuf((stream), NULL)
  #endif /* HAVE_SETVBUF */
#endif /* HAVE_SETLINEBUF */

#endif /* SYSCOMPAT_H */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

