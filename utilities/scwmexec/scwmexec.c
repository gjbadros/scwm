/* $Id$
 * Copyright (C) 1997-2000, Maciej Stachowiak and Greg J. Badros
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

#include "../libscwmexec/scwmexec.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

Display *display;
char *dispName;

int
init_display()
{
  if (!(dispName = getenv("DISPLAY")))
    return 0;
  if (!(display = XOpenDisplay(dispName)))
    return 0;
  return 1;
}


void die(char *str)
{
  fputs(str,stderr);
  exit(-1);
}

int
main(int argc, char **argv)
{
  Window w;
  char *result, *output, *error;

  if (argc != 2)
    die("Usage: scwmexec EXPRESSION\n");
  if (!init_display())
    die("Could not connect to server. Check your DISPLAY environment variable.\n");

  w=scwmexec_init(display);

  if (w==None)
    die("Unable to establish scwmexec connection.\n");

  result = scwmexec_exec_full(display,w,argv[1],&output,&error);

  fputs(output, stdout);
  if (strlen(error)!=0) {
    fputs(error, stderr);
  } else {
    fputs(result, stdout);
  }

  if (result) XFree(result);
  if (error) XFree(error);
  if (output) XFree(output);

  XCloseDisplay (display);

  return 0;
}
