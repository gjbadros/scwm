/* $Id$ 
 * run-renamed.c
 * Copyright (C) 1999, 2000 By Ken Pizzini and Greg J. Badros
 * GPL'd
 */

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include <unistd.h> 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *r1bindex(char *p, int c)
{
  char *r = strrchr(p, c); 
  return r ? r+1 : p;
}

int main(int argc, char **argv) 
{ 
  if (argc < 1) exit(1);

  if (argc < 2) {
    fprintf(stderr, "USAGE: %s command-to-run [renamed-to args...]\n", argv[0]); 
    exit(1);
  }
  if (!strcmp(r1bindex(argv[0], '/'), "run-renamed")) { 
    setenv("RESOURCE_NAME",argv[1],1);
  } else {
    char *sz = (char *) malloc(strlen(argv[1]) + strlen(argv[0]) + 12);
    sprintf(sz,"rename_to=%s %s",argv[1],argv[0]);
    argv[1] = sz;
  }
  execvp(argv[0], argv+1);
  perror("run-renamed");
  exit(127); 
} 
