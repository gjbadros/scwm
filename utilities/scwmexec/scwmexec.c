#include "../libscwmexec/scwmexec.h"
#include <stdio.h>
#include <stdlib.h>

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

void 
main(int argc, char **argv)
{
  Window w;
  
  if (argc != 2)
    die("Usage: scwmexec expression\n");
  if (!init_display())
    die("Could not connect to server. Check your DISPLAY environment variable.\n");

  w=scwmexec_init(display);

  if (w==None)
    die ("Unable to establish scwmexec connection.\n");

  printf("%s\n",scwmexec_exec(display,w,argv[1]));

  XCloseDisplay (display);
  
  exit (0);
}




