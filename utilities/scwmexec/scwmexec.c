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
  char *result, *output, *error;
  
  if (argc != 2)
    die("Usage: scwmexec expression\n");
  if (!init_display())
    die("Could not connect to server. Check your DISPLAY environment variable.\n");

  w=scwmexec_init(display);

  if (w==None)
    die ("Unable to establish scwmexec connection.\n");

  result=scwmexec_exec_full(display,w,argv[1],&output,&error);
  printf(output);
  if (strlen(error)!=0) {
    fprintf(stderr,error);
  } else {
    fprintf(stdout,result);
  }
  
  XFree(result);
  XFree(error);
  XFree(output);


  XCloseDisplay (display);
  
  exit (0);
}




