/* $Id$
 * (C) 1999 By Greg J. Badros
 * scwm-corba-exec.c
 */

#include <gtk/gtk.h>
#include <orb/orbit.h>
#include <stdio.h>

#include "scwm_scheme_evaluator.h"

scwm_scheme_evaluator_object client;
CORBA_Environment ev;

#define NAME "scwm-corba-exec"
#define VERSION "v1.0"

int
main (int argc, char *argv[])
{
  CORBA_ORB orb;

  CORBA_exception_init(&ev);

  orb = gnome_CORBA_init(NAME,VERSION,&argc, argv, 0, &ev);

  if (argc < 2) {
    fprintf(stderr,"Need a binding ID (IOR address) as argv[1]\n");
    return 1;
  }

  /* The client needs the IOR address to find the server.  In this case it's
   * given on the command line as argv[1].  We print this address in the server
   * when it initializes. */
  client = CORBA_ORB_string_to_object(orb, argv[1], &ev);
  if (!client) {
    fprintf(stderr,"Cannot bind to %s\n", argv[1]);
    return 1;
  }

  if (argc < 3) {
    fprintf(stderr,"Give expression as second argument\n");
    return 1;
  }

  { /* scope */
    char *szAnswer;
    char *szOut;
    char *szError;
    scwm_scheme_evaluator_object_evaluate_sexp(client, 
                                               argv[2],&szAnswer,&szOut,&szError,
                                               &ev);
    printf("szAnswer = %s\nszOut=%s\nszError=%s\n",
           szAnswer,szOut,szError);
  }
  
  CORBA_Object_release(client, &ev);

  return 0;
}
