/* $Id$
 * (C) 1999, 2000 By Greg J. Badros
 * scwm-corba-exec.c
 */

#include <gtk/gtk.h>
#include <orb/orbit.h>
#include <libgnorba/gnorba.h>
#include <stdio.h>

#include "scwm_scheme_evaluator.h"

scwm_scheme_evaluator client;
CORBA_Environment ev;

#define CORBA_NAME "scwm-corba-exec"
#define CORBA_EXEC_VERSION "v1.0"

void Exception( CORBA_Environment* ev )
{
  switch( ev->_major ) {
  case CORBA_SYSTEM_EXCEPTION:
    g_log(CORBA_NAME, G_LOG_LEVEL_DEBUG, "CORBA system exception %s.\n",
          CORBA_exception_id(ev));
    exit ( 1 );
  case CORBA_USER_EXCEPTION:
    g_log(CORBA_NAME, G_LOG_LEVEL_DEBUG, "CORBA user exception: %s.\n",
          CORBA_exception_id( ev ) );
    exit ( 1 );
  default:
    break;
  }
}


int
main (int argc, char *argv[])
{
  CORBA_ORB orb;

  CORBA_exception_init(&ev);
  Exception(&ev);

  orb = gnome_CORBA_init(CORBA_NAME,CORBA_EXEC_VERSION,&argc, argv, 0, &ev);
  Exception(&ev);

  if (argc < 2) {
    fprintf(stderr,"Need a binding ID (IOR address) as first argument\n");
    return 1;
  }

  if (argc < 3) {
    fprintf(stderr,"Give expression as second argument\n");
    return 1;
  }

  /* The client needs the IOR address to find the server.  In this case it's
   * given on the command line as argv[1].  We print this address in the server
   * when it initializes. */
  client = CORBA_ORB_string_to_object(orb, argv[1], &ev);
  Exception(&ev);

  if (!client) {
    fprintf(stderr,"Cannot bind to %s\n", argv[1]);
    return 1;
  }

  { /* scope */
    char *szAnswer;
    char *szOut;
    char *szError;
    scwm_scheme_evaluator_evaluate_sexp(client, 
                                        argv[2],&szAnswer,&szOut,&szError,
                                        &ev);
    Exception(&ev);
    printf("szAnswer = %s\nszOut=%s\nszError=%s\n",
           szAnswer,szOut,szError);
  }
  
  CORBA_Object_release(client, &ev);
  Exception(&ev);

  return 0;
}
