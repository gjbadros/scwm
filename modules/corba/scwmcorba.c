/* $Id$
 */

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include <orb/orbit.h>
#include <libgnorba/gnorba.h>
#include <gtk/gtk.h>
#include <guile/gh.h>
#include "guile-compat.h"
#include <string.h>
#include <stdio.h>

#include "scwm_msg.h"
#include "scwm-snarf.h"
#include "scwm-guile.h"
#include "callbacks.h"

#include "scwm_scheme_evaluator.h"
#include "scwm_scheme_evaluator-impl.c"

#define SCWM_CORBA_VERSION "0.01"


/* decleration to avoid compiler error.  Many of these functions would be 
 * in a header in a normal application. */
void scwm_scheme_evaluator_corba_gtk_init(int argc, char *argv[]);

static CORBA_ORB orb = NULL;
static CORBA_Environment ev;
static scwm_scheme_evaluator evaluator_object;
static SCM scmIOR;

/* Shutdown the orb interface, and shut down GTK */
void
scwm_scheme_evaluator_corba_gtk_main_quit(void)
{
  CORBA_ORB_shutdown(orb, CORBA_FALSE, &ev);
  gtk_main_quit();
}


#define NAME "scwm-evaluator"

void Exception( CORBA_Environment* ev )
{
  switch( ev->_major ) {
  case CORBA_SYSTEM_EXCEPTION:
    g_log(NAME, G_LOG_LEVEL_DEBUG, "CORBA system exception %s.\n",
          CORBA_exception_id(ev));
    break;
  case CORBA_USER_EXCEPTION:
    g_log(NAME, G_LOG_LEVEL_DEBUG, "CORBA user exception: %s.\n",
          CORBA_exception_id( ev ) );
    break;
  default:
    break;
  }
}


/* initialize the gtk handling of the corba services */
void
scwm_scheme_evaluator_corba_gtk_init(int argc, char *argv[])
{
  PortableServer_POA          root_poa;
  PortableServer_POAManager   pm;
  gchar *ior;

  CORBA_exception_init(&ev);

  orb = gnome_CORBA_init(NAME,SCWM_CORBA_VERSION,&argc, argv, 
                         GNORBA_INIT_SERVER_FUNC, &ev);

  root_poa = (PortableServer_POA) 
    CORBA_ORB_resolve_initial_references(orb, "RootPOA", &ev);
  Exception(&ev);
  
  evaluator_object = impl_scwm_scheme_evaluator__create(root_poa, &ev);
  Exception(&ev);
  
  pm = PortableServer_POA__get_the_POAManager(root_poa, &ev);
  Exception(&ev);
  
  PortableServer_POAManager_activate(pm, &ev);
  Exception(&ev);
  
  goad_server_register(NULL, evaluator_object, "scwm-scheme-evaluator", "object", &ev);
  
  ior = CORBA_ORB_object_to_string(orb, evaluator_object , &ev);

  scwm_defer_ints();
  scmIOR = gh_str02scm(ior);
  scm_permanent_object(scmIOR);
  scwm_allow_ints();
    
  ORBit_custom_run_setup(orb, &ev);
}

/* This is the beginning of the GTK interface.  In a larger project,
 * this would likely be in a seperate file. 
 * Note that I'm not going to comment this part, as I'll assume you know
 * how GTK works */

gint delete_event(GtkWidget *ARG_UNUSED(widget), GdkEvent *ARG_UNUSED(event), gpointer ARG_UNUSED(data))
{
  scwm_scheme_evaluator_corba_gtk_main_quit();
  return (FALSE);
}


SCWM_PROC(corba_evaluator_ior, "corba-evaluator-ior", 0, 0, 0,
          (),
"Returns the \"IOR\" string for the scwm-scheme-evaluator servant.")
#define FUNC_NAME s_corba_evaluator_ior
{
  return scmIOR;
}
#undef FUNC_NAME


static void
init_scwmcorba()
{
  int argc = 1;
  char *argv[1] = { NAME };
  
  scwm_scheme_evaluator_corba_gtk_init(argc,argv);

#ifndef SCM_MAGIC_SNARFER
 #include "scwmcorba.x"
#endif
}

void scm_init_app_scwm_scwmcorba_module()
{
  scm_register_module_xxx("app scwm scwmcorba", init_scwmcorba);
}

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
