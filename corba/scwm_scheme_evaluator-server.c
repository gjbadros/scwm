/* $Id$
 */

#include <orb/orbit.h>
#include <libgnorba/gnorba.h>
#include <gtk/gtk.h>
#include "scwm_scheme_evaluator.h"

#include <string.h>
#include <stdio.h>
#include "scwm_scheme_evaluator-impl.c"

/* so the callback can pack into the box. */
GtkWidget *box;

/* decleration to avoid compiler error.  Many of these functions would be 
 * in a header in a normal application. */
void scwm_scheme_evaluator_corba_gtk_init(int argc, char *argv[]);

CORBA_ORB orb = NULL;
CORBA_Environment ev;

/* The following few functions are all glue for using ORBit inside of GTK+.  
 * Setting up gdk_input_add's, handlers for incoming data and 
 * exceptions. */
static void orb_handle_connection(GIOPConnection *cnx, gint source, GdkInputCondition cond)
{
  switch(cond) {
  case GDK_INPUT_EXCEPTION:
    giop_main_handle_connection_exception(cnx);
    break;
  default:
    giop_main_handle_connection(cnx);
  }
}

/* more glue */

static void orb_add_connection(GIOPConnection *cnx)
{
  cnx->user_data = (gpointer)gtk_input_add_full(GIOP_CONNECTION_GET_FD(cnx),
                                                GDK_INPUT_READ|GDK_INPUT_EXCEPTION,
                                                (GdkInputFunction)orb_handle_connection,
                                                NULL, cnx, NULL);
}

static void orb_remove_connection(GIOPConnection *cnx)
{
  gtk_input_remove((guint)cnx->user_data);
  cnx->user_data = (gpointer)-1;
}


/* This takes the place of gtk_main(), although you can see it merely checks
 * that the corba intialization has been done.  All the work of handling connections
 * is now done in gtk_main() via the gdk_input_add() stuff. */
void
scwm_scheme_evaluator_corba_gtk_main (int argc, char *argv[])
{
  if(!orb)
    scwm_scheme_evaluator_corba_gtk_init(argc, argv);

  gtk_main();
}

/* Shutdown the orb interface, and shut down GTK */
void
scwm_scheme_evaluator_corba_gtk_main_quit(void)
{
  CORBA_ORB_shutdown(orb, CORBA_FALSE, &ev);

  gtk_main_quit();
}


#define NAME "scwm-evalauator"
#define VERSION "v1.0"


void Exception( CORBA_Environment* ev )
{
  switch( ev->_major )
    {
    case CORBA_SYSTEM_EXCEPTION:
      g_log("ScwmEvaluator", G_LOG_LEVEL_DEBUG, "CORBA system exception %s.\n",
	    CORBA_exception_id(ev));
      exit ( 1 );
    case CORBA_USER_EXCEPTION:
      g_log("ScwmEvaluator", G_LOG_LEVEL_DEBUG, "CORBA user exception: %s.\n",
	    CORBA_exception_id( ev ) );
      exit ( 1 );
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

  scwm_scheme_evaluator_object evaluator_object;
  gchar *objref;
  gchar *ior;

  CORBA_exception_init(&ev);

  orb = gnome_CORBA_init(NAME,VERSION,&argc, argv, GNORBA_INIT_SERVER_FUNC, &ev);

  root_poa = (PortableServer_POA) 
    CORBA_ORB_resolve_initial_references(orb, "RootPOA", &ev);
  Exception(&ev);

  evaluator_object = impl_scwm_scheme_evaluator_object__create(root_poa, &ev);
  Exception(&ev);

  pm = PortableServer_POA__get_the_POAManager(root_poa, &ev);
  Exception(&ev);
  
  PortableServer_POAManager_activate(pm, &ev);
  Exception(&ev);
  
  goad_server_register(NULL, evaluator_object, "scwm-scheme-evaluator", "object", &ev);

  ior = CORBA_ORB_object_to_string(orb, evaluator_object , &ev);
  
  /* print IOR address so the client can use it to connect to us. */
  g_print ("%s\n", ior);

  CORBA_free(ior);

  ORBit_custom_run_setup(orb, &ev);
}



/* This is the beginning of the GTK interface.  In a larger project,
 * this would likely be in a seperate file. 
 * Note that I'm not going to comment this part, as I'll assume you know
 * how GTK works */

gint delete_event(GtkWidget *widget, GdkEvent *event, gpointer data)
{
  scwm_scheme_evaluator_corba_gtk_main_quit();
  return (FALSE);
}

int main (int argc, char *argv[])
{
  GtkWidget *window;


  gtk_init (&argc, &argv);
  scwm_scheme_evaluator_corba_gtk_init(argc,argv);
    

  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_signal_connect (GTK_OBJECT (window), "delete_event",
                      GTK_SIGNAL_FUNC (delete_event), NULL);
  gtk_window_set_title (GTK_WINDOW (window), "Scwm Evaluator Server");
  gtk_container_border_width (GTK_CONTAINER (window), 10);
  gtk_widget_set_usize (GTK_WIDGET (window), 300, -1);

  /* box is global */
  box = gtk_vbox_new(FALSE, 0);
  gtk_container_add (GTK_CONTAINER (window), box);

  gtk_widget_show_all (window);
  
  scwm_scheme_evaluator_corba_gtk_main (argc,argv);
                        
  return 0;
}
