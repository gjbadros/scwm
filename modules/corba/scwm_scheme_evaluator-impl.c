/* $Id$
 * (C) 1999, 2000 By Greg J. Badros
 * This is just a dummy, but the CORBA bit seems to be working --09/05/99 gjb
 */

#include <stdio.h>
#include "guile-compat.h"
#include "arg_unused.h"

#include "scwm_scheme_evaluator.h"

/*** App-specific servant structures ***/
typedef struct {
  POA_scwm_scheme_evaluator servant;
  PortableServer_POA poa;

} impl_POA_scwm_scheme_evaluator;




/*** Implementation stub prototypes ***/
static void impl_scwm_scheme_evaluator__destroy(impl_POA_scwm_scheme_evaluator *servant,
                                                       CORBA_Environment *ev);
static void
impl_scwm_scheme_evaluator_evaluate_sexp(impl_POA_scwm_scheme_evaluator *servant,
                                         CORBA_char * expr,
                                         CORBA_char ** answer,
                                         CORBA_char ** output,
                                         CORBA_char ** error_output,
                                         CORBA_Environment *ev);




/*** epv structures ***/
static PortableServer_ServantBase__epv impl_scwm_scheme_evaluator_base_epv = {
  NULL, /* _private data */
  NULL, /* finalize routine */
  NULL, /* default_POA routine */
};
static POA_scwm_scheme_evaluator__epv impl_scwm_scheme_evaluator_epv = {
  NULL, /* _private */
  (gpointer)&impl_scwm_scheme_evaluator_evaluate_sexp,

};


/*** vepv structures ***/
static POA_scwm_scheme_evaluator__vepv impl_scwm_scheme_evaluator_vepv = {
  &impl_scwm_scheme_evaluator_base_epv,
  &impl_scwm_scheme_evaluator_epv,
};


/*** Stub implementations ***/
static scwm_scheme_evaluator impl_scwm_scheme_evaluator__create(PortableServer_POA poa, CORBA_Environment *ev)
{
  scwm_scheme_evaluator retval;
  impl_POA_scwm_scheme_evaluator *newservant;
  PortableServer_ObjectId *objid;

  newservant = g_new0(impl_POA_scwm_scheme_evaluator, 1);
  newservant->servant.vepv = &impl_scwm_scheme_evaluator_vepv;
  newservant->poa = poa;
  POA_scwm_scheme_evaluator__init((PortableServer_Servant)newservant, ev);
  objid = PortableServer_POA_activate_object(poa, newservant, ev);
  CORBA_free(objid);
  retval = PortableServer_POA_servant_to_reference(poa, newservant, ev);

  return retval;
}

static void
impl_scwm_scheme_evaluator__destroy(impl_POA_scwm_scheme_evaluator *servant, CORBA_Environment *ev)
{
  PortableServer_ObjectId *objid;

  objid = PortableServer_POA_servant_to_id(servant->poa, servant, ev);
  PortableServer_POA_deactivate_object(servant->poa, objid, ev);
  CORBA_free(objid);

  POA_scwm_scheme_evaluator__fini((PortableServer_Servant)servant, ev);
  g_free(servant);
}

static void
impl_scwm_scheme_evaluator_evaluate_sexp(impl_POA_scwm_scheme_evaluator *ARG_UNUSED(servant),
                                         CORBA_char * expr,
                                         CORBA_char ** answer,
                                         CORBA_char ** output,
                                         CORBA_char ** error_output,
                                         CORBA_Environment *ARG_UNUSED(ev))
{
#define FUNC_NAME "impl_scwm_scheme_evaluator_evaluate_sexp"
  SCM val, str_val;
  SCM o_port, e_port;
  SCM saved_def_e_port;
  char *szAnswer;
  char *szOutput;
  char *szErrorOutput;
  
  /* Temporarily redirect output and error to string ports. 
     Note that the port setting functions return the current previous
     port. */
  o_port = scm_set_current_output_port(make_output_strport(FUNC_NAME));
  e_port = scm_set_current_error_port(make_output_strport(FUNC_NAME));
  
  /* Workaround for a problem with older Guiles */
  saved_def_e_port = scm_def_errp;
  scm_def_errp = scm_current_error_port();
  
  /* Evaluate the request expression and free it. */
  val = scwm_safe_eval_str(expr);
  str_val = scm_strprint_obj(val);
  szAnswer = scm_to_locale_string(str_val);
  
  /* restore output and error ports; use returned o_port/e_port
     below for getting the strings back */
  o_port = scm_set_current_output_port(o_port);
  e_port = scm_set_current_error_port(e_port);
  scm_def_errp = saved_def_e_port;
  
  /* Retrieve output and errors */
  szOutput = scm_to_locale_string(scm_strport_to_string(o_port));
  szErrorOutput = scm_to_locale_string(scm_strport_to_string(e_port));
          
  *answer = CORBA_string_dup(szAnswer);
  *output = CORBA_string_dup(szOutput);
  *error_output = CORBA_string_dup(szErrorOutput);
  free(szAnswer);
  free(szOutput);
  free(szErrorOutput);
}
#undef FUNC_NAME
