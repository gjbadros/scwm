/* $Id$
 * (C) 1999 By Greg J. Badros
 * This is just a dummy, but the CORBA bit seems to be working --09/05/99 gjb
 */

#include "scwm_scheme_evaluator.h"
#include <stdio.h>

/*** App-specific servant structures ***/
typedef struct {
POA_scwm_scheme_evaluator_object servant;
PortableServer_POA poa;

} impl_POA_scwm_scheme_evaluator_object;




/*** Implementation stub prototypes ***/
static void impl_scwm_scheme_evaluator_object__destroy(impl_POA_scwm_scheme_evaluator_object *servant,
CORBA_Environment *ev);
static void
impl_scwm_scheme_evaluator_object_evaluate_sexp(impl_POA_scwm_scheme_evaluator_object *servant,
CORBA_char * expr,
CORBA_char ** answer,
CORBA_char ** output,
CORBA_char ** error_output,
CORBA_Environment *ev);




/*** epv structures ***/
static PortableServer_ServantBase__epv impl_scwm_scheme_evaluator_object_base_epv = {
NULL, /* _private data */
NULL, /* finalize routine */
NULL, /* default_POA routine */
};
static POA_scwm_scheme_evaluator_object__epv impl_scwm_scheme_evaluator_object_epv = {
NULL, /* _private */
(gpointer)&impl_scwm_scheme_evaluator_object_evaluate_sexp,

};


/*** vepv structures ***/
static POA_scwm_scheme_evaluator_object__vepv impl_scwm_scheme_evaluator_object_vepv = {
&impl_scwm_scheme_evaluator_object_base_epv,
&impl_scwm_scheme_evaluator_object_epv,
};


/*** Stub implementations ***/
static scwm_scheme_evaluator_object impl_scwm_scheme_evaluator_object__create(PortableServer_POA poa, CORBA_Environment *ev)
{
scwm_scheme_evaluator_object retval;
impl_POA_scwm_scheme_evaluator_object *newservant;
PortableServer_ObjectId *objid;

newservant = g_new0(impl_POA_scwm_scheme_evaluator_object, 1);
newservant->servant.vepv = &impl_scwm_scheme_evaluator_object_vepv;
newservant->poa = poa;
POA_scwm_scheme_evaluator_object__init((PortableServer_Servant)newservant, ev);
objid = PortableServer_POA_activate_object(poa, newservant, ev);
CORBA_free(objid);
retval = PortableServer_POA_servant_to_reference(poa, newservant, ev);

return retval;
}

static void
impl_scwm_scheme_evaluator_object__destroy(impl_POA_scwm_scheme_evaluator_object *servant, CORBA_Environment *ev)
{
PortableServer_ObjectId *objid;

objid = PortableServer_POA_servant_to_id(servant->poa, servant, ev);
PortableServer_POA_deactivate_object(servant->poa, objid, ev);
CORBA_free(objid);

POA_scwm_scheme_evaluator_object__fini((PortableServer_Servant)servant, ev);
g_free(servant);
}

static void
impl_scwm_scheme_evaluator_object_evaluate_sexp(impl_POA_scwm_scheme_evaluator_object *servant,
CORBA_char * expr,
CORBA_char ** answer,
CORBA_char ** output,
CORBA_char ** error_output,
CORBA_Environment *ev)
{
  fprintf(stdout,"%s",expr);
  *answer = CORBA_string_dup(expr);
  *output = CORBA_string_dup("Output string");
  *error_output = CORBA_string_dup("Error string");
}
