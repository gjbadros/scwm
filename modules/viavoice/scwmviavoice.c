/* $Id$
 * (C) 1999 Greg J. Badros
 * Derived from gramsam.c, (C)1999 IBM
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <X11/Xlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include "scwm-snarf.h"
#include "scwm_msg.h"
#include "scwmpaths.h"
#include "callbacks.h"
#include "arg_unused.h"
#include "validate.h"

#include <guile/gh.h>
#include "guile-compat.h"
#include <gtk/gtk.h>


/* All of the speech-related things (prototypes,      */
/* data types, etc)                                   */
#include <smapi.h>

/* A macro to check the return code from various Sm   */
/* calls (makes the code below look cleaner)          */
#define CheckSmRC(fn)                                  \
do {                                                   \
  int rc;                                              \
                                                       \
  SmGetRc ( reply, & rc );                             \
                                                       \
  scwm_msg(INFO,"ViaVoice","%s: rc = %d", fn, rc);     \
                                                       \
  if ( rc != SM_RC_OK ) return ( SM_RC_OK );           \
} while (0)

/* A couple of important values that get passed about */
typedef struct
{
  void * top_level;
  int x_input_id;
} MyClientData;

/* A couple of callbacks - for the exit and mic buttons     */
static void ConnectStuff ( MyClientData * );


static Bool    fMicOn = False;
static Bool    fConnected = False;
static Bool    fOpened = False;

/* Use whatever default speech user info there is..         */
static char   userid   [ 80 ] = SM_USE_CURRENT;
static char   enrollid [ 80 ] = SM_USE_CURRENT;
static char   taskid   [ 80 ] = SM_USE_CURRENT;



SCWM_HOOK(vv_recognition_hook,"vv-recognition-hook", 3,
"This hook is invoked when ViaVoice recognizes a phrase. 
Called with 3 arguments: (was-accepted? phrase-string annotations-vector)");


static
int ViaVoiceNotifier(int socket_handle, int (*recv_fn)(), void *recv_data,
                     void *client_data)
{
  int *x_input_id = (int *) client_data;

  if (recv_fn == NULL) {
    /* drop the input ID added below */
    gdk_input_remove( *x_input_id );
    return SM_RC_OK;
  }
  *x_input_id = gdk_input_add(socket_handle, 1,
                              (GdkInputFunction) recv_fn, recv_data);
  return SM_RC_OK;
}


/* These callbacks handle the various messages that the speech       */
/* engine might be sending back                                      */
static SmHandler ConnectCB     ( SM_MSG reply, caddr_t client, caddr_t call_data );
SmHandler DisconnectCB  ( SM_MSG reply, caddr_t client, caddr_t call_data );
SmHandler SetCB         ( SM_MSG reply, caddr_t client, caddr_t call_data );
SmHandler MicOnCB       ( SM_MSG reply, caddr_t client, caddr_t call_data );
SmHandler MicOffCB      ( SM_MSG reply, caddr_t client, caddr_t call_data );
SmHandler DefineGrammarCB ( SM_MSG reply, caddr_t client, caddr_t call_data );
SmHandler DefineVocabCB ( SM_MSG reply, caddr_t client, caddr_t call_data );
SmHandler EnableVocabCB ( SM_MSG reply, caddr_t client, caddr_t call_data );
SmHandler GetNextWordCB ( SM_MSG reply, caddr_t client, caddr_t call_data );
SmHandler RecoWordCB    ( SM_MSG reply, caddr_t client, caddr_t call_data );
SmHandler RecoPhraseCB  ( SM_MSG reply, caddr_t client, caddr_t call_data );
SmHandler UtteranceCB   ( SM_MSG reply, caddr_t client, caddr_t call_data );
SmHandler FocusGrantedCB   ( SM_MSG reply, caddr_t client, caddr_t call_data );

SCM ConnectCB_proc = SCM_BOOL_F;
SCM DisconnectCB_proc = SCM_BOOL_F;
SCM SetCB_proc = SCM_BOOL_F;
SCM MicOnCB_proc = SCM_BOOL_F;
SCM MicOffCB_proc = SCM_BOOL_F;
SCM DefineGrammarCB_proc = SCM_BOOL_F;
SCM DefineVocabCB_proc = SCM_BOOL_F;
SCM EnableVocabCB_proc = SCM_BOOL_F;
SCM GetNextWordCB_proc = SCM_BOOL_F;
SCM RecoWordCB_proc = SCM_BOOL_F;
SCM RecoPhraseCB_proc = SCM_BOOL_F;
SCM UtteranceCB_proc = SCM_BOOL_F;
SCM FocusGrantedCB_proc = SCM_BOOL_F;


static
void
vv_scwm_cb(SCM proc, SM_MSG reply, caddr_t ARG_UNUSED(client), caddr_t ARG_UNUSED(calldata) )
{
  if (gh_procedure_p(proc)) {
    scwm_safe_apply(proc, gh_list(gh_int2scm((int) reply), SCM_UNDEFINED));
  }
}

static
int DisconnectStuff( MyClientData * ARG_UNUSED(MyData) )
{
  int smc = 0;
  SmArg smargs[30];
  SmSetArg ( smargs [ smc ], SmNuserId,       userid    );  smc++;
  SmSetArg ( smargs [ smc ], SmNenrollId,     enrollid  );  smc++;
  SmSetArg ( smargs [ smc ], SmNtask,         taskid    );  smc++;
  SmSetArg ( smargs [ smc ], SmNrecognize,    True      );  smc++;
  SmSetArg ( smargs [ smc ], SmNoverrideLock, True      );  smc++;

  return SmDisconnect ( smc, smargs, SmAsynchronous );
}


static
void ConnectStuff ( MyClientData * MyData )
{
  int        rc;
  int        smc;
  SmArg      smargs [ 30 ];
  int input_id;

  scwm_msg(INFO,"ViaVoice","ConnectStuff invoked");

  if ( !fOpened ) {
    smc = 0;
    SmSetArg ( smargs [ smc ], SmNapplicationName,      "Scwm" );  smc++;
    SmSetArg ( smargs [ smc ], SmNexternalNotifier,     ViaVoiceNotifier ); smc++;
    SmSetArg ( smargs [ smc ], SmNexternalNotifierData, &input_id );     smc++; 
   

    /* The call to SmOpen initializes any data that's inside of libSm  */
    rc = SmOpen ( smc, smargs );

    if ( rc != SM_RC_OK ) {
      scwm_msg(INFO,"ViaVoice","SmOpen() failed, rc = %d", rc );
      return;
    }
    
    /* Add the callbacks to catch the messages coming back from the    
       reco engine */
    SmAddCallback ( SmNconnectCallback,             ConnectCB,       NULL );
    SmAddCallback ( SmNdisconnectCallback,          DisconnectCB,    NULL );
    SmAddCallback ( SmNsetCallback,                 SetCB,           NULL );
    SmAddCallback ( SmNmicOnCallback,               MicOnCB,         NULL );
    SmAddCallback ( SmNmicOffCallback,              MicOffCB,        NULL );
    SmAddCallback ( SmNenableVocabCallback,         EnableVocabCB,   NULL );
    SmAddCallback ( SmNdefineVocabCallback,         DefineVocabCB,   NULL );
    SmAddCallback ( SmNdefineGrammarCallback,       DefineGrammarCB, NULL );
    SmAddCallback ( SmNrecognizeNextWordCallback,   GetNextWordCB,   NULL );
    SmAddCallback ( SmNrecognizedWordCallback,      RecoWordCB,      NULL );
    SmAddCallback ( SmNrecognizedPhraseCallback,    RecoPhraseCB,    (void *)MyData);
    SmAddCallback ( SmNutteranceCompletedCallback,  UtteranceCB,     NULL );
    SmAddCallback ( SmNfocusGrantedCallback,  FocusGrantedCB,     NULL );

    fOpened = True;
  }

  /*-----------------------------------------------------------------*/
  /* Now connect to the engine (asynchronously, which means that     */
  /* the ConnectCB will get the results)                             */
  /*-----------------------------------------------------------------*/
  smc = 0;
  SmSetArg ( smargs [ smc ], SmNuserId,       userid    );  smc++;
  SmSetArg ( smargs [ smc ], SmNenrollId,     enrollid  );  smc++;
  SmSetArg ( smargs [ smc ], SmNtask,         taskid    );  smc++;
  SmSetArg ( smargs [ smc ], SmNrecognize,    True      );  smc++;
  SmSetArg ( smargs [ smc ], SmNoverrideLock, True      );  smc++;

  rc = SmConnect ( smc, smargs, SmAsynchronous );

  scwm_msg(INFO,"ViaVoice","ConnectStuff: SmConnect() rc = %d", rc);
}


static
SmHandler ConnectCB ( SM_MSG reply, caddr_t client, caddr_t calldata )
{
  CheckSmRC("ConnectCB");

  fConnected = True;
  
  vv_scwm_cb(ConnectCB_proc, reply, client, calldata);
  return ( SM_RC_OK );
}


SmHandler DisconnectCB  ( SM_MSG reply, caddr_t client,
                          caddr_t calldata )
{
  fConnected = False;

  vv_scwm_cb(DisconnectCB_proc, reply, client, calldata);
  return ( SM_RC_OK );
}


SmHandler SetCB  ( SM_MSG reply, caddr_t client,
                   caddr_t calldata )
{
  vv_scwm_cb(SetCB_proc, reply, client, calldata);
  return ( SM_RC_OK );
}


SmHandler MicOnCB ( SM_MSG reply, caddr_t client, 
                    caddr_t calldata )
{
  CheckSmRC("MicOnCB");

  fMicOn = True;

  /* VERY IMPORTANT - this tells the recognizer to 'go' (ie. start     */
  /* capturing the audio and processing it)                            */
  SmRecognizeNextWord ( SmAsynchronous );

  vv_scwm_cb(MicOnCB_proc, reply, client, calldata);

  return ( SM_RC_OK );
}


SmHandler MicOffCB ( SM_MSG reply, caddr_t client, caddr_t calldata )
{
  CheckSmRC("MicOffCB");

  fMicOn = False;

  vv_scwm_cb(MicOffCB_proc, reply, client, calldata);

  return ( SM_RC_OK );
}


SmHandler EnableVocabCB ( SM_MSG reply, caddr_t client, caddr_t calldata )
{
  CheckSmRC("EnableVocabCB");

  vv_scwm_cb(EnableVocabCB_proc, reply, client, calldata);
  return ( SM_RC_OK );
}


SmHandler DefineVocabCB ( SM_MSG reply, caddr_t client, 
                          caddr_t calldata )
{
  char          * vocab;
  int             rc;
  int             i;
  SM_VOCWORD    * missing;
  unsigned long   num_missing;

  CheckSmRC("DefineVocabCB");

  SmGetVocabName ( reply, & vocab );

  scwm_msg(INFO,"ViaVoice","DefineVocabCB: vocab = %s", vocab );

  /* Check to see if any of the words from the vocabulary are missing  */
  /* from the recognizers pool(s)                                      */
  rc = SmGetVocWords ( reply, & num_missing, & missing );

  scwm_msg(INFO,"ViaVoice", "DefineVocabCB: There are %ld words missing", num_missing );

  for ( i = 0 ; i < ( int ) num_missing; i++ ) {
    scwm_msg(INFO,"ViaVoice", "DefineVocabCB: word [ %d ] = '%s'",
             i, missing [ i ].spelling );
  }

  vv_scwm_cb(DefineVocabCB_proc, reply, client, calldata);

  /* Enable the vocabulary (tells the recognizer to listen for words */
  /* from it) */
  SmEnableVocab ( vocab, SmAsynchronous );

  return ( SM_RC_OK );
}


SmHandler DefineGrammarCB ( SM_MSG reply, caddr_t ARG_UNUSED(client), 
                            caddr_t ARG_UNUSED(caller) )
{
  int             rc;
  int             i;
  char            buffer [ 512 ];
  char          * vocab_name;
  SM_VOCWORD    * missing;
  unsigned long   num_missing;


  /*--------------------------------------------------------------------*/
  /* One of the reasons a grammar can fail to be defined is if the fsg  */
  /* has words that we don't have pronunciations for.  If this happens, */
  /* the missing words are included in the reply structure.             */
  /*--------------------------------------------------------------------*/
  SmGetRc ( reply, & rc );

  if ( rc == SM_RC_NOT_INVOCAB ) {
    rc = SmGetVocWords ( reply, & num_missing, & missing );

    CheckSmRC("SmGetVocWords");

    if ( num_missing ) {
      sprintf ( buffer, "Missing %ld word(s) from '%s': ",
                num_missing, vocab_name );

      for ( i = 0 ; i < ( int ) num_missing ; i++ )       {
        strcat ( buffer, missing [ i ].spelling );
        strcat ( buffer, " " );
      }

      scwm_msg(INFO,"ViaVoice","%s",buffer);
    }

    return ( -1 );
  }

  /* Get the vocabulary name out of the reply structure so we will know */
  /* what grammar (vocabulary) to enable                                */
  rc = SmGetVocabName ( reply, & vocab_name );

  CheckSmRC("SmGetVocabName");

  scwm_msg(INFO,"ViaVoice", "DefineGrammarCB: Defined '%s'", vocab_name );

  rc = SmEnableVocab ( vocab_name, SmAsynchronous );

  CheckSmRC("SmGetEnableVocab");

  return ( 0 );
}

SmHandler GetNextWordCB ( SM_MSG reply, caddr_t ARG_UNUSED(client), 
                          caddr_t ARG_UNUSED(calldata) )
{
  CheckSmRC("GetNextWordCB");

  /* This gets called whenever SmRecognizeNextWord() is called         */
  return ( SM_RC_OK );
}


SCM
ScmAnnotationsFromRgAnnotations(int n, SM_ANNOTATION rgannot[])
{
  SCM answer = gh_make_vector(gh_int2scm(n), SCM_BOOL_F);
  int i = 0;
  for (; i < n ; ++i ) {
    SCM value = SCM_BOOL_F;
    switch (rgannot[i].type) {
    case SM_ANNOTATION_NUMERIC:
      value = gh_long2scm(rgannot[i].annodata.numeric);
      break;
    case SM_ANNOTATION_STRING:
      value = gh_str02scm(rgannot[i].annodata.string);
      break;
    case SM_ANNOTATION_OTHER:
      /* GJB:FIXME:: maybe use 'other */
      break;
    }
    gh_vector_set_x(answer, gh_int2scm(i), value);
  }
  return answer;
}

SmHandler RecoWordCB ( SM_MSG reply, caddr_t ARG_UNUSED(client), 
                       caddr_t ARG_UNUSED(calldata) )
{
  int             rc;
  int             i;
  unsigned long   num_firm;
  SM_WORD       * firm;

  CheckSmRC("RecoWordCB");

  /* SOMETIMES, the reco engine will have no idea what was said, and 
     will call the recognized word callback - an application had best
     have a RecognizedWord callback setup to tell the engine to go
     again (SmRecognizeNextWord) if this happens.. */

  rc = SmGetFirmWords ( reply, & num_firm, & firm );

  for ( i = 0 ; i < ( int ) num_firm; i++ ) {
    scwm_msg(INFO,"ViaVoice", "RecoWordCB: firm[%d] = '%s' ('%s')\n",
             i, firm [ i ].spelling, firm [ i ].vocab );
  }

  /* Tell the recognizer to 'go' again.  It stops so that if we wanted */
  /* to, we could change vocabs...                                     */
  rc = SmRecognizeNextWord ( SmAsynchronous );

  return ( SM_RC_OK );
}


SmHandler RecoPhraseCB ( SM_MSG reply, 
                         caddr_t ARG_UNUSED(client), 
                         caddr_t ARG_UNUSED(caller) )
{
  int       rc;
  SM_WORD * firm;
  unsigned  long num_firm;
  int       i;
  char      phrase [ 255 ];
  unsigned  long flags;
  SM_ANNOTATION * annots;
  unsigned  long num_annots;

  SmGetRc ( reply, & rc );

  /* Get the phrase that was recognized out of the reply structure.    */
  if ( rc != SM_RC_OK ) {
    scwm_msg(INFO,"ViaVoice", "RecoPhraseCB: SmGetRc rc = %d\n", rc );

    rc = SmRecognizeNextWord ( SmAsynchronous );

    return ( rc );
  }


  /* Get the 'phrase state' - this tells (among other things) whether */
  /* the phrase was accepted or rejected by the engine                */
  rc = SmGetPhraseState ( reply, & flags );

  if ( rc != SM_RC_OK ) {
    scwm_msg(INFO,"ViaVoice", "RecoPhraseCB: SmGetPhraseState rc = %d\n", rc );
    rc = SmRecognizeNextWord ( SmAsynchronous );

    return ( rc );
  }

  /* As with a recognized command, extract the recognized words       */
  /* (the 'phrase') from the reply structure                          */
  rc = SmGetFirmWords ( reply, & num_firm, & firm );

  if ( rc != SM_RC_OK ) {
    scwm_msg(INFO,"ViaVoice", "RecoPhraseCB: SmGetFirmWords rc = %d\n", rc );
    rc = SmRecognizeNextWord ( SmAsynchronous );

    return ( rc );
  }


  /* Check the phrase state to see if it was accepted or not..        */
  if ( flags & SM_PHRASE_ACCEPTED ) {
    strcpy ( phrase, "Acc: " );
  } else {
    strcpy ( phrase, "Rej: " );
  }


  /*------------------------------------------------------------------*/
  /* And go through them, making a string that has the complete       */
  /* phrase in it so that it can be displayed                         */
  /*------------------------------------------------------------------*/
  for ( i = 0 ; i < num_firm ; i++ ) {
    strcat ( phrase, firm [ i ].spelling );
    strcat ( phrase, " " );
  }

  scwm_msg(INFO,"ViaVoice", "%s", phrase);

  /* Get the annotation that's tied to the last part of the phrase 
     (it contains the amount we want to move the window)..  If it's 
     not there or is the wrong type, then the grammar has probably
     changed since this part of the code was written.. */
  rc = SmGetAnnotations ( reply, & num_annots, & annots );

  { /* scope */
    SCM annotations = ScmAnnotationsFromRgAnnotations(num_annots, annots);

    scwm_run_hook(vv_recognition_hook,
                  gh_list(gh_bool2scm( (flags & SM_PHRASE_ACCEPTED ) ),
                          gh_str02scm(phrase),
                          annotations,
                          SCM_UNDEFINED));
  }
  
  /* Tell the engine to 'go' again */
  rc = SmRecognizeNextWord ( SmAsynchronous );

  return ( rc );
}


SmHandler UtteranceCB ( SM_MSG reply,
                        caddr_t client, 
                        caddr_t calldata )
{
  scwm_msg(INFO,"ViaVoice", "UtteranceCB\n" );

  vv_scwm_cb(UtteranceCB_proc, reply, client, calldata);

  /* The engine has turned the mic off and processed all of the audio  */
  return ( SM_RC_OK );
}

SmHandler FocusGrantedCB( SM_MSG reply,
                          caddr_t client, 
                          caddr_t calldata )
{
  scwm_msg(INFO,"ViaVoice", "FocusGrantedCB\n" );

  vv_scwm_cb(FocusGrantedCB_proc, reply, client, calldata);
  return ( SM_RC_OK );
}

SCWM_PROC(vv_connect,"vv-connect",0,1,0,
          (SCM proc),
"Connect to the ViaVoice speech recognizer, calling PROC after connected.
See <filename>modules/viavoice/README</filename> for details. See also `vv-initialize'.")
#define FUNC_NAME s_vv_connect 
{
  VALIDATE_ARG_PROC_USE_F(1,proc);
  ConnectCB_proc = proc;
  /* Connect to the speech recognizer */
  ConnectStuff ( NULL );
  return SCM_UNDEFINED;
}
#undef FUNC_NAME

SCWM_PROC(vv_disconnect,"vv-disconnect",0,1,0,
          (SCM proc),
"Disconnect from the ViaVoice speech recognizer, calling PROC after disconnected.")
#define FUNC_NAME s_vv_disconnect 
{
  VALIDATE_ARG_PROC_USE_F(1,proc);
  DisconnectCB_proc = proc;
  return gh_int2scm(DisconnectStuff(NULL));
}
#undef FUNC_NAME


SCWM_PROC(vv_close,"vv-close",0,0,0,
          (),
"Close connect to the ViaVoice speech recognizer.")
#define FUNC_NAME s_vv_close
{
  fOpened = False;
  return gh_int2scm(SmClose());
}
#undef FUNC_NAME



SCWM_PROC(vv_connected_p,"vv-connected?",0,0,0,
          (),
"Return #t if we are connected to the ViaVoice speech recognizer.
Returns #f if we are not connected.")
#define FUNC_NAME s_vv_connected_p
{
  return gh_bool2scm(fConnected);
}
#undef FUNC_NAME


SCWM_PROC(vv_define_grammar,"vv-define-grammar",2,1,0,
          (SCM name, SCM grammar_file, SCM proc),
"Use GRAMMAR-FILE as the ViaVoice grammar and give it name NAME. 
Returns #f if not connected, otherwise returns the return
code from DoSimpleGrammar. PROC is invoked with the response
code when the asynchronous procedure completes.")
#define FUNC_NAME s_vv_define_grammar
{
  VALIDATE_ARG_STR(1,name);
  VALIDATE_ARG_STR(2,grammar_file);
  VALIDATE_ARG_PROC_USE_F(3, proc);

  if (!fConnected) {
    return SCM_BOOL_F;
  } 

  DefineGrammarCB_proc = proc;

  { /* scope */
    char *szName = gh_scm2newstr(name, NULL);
    char *szFile = gh_scm2newstr(grammar_file, NULL);
    int rc = SmDefineGrammar ( szName, szFile, 0, SmAsynchronous );
    gh_free(szName);
    gh_free(szFile);
    return gh_int2scm(rc);
  }
}
#undef FUNC_NAME

SCWM_PROC(vv_enable_vocab,"vv-enable-vocab",1,1,0,
          (SCM name, SCM proc),
"Enable vocabulary/grammar NAME. 
PROC is invoked with the response code when the asynchronous procedure completes.")
#define FUNC_NAME s_vv_enable_vocab
{
  VALIDATE_ARG_STR(1,name);
  VALIDATE_ARG_PROC_USE_F(2,proc);
  EnableVocabCB_proc = proc;
  { /* scope */
    char *sz = gh_scm2newstr(name,NULL);
    return gh_int2scm(SmEnableVocab(sz,SmAsynchronous));
  }
}
#undef FUNC_NAME

SCWM_PROC(vv_turn_microphone_on,"vv-turn-microphone-on",0,1,0,
          (SCM proc),
"Turn the microphone on to start recognizing commands. 
See also `vv-initialize'. 
PROC is invoked with the response code when the asynchronous procedure completes.")
#define FUNC_NAME s_vv_turn_microphone_on
{
  VALIDATE_ARG_PROC_USE_F(1,proc);
  MicOnCB_proc = proc;
  scwm_msg(INFO,"ViaVoice","TurnMicOn called" );
  /* Send a request to tell the engine to turn the mic on.  The reply 
     comes back to the MicOnCB */
  return gh_int2scm(SmMicOn(SmAsynchronous));
}
#undef FUNC_NAME


SCWM_PROC(vv_turn_microphone_off,"vv-turn-microphone-off",0,1,0,
          (SCM proc),
"Turn the microphone off to stop recognizing commands. 
PROC is invoked with the response code when the asynchronous procedure completes.")
#define FUNC_NAME s_vv_turn_microphone_off
{
  VALIDATE_ARG_PROC_USE_F(1,proc);
  MicOffCB_proc = proc;
  scwm_msg(INFO,"ViaVoice","TurnMicOff called" );
  /* Send a request to tell the engine to turn the mic on.  The reply 
     comes back to the MicOnCB */
  return gh_int2scm(SmMicOff(SmAsynchronous));
}
#undef FUNC_NAME


static void
init_scwmviavoice()
{
#ifndef SCM_MAGIC_SNARFER
 #include "scwmviavoice.x"
#endif
}


void scm_init_app_scwm_scwmviavoice_module()
{
  scm_register_module_xxx("app scwm scwmviavoice", init_scwmviavoice);
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
