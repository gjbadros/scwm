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

#include <guile/gh.h>
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
static void TurnMicOn    ( );
static void TurnMicOff   ( );


static Bool    fMicOn = False;

/* Use whatever default speech user info there is..         */
static char   userid   [ 80 ] = SM_USE_CURRENT;
static char   enrollid [ 80 ] = SM_USE_CURRENT;
static char   taskid   [ 80 ] = SM_USE_CURRENT;



SCWM_HOOK(vv_recognition_hook,"vv-recognition-hook", 2);
/** This hook is invoked when ViaVoice recognizes a phrase. */


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


static
void ConnectStuff ( MyClientData * MyData )
{
  static int first = True;
  int        rc;
  int        smc;
  SmArg      smargs [ 30 ];
  int input_id;

  /*-------------------------------------------------------------------*/
  /* These callbacks handle the various messages that the speech       */
  /* engine might be sending back                                      */
  /*-------------------------------------------------------------------*/
  SmHandler ConnectCB     ( SM_MSG reply, caddr_t client, caddr_t call_data );
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

  scwm_msg(INFO,"ViaVoice","ConnectStuff invoked");

  if ( first ) {
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

    first = False;
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
void TurnMicOn ( )
{
  int     rc;

  scwm_msg(INFO,"ViaVoice","TurnMicOn called" );

  /* Send a request to tell the engine to turn the mic on.  The reply  */
  /* comes back to the MicOnCB                                         */
  rc = SmMicOn ( SmAsynchronous );

  scwm_msg(INFO,"ViaVoice","TurnMicOn: SmMicOn() rc = %d", rc );
}


static
void TurnMicOff ( )
{
  int     rc;

  scwm_msg(INFO,"ViaVoice","TurnMicOff called");

  /* Same as above, but turning the mic off...                         */
  rc = SmMicOff( SmAsynchronous );

  scwm_msg(INFO,"ViaVoice","TurnMicOff: SmMicOff() rc = %d", rc );
}


static
int DoSimpleGrammar ( )
{
  int    rc;
  char   grammar [ 512 ];
  char   currdir [ 512 ];
  char * cp;
  
  /* The grammar that we're using should be in the same directory as
     the program was run from - the reco engine expects a complete path
     to it (or a path relative to where the engine is run from) so we
     build it using getcwd() */
  cp = (char *) getcwd ( currdir, sizeof ( currdir ) - 1 );

  sprintf( grammar, "%s/scwmgrammar.fsg", currdir );

  scwm_msg(INFO,"ViaVoice","DoSimpleGrammar: grammar is %s\n", grammar );
  /* And now just tell the engine where the grammar is and let it do
     its thing.  The DefineGrammarCB will get the results back.. */
  rc = SmDefineGrammar ( "ScwmGrammar", grammar, 0, SmAsynchronous );
  
  scwm_msg(INFO,"ViaVoice","DoSimpleGrammar: SmDefineGrammar rc = %d\n", rc );
  return ( rc );
}


SmHandler ConnectCB ( SM_MSG reply, caddr_t ARG_UNUSED(client), caddr_t ARG_UNUSED(call_data) )
{
  CheckSmRC("ConnectCB");

  /* We're "here", so we're connected to the engine, so now we define  */
  /* the grammar that we want the engine to use, and make the mic      */
  /* sensitive (so it can be pressed)                                  */
  DoSimpleGrammar ( );

  return ( SM_RC_OK );
}


SmHandler DisconnectCB  ( SM_MSG ARG_UNUSED(reply), caddr_t ARG_UNUSED(client),
                          caddr_t ARG_UNUSED(call_data) )
{
  return ( SM_RC_OK );
}


SmHandler SetCB  ( SM_MSG ARG_UNUSED(reply), caddr_t ARG_UNUSED(client),
                   caddr_t ARG_UNUSED(call_data) )
{
  return ( SM_RC_OK );
}


SmHandler MicOnCB ( SM_MSG reply, caddr_t ARG_UNUSED(client), 
                    caddr_t ARG_UNUSED(call_data ))
{
  CheckSmRC("MicOnCB");

  fMicOn = True;

  /* VERY IMPORTANT - this tells the recognizer to 'go' (ie. start     */
  /* capturing the audio and processing it)                            */
  SmRecognizeNextWord ( SmAsynchronous );

  return ( SM_RC_OK );
}


SmHandler MicOffCB ( SM_MSG reply, caddr_t ARG_UNUSED(client), caddr_t ARG_UNUSED(call_data) )
{
  CheckSmRC("MicOffCB");

  fMicOn = False;

  return ( SM_RC_OK );
}


SmHandler EnableVocabCB ( SM_MSG reply, caddr_t ARG_UNUSED(client), caddr_t ARG_UNUSED(call_data) )
{
  CheckSmRC("EnableVocabCB");

  return ( SM_RC_OK );
}


SmHandler DefineVocabCB ( SM_MSG reply, caddr_t ARG_UNUSED(client), 
                          caddr_t ARG_UNUSED(call_data) )
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

  /* Enable the vocabulary (tells the recognizer to listen for words   */
  /* from it)                                                          */
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
                          caddr_t ARG_UNUSED(call_data) )
{
  CheckSmRC("GetNextWordCB");

  /* This gets called whenever SmRecognizeNextWord() is called         */
  return ( SM_RC_OK );
}


SmHandler RecoWordCB ( SM_MSG reply, caddr_t ARG_UNUSED(client), 
                       caddr_t ARG_UNUSED(call_data) )
{
  int             rc;
  int             i;
  unsigned long   num_firm;
  SM_WORD       * firm;

  CheckSmRC("NextWordCB");

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


SmHandler RecoPhraseCB ( SM_MSG reply, caddr_t ARG_UNUSED(client), caddr_t ARG_UNUSED(caller) )
{
  int       rc;
  SM_WORD * firm;
  unsigned  long num_firm;
  int       i;
  char      phrase [ 255 ];
  unsigned  long flags;
  int       increment = 10;
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


  /* If the phrase was accepted, then move the toplevel window a bit  */
  /* in whatever direction was specified (the direction is determined */
  /* by looking at the 1st character of the 3rd word of the phrase,   */
  /* the amount is determined by the annotation on the last word of   */
  /* the phrase (if it exists; if it doesn't, the default us used)    */
  if ( flags & SM_PHRASE_ACCEPTED )
  {
    /* Get the annotation that's tied to the last part of the phrase  */
    /* (it contains the amount we want to move the window)..  If it's */
    /* not there or is the wrong type, then the grammar has probably  */
    /* changed since this part of the code was written..              */
    rc = SmGetAnnotations ( reply, & num_annots, & annots );

    if ( num_annots == 4 ) {
      if ( annots [ 3 ].type == SM_ANNOTATION_NUMERIC ) {
        increment = annots [ 3 ].annodata.numeric;
      } else {
        scwm_msg(INFO,"ViaVoice", "RecoPhraseCB: Annotation data type is wrong!\n" );
      }
    } else {
      scwm_msg(INFO,"ViaVoice", "RecoPhraseCB: Annotation data is missing!\n" );
    }

    scwm_run_hook(vv_recognition_hook,
                  gh_list(gh_str02scm(firm[2].spelling),
                          gh_int2scm(increment),
                          SCM_UNDEFINED));
  }


  /* Tell the engine to 'go' again                                    */
  rc = SmRecognizeNextWord ( SmAsynchronous );

  return ( rc );
}


SmHandler UtteranceCB ( SM_MSG ARG_UNUSED(reply),
                        caddr_t ARG_UNUSED(client), 
                        caddr_t ARG_UNUSED(call_data) )
{
  scwm_msg(INFO,"ViaVoice", "UtteranceCB\n" );

  /* The engine has turned the mic off and processed all of the audio  */
  return ( SM_RC_OK );
}

SCWM_PROC(vv_connect,"vv-connect",0,0,0,
          ())
#define FUNC_NAME s_vv_connect 
{
  /* Connect to the speech recognizer                       */
  ConnectStuff ( NULL );
  return SCM_UNDEFINED;
}
#undef FUNC_NAME


SCWM_PROC(vv_turn_microphone_on,"vv-turn-microphone-on",0,0,0,
          ())
#define FUNC_NAME s_vv_turn_microphone_on
{
  TurnMicOn();
  return SCM_UNDEFINED;
}
#undef FUNC_NAME


SCWM_PROC(vv_turn_microphone_off,"vv-turn-microphone-off",0,0,0,
          ())
#define FUNC_NAME s_vv_turn_microphone_off
{
  TurnMicOff();
  return SCM_UNDEFINED;
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
