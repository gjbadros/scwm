/* $Id
 * scwm_msg.h
 * Copyright (C) 1997-1999, Greg J. Badros and Maciej Stachowiak
 */


#ifndef SCWM_MSG_H__
#define SCWM_MSG_H__

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include <guile/gh.h>

/* if you would like to see lots of debug messages from scwm, for debugging
   purposes, uncomment the next line */

/* #define SCWM_DEBUG_MSGS */
/* #define SCWM_EVENT_DEBUG_MSGS */


#ifdef SCWM_DEBUG_MSGS
#  define DBUG(X) scwm_msg X
#else
#  define DBUG(X)		/* no messages */
#endif

#ifdef SCWM_EVENT_DEBUG_MSGS
#  define DBUG_EVENT(X) scwm_msg X
#else
#  define DBUG_EVENT(X)		/* no messages */
#endif

/*
   ** message levels for scwm_msg:
 */
typedef enum scwm_msg_levels_tag { DBG = -1, INFO, WARN, ERR } scwm_msg_levels;

void scwm_message(scwm_msg_levels type, const char *id, const char *msg, SCM args);
void scwm_msg(scwm_msg_levels type, const char *id, const char *msg,...);

#define SCWM_DISPLAY "~A"
/* #define SCWM_DISPLAY "%s" */
#define SCWM_WRITE "~S"
/* #define SCWM_WRITE "%S" */

#endif
