/* $Id
 * scwm.h
 * Copyright (C) 1997-1999, Maciej Stachowiak and Greg J. Badros
 */

/*
 * This module is based on code by Rob Nation, originally derived from Twm
 */
/*****************************************************************************/
/**       Copyright 1988 by Evans & Sutherland Computer Corporation,        **/
/**                          Salt Lake City, Utah                           **/
/**  Portions Copyright 1989 by the Massachusetts Institute of Technology   **/
/**                        Cambridge, Massachusetts                         **/
/**                                                                         **/
/**                           All Rights Reserved                           **/
/**                                                                         **/
/**    Permission to use, copy, modify, and distribute this software and    **/
/**    its documentation  for  any  purpose  and  without  fee is hereby    **/
/**    granted, provided that the above copyright notice appear  in  all    **/
/**    copies and that both  that  copyright  notice  and  this  permis-    **/
/**    sion  notice appear in supporting  documentation,  and  that  the    **/
/**    names of Evans & Sutherland and M.I.T. not be used in advertising    **/
/**    in publicity pertaining to distribution of the  software  without    **/
/**    specific, written prior permission.                                  **/
/**                                                                         **/
/**    EVANS & SUTHERLAND AND M.I.T. DISCLAIM ALL WARRANTIES WITH REGARD    **/
/**    TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES  OF  MERCHANT-    **/
/**    ABILITY  AND  FITNESS,  IN  NO  EVENT SHALL EVANS & SUTHERLAND OR    **/
/**    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAM-    **/
/**    AGES OR  ANY DAMAGES WHATSOEVER  RESULTING FROM LOSS OF USE, DATA    **/
/**    OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER    **/
/**    TORTIOUS ACTION, ARISING OUT OF OR IN  CONNECTION  WITH  THE  USE    **/
/**    OR PERFORMANCE OF THIS SOFTWARE.                                     **/
/*****************************************************************************/

#ifndef SCWM_H__
#define SCWM_H__

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifndef __GNUC__
#define __inline__ 
#endif


/* Can mark unused formals as ARG_IGNORE or ARG_UNUSED and avoid warning;
   uses a gcc feature, but C++ also can do this by just
   not giving a formal name.
   ARG_IGNORE is for arguments that really won't be used.
   whereas ARG_UNUSED just comments that the argument is not used at present
     and might be worth revisiting to see if we can generalize the code
     to use it. (Or if alternate implementations might use the variable) */
#ifdef __GNUC__
/* do not use the variable name as given-- paste an
   "_unused" on to the end so we force an error if
   it is used. */
#define ARG_IGNORE(x) x ## _ignore __attribute__ ((unused))
#define ARG_UNUSED(x) x ## _unused __attribute__ ((unused))
#elif defined(__cplusplus)
#define ARG_IGNORE(x) /* empty */
#define ARG_UNUSED(x) /* empty */
#else
#define ARG_IGNORE(x) x ## _ignore
#define ARG_UNUSED(x) x ## _unused
#endif

#define ARRAY_SIZE(x) ((long)(sizeof((x))/sizeof(*(x))))

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Intrinsic.h>

#include <guile/gh.h>

#include "color.h"
#include "module-types.h"
#include <stdlib.h>
#include "scwm-snarf.h"
#include "validate.h"
#include "system.h"
#include "window_fwd.h"

#undef EXTERN
#undef EXTERN_SET
#ifdef SCWM_IMPLEMENTATION
#define EXTERN
#define EXTERN_SET(x,y) x = y
#else
#define EXTERN extern
#define EXTERN_SET(x,y) extern x
#endif

extern char **g_argv;
extern int g_argc;

#define STATIC_CAST(cast,val) ((cast) (val))
#define CONST_CAST(cast,val) ((cast) (val))
#define REINTERPRET_CAST(cast,val) ((cast) (val))

#define min(a,b) (((a)<(b)) ? (a) : (b))
#define max(a,b) (((a)>(b)) ? (a) : (b))

#define STREQ(a,b) (!strcmp(a,b))

#define MAKE_SMOBFUNS(T) \
static scm_smobfuns T ## _smobfuns = { \
  &mark_ ## T, \
  &free_ ## T, \
  &print_ ## T,  0 }

#define REGISTER_SCWMSMOBFUNS(T) do { scm_tc16_scwm_ ## T = scm_newsmob(& T ## _smobfuns); } while (0)


#ifndef SCWM_EXTRACT_COMMENTS
/* do not define this macro if we are extracting comments since
   the macro name is used as a lexical cue to the extractor */

/* SCWM_VAR_INIT, SCWM_VAR still require a variable declaration */

#define SCWM_VAR_INIT(cvar, name, val) \
  do { pscm_ ## cvar = SCM_CDRLOC( \
      scm_sysintern(name, val) ); } while (0)

#define SCWM_VAR(cvar, name) \
  do { pscm_ ## cvar = SCM_CDRLOC( \
      scm_sysintern0(name) ); } while (0)


/* GJB:FIXME:: Note that cvar is ignored for now */
#define SCWM_VAR_READ_ONLY(cvar, name,val) \
  do { scm_sysintern(name,val); \
     } while (0)


#endif /* !SCWM_EXTRACT_COMMENTS */


/* Check if the scm variable is undefined or #f -- these cases
   correspond to places where we want to use a default value
   either because the args were omitted, or #f was used to skip
   the argument to get to an argument that the client wanted to 
   specify.
   Intentionally not named SCM_UNSET, since that would imply
   it's part of guile */
#define UNSET_SCM(x) (((x) == SCM_UNDEFINED) || ((x) == SCM_BOOL_F))

#define GC_MARK_SCM_IF_SET(scm) do { if (scm && !UNSET_SCM((scm))) \
     { scm_gc_mark((scm)); } } while (0)

/* use PanFrames! this replaces the 3 pixel margin with PanFrame windows
   it should not be an option, once it works right. HEDU 2/2/94 */
#define PAN_FRAME_THICKNESS 2	/* or just 1 ? */

/* the maximum number of mouse buttons that X Servers know about */
#define XSERVER_MAX_BUTTONS 5

#ifdef SIGNALRETURNSINT
#define SIGNAL_T int
#define SIGNAL_RETURN return 0
#else
#define SIGNAL_T void
#define SIGNAL_RETURN return
#endif

#define BW 1			/* border width */
#define BOUNDARY_WIDTH 7	/* border width */

# define HEIGHT_EXTRA 4		/* Extra height for texts in popus */
# define HEIGHT_EXTRA_TITLE 4	/* Extra height for underlining title */
# define HEIGHT_SEPARATOR 4	/* Height of separator lines */

#define SCROLL_REGION 2		/* region around screen edge that */
				/* triggers scrolling */


/* The option that handles client id for session management */
#define CLIENT_ID_STRING	"client-id"
#define CLIENT_ID_OPT_STRING	("--" CLIENT_ID_STRING)

/* long options that are not equivalent to short ones */
enum long_option_codes {
  CLIENT_ID = 256
};

/* the set of functions that scwm perform on top 
   level windows;  
   psw->functions is a bit mask of which such actions
   are respected for a given window */
enum wm_client_functions {
  F_RESIZE = 100, F_ICONIFY = 106, F_MAXIMIZE = 109, 
  F_DELETE = 104, F_DESTROY = 103
};

#define SCWM_NEWCELL_SMOB(ANSWER,ID,PSMOB) \
   do { \
     SCM_NEWCELL((ANSWER)); \
     SCM_SETCDR((ANSWER),(SCM) (PSMOB)); \
     SCM_SETCAR((ANSWER),(ID)); \
   } while (0)

#define DEREF_IF_SYMBOL(x) do { if (gh_symbol_p((x))) { \
                                   (x) = scm_symbol_binding(SCM_BOOL_F,(x)); \
                                } } while (0)

#define DYNAMIC_PROCEDURE_P(x) (gh_procedure_p((x)) || \
				(gh_symbol_p((x)) && \
				 gh_procedure_p(scm_symbol_binding(SCM_BOOL_F,(x)))))

#define PROCEDURE_OR_SYMBOL_P(x) (gh_procedure_p((x)) || gh_symbol_p((x)))

#define RESTP_SCM 1


#define scwm_ptr2scm(p) gh_long2scm((long)(p))

#define SCM_BOOL_FromBool(x) ((x)? SCM_BOOL_T: SCM_BOOL_F)



#define PackedBool(x) unsigned short x:1

/* if you would like to see lots of debug messages from scwm, for debugging
   purposes, uncomment the next line */

/* #define SCWM_DEBUG_MSGS */

#ifdef SCWM_DEBUG_MSGS
#  define DBUG(X) scwm_msg X
#else
#  define DBUG(X)		/* no messages */
#endif

#ifndef NDEBUG
/* Use "handle SIGUSR2 stop nopass" as a gdb option (in .gdbinit, e.g.)
   and then put this in functions that you change so you can single
   step through them --08/05/98 gjb */
#define GDB_STOP do { raise(12 /* SIGUSR2 */); } while (0)
#else
#define GDB_STOP
#endif


/*
   ** message levels for scwm_msg:
 */
typedef enum scwm_msg_levels_tag { DBG = -1, INFO, WARN, ERR } scwm_msg_levels;

struct ScwmWindow;

/* Prototypes for functions in scwm.c */

void scwm_msg(scwm_msg_levels type, const char *id, const char *msg,...);
void Reborder(Bool fRestart);
void RestoreWithdrawnLocation(struct ScwmWindow *psw, Bool fRestart);
void SigDone(int);
void SigDoneSegv(int);
void Restart(int nonsense);
void BlackoutScreen(void);
void UnBlackoutScreen(void);
void init_scwm_load_path();
void CaptureAllWindows(void);
void reset_signal_handler(int sig);
void newhandler(int sig);
void newhandler_doreset(int sig);
void newsegvhandler(int sig);


/* Global variables */
extern int master_pid;

EXTERN char *UserName, *UserHome;

extern Display *dpy;

extern struct ScwmWindow *FocusOnNextTimeStamp;

extern XContext ScwmContext;

extern Bool ShapesSupported;


extern Window JunkChild, JunkRoot;
extern Window JunkWindow;
extern int JunkX, JunkY;
extern unsigned int JunkWidth, JunkHeight, JunkBW, JunkDepth, JunkMask;

extern Atom XA_MOTIF_WM;
extern Atom XA_WM_CLIENT_LEADER;
extern Atom XA_MIT_PRIORITY_COLORS;
extern Atom XA_WM_CHANGE_STATE;
extern Atom XA_WM_STATE;
extern Atom XA_WM_COLORMAP_WINDOWS;
extern Atom XA_WM_PROTOCOLS;
extern Atom XA_WM_TAKE_FOCUS;
extern Atom XA_WM_SAVE_YOURSELF;
extern Atom XA_WM_DELETE_WINDOW;
extern Atom XA_WM_DESKTOP;
extern Atom XA_SCWM_STICKS_TO_GLASS;
extern Atom XA_SCWM_CLIENT;
extern Atom XA_OL_WIN_ATTR;
extern Atom XA_OL_WT_BASE;
extern Atom XA_OL_WT_CMD;
extern Atom XA_OL_WT_HELP;
extern Atom XA_OL_WT_NOTICE;
extern Atom XA_OL_WT_OTHER;
extern Atom XA_OL_DECOR_ADD;
extern Atom XA_OL_DECOR_DEL;
extern Atom XA_OL_DECOR_CLOSE;
extern Atom XA_OL_DECOR_RESIZE;
extern Atom XA_OL_DECOR_HEADER;
extern Atom XA_OL_DECOR_ICON_NAME;

extern Atom XA_SCWM_RESTARTING;

extern Atom XA_SCWM_EXECUTE;
extern Atom XA_SCWM_RESULT;
extern Atom XA_SCWMEXEC_LISTENER;
extern Atom XA_SCWMEXEC_REQWIN;
extern Atom XA_SCWMEXEC_REQUEST;
extern Atom XA_SCWMEXEC_REPLY;
extern Atom XA_SCWMEXEC_NOTIFY;
extern Atom XA_SCWMEXEC_OUTPUT;
extern Atom XA_SCWMEXEC_ERROR;


#endif /* SCWM_H__ */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta */

