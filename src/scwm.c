/* $Id$
 * scwm.c
 * Copyright (C) 1997-1999 Greg J. Badros and Maciej Stachowiak
 *
 * This module has been significantly modified from fvwm2
 * It may be used under the terms indicated by the copyright below.
 *
 */

/*
 * This module is derived from all original code by Rob Nation 
 * Copyright 1993, Robert Nation
 *     You may use this code for any purpose, as long as the original
 *     copyright remains in the source code and all documentation
 */

/*
 * Scwm - "Scheme Constraints Window Manager"
 * A highly dynamic, extensible and programmable
 * X11 window manager embedding guile scheme.
 */

#define SCWM_IMPLEMENTATION

/* #define SCWM_DEBUG_MALLOC */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>
#include <pwd.h>
#include <setjmp.h>

#include "scwm.h"
#include <stdarg.h>
#include <X11/Xlib.h>
#include <X11/Xproto.h>
#include <X11/Xatom.h>
/* need to get prototype for XrmUniqueQuark for XUniqueContext call */
#include <X11/Xresource.h>
#include <X11/Xutil.h>
#ifdef HAVE_SHAPE
#include <X11/extensions/shape.h>
#endif

#ifdef HAVE_LIBXMU
#include <X11/Xmu/Error.h>
#endif
#ifdef HAVE_GETOPT_H
#include <getopt.h>
#else 
#include "getopt.h"
#endif /* END HAVE_GETOPT_H */

#if defined (sparc) && defined (SVR4)
/* Solaris has sysinfo instead of gethostname.  */
#include <sys/systeminfo.h>
#endif

#ifdef I18N
#ifdef X_LOCALE
#include <X11/Xlocale.h>
#else
#include <locale.h>
#endif
#endif

#include <libguile.h>
#include <guile/gh.h>

#include "scwm.h"

#include "guile-compat.h"
#include "syscompat.h"
#include "scwmpaths.h"
#include "scm_init_funcs.h"
#include "screen.h"
#include "window.h"
#include "add_window.h"
#include "decor.h"
#include "image.h"
#include "menu.h"
#include "callbacks.h"
#include "font.h"
#include "resize.h"
#include "virtual.h"
#include "Grab.h"
#include "shutdown.h"
#include "xmisc.h"
#include "colormaps.h"
#include "module-interface.h"
#include "log-usage.h"
#include "drawmenu.h"
#include "events.h"
#include "message-window.h"
#include "cursor.h"

#ifdef HAVE_LIBSM_LIBICE
#include "session-manager.h"
#endif

#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif

#ifdef USE_CASSOWARY
void init_cassowary_scm();           /* from the cassowary distribution */
#endif

#ifdef HAVE_XTEST
#include <X11/extensions/XTest.h>
#endif

#define MAXHOSTNAME 255

#ifdef ENABLE_DUMP

#define GDB_TYPE SCM
#include <libguile/gdb_interface.h>
GDB_INTERFACE;

extern int scm_boot_guile_1_live;
extern int scm_ice_9_already_loaded;

void
scm_prepare_dump()
{
  scm_boot_guile_1_live = 0;
  scm_ice_9_already_loaded = 1;
  scm_flush_all_ports();
}

void
scwm_init_after_dump ()
{
  scwm_msg(ERR,"scwm_init_after_dump","Initializing from dump file.");
}

void unexec (char *new_name, char *old_name, 
             unsigned data_start, unsigned bss_start,
             unsigned entry_address);

static void
dodump(char *progname, char *dumpfile)
{
  scm_prepare_dump();
  scm_igc("dump");
  sbrk(4096);  /* work around a bug somewhere */
  unexec(dumpfile, progname, 0, 0, 0);
  printf("dumped `%s'.\n", dumpfile);
}

#ifdef DUMP_DECL
DUMP_DECL
#endif

#endif /* ENABLE_DUMP */



void init_borders(); /* borders.c */

static char rcsid[] = "$Id$";
extern char *szRepoLastChanged;

int master_pid;			/* process number of 1st scwm process */

ScreenInfo Scr;			/* structures for the screen */
SCM scmScreen;                  /* the scheme object wrapping Scr */
Display *dpy;			/* which display are we talking to */

char *szCmdConfig;

static char *szLoad_pre = "(safe-load \"";
static char *szLoad_post = "\")";


char *output_file = NULL;

XErrorHandler ScwmErrorHandler(Display *, XErrorEvent *);
XIOErrorHandler CatchFatal(Display *);
XErrorHandler CatchRedirectError(Display *, XErrorEvent *);

void ChildDied(int nonsense);
void SetMWM_INFO(Window window);
void StartupStuff(void);
void usage(void);

XContext ScwmContext;		/* context for scwm windows */
XContext MenuContext;           /* context for menus */

Window JunkRoot, JunkChild;	/* junk window */
Window JunkWindow;
int JunkX = 0, JunkY = 0;

ScwmWindow *FocusOnNextTimeStamp = NULL;

Bool debugging = False, PPosOverride = False, Blackout = False;
Bool fDisableBacktrace = False;
Bool fDocumentPrimitiveFormals = False;
Bool segvs_to_reset = 0;
Bool fHandleSegv = True;
Bool scwm_gc_often = False;
Bool fDoneStartup = False;


char **g_argv;
int g_argc;

/* the jump buffer environment for the handle events loop
   to implement restarting the event loop */
sigjmp_buf envHandleEventsLoop;

/* assorted gray bitmaps for decorative borders */
#define g_width 2
#define g_height 2
static char g_bits[] =
{0x02, 0x01};

#define l_g_width 4
#define l_g_height 2
static char l_g_bits[] =
{0x08, 0x02};

#define s_g_width 4
#define s_g_height 4

int ShapeEventBase, ShapeErrorBase;
Bool ShapesSupported = False;

int XTestEventBase, XTestErrorBase, XTestMajorP, XTestMinorP;
Bool XTestSupported = False;

long isIconicState = 0;
extern XEvent Event;
Bool Restarting = False;
int restart_vp_offset_x = 0, restart_vp_offset_y = 0;

int fd_width, x_fd;
char *display_name = NULL;

void scwm_main(int, char **);

Atom XA_MIT_PRIORITY_COLORS;
Atom XA_WM_CHANGE_STATE;
Atom XA_WM_STATE;
Atom XA_WM_COLORMAP_WINDOWS;
Atom XA_WM_TAKE_FOCUS;
Atom XA_WM_DELETE_WINDOW;
Atom XA_WM_DESKTOP;
Atom XA_MwmAtom;
Atom XA_MOTIF_WM;
Atom XA_WM_CLIENT_LEADER;

Atom XA_OL_WIN_ATTR;
Atom XA_OL_WT_BASE;
Atom XA_OL_WT_CMD;
Atom XA_OL_WT_HELP;
Atom XA_OL_WT_NOTICE;
Atom XA_OL_WT_OTHER;
Atom XA_OL_DECOR_ADD;
Atom XA_OL_DECOR_DEL;
Atom XA_OL_DECOR_CLOSE;
Atom XA_OL_DECOR_RESIZE;
Atom XA_OL_DECOR_HEADER;
Atom XA_OL_DECOR_ICON_NAME;

Atom XA_SCWM_RESTARTING;

Atom XA_SCWM_EXECUTE;
Atom XA_SCWM_RESULT;
Atom XA_SCWMEXEC_LISTENER;
Atom XA_SCWMEXEC_REQWIN;
Atom XA_SCWMEXEC_REQUEST;
Atom XA_SCWMEXEC_REPLY;
Atom XA_SCWMEXEC_NOTIFY;
Atom XA_SCWMEXEC_OUTPUT;
Atom XA_SCWMEXEC_ERROR;

Atom XA_SCWM_VIEWPORT_OFFSET_X;
Atom XA_SCWM_VIEWPORT_OFFSET_Y;

/*
#define GNOME_SUPPORT_IN_C
*/

#ifdef GNOME_SUPPORT_IN_C
Atom XA_WIN_SUPPORTING_WM_CHECK;
#endif

static Window BlackoutWin = None; /* window to hide window captures */


/* GJB:FIXME:: E does ResizeRedirectMask and PointerMotionMask/ButtonMotionMask, too */
long basic_event_mask = (LeaveWindowMask | EnterWindowMask | PropertyChangeMask |
                         SubstructureRedirectMask | KeyPressMask | KeyReleaseMask |
                         SubstructureNotifyMask |
                         ButtonPressMask | ButtonReleaseMask);

static void 
InternUsefulAtoms(void)
{
  /* 
   * Create priority colors if necessary.
   */
  XA_MIT_PRIORITY_COLORS = XInternAtom(dpy, "_MIT_PRIORITY_COLORS", False);
  XA_WM_CHANGE_STATE = XInternAtom(dpy, "WM_CHANGE_STATE", False);
  XA_WM_STATE = XInternAtom(dpy, "WM_STATE", False);
  XA_WM_COLORMAP_WINDOWS = XInternAtom(dpy, "WM_COLORMAP_WINDOWS", False);
  XA_WM_PROTOCOLS = XInternAtom(dpy, "WM_PROTOCOLS", False);
  XA_WM_TAKE_FOCUS = XInternAtom(dpy, "WM_TAKE_FOCUS", False);
  XA_WM_DELETE_WINDOW = XInternAtom(dpy, "WM_DELETE_WINDOW", False);
  XA_WM_DESKTOP = XInternAtom(dpy, "WM_DESKTOP", False);
  XA_MwmAtom = XInternAtom(dpy, "_MOTIF_WM_HINTS", False);
  XA_MOTIF_WM = XInternAtom(dpy, "_MOTIF_WM_INFO", False);
  XA_WM_CLIENT_LEADER = XInternAtom(dpy, "WM_CLIENT_LEADER", False);

  XA_OL_WIN_ATTR = XInternAtom(dpy, "_OL_WIN_ATTR", False);
  XA_OL_WT_BASE = XInternAtom(dpy, "_OL_WT_BASE", False);
  XA_OL_WT_CMD = XInternAtom(dpy, "_OL_WT_CMD", False);
  XA_OL_WT_HELP = XInternAtom(dpy, "_OL_WT_HELP", False);
  XA_OL_WT_NOTICE = XInternAtom(dpy, "_OL_WT_NOTICE", False);
  XA_OL_WT_OTHER = XInternAtom(dpy, "_OL_WT_OTHER", False);
  XA_OL_DECOR_ADD = XInternAtom(dpy, "_OL_DECOR_ADD", False);
  XA_OL_DECOR_DEL = XInternAtom(dpy, "_OL_DECOR_DEL", False);
  XA_OL_DECOR_CLOSE = XInternAtom(dpy, "_OL_DECOR_CLOSE", False);
  XA_OL_DECOR_RESIZE = XInternAtom(dpy, "_OL_DECOR_RESIZE", False);
  XA_OL_DECOR_HEADER = XInternAtom(dpy, "_OL_DECOR_HEADER", False);
  XA_OL_DECOR_ICON_NAME = XInternAtom(dpy, "_OL_DECOR_ICON_NAME", False);

  XA_SCWM_RESTARTING = XInternAtom(dpy, "SCWM_RESTARTING", False);

  XA_SCWM_EXECUTE = XInternAtom(dpy, "SCWM_EXECUTE", False);
  XA_SCWM_RESULT = XInternAtom(dpy, "SCWM_RESULT", False);
  XA_SCWMEXEC_LISTENER=XInternAtom(dpy,"SCWMEXEC_LISTENER", False);
  XA_SCWMEXEC_REQWIN=XInternAtom(dpy,"SCWMEXEC_REQWIN", False);
  XA_SCWMEXEC_REQUEST=XInternAtom(dpy,"SCWMEXEC_REQUEST", False);
  XA_SCWMEXEC_REPLY=XInternAtom(dpy,"SCWMEXEC_REPLY", False);
  XA_SCWMEXEC_NOTIFY=XInternAtom(dpy,"SCWMEXEC_NOTIFY", False);
  XA_SCWMEXEC_OUTPUT=XInternAtom(dpy,"SCWMEXEC_OUTPUT", False);
  XA_SCWMEXEC_ERROR=XInternAtom(dpy,"SCWMEXEC_ERROR", False);

  XA_SCWM_VIEWPORT_OFFSET_X = XInternAtom(dpy,"SCWM_VIEWPORT_OFFSET_X", False);
  XA_SCWM_VIEWPORT_OFFSET_Y = XInternAtom(dpy,"SCWM_VIEWPORT_OFFSET_Y", False);

  return;
}

#ifdef GNOME_SUPPORT_IN_C
void
AnnounceGnomeCompliancy(Window w)
{
  /* for announcing GNOME compliance */
  XA_WIN_SUPPORTING_WM_CHECK=XInternAtom(dpy,"_WIN_SUPPORTING_WM_CHECK",False);
  XChangeProperty(dpy, w, XA_WIN_SUPPORTING_WM_CHECK,
                  XA_CARDINAL,32,PropModeReplace, 
                  (unsigned char *) &Scr.Root, 1);
  XChangeProperty(dpy, Scr.Root, XA_WIN_SUPPORTING_WM_CHECK,
                  XA_CARDINAL,32,PropModeReplace, 
                  (unsigned char *) &Scr.Root, 1);
}
#endif


/* if the XA_SCWMEXEC_REQWIN window is already set at 
   startup, the first scwm-exec protocol request will cause
   lots of X errors */
static void
ResetScwmexecProtocol()
{
  XDeleteProperty(dpy,Scr.Root,XA_SCWMEXEC_REQWIN);
}


/*
 * CreateGCs - create all the needed GC's.  done only once during startup
 */
void 
CreateGCs(void)
{
  XGCValues gcv;
  unsigned long gcm;

  /* create scratch GC's */
  gcm = GCFunction | GCPlaneMask | GCGraphicsExposures | GCLineWidth;
  gcv.line_width = 0;
  gcv.function = GXcopy;
  gcv.plane_mask = AllPlanes;
  gcv.graphics_exposures = False;

  Scr.ScratchGC1 = XCreateGC(dpy, Scr.Root, gcm, &gcv);
  Scr.ScratchGC2 = XCreateGC(dpy, Scr.Root, gcm, &gcv);
  Scr.ScratchGC3 = XCreateGC(dpy, Scr.Root, gcm, &gcv);
}

static void
SetRestartingGlobal()
{ /* scope */
  Atom atype;
  int aformat;
  unsigned long nitems, bytes_remain;
  unsigned char *prop;
  
  if ((XGetWindowProperty(dpy, Scr.Root, XA_SCWM_RESTARTING, 0L, 1L, True,
                          AnyPropertyType, &atype, &aformat, &nitems,
                          &bytes_remain, &prop)) == Success) {
    if (prop != NULL) {
      Restarting = True;
      XDeleteProperty(dpy, Scr.Root, XA_SCWM_RESTARTING);
    }
  }
}



/*
 * InitVariables - initialize scwm variables
 * This happens after the display has been opened,
 * so all sorts of X-related variables can be setup
 * (e.g., fonts, GCs can be created, etc.)
 */
static void 
InitVariables(void)
{
  ScwmContext = XUniqueContext();
  MenuContext = XUniqueContext();
  MsgWindowContext = XUniqueContext();
  ExposeWindowProcContext = XUniqueContext();

  /* initialize some lists */
  Scr.AllBindings = NULL;
  Scr.DefaultIcon = NULL;

  /* create graphics contexts */
  CreateGCs();

  Scr.pssci = NULL;
  Scr.d_depth = DefaultDepth(dpy, Scr.screen);
  Scr.ScwmRoot.w = Scr.Root;
  Scr.ScwmRoot.next = 0;
  XGetWindowAttributes(dpy, Scr.Root, &(Scr.ScwmRoot.attr));
  Scr.root_pushes = 0;
  Scr.pushed_window = &Scr.ScwmRoot;
  Scr.ScwmRoot.number_cmap_windows = 0;


  Scr.DisplayWidth = DisplayWidth(dpy, Scr.screen);
  Scr.DisplayHeight = DisplayHeight(dpy, Scr.screen);

  Scr.NoBoundaryWidth = 1;
  Scr.BoundaryWidth = BOUNDARY_WIDTH;
  Scr.Hilite = NULL;
  Scr.Focus = NULL;
  Scr.Ungrabbed = NULL;

  Scr.icon_font = SCM_UNDEFINED;
  Scr.nonant_highlight_color = WHITE_COLOR;
  Scr.msg_window_font = make_font(str_fixed);
  Scr.msg_window_fg = BLACK_COLOR;
  Scr.msg_window_bg = WHITE_COLOR;
  Scr.msg_window_highlight = WHITE_COLOR;
  Scr.msg_window_shadow = WHITE_COLOR;
  Scr.msg_window_x = Scr.DisplayWidth/2;
  Scr.msg_window_y = Scr.DisplayHeight/2;
  Scr.msg_window_x_align = -.5;
  Scr.msg_window_y_align = -.5;
  Scr.NotMenuColors.bg = SCM_UNDEFINED;
  Scr.DefaultDecor.HiColors.bg = SCM_UNDEFINED;

  scm_permanent_object(scmFixedFont = make_font(str_fixed));

  Scr.VxMax = 2 * Scr.DisplayWidth;
  Scr.VyMax = 2 * Scr.DisplayHeight;
  Scr.Vx = Scr.Vy = 0;

  CassowaryInitClVarsInPscreen(&Scr);

  scwm_defer_ints();
  scm_protect_object(scmScreen = ScmFromPScreenInfo(&Scr));
  Scr.schscreen = scmScreen;
  scwm_allow_ints();

  /* Sets the current desktop number from prior Scwm running */
  /* Multiple desks are available even in non-virtual
   * compilations */
  { /* scope */
    Atom atype;
    int aformat;
    unsigned long nitems, bytes_remain;
    unsigned char *prop;

    Scr.CurrentDesk = 0;
    if ((XGetWindowProperty(dpy, Scr.Root, XA_WM_DESKTOP, 0L, 1L, True,
			    XA_WM_DESKTOP, &atype, &aformat, &nitems,
			    &bytes_remain, &prop)) == Success) {
      if (prop != NULL) {
	Scr.CurrentDesk = *(unsigned long *) prop;
      }
      XFree(prop);
    }
  }

  Scr.EdgeScrollX = Scr.EdgeScrollY = 100;
  Scr.ScrollResistance = Scr.MoveResistance = 0;
  Scr.ClickTime = 150;
  Scr.fColormapFollowsMouse = True;

  /* set major operating modes */
  Scr.randomx = Scr.randomy = 0;
  Scr.buttons2grab = (1 << XSERVER_MAX_BUTTONS) - 1;

  scm_permanent_object(decor2scm(&Scr.DefaultDecor));
  DECORREF(Scr.DefaultDecor.scmdecor);

  Scr.DefaultDecor.tag = strdup("default");

  Scr.fSmartPlacementIsClever = False;
  Scr.fClickToFocusPassesClick = True;
  Scr.fClickToFocusRaises = True;
  Scr.fMouseFocusClickRaises = False;

  return;
}

static void
scwm_maybe_send_thankyou_packet()
{
  char buf[256];
  SCM log_usage = gh_lookup("thank-scwm-authors-with-usage-note");

  /* use (define thank-scwm-authors-with-usage-note #t) to
     send a packet to let us know you're using scwm.
     Or set the environment variable SCWM_DO_NOT_LOG_USAGE to 
     never do anything */
  if (log_usage != SCM_BOOL_T)
    return;
  
  sprintf(buf, "%s, %s: STARTED", VERSION, szRepoLastChanged);
  SendUsagePacket(0, 0, "scwm", 0, buf);
}

static void
InitUserData()
{
  struct passwd *pw;
  char *user = NULL, *home = NULL;

  if (!(home = getenv("HOME"))
      || (!(user = getenv("USER")) && !(user = getenv("LOGNAME")))) {
    /* if setting either user or home from env failed, we need passwd */
    pw = getpwuid(getuid());
    if (pw) {
      if (!user)
	user = pw->pw_name;
      if (!home)
	home = pw->pw_dir;
    } else {
      if (!user) {
	user = "nobody";
	scwm_msg(WARN, "InitUserData", "Could not determine user name "
		 "- assuming `nobody'");
      }
      if (!home) { 
	home = "/tmp";
	scwm_msg(WARN, "InitUserData", "Could not determine home directory "
		 "- assuming `/tmp'");
      }
    }
  }
  UserName = strdup(user);
  UserHome = strdup(home);
}



/*
 *  Procedure:
 *	main - Enters scwm_main using the gh_enter convention.
 */

int
main(int argc, char **argv)
{
#if 0 || defined(HAVE_SCM_INIT_HEAP_SIZE_FACTOR)
  scm_init_heap_size_factor = 24;
#endif
  scwm_gh_enter(argc, argv, scwm_main);
  return 0;
}

#ifdef SCWM_DEBUG_MALLOC

#if defined (__STDC__) && __STDC__
#include <stddef.h>
#define	__malloc_size_t		size_t
#define	__malloc_ptrdiff_t	ptrdiff_t
#else
#define	__malloc_size_t		unsigned int
#define	__malloc_ptrdiff_t	int
#endif

extern __ptr_t _malloc_internal __P ((__malloc_size_t __size));
extern __ptr_t _realloc_internal __P ((__ptr_t __ptr, __malloc_size_t __size));
extern void _free_internal __P ((__ptr_t __ptr));

extern void (*__free_hook) __P ((__ptr_t __ptr));
extern __ptr_t (*__malloc_hook) __P ((__malloc_size_t __size));
extern __ptr_t (*__realloc_hook) __P ((__ptr_t __ptr, __malloc_size_t __size));
extern __ptr_t (*__memalign_hook) __P ((__malloc_size_t __size,
					__malloc_size_t __alignment));

__ptr_t scwm_malloc_debug(__malloc_size_t size)
{
  assert(size > 0);
  {
  __ptr_t answer = _malloc_internal(size); /* 2*size + 3 to check for overflow errors */
  fprintf(stderr,"%%@! malloc %d @ %p\n",size,answer);
  return answer;
  }
}

void scwm_free_debug(__ptr_t ptr)
{
  fprintf(stderr,"%%@! free @ %p\n",ptr);
  _free_internal(ptr);
}

__ptr_t scwm_realloc_debug(__ptr_t ptr, __malloc_size_t size)
{
  assert(size > 0);
  {
  __ptr_t answer = _realloc_internal(ptr,2*size+3);
  fprintf(stderr,"%%@! realloc %d @ %p now %p\n",size,ptr,answer);
  return answer;
  }
}

#endif

/*
 * scwm_main - main routine for scwm
 */
void 
scwm_main(int argc, char **argv)
{
  static int dumped = 0;
  static Bool fShouldDump = False;
  static char *szBinaryPath = NULL;
  static char *szDumpFile = NULL;
  int i;
  extern int x_fd;
  int len;

  /* getopt vars */
  int getopt_ret;
  extern char *optarg;
  extern int /* optind, opterr, */ optopt;

  char *display_string;
  char message[255];
  Bool single = False;
  Bool option_error = False;

#ifdef I18N
  char *Lang,*territory,*tmp;
#endif

#ifdef SCWM_DEBUG_MALLOC
  __malloc_hook = scwm_malloc_debug;
  __free_hook = scwm_free_debug;
  __realloc_hook = scwm_realloc_debug;
#endif

  gh_eval_str ("(define-module (guile))");  

#ifdef I18N
  /* setlocale in guile */
  scm_setlocale( gh_lookup("LC_CTYPE"), gh_str02scm("") ); 
  /* setlocale in X (system native locale or X_LOCALE) */
  if ((Lang = setlocale (LC_CTYPE,"")) == (char *)NULL) {
    scwm_msg(WARN,"main","Can't set specified locale.\n");
    Lang = "C";
  }
  if (! XSupportsLocale()) {
    scwm_msg(ERR, "main", "locale not supported by Xlib, locale set to C");
    Lang = setlocale(LC_ALL, "C");
  }
  if (! XSetLocaleModifiers(""))
    scwm_msg(ERR, "main", "X locale modifiers not supported, using default");

  tmp = index(Lang,'.');
  if (tmp) {
      territory = NEWC(tmp-Lang+1,char);
      strncpy(territory,Lang,(tmp-Lang));
      *(territory+(size_t)(tmp-Lang)) = '\0';
  } else {
      territory = Lang;
  }
  SCWM_VAR_READ_ONLY(NULL,"locale-fullname",gh_str02scm(Lang));
  /** Full name of the current locale, as a string. */

  SCWM_VAR_READ_ONLY(NULL,"locale-language-territory",gh_str02scm(territory));
  /** The language territory name, as a string */
#endif

  
  /* Avoid block buffering on stderr, stdout even if it's piped somewhere;
     it's useful to pipe through to grep -v or X-error-describe
     while debugging: GJB:FIXME:MS: make these runtime options -- also,
     isn't stderr never block bufferred?? */
  setlinebuf(stderr);
  setlinebuf(stdout);

  scwm_defer_ints();
  init_font();
  init_decor();
  init_screen();
  init_callbacks();
  init_add_window();
  init_color();
  init_module_interface();
  init_miscprocs();
  init_menuitem();
  init_menulook();
  init_drawmenu();
  init_menu();
  init_binding();
  init_winprop();
  init_window();
  init_resize();
  init_move();
  init_face();
  init_shutdown();
  init_xproperty();
  init_events();
  init_focus();
  init_deskpage();
  init_placement();
  init_Grab();
  init_virtual();
  init_icons();
  init_ICCCM();
  init_cursor();
#ifdef USE_CASSOWARY
  init_constraint_primitives();
#endif
  scwm_allow_ints();

  InitUserData();

  szCmdConfig = NEWC(1,char);
  
  szCmdConfig[0] = '\0';
  
  g_argv = argv;
  g_argc = argc;

  DBUG((DBG,"main", "Entered, about to parse args"));
  
  /** CONCEPT: Run-time command-line options
<segmentedlist>
<segtitle/Option/
<segtitle/Description/

<seglistitem>
<seg/-D or --debug/ <seg/enable lots of debugging messages./
</seglistitem><seglistitem>

<seg/-n or --nobacktrace/ <seg/disable guile's debugging backtraces./
</seglistitem><seglistitem>

<seg/-p or --segv-cleanup-and-stop/ <seg/abort on segv signal, but cleanup first/
</seglistitem><seglistitem>

<seg/-P or --segv-just-stop/ <seg/abort on segv signal without catching signal at all/
</seglistitem><seglistitem>

<seg/-s or --single-screen/ <seg/run only on on the first screen of the display./
</seglistitem><seglistitem>

<seg/-d or --display dpy/ <seg/run on display dpy./
</seglistitem><seglistitem>

<seg/-f or --file file/ <seg>read startup commands from the specified
file instead of ".scwmrc" or "system.scwmrc".</seg>
</seglistitem><seglistitem>

<seg/-e or --expression expr/ <seg>evaluate Scheme expression expr
instead of reading from ".scwmrc" or "system.scwmrc". Multiple -e and
-f options may be specified on a single command line and in this case
will be processed in the order in which they were specified.</seg>
</seglistitem><seglistitem>

<seg/-F or --document-formals/ <seg/document formal parameters of primitives./
</seglistitem><seglistitem>

<seg/-h or --help/ <seg/prints a usage message and exits./
</seglistitem><seglistitem>

<seg/-b or --blackout/ <seg/blacks the screen out to hide the initial capture./
</seglistitem><seglistitem>

<seg/--client-id id/ <seg>sets scwm's client id to a specific value. This
is probably of no use to you unless you're a session manager or debbuging.
</seg>
</seglistitem><seglistitem>

<seg/-v or --version/ <seg/prints the version and exits./
</seglistitem>
</segmentedlist>
  */


  
  while(1) {
    static const char *getopt_opts = "Dsd:f:e:hibVnp:Pg";
    static struct option getopt_longopts[] =
    {
      {"debug", 0, NULL, 'D'}, /* turns on Scwm debugging */
      {"single-screen", 0, NULL, 's'},
      {"display", 1, NULL, 'd'},
      {"file", 1, NULL, 'f'},
      {"expression", 1, NULL, 'e'},
      {"help", 0, NULL, 'h'},
      {"blackout", 0, NULL, 'b'},
      {"version", 0, NULL, 'V'},
      {"dump", 1, NULL, 'm'},
      {"document-formals", 0, NULL, 'F'},
      {"gc-often", 0, NULL, 'g'},
      {"segv-reset-count", 1, NULL, 'p'},
      {"segv-just-stop", 0, NULL, 'P'},
      {"nobacktrace", 0, NULL, 'n'}, /* turns off guile backtraces */
      {CLIENT_ID_STRING, required_argument, NULL, CLIENT_ID},
      {NULL, 0, NULL, 0}
    };
    
    getopt_ret = getopt_long(argc, argv, getopt_opts,
			     getopt_longopts, 0);
    if(getopt_ret == EOF) break;
    
    switch(getopt_ret) {
    case 'm':
      fShouldDump = True;
      szBinaryPath = argv[0];
      szDumpFile = strdup(optarg);
      break;
    case 'g':
      scwm_gc_often = True;
      break;
    case 'D':
      debugging = True; break;
    case 's':
      single = True; break;
    case 'p': /* --segv-reset-count */
      /* still handle the segv, just do not reset to the main event handler */
      if (optarg == NULL) {
        segvs_to_reset = 0;
      } else {
        segvs_to_reset = atoi(optarg);
      };
      break;
    case 'P': /* --segv-just-stop */
      /* do not even catch segv's */
      fHandleSegv = False; break;
    case 'd':
      if(optarg == NULL) {
        option_error = True;
      break;
      } else {
        display_name = optarg;
        break;
      }
    case 'n':
      fDisableBacktrace = True; break;
    case 'F':
      /* GJB:FIXME:: invert sense of this switch when
         undefined quote problem is taken care of */
      fDocumentPrimitiveFormals = True; break;
    case 'f':
      if(optarg == NULL) {
        option_error=True;
        break;
      } else {
        szCmdConfig = (char *) realloc(szCmdConfig, sizeof(char) * 
                                       (strlen(szCmdConfig) +
                                        strlen(szLoad_pre) +
                                        strlen(optarg) +
                                        strlen(szLoad_post) + 1));
        
        szCmdConfig = strcat(szCmdConfig, szLoad_pre);
        szCmdConfig = strcat(szCmdConfig, optarg);
	szCmdConfig = strcat(szCmdConfig, szLoad_post);
        break;
      }
    case 'e':
      if(optarg == NULL) {
        option_error=True;
      break;
      } else {
        szCmdConfig = (char *) realloc(szCmdConfig, sizeof(char) *
                                       (strlen(szCmdConfig) +
                                        strlen(optarg) + 1));
        
	szCmdConfig = strcat(szCmdConfig, optarg);
        break;
      }
    case 'h':
      usage(); 
      exit(0);
    case 'b':
      Blackout = True; 
      break;
    case 'V':
      printf("Scwm Version %s compiled on %s at %s\nRCS_ID=%s\n\
Repository Timestamp: %s\n",
             VERSION, __DATE__, __TIME__, rcsid, szRepoLastChanged);
      exit(0);
    case CLIENT_ID:
#ifdef HAVE_LIBSM_LIBICE
      SmcId = optarg;
#endif
      break;
    case ':':
      scwm_msg(ERR, "main", "Missing option argument: `-%c'\n",
               (char)optopt);
      option_error = True; 
      break;
    case '?': /* getopt's dunno return */
      option_error = True; 
      break;
    default:  /* if we made an error */
      scwm_msg(ERR, "main", "Unknown option: `-%c'\n",  (char)optopt);
      option_error = True; 
      break;
    }
  }
  
  if(option_error) {
    usage();
    exit(-1);
  }
  
  DBUG((DBG,"main", "Done parsing args"));
  
  DBUG((DBG,"main", "Installing signal handlers"));

  /* this code is coupled with the code that restores this
     behaviour in scwmgtkhelper.c's restore_scwm_handlers */
  newhandler(SIGQUIT);
  newhandler(SIGTERM);
  /* GJB:FIXME:: I seem to lose the last stack frame in my backtrace if this is
     set... do others not see this? --07/24/98 gjb */
  if (fHandleSegv)
    newsegvhandler(SIGSEGV);

  signal(SIGUSR1, Restart);
  
  ReapChildren();
  
  if (!(dpy = XOpenDisplay(display_name))) {
    scwm_msg(ERR, "main", "can't open display %s", XDisplayName(display_name));
    exit(1);
  }
  Scr.screen = DefaultScreen(dpy);
  Scr.NumberOfScreens = ScreenCount(dpy);

  master_pid = getpid();

  if (!single) {
    int myscreen = 0;
    char *cp;
    
    strcpy(message, XDisplayString(dpy));
    
    for (i = 0; i < Scr.NumberOfScreens; i++) {
      if (i != Scr.screen && fork() == 0) {
	myscreen = i;
	
	/*
	 * Truncate the string 'whatever:n.n' to 'whatever:n',
	 * and then append the screen number.
	 */
	cp = strchr(message, ':');
	if (cp != NULL) {
	  cp = strchr(cp, '.');
	  if (cp != NULL)
	    *cp = '\0';         /* truncate at display part */
	}
	sprintf(message + strlen(message), ".%d", myscreen);
	dpy = XOpenDisplay(message);
        if (!dpy) {
          scwm_msg(ERR,"main",
                   "Could not open display %s",message);
        }
	Scr.screen = myscreen;
	Scr.NumberOfScreens = ScreenCount(dpy);
	
	break;
      }
    }
  }
  x_fd = XConnectionNumber(dpy);
  fd_width = 0;
  
  if (fcntl(x_fd, F_SETFD, 1) == -1) {
    scwm_msg(ERR, "main", "close-on-exec failed");
    exit(1);
  }
  /*  Add a DISPLAY entry to the environment, incase we were started
   * with scwm --display term:0.0
   */
  len = strlen(XDisplayString(dpy));
  display_string = NEWC(len+10,char);
  sprintf(display_string, "DISPLAY=%s", XDisplayString(dpy));
  putenv(display_string);
  /* Add a HOSTDISPLAY environment variable, which is the same as
   * DISPLAY, unless display = :0.0 or unix:0.0, in which case the full
   * host name will be used for ease in networking . */
  /* Note: Can't free the rdisplay_string after putenv, because it
   * becomes part of the environment! */
  if (strncmp(display_string, "DISPLAY=:", 9) == 0) {
    char client[MAXHOSTNAME];
    char *rdisplay_string = NULL;
    
    gethostname(client, MAXHOSTNAME);
    rdisplay_string = NEWC(len + 14 + strlen(client), char);
    sprintf(rdisplay_string, "HOSTDISPLAY=%s:%s", client, &display_string[9]);
    putenv(rdisplay_string);
    FREEC(rdisplay_string);
  } else if (strncmp(display_string, "DISPLAY=unix:", 13) == 0) {
    char client[MAXHOSTNAME];
    char *rdisplay_string = NULL;
    
    gethostname(client, MAXHOSTNAME);
    rdisplay_string = NEWC(len + 14 + strlen(client), char);
    sprintf(rdisplay_string, "HOSTDISPLAY=%s:%s", client,
	    &display_string[13]);
    putenv(rdisplay_string);
    FREEC(rdisplay_string);
  } else {
    char *rdisplay_string = NEWC(len + 14, char);
    sprintf(rdisplay_string, "HOSTDISPLAY=%s", XDisplayString(dpy));
    putenv(rdisplay_string);
    FREEC(rdisplay_string);
  }
  
  Scr.Root = RootWindow(dpy, Scr.screen);
  if (Scr.Root == None) {
    scwm_msg(ERR, "main", "Screen %d is not a valid screen", (char *) Scr.screen);
    exit(1);
  }
#ifdef HAVE_SHAPE
  ShapesSupported = XShapeQueryExtension(dpy, &ShapeEventBase, &ShapeErrorBase);
#endif

#ifdef HAVE_XTEST
  XTestSupported = XTestQueryExtension(dpy, &XTestEventBase, &XTestErrorBase,
                                       &XTestMajorP,&XTestMinorP);
#endif
  
  if (debugging)
    XSynchronize(dpy, True);
  /* Need to do this after Scr.Root gets set */
  InternUsefulAtoms();
  CreateScmGlobalCursors();
  SetRestartingGlobal();
  ResetScwmexecProtocol();
  init_modifiers();
  init_pointer_mapping();
#ifdef HAVE_LIBSM_LIBICE
  initSM();
#endif
  
  /* Make sure property priority colors is empty */
  XChangeProperty(dpy, Scr.Root, XA_MIT_PRIORITY_COLORS,
		  XA_CARDINAL, 32, PropModeReplace, NULL, 0);

  /* Initialize scwmexec response window (used in shutdown.c's Done,
     as well as by HandleScwmExec) */
  w_for_scwmexec_response = None;
  /* Announce support for scwmexec protocol. */
  XChangeProperty(dpy, Scr.Root, 
		  XA_SCWMEXEC_LISTENER, XA_STRING,
		  8, PropModeReplace, (unsigned char *) "scwm", 5);
  
  XSetErrorHandler((XErrorHandler) CatchRedirectError);
  XSetIOErrorHandler((XIOErrorHandler) CatchFatal);
  XSelectInput(dpy, Scr.Root,basic_event_mask);
  XSync(dpy, False);
  
  XSetErrorHandler((XErrorHandler) ScwmErrorHandler);
  
  BlackoutScreen();           /* if they want to hide the capture/startup */
  
  InitVariables();
  init_message_window();
  init_image_colormap();
  init_borders();
  init_resize_gcs();
  XrmInitialize();
  init_xrm();
  init_image();

  InitEventHandlerJumpTable();


  { /* scope */
    /* create a window which will accept the keyboard focus when no other 
       windows have it */
    XSetWindowAttributes attributes;	/* attributes for create windows */
    attributes.event_mask = KeyPressMask | KeyReleaseMask | FocusChangeMask;
    attributes.override_redirect = True;
    Scr.NoFocusWin = XCreateWindow(dpy, Scr.Root, -10, -10, 10, 10, 0, 0,
                                   InputOnly, CopyFromParent,
                                   CWEventMask | CWOverrideRedirect,
                                   &attributes);
    XMapWindow(dpy, Scr.NoFocusWin);
  
    SetMWM_INFO(Scr.NoFocusWin);
#ifdef GNOME_SUPPORT_IN_C
    AnnounceGnomeCompliancy(Scr.NoFocusWin);
#endif

  } /* end scope */

  
  Scr.gray_bitmap =
    XCreateBitmapFromData(dpy, Scr.Root, g_bits, g_width, g_height);
  

  /* Add the SCWM_LOAD_PATH preprocessor symbol and evironment
     variable to the guile load path. */
  
  init_scwm_load_path();
  
  DBUG((DBG,"main", "Setting up rc file defaults..."));
  
  /* the default for this seems to have changed between guile-1.3
     and guile-1.3.2;  only the first clause is needed when 
     we drop support for guile-1.3.2 */
  if (!fDisableBacktrace) {
    gh_eval_str("(debug-enable 'debug) (read-enable 'positions)");
  } else {
    gh_eval_str("(debug-disable 'debug) (read-disable 'positions)");
  }
  
  /* the compiled-in .scwmrc comes from minimal.scm,
     built into init_scheme_string.c by the make file */
  { /* scope */
    extern char *init_scheme_string;
    scwm_safe_eval_str(init_scheme_string);
  } /* end scope */
  
  if (!dumped) {
    DBUG((DBG,"main", "Running config_commands..."));
    
#ifndef SCWMRC
#define SCWMRC ".scwmrc"
#endif
    
    /* MS:FIXME:: clean this ugly mess up. */
    /* GJB:FIXME:: look for $HOME/scwm/.scwmrc, too */
    if (strlen(szCmdConfig) == 0) {
#ifdef I18N
      scwm_safe_eval_str(
           "(let ((home-scwmrc"
	   "       (string-append (getenv \"HOME\") \"/\" \"" SCWMRC "\"))"
	   "      (system-scwmrc \"" SCWMRCDIR "/system" SCWMRC "\"))"
	   " (if (access? (string-append home-scwmrc \".\" locale-fullname) R_OK)"
	   "     (safe-load (string-append home-scwmrc \".\" locale-fullname))"
           "     (if (access? (string-append home-scwmrc \".\" locale-language-territory) R_OK)"
           "         (safe-load (string-append home-scwmrc \".\" locale-language-territory))"
	   "         (if (access? home-scwmrc R_OK)"
	   "             (safe-load home-scwmrc)"
	   "             (if (access? (string-append system-scwmrc \".\" locale-fullname) R_OK)"
	   "                 (safe-load (string-append system-scwmrc \".\" locale-fullname))"
	   "                 (if (access? (string-append system-scwmrc \".\" locale-language-territory) R_OK)"
	   "                     (safe-load (string-append system-scwmrc \".\" locale-language-territory))"
	   "                     (if (access? system-scwmrc R_OK)"
	   "                         (safe-load system-scwmrc))))))))");
#else
      scwm_safe_eval_str("(let ((home-scwmrc"
                         "       (string-append (getenv \"HOME\") \"/\" \"" SCWMRC "\"))"
                         "      (system-scwmrc \"" SCWMRCDIR "/system" SCWMRC "\"))"
                         " (if (access? home-scwmrc R_OK)"
                         "     (safe-load home-scwmrc)"
                         "     (if (access? system-scwmrc R_OK)"
                         "         (safe-load system-scwmrc))))");
#endif
    } else {
      scwm_safe_eval_str(szCmdConfig);
    }
    
    FREEC(szCmdConfig);
    
  }

#ifdef ENABLE_DUMP  
  if (fShouldDump) {
    dumped = 1;
    fShouldDump = False;
    dodump(szBinaryPath,szDumpFile);
    exit(0); /* not reached */
  }
#endif

#ifdef ENABLE_DUMP
  if (dumped) {
    scwm_init_after_dump();
  }
#endif
  restart_vp_offset_x = 
    NFromXPropertyCardinal(Scr.Root, XA_SCWM_VIEWPORT_OFFSET_X, 
                           False,0);
  restart_vp_offset_y = 
    NFromXPropertyCardinal(Scr.Root, XA_SCWM_VIEWPORT_OFFSET_Y, 
                           False,0);
  CaptureAllWindows();

  MoveViewport_internal(restart_vp_offset_x,restart_vp_offset_y);

  restart_vp_offset_x = restart_vp_offset_y = 0;
  
  DBUG((DBG,"main", "Done running config_commands"));

  if (Scr.d_depth < 2) {
    Scr.gray_pixmap =
      XCreatePixmapFromBitmapData(dpy, Scr.Root, g_bits, g_width, g_height,
				  XCOLOR(Scr.NotMenuColors.fg), 
				  XCOLOR(Scr.NotMenuColors.bg),
				  Scr.d_depth);
    Scr.light_gray_pixmap =
      XCreatePixmapFromBitmapData(dpy, Scr.Root, l_g_bits, l_g_width, l_g_height,
				  XCOLOR(Scr.NotMenuColors.fg),
				  XCOLOR(Scr.NotMenuColors.bg),
				  Scr.d_depth);
  }
  
  XSync(dpy, False);

  initPanFrames();

  XGrabServer_withSemaphore(dpy);
  checkPanFrames();
  XUngrabServer_withSemaphore(dpy);

  UnBlackoutScreen();         /* if we need to remove blackout window */
  /* set the focus to the current window if appropriate */
  CoerceEnterNotifyOnCurrentWindow();

  run_startup_hook();

  fDoneStartup = True;

  scwm_maybe_send_thankyou_packet();

  DBUG((DBG,"main", "Entering HandleEvents loop..."));

  sigsetjmp(envHandleEventsLoop,1);
  /* GJB:FIXME:: Hubert Canon reports that he needs
     this following line because sigsetjmp on Solaris
     is corrupting several of the global variable values!
     InternUsefulAtoms(); 
  */

  /* BEWARE: this code is coupled with the code that restores this
     behaviour in scwmgtkhelper.c's restore_scwm_handlers */
  newhandler_doreset(SIGHUP);
  newhandler_doreset(SIGFPE);
/*  signal(SIGPIPE,DeadPipe); */
#ifdef SCWM_RESET_ON_SIGINT
  newhandler_doreset(SIGINT);
#else
  newhandler(SIGINT);
#endif
  HandleEvents();
  DBUG((DBG,"main", "Back from HandleEvents loop?  Exitting..."));
  return;
}



/*
 * MappedNotOverride - checks to see if we should really
 *		put a scwm frame on the window
 *
 *  Returned Value:
 *	True	- go ahead and frame the window
 *	False	- don't frame the window
 *
 *  Inputs:
 *	w	- the window to check
 */

static int 
MappedNotOverride(Window w)
{
  XWindowAttributes wa;
  Atom atype;
  int aformat;
  unsigned long nitems, bytes_remain;
  unsigned char *prop;

  isIconicState = DontCareState;

  if ((w == Scr.NoFocusWin) || (!XGetWindowAttributes(dpy, w, &wa)))
    return False;

  if (XGetWindowProperty(dpy, w, XA_WM_STATE, 0L, 3L, False, XA_WM_STATE,
	      &atype, &aformat, &nitems, &bytes_remain, &prop) == Success) {
    if (prop != NULL) {
      isIconicState = *(long *) prop;
      XFree(prop);
    }
  }
  if (wa.override_redirect == True) {
    XSelectInput(dpy, w, FocusChangeMask);
  }
  return (((isIconicState == IconicState) || (wa.map_state != IsUnmapped)) &&
	  (wa.override_redirect != True));
}


/*
 *  Procedure:
 *      CaptureAllWindows
 *
 *   Decorates all windows at start-up and during recaptures
 * AddWindow gets called by X11
 * when the windows make their request to be mapped
 */
void 
CaptureAllWindows(void)
{
  int i, j;
  unsigned int nchildren;
  Window root, parent, *children;
  ScwmWindow *psw, *pswNext;	/* temp scwm window structure */
  Window w;
  unsigned long data[1];
  unsigned char *prop;
  Atom atype;
  int aformat;
  unsigned long nitems, bytes_remain;

  XGrabServer_withSemaphore(dpy);

  if (!XQueryTree(dpy, Scr.Root, &root, &parent, &children, &nchildren)) {
    XUngrabServer_withSemaphore(dpy);
    return;
  }
  PPosOverride = True;

  if (!Scr.fWindowsCaptured) {		/* initial capture? */
    /* weed out icon windows */
    for (i = 0; i < (int) nchildren; i++) {
      if (children[i]) {
	XWMHints *wmhintsp = XGetWMHints(dpy, children[i]);

	if (wmhintsp) {
	  if (wmhintsp->flags & IconWindowHint) {
	    for (j = 0; j < (int) nchildren; j++) {
	      if (children[j] == wmhintsp->icon_window) {
		children[j] = None;
		break;
	      }
	    }
	  }
	  XFree(wmhintsp);
	}
      }
    }
    /* map all of the non-override, non-icon windows,
       non message-window windows */
    for (i = 0; i < (int) nchildren; i++) {
      if (children[i] && MappedNotOverride(children[i])) {
        XPointer msg = NULL;
        /* only continue if not a message window */
        if ( XFindContext(dpy, children[i],
                          MsgWindowContext, &msg) != 0 || msg == NULL ) {
          XUnmapWindow(dpy, children[i]);
          Event.xmaprequest.window = children[i];
          HandleMapRequestKeepRaised(BlackoutWin);
        }
      }
    }
    Scr.fWindowsCaptured = True;
  } else {			/* must be a recapture */
    /* reborder all windows */
    psw = Scr.ScwmRoot.next;
    for (i = 0; i < (int) nchildren; i++) {
      psw = PswFromWindow(dpy,children[i]);
      if (psw) { 
	isIconicState = DontCareState;
	if (XGetWindowProperty(dpy, psw->w, XA_WM_STATE, 0L, 3L, False,
			       XA_WM_STATE, &atype, &aformat, 
			       &nitems, &bytes_remain, &prop) == Success) {
	  if (prop != NULL) {
	    isIconicState = *(long *) prop;
	    XFree(prop);
	  }
	}
	pswNext = psw->next;
	data[0] = (unsigned long) psw->Desk;
	XChangeProperty(dpy, psw->w, XA_WM_DESKTOP, XA_WM_DESKTOP, 32,
			PropModeReplace, (unsigned char *) data, 1);

	XSelectInput(dpy, psw->w, 0);
	w = psw->w;
	XUnmapWindow(dpy, psw->frame);
	XUnmapWindow(dpy, w);
	RestoreWithdrawnLocation(psw, True);
	DestroyScwmWindow(psw);
        /* This will cause AddWindow to get called for w */
	Event.xmaprequest.window = w;
	HandleMapRequestKeepRaised(BlackoutWin);
	psw = pswNext;
      }
    }
    /* We only need to XSync on a recapture, since the initial capture
       calls XSync itself after we return */
    XSync(dpy, False);
  }

  isIconicState = DontCareState;

  if (nchildren > 0)
    XFree(children);

  /* after the windows already on the screen are in place,
   * don't use PPosition */
  PPosOverride = False;
  KeepOnTop();
  XUngrabServer_withSemaphore(dpy);

}

SIGNAL_T
SigResetLoop(int ARG_IGNORE(ignored))
{
  newhandler_doreset(SIGHUP);
  if (envHandleEventsLoop) {
    scwm_msg(INFO,"SigResetLoop","Got a reset signal, so longjmp-ing to event handler");
    XUngrabServer_withSemaphore(dpy);
    XUngrabPointer(dpy,CurrentTime);
    XUngrabKeyboard(dpy,CurrentTime);
    siglongjmp(envHandleEventsLoop,1 /* ret. val for setjmp */);
  }
  SIGNAL_RETURN;
}


/*
 * newhandler: Installs new signal handler to call SigDone
 */
void 
newhandler(int sig)
{
  if (signal(sig, SIG_IGN) != SIG_IGN) {
    void *pv = signal(sig, SigDone);
    if (pv == SIG_ERR) 
      scwm_msg(WARN,"newhandler","signal returned SIG_ERR");
  }
}

/*
 * newhandler_doreset: Installs new signal handler to reset to main loop
 */
void 
newhandler_doreset(int sig)
{
  if (signal(sig, SIG_IGN) != SIG_IGN) {
    void *pv = signal(sig, SigResetLoop);
    if (pv == SIG_ERR) 
      scwm_msg(WARN,"newhandler_doreset","signal returned SIG_ERR");
  }
}

/*
 * newsegvhandler: Installs new signal handler for segment violations
 */

void 
newsegvhandler(int sig)
{
  if (signal(sig, SIG_IGN) != SIG_IGN) {
    void *pv = signal(sig, SigDoneSegv);
    if (pv == SIG_ERR) 
      scwm_msg(WARN,"newsegvhandler","signal returned SIG_ERR");
  }
}

void 
reset_signal_handler(int sig)
{
  if (signal(sig, SIG_IGN) != SIG_IGN) {
    void *pv = signal(sig, SIG_DFL);
    if (pv == SIG_ERR) 
      scwm_msg(WARN,"reset_signal_handler","signal returned SIG_ERR");
  }
}



/*************************************************************************
 * Restart on a signal
 ************************************************************************/
void 
Restart(int ARG_IGNORE(ignored))
{
  Done(1, *g_argv);
  SIGNAL_RETURN;
}

/* RestoreWithdrawnLocation
 * 
 *  Puts window back where it was before Scwm took over 
 */
void 
RestoreWithdrawnLocation(ScwmWindow *psw, Bool fRestart)
{
#define FUNC_NAME "RestoreWithdrawnLocation"
  XWindowChanges xwc;

  if (!psw)
    return;

  if (FXGetWindowTopLeft(psw->w, &xwc.x, &xwc.y )) {
    unsigned int mask;
    /* Undo gravity adjustments. */
    xwc.x = FRAME_X_VP(psw) - GRAV_X_ADJUSTMENT(psw);
    xwc.y = FRAME_Y_VP(psw) - GRAV_Y_ADJUSTMENT(psw);

    xwc.border_width = psw->old_bw;
    mask = (CWX | CWY | CWBorderWidth);

#ifdef DEBUG_RESTORE
    scwm_msg(DBG,FUNC_NAME,"Reparenting %s to %d,%d",
             psw->name, xwc.x, xwc.y);
#endif
    XReparentWindow(dpy, psw->w, Scr.Root, xwc.x, xwc.y);

    if (psw->fIconified && !(psw->fSuppressIcon)) {
      if (psw->icon_w)
	XUnmapWindow(dpy, psw->icon_w);
      if (psw->icon_pixmap_w)
	XUnmapWindow(dpy, psw->icon_pixmap_w);
    }
    XConfigureWindow(dpy, psw->w, mask, &xwc);
    if (!fRestart)
      XSync(dpy, False);
  }
}
#undef FUNC_NAME


/*
 * Reborder - Removes scwm border windows
 */
void 
Reborder(Bool fRestart)
{
  ScwmWindow *psw = NULL;		/* temp scwm window structure */
  SCM p;

  /* put a border back around all windows */
  XGrabServer_withSemaphore(dpy);

  InstallWindowColormaps(&Scr.ScwmRoot);	/* force reinstall */

  /* MS:FIXME:: Gratuitous to use list_stacking_order, we should
     perhaps track stacking order explicitly somewhere in the window
     struct. */

  for (p=gh_reverse(list_stacking_order());
       p != SCM_EOL;
       p = SCM_CDR(p)) {
    psw = PSWFROMSCMWIN(SCM_CAR(p));
      
    RestoreWithdrawnLocation(psw, fRestart);
    XUnmapWindow(dpy, psw->frame);
    XDestroyWindow(dpy, psw->frame);
  }

  XUngrabServer_withSemaphore(dpy);
  XSync(dpy, False);
}

/*
 * SigDone - the signal handler installed by newhandler
 *
 * GJB:FIXME:MS: I do not think it is portable to call a function
 * that uses libraries in a signal handler!
 */
SIGNAL_T
SigDone(int ARG_IGNORE(ignored))
{
  Done(0, NULL);
  SIGNAL_RETURN;
}

SIGNAL_T
SigDoneSegv(int ARG_IGNORE(ignored))
{
  if (segvs_to_reset-- > 0) {
    scwm_msg(ERR, "SigDoneSegv","Caught seg fault...\n"
             "please run with '--segv-just-stop' or '--segv-reset-count 0'\n"
             "and report a bug!\n"
             "Trying to continue... save your work if you still can!");
    siglongjmp(envHandleEventsLoop,1);
  } else {
    reset_signal_handler(SIGSEGV);
    scwm_msg(ERR, "SigDoneSegv","Doing some cleanup to restore sanity");
    Done(-1, NULL);
  }
  SIGNAL_RETURN;
}


XErrorHandler 
CatchRedirectError(Display *ARG_IGNORE(dpy), XErrorEvent *ARG_IGNORE(event))
{
  scwm_msg(ERR, "CatchRedirectError", "another WM is running");
  exit(1);
}

/*
 * CatchFatal - Shuts down if the server connection is lost
 */
XIOErrorHandler 
CatchFatal(Display *ARG_IGNORE(dpy))
{
  /* No action is taken because usually this action is caused by someone
     using "xlogout" to be able to switch between multiple window managers
   */
  exit(1);
}


/*
 * ScwmReportX11Error
 * These are real X11 errors that we aren't expecting.
 *
 * Turn on synchronous mode (using --debug at startup) and set a
 * breakpoint here to debug what's causing the error message.  */
static XErrorHandler
ScwmReportX11Error(Display *dpy, XErrorEvent *event) 
{
#ifdef HAVE_LIBXMU
  XmuPrintDefaultErrorMessage(dpy,event,stderr);
#else
  { /* scope */
    extern int last_event_type;
    scwm_msg(ERR, "ScwmErrorHandler", "*** X11 error ***\nRequest %d, Error %d, EventType: %d",
             event->request_code,
             event->error_code,
             last_event_type);
  }
#endif
  return 0;
}


/*
 * ScwmErrorHandler - handle internal X11 errors
 * This just filters out a bunch, and passes the
 * meanies off to ScwmReportX11Error
 */
XErrorHandler 
ScwmErrorHandler(Display * dpy, XErrorEvent * event)
{
  /* some errors are acceptable, mostly they're caused by 
   * trying to update a lost  window */
  if ((event->error_code == BadWindow) || 
      (event->request_code == X_GetGeometry) ||
      (event->error_code == BadDrawable) || 
      (event->request_code == X_SetInputFocus) ||
      (event->request_code == X_GrabButton) ||
      (event->request_code == X_ChangeWindowAttributes) ||
      (event->request_code == X_InstallColormap))
    return 0;

  return ScwmReportX11Error(dpy,event);
}

void 
usage(void)
{
  fprintf(stderr, "\nScwm Version %s\n", VERSION);
  fprintf(stderr, "Usage: %s [--display|-d dpy]\n"
	  "      [--expression|-e expression]\n"
	  "      [--file|-f rc_file] [--single-screen|-s]\n"
          "      [--gc-often|-g] [--debug|-D]\n"
	  "      [--nobacktrace|-n] [--segv-cleanup-and-stop|-p] [--segv-just-stop|-P]\n"
          "      [--no-document-formals|-N]\n"
	  "      [--blackout|-b] [--" CLIENT_ID_STRING " id]\n"
	  "      [--version|-V] [--help|-h]\n"
	  , g_argv[0]);
}



void 
SetMWM_INFO(Window ARG_UNUSED(window))
{
/* GJB:FIXME:: make this a per-window runtime option */
#ifdef MODALITY_IS_EVIL
  struct mwminfo {
    long flags;
    Window win;
  } motif_wm_info;

  /* Set Motif WM_INFO atom to make motif relinquish 
   * broken handling of modal dialogs */
  motif_wm_info.flags = 2;
  motif_wm_info.win = window;

  XChangeProperty(dpy, Scr.Root, XA_MOTIF_WM, XA_MOTIF_WM, 32,
		  PropModeReplace, (char *) &motif_wm_info, 2);
#endif
}


void 
BlackoutScreen()
{
  XSetWindowAttributes attributes;
  unsigned long valuemask;

  if (Blackout && (BlackoutWin == None) && !debugging) {
    DBUG((DBG,"BlackoutScreen", "Blacking out screen during init..."));
    /* blackout screen */
    attributes.border_pixel = BlackPixel(dpy, Scr.screen);
    attributes.background_pixel = BlackPixel(dpy, Scr.screen);
    attributes.bit_gravity = NorthWestGravity;
    attributes.override_redirect = True;	/* is override redirect needed? */
    valuemask = CWBorderPixel |
      CWBackPixel |
      CWBitGravity |
      CWOverrideRedirect;
    BlackoutWin = XCreateWindow(dpy, Scr.Root, 0, 0,
				DisplayWidth(dpy, Scr.screen),
				DisplayHeight(dpy, Scr.screen), 0,
				CopyFromParent,
				CopyFromParent, CopyFromParent,
				valuemask, &attributes);
    XMapWindow(dpy, BlackoutWin);
    XSync(dpy, False);
  }
}				/* BlackoutScreen */

void 
UnBlackoutScreen()
{
  if (Blackout && (BlackoutWin != None) && !debugging) {
    DBUG((DBG,"UnBlackoutScreen", "UnBlacking out screen"));
    XDestroyWindow(dpy, BlackoutWin);	/* unblacken the screen */
    XSync(dpy, False);
    BlackoutWin = None;
  }
}				/* UnBlackoutScreen */


SCM
scwm_make_gsubr(const char *name, int req, int opt, int var, SCM (*fcn)(), char *szArgList)
{
  static SCM sym_arglist = SCM_UNDEFINED;
  if (SCM_UNDEFINED == sym_arglist)
    sym_arglist = scm_permanent_object(((scm_cell *)scm_intern0("arglist"))->car);
  { /* scope */
  SCM p = scm_make_gsubr(name,req,opt,var,fcn);
  if (fDocumentPrimitiveFormals) {
    SCM arglist = gh_eval_str(szArgList);
    scm_permanent_object(arglist);
    scm_set_procedure_property_x(p,sym_arglist,arglist);
  }
  return p;
  }
}

SCM
scwm_make_igsubr(const char *name, int req, int opt, int var,
                 SCM (*fcn)(), char *szInteractiveSpecification, char *szArgList)
{
  extern SCM sym_interactive;
  /* GJB:FIXME:: a hack to guarantee that this is initialized
     since events.c has the SCWM_GLOBAL_SYMBOL definition of it */
  if (SCM_BOOL_F == sym_interactive)
    sym_interactive = 
      scm_permanent_object(((scm_cell *)scm_intern0("interactive"))->car);
  { /* scope */
    SCM p = scwm_make_gsubr(name,req,opt,var,fcn,szArgList);
    scm_set_procedure_property_x(p,sym_interactive,
                                 szInteractiveSpecification?
                                 gh_str02scm(szInteractiveSpecification):SCM_BOOL_T);
    return p;
  }
}


void init_scwm_load_path()
{
  SCM *pscm_pct_load_path;
  SCM path;

  SCWM_VAR(pct_load_path,"%load-path");
  /** The internal compiled-in loading path for scwm. */

  path = *pscm_pct_load_path;
  path = gh_cons(gh_str02scm(SCWM_LOAD_PATH),path);
  path = gh_cons(gh_str02scm(SCWM_BIN_LOAD_PATH),path);
  path = scm_internal_parse_path(getenv("SCWM_LOAD_PATH"), path);
  *pscm_pct_load_path = path;
}


/*
   ** Scwm_msg: used to send output from Scwm to files and or stderr/stdout
   **
   ** type -> DBG == Debug, ERR == Error, INFO == Information, WARN == Warning
   ** id -> name of function, or other identifier
 */
void 
scwm_msg(scwm_msg_levels type, const char *id, const char *msg,...)
{
  char *typestr;
  va_list args;
  int length_printed = 0;

  switch (type) {
  case DBG:
    typestr = "<<DEBUG>>";
    break;
  case ERR:
    typestr = "<<ERROR>>";
    break;
  case WARN:
    typestr = "<<WARNING>>";
    break;
  case INFO:
  default:
    typestr = "";
    break;
  }

  va_start(args, msg);

  length_printed += fprintf(stderr, "[Scwm][%s]: %s ", id, typestr);
  length_printed += vfprintf(stderr, msg, args);
  length_printed += fprintf(stderr, "\n");

  if (type == ERR) {
    char *sz = malloc( (length_printed+2) * sizeof(char));
    sprintf(sz, "[Scwm][%s]: %s ", id, typestr);
    vsprintf(sz + strlen(sz), msg, args);
    strcat(sz,"\n");
    BroadcastName(M_FVWM_ERROR, 0, 0, 0, sz);
    FREE(sz);
  }
  va_end(args);
} /* scwm_msg */


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta
 */
