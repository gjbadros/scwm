/* $Id$
 * scwm.c
 * (C) 1997, 1998 Maciej Stachowiak and Greg J. Badros
 */

/****************************************************************************
 * This module has been significantly modified from fvwm2
 * It may be used under the terms indicated by the copyright below.
 * Changes Copyright 1997, 1998, Maciej Stachowiak and Greg J. Badros
 ****************************************************************************/

/*
 * This module is derived from all original code by Rob Nation 
 * Copyright 1993, Robert Nation
 *     You may use this code for any purpose, as long as the original
 *     copyright remains in the source code and all documentation
 */

/***********************************************************************
 * scwm - "Scheme Configurable Window Manager"
 * based on fvwm2 with Guile Scheme used as the configuration 
 * language.
 ***********************************************************************/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>

#include "scwm.h"
#include <stdarg.h>
#include <guile/gh.h>
#include "guile-compat.h"
#include "syscompat.h"
#include "scwmpaths.h"
#include "scm_init_funcs.h"
#ifdef USE_CASSOWARY
void init_cassowary_scm();           /* from the cassowary distribution */
#endif

#include "screen.h"
#include "window.h"
#include "decor.h"
#include "image.h"
#include "menu.h"
#include "events.h"
#include "callbacks.h"
#include "font.h"
#include "resize.h"
#include "virtual.h"
#include "Grab.h"
#include "shutdown.h"
#include "xmisc.h"
#include "colormaps.h"
#include "module-interface.h"

#include <X11/Xlib.h>
#include <X11/Xproto.h>
#include <X11/Xatom.h>
/* need to get prototype for XrmUniqueQuark for XUniqueContext call */
#include <X11/Xresource.h>
#include <X11/extensions/shape.h>

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
#include <locale.h>
#endif

#define MAXHOSTNAME 255

static char rcsid[] = "$Id$";

int master_pid;			/* process number of 1st scwm process */

ScreenInfo Scr;			/* structures for the screen */
SCM scmScreen;                  /* the scheme object wrapping Scr */
Display *dpy;			/* which display are we talking to */

Window BlackoutWin = None;	/* window to hide window captures */

char *szCmdConfig;

static char *szLoad_pre = "(safe-load \"";
static char *szLoad_post = "\")";


char *output_file = NULL;

XErrorHandler ScwmErrorHandler(Display *, XErrorEvent *);
XIOErrorHandler CatchFatal(Display *);
XErrorHandler CatchRedirectError(Display *, XErrorEvent *);

static void newhandler(int sig);
static void newsegvhandler(int sig);

void CreateCursors(void);
void ChildDied(int nonsense);
void SetMWM_INFO(Window window);
void StartupStuff(void);
void usage(void);

XContext ScwmContext;		/* context for scwm windows */
XContext MenuContext;           /* context for menus */

Window JunkRoot, JunkChild;	/* junk window */
int JunkX = 0, JunkY = 0;
unsigned int JunkWidth, JunkHeight, JunkBW, JunkDepth, JunkMask;

ScwmWindow *FocusOnNextTimeStamp = NULL;

Boolean debugging = False, PPosOverride, Blackout = False;

char **g_argv;
int g_argc;

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
Boolean ShapesSupported = False;


long isIconicState = 0;
extern XEvent Event;
Bool Restarting = False;
int fd_width, x_fd;
char *display_name = NULL;

void scwm_main(int, char **);

typedef void (*main_prog_t) (int argc, char **argv);

Atom XA_MIT_PRIORITY_COLORS;
Atom XA_WM_CHANGE_STATE;
Atom XA_WM_STATE;
Atom XA_WM_COLORMAP_WINDOWS;
extern Atom XA_WM_PROTOCOLS;
Atom XA_WM_TAKE_FOCUS;
Atom XA_WM_DELETE_WINDOW;
Atom XA_WM_DESKTOP;
Atom XA_MwmAtom;
Atom XA_MOTIF_WM;

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
Atom XA_SCWM_EXECUTE;
Atom XA_SCWM_RESULT;
Atom XA_SCWMEXEC_LISTENER;
Atom XA_SCWMEXEC_REQWIN;
Atom XA_SCWMEXEC_REQUEST;
Atom XA_SCWMEXEC_REPLY;
Atom XA_SCWMEXEC_NOTIFY;
Atom XA_SCWMEXEC_OUTPUT;
Atom XA_SCWMEXEC_ERROR;


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
  XA_SCWM_EXECUTE = XInternAtom(dpy, "SCWM_EXECUTE", False);
  XA_SCWM_RESULT = XInternAtom(dpy, "SCWM_RESULT", False);
  XA_SCWMEXEC_LISTENER=XInternAtom(dpy,"SCWMEXEC_LISTENER", False);
  XA_SCWMEXEC_REQWIN=XInternAtom(dpy,"SCWMEXEC_REQWIN", False);
  XA_SCWMEXEC_REQUEST=XInternAtom(dpy,"SCWMEXEC_REQUEST", False);
  XA_SCWMEXEC_REPLY=XInternAtom(dpy,"SCWMEXEC_REPLY", False);
  XA_SCWMEXEC_NOTIFY=XInternAtom(dpy,"SCWMEXEC_NOTIFY", False);
  XA_SCWMEXEC_OUTPUT=XInternAtom(dpy,"SCWMEXEC_OUTPUT", False);
  XA_SCWMEXEC_ERROR=XInternAtom(dpy,"SCWMEXEC_ERROR", False);

  return;
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

  Scr.TransMaskGC = XCreateGC(dpy, Scr.Root, gcm, &gcv);
}


/*
 * InitVariables - initialize scwm variables
 */
static void 
InitVariables(void)
{
  ScwmContext = XUniqueContext();
  MenuContext = XUniqueContext();

  /* initialize some lists */
  Scr.AllBindings = NULL;
  Scr.DefaultIcon = NULL;

  /* create graphics contexts */
  CreateGCs();

  SCM_DEFER_INTS;
  scmScreen = ScmFromPScreenInfo(&Scr);
  scm_protect_object(scmScreen);
  SCM_ALLOW_INTS;

  Scr.d_depth = DefaultDepth(dpy, Scr.screen);
  Scr.ScwmRoot.w = Scr.Root;
  Scr.ScwmRoot.next = 0;
  XGetWindowAttributes(dpy, Scr.Root, &(Scr.ScwmRoot.attr));
  Scr.root_pushes = 0;
  Scr.pushed_window = &Scr.ScwmRoot;
  Scr.ScwmRoot.number_cmap_windows = 0;


  Scr.MyDisplayWidth = DisplayWidth(dpy, Scr.screen);
  Scr.MyDisplayHeight = DisplayHeight(dpy, Scr.screen);

  Scr.NoBoundaryWidth = 1;
  Scr.BoundaryWidth = BOUNDARY_WIDTH;
  Scr.CornerWidth = CORNER_WIDTH;
  Scr.Hilite = NULL;
  Scr.Focus = NULL;
  Scr.Ungrabbed = NULL;

  Scr.menu_font = SCM_UNDEFINED;
  Scr.icon_font = SCM_UNDEFINED;
  Scr.msg_window_font = make_font(str_fixed);
  Scr.msg_window_fg = BLACK_COLOR;
  Scr.msg_window_bg = WHITE_COLOR;
  Scr.MenuColors.bg = SCM_UNDEFINED;
  Scr.DefaultDecor.HiColors.bg = SCM_UNDEFINED;

#ifndef NON_VIRTUAL
  Scr.VxMax = 2 * Scr.MyDisplayWidth;
  Scr.VyMax = 2 * Scr.MyDisplayHeight;
#else
  Scr.VxMax = 0;
  Scr.VyMax = 0;
#endif
  Scr.Vx = Scr.Vy = 0;

  Scr.SizeWindow = None;

  /* Sets the current desktop number to zero */
  /* Multiple desks are available even in non-virtual
   * compilations */
  {
    Atom atype;
    int aformat;
    unsigned long nitems, bytes_remain;
    unsigned char *prop;

    Scr.CurrentDesk = 0;
    if ((XGetWindowProperty(dpy, Scr.Root, XA_WM_DESKTOP, 0L, 1L, True,
			    XA_WM_DESKTOP, &atype, &aformat, &nitems,
			    &bytes_remain, &prop)) == Success) {
      if (prop != NULL) {
	Restarting = True;
	Scr.CurrentDesk = *(unsigned long *) prop;
      }
    }
  }

  Scr.EdgeScrollX = Scr.EdgeScrollY = 100;
  Scr.ScrollResistance = Scr.MoveResistance = 0;
  Scr.OpaqueSize = 5;
  Scr.ClickTime = 150;
  Scr.ColormapFocus = COLORMAP_FOLLOWS_MOUSE;

  /* set major operating modes */
  Scr.NumBoxes = 0;

  Scr.randomx = Scr.randomy = 0;
  Scr.buttons2grab = 7;

  decor2scm(&Scr.DefaultDecor);
  DECORREF(Scr.DefaultDecor.scmdecor);

  Scr.DefaultDecor.tag = "default";

  Scr.SmartPlacementIsClever = False;
  Scr.ClickToFocusPassesClick = True;
  Scr.ClickToFocusRaises = True;
  Scr.MouseFocusClickRaises = False;

  init_image_colormap();
  return;
}



/*
 *  Procedures:
 *	scwm_gh_enter, scwm_gh_launch_pad - Replacement for gh_enter that 
 *      guarantees loading of boot-9.scm
 */

static void 
scwm_gh_launch_pad (void *closure, int argc, char **argv)
{
  main_prog_t c_main_prog = (main_prog_t) closure;

  gh_eval_str ("(primitive-load-path \"ice-9/boot-9.scm\")");
  c_main_prog (argc, argv);
  exit (0);
}

static void 
scwm_gh_enter (int argc, char *argv[], main_prog_t c_main_prog)
{
  scm_boot_guile (argc, argv, scwm_gh_launch_pad, (void *) c_main_prog);
  /* never returns */
}

/*
 *  Procedure:
 *	main - Enters scwm_main using the gh_enter convention.
 */

int
main(int argc, char **argv)
{
  scwm_gh_enter(argc, argv, scwm_main);
  return 0;
}


/*
 * scwm_main - main routine for scwm
 */
void 
scwm_main(int argc, char **argv)
{
  int i;
  extern int x_fd;
  int len;

  /* getopt vars */
  int getopt_ret;
  char *getopt_opts;
  extern char *optarg;
  extern int optind, opterr, optopt;

  char *display_string;
  char message[255];
  Bool single = False;
  Bool option_error = False;

#ifdef I18N
  char *Lang,*territory,*tmp;
#endif

  gh_eval_str ("(define-module (guile))");  

#ifdef I18N
  if ((Lang = setlocale (LC_CTYPE,"")) == (char *)NULL) {
    scwm_msg(WARN,"main","Can't set specified locale.\n");
    Lang = "C";
  }
  tmp = index(Lang,'.');
  if (tmp) {
      territory = NEWC(tmp-Lang+1,char);
      strncpy(territory,Lang,(tmp-Lang));
      *(territory+(size_t)(tmp-Lang)) = '\0';
  } else {
      territory = Lang;
  }
  scm_sysintern("locale-fullname",gh_str02scm(Lang));
  scm_sysintern("locale-language-territory",gh_str02scm(territory));
#endif

  
  /* Avoid block buffering on stderr, stdout even if it's piped somewhere;
     it's useful to pipe through to grep -v or X-error-describe
     while debugging: FIXGJB: make these runtime options -- also,
     isn't stderr never block bufferred?? */
  setlinebuf(stderr);
  setlinebuf(stdout);
  init_font();
  init_decor();
  init_screen();
  init_callbacks();
  init_add_window();
  init_image();
  init_color();
  init_module_interface();
  init_miscprocs();
  init_menuitem();
  init_menu();
  init_binding();
  init_window();
  init_resize();
  init_face();
  init_shutdown();
  init_xproperty();
  init_events();
  init_deskpage();
  init_placement();
#ifdef USE_CASSOWARY
  init_constraint_primitives();
  init_cassowary_scm();
#endif

  szCmdConfig = NEWC(1,char);
  
  szCmdConfig[0] = '\0';
  
  g_argv = argv;
  g_argc = argc;

  DBUG("main", "Entered, about to parse args");
  
  getopt_opts = "Dsd:f:e:hibV";
  
  while(1) {
    static struct option getopt_longopts[] =
    {
      {"debug", 0, NULL, 'D'},
      {"single-screen", 0, NULL, 's'},
      {"display", 1, NULL, 'd'},
      {"file", 1, NULL, 'f'},
      {"expression", 1, NULL, 'e'},
      {"help", 0, NULL, 'h'},
      {"blackout", 0, NULL, 'b'},
      {"version", 0, NULL, 'V'},
      {NULL, 0, NULL, 0}
    };
    
    getopt_ret = getopt_long(argc, argv, getopt_opts,
			     getopt_longopts, 0);
    if(getopt_ret == EOF) break;
    
    switch(getopt_ret) {
    case 'D':
      debugging = True; break;
    case 's':
      single = True; break;
    case 'd':
      if(optarg == NULL) {
        option_error = True;
      break;
      } else {
        display_name = optarg;
        break;
      }
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
      printf("Scwm Version %s compiled on %s at %s\nRCS_ID=%s\n",
             VERSION, __DATE__, __TIME__, rcsid);
      exit(0);
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
  
  DBUG("main", "Done parsing args");
  
  
  DBUG("main", "Installing signal handlers");
  
  newhandler(SIGINT);
  newhandler(SIGHUP);
  newhandler(SIGQUIT);
  newhandler(SIGTERM);
  /* FIXGJB: I seem to lose the last stack frame in my backtrace if this is
     set... do others not see this? --07/24/98 gjb */
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
   * with scwm -display term:0.0
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
  ShapesSupported = XShapeQueryExtension(dpy, &ShapeEventBase, &ShapeErrorBase);
  
  /* Need to do this after Scr.Root gets set */
  menu_init_gcs();
  InternUsefulAtoms();
  init_modifiers();
  init_pointer_mapping();
  
  /* Make sure property priority colors is empty */
  XChangeProperty(dpy, Scr.Root, XA_MIT_PRIORITY_COLORS,
		  XA_CARDINAL, 32, PropModeReplace, NULL, 0);
  
  /* Announce support for scwmexec protocol. */
  XChangeProperty(dpy, Scr.Root, 
		  XA_SCWMEXEC_LISTENER, XA_STRING,
		  8, PropModeReplace, (unsigned char *) "scwm", 5);
  
  XSetErrorHandler((XErrorHandler) CatchRedirectError);
  XSetIOErrorHandler((XIOErrorHandler) CatchFatal);
  XSelectInput(dpy, Scr.Root,
	       LeaveWindowMask | EnterWindowMask | PropertyChangeMask |
	       SubstructureRedirectMask | KeyPressMask |
	       SubstructureNotifyMask |
	       ButtonPressMask | ButtonReleaseMask);
  XSync(dpy, 0);
  
  XSetErrorHandler((XErrorHandler) ScwmErrorHandler);
  
  BlackoutScreen();           /* if they want to hide the capture/startup */
  
  CreateCursors();
  InitVariables();

  /* must come after variables are init'd */
  Scr.SizeWindow = CreateMessageWindow( BlackPixel(dpy,Scr.screen), 
                                        WhitePixel(dpy,Scr.screen), 
                                        Scr.flags & MWMMenus);


  InitEventHandlerJumpTable();

  
  Scr.gray_bitmap =
    XCreateBitmapFromData(dpy, Scr.Root, g_bits, g_width, g_height);
  
  /* Add the SCWM_LOAD_PATH preprocessor symbol and evironment
     variable to the guile load path. */
  
  init_scwm_load_path();
  
  DBUG("main", "Setting up rc file defaults...");

  /* the compiled-in .scwmrc comes from minimal.scm,
     built into init_scheme_string.c by the make file */
  { /* scope */
  extern char *init_scheme_string;
  scwm_safe_eval_str(init_scheme_string);
  } /* end scope */

  DBUG("main", "Running config_commands...");
  
#ifndef SCWMRC
#define SCWMRC ".scwmrc"
#endif
  
  /* FIXMS: clean this ugly mess up. */
  if (strlen(szCmdConfig) == 0) {
#ifdef I18N
      scwm_safe_eval_str(
           "(let ((home-scwmrc"
	   "       (string-append (getenv \"HOME\") \"/\" \"" SCWMRC "\"))"
	   "      (system-scwmrc \"" SCWMDIR "/system" SCWMRC "\"))"
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
		       "      (system-scwmrc \"" SCWMDIR "/system" SCWMRC "\"))"
		       " (if (access? home-scwmrc R_OK)"
		       "     (safe-load home-scwmrc)"
		       "     (if (access? system-scwmrc R_OK)"
		       "         (safe-load system-scwmrc))))");
#endif
  } else {
    scwm_safe_eval_str(szCmdConfig);
  }
  
  FREEC(szCmdConfig);
  
  CaptureAllWindows();
  
  DBUG("main", "Done running config_commands");

  if (Scr.d_depth < 2) {
    Scr.gray_pixmap =
      XCreatePixmapFromBitmapData(dpy, Scr.Root, g_bits, g_width, g_height,
				  XCOLOR(Scr.MenuColors.fg), 
				  XCOLOR(Scr.MenuColors.bg),
				  Scr.d_depth);
    Scr.light_gray_pixmap =
      XCreatePixmapFromBitmapData(dpy, Scr.Root, l_g_bits, l_g_width, l_g_height,
				  XCOLOR(Scr.MenuColors.fg),
				  XCOLOR(Scr.MenuColors.bg),
				  Scr.d_depth);
  }
  { /* scope */
    /* create a window which will accept the keyboard focus when no other 
       windows have it */
    XSetWindowAttributes attributes;	/* attributes for create windows */
    attributes.event_mask = KeyPressMask | FocusChangeMask;
    attributes.override_redirect = True;
    Scr.NoFocusWin = XCreateWindow(dpy, Scr.Root, -10, -10, 10, 10, 0, 0,
                                   InputOnly, CopyFromParent,
                                   CWEventMask | CWOverrideRedirect,
                                   &attributes);
    XMapWindow(dpy, Scr.NoFocusWin);
  
    SetMWM_INFO(Scr.NoFocusWin);
  
    XSetInputFocus(dpy, Scr.NoFocusWin, RevertToParent, CurrentTime);
  } /* end scope */
  
  XSync(dpy, False);
  if (debugging)
    XSynchronize(dpy, True);

#ifndef NON_VIRTUAL
  initPanFrames();
#endif
  
  
#ifndef NON_VIRTUAL
  XGrabServer_withSemaphore(dpy);
  checkPanFrames();
  XUngrabServer_withSemaphore(dpy);
#endif
  UnBlackoutScreen();         /* if we need to remove blackout window */
  /* set the focus to the current window if appropriate */
  CoerceEnterNotifyOnCurrentWindow();
  run_startup_hook();
  DBUG("main", "Entering HandleEvents loop...");
  HandleEvents();
  DBUG("main", "Back from HandleEvents loop?  Exitting...");
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

  if (!(Scr.flags & WindowsCaptured)) {		/* initial capture? */
    /*
       ** weed out icon windows
     */
    for (i = 0; i < nchildren; i++) {
      if (children[i]) {
	XWMHints *wmhintsp = XGetWMHints(dpy, children[i]);

	if (wmhintsp) {
	  if (wmhintsp->flags & IconWindowHint) {
	    for (j = 0; j < nchildren; j++) {
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
    /*
       ** map all of the non-override, non-icon windows
     */
    for (i = 0; i < nchildren; i++) {
      if (children[i] && MappedNotOverride(children[i])) {
	XUnmapWindow(dpy, children[i]);
	Event.xmaprequest.window = children[i];
	HandleMapRequestKeepRaised(BlackoutWin);
      }
    }
    Scr.flags |= WindowsCaptured;
  } else {			/* must be recapture */
    /* reborder all windows */
    psw = Scr.ScwmRoot.next;
    for (i = 0; i < nchildren; i++) {
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
	Event.xmaprequest.window = w;
	HandleMapRequestKeepRaised(BlackoutWin);
	psw = pswNext;
      }
    }
    /* We only need to XSync on a recapture, since the initial capture
       calls XSync itself after we return */
    XSync(dpy, 0);
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

/***********************************************************************
 *
 *  Procedure:
 *	newhandler: Installs new signal handler
 *
 ************************************************************************/
static void 
newhandler(int sig)
{
  if (signal(sig, SIG_IGN) != SIG_IGN)
    signal(sig, SigDone);
}

static void 
newsegvhandler(int sig)
{
  if (signal(sig, SIG_IGN) != SIG_IGN)
    signal(sig, SigDoneSegv);
}


/*************************************************************************
 * Restart on a signal
 ************************************************************************/
void 
Restart(int nonsense)
{
  Done(1, *g_argv);
  SIGNAL_RETURN;
}

/***********************************************************************
 *
 *  Procedure:
 *	CreateCursors - Loads scwm cursors
 *
 ***********************************************************************
 */
void 
CreateCursors(void)
{
  /* define cursors */
  Scr.ScwmCursors[CURSOR_POSITION] = XCreateFontCursor(dpy, XC_top_left_corner);
  Scr.ScwmCursors[CURSOR_DEFAULT] = XCreateFontCursor(dpy, XC_top_left_arrow);
  Scr.ScwmCursors[CURSOR_SYS] = XCreateFontCursor(dpy, XC_hand2);
  Scr.ScwmCursors[CURSOR_TITLE] = XCreateFontCursor(dpy, XC_top_left_arrow);
  Scr.ScwmCursors[CURSOR_MOVE] = XCreateFontCursor(dpy, XC_fleur);
  Scr.ScwmCursors[CURSOR_MENU] = XCreateFontCursor(dpy, XC_sb_left_arrow);
  Scr.ScwmCursors[CURSOR_WAIT] = XCreateFontCursor(dpy, XC_watch);
  Scr.ScwmCursors[CURSOR_SELECT] = XCreateFontCursor(dpy, XC_dot);
  Scr.ScwmCursors[CURSOR_DESTROY] = XCreateFontCursor(dpy, XC_pirate);
  Scr.ScwmCursors[CURSOR_LEFT] = XCreateFontCursor(dpy, XC_left_side);
  Scr.ScwmCursors[CURSOR_RIGHT] = XCreateFontCursor(dpy, XC_right_side);
  Scr.ScwmCursors[CURSOR_TOP] = XCreateFontCursor(dpy, XC_top_side);
  Scr.ScwmCursors[CURSOR_BOTTOM] = XCreateFontCursor(dpy, XC_bottom_side);
  Scr.ScwmCursors[CURSOR_TOP_LEFT] = XCreateFontCursor(dpy, XC_top_left_corner);
  Scr.ScwmCursors[CURSOR_TOP_RIGHT] = XCreateFontCursor(dpy, XC_top_right_corner);
  Scr.ScwmCursors[CURSOR_BOTTOM_LEFT] = XCreateFontCursor(dpy, XC_bottom_left_corner);
  Scr.ScwmCursors[CURSOR_BOTTOM_RIGHT] = XCreateFontCursor(dpy, XC_bottom_right_corner);
}


/*
 * Reborder - Removes scwm border windows
 */
void 
Reborder(void)
{
  ScwmWindow *psw = NULL;		/* temp scwm window structure */

  /* put a border back around all windows */
  XGrabServer_withSemaphore(dpy);

  InstallWindowColormaps(&Scr.ScwmRoot);	/* force reinstall */
  for (psw = Scr.ScwmRoot.next; psw != NULL; psw = psw->next) {
    RestoreWithdrawnLocation(psw, True);
    XUnmapWindow(dpy, psw->frame);
    XDestroyWindow(dpy, psw->frame);
  }

  XUngrabServer_withSemaphore(dpy);
  XSetInputFocus(dpy, PointerRoot, RevertToPointerRoot, CurrentTime);
  XSync(dpy, 0);

}

/*
 * SigDone - the signal handler installed by newhandler
 *
 * FIXGJB: I don't think this is legit to call a function
 * that uses libraries in a signal handler!
 */
SIGNAL_T
SigDone(int nonsense)
{
  Done(0, NULL);
  SIGNAL_RETURN;
}

SIGNAL_T
SigDoneSegv(int nonsense)
{
  Done(-1, NULL);
  SIGNAL_RETURN;
}


XErrorHandler 
CatchRedirectError(Display * dpy, XErrorEvent * event)
{
  scwm_msg(ERR, "CatchRedirectError", "another WM is running");
  exit(1);
}

/***********************************************************************
 *
 *  Procedure:
 *	CatchFatal - Shuts down if the server connection is lost
 *
 ************************************************************************/
XIOErrorHandler 
CatchFatal(Display * dpy)
{
  /* No action is taken because usually this action is caused by someone
     using "xlogout" to be able to switch between multiple window managers
   */
  exit(1);
}

/***********************************************************************
 *
 *  Procedure:
 *	ScwmErrorHandler - displays info on internal errors
 *
 ************************************************************************/
XErrorHandler 
ScwmErrorHandler(Display * dpy, XErrorEvent * event)
{
  extern int last_event_type;

  /* some errors are acceptable, mostly they're caused by 
   * trying to update a lost  window */
  if ((event->error_code == BadWindow) || (event->request_code == X_GetGeometry) ||
      (event->error_code == BadDrawable) || (event->request_code == X_SetInputFocus) ||
      (event->request_code == X_GrabButton) ||
      (event->request_code == X_ChangeWindowAttributes) ||
      (event->request_code == X_InstallColormap))
    return 0;


  scwm_msg(ERR, "ScwmErrorHandler", "*** internal error ***");
  scwm_msg(ERR, "ScwmErrorHandler", "Request %d, Error %d, EventType: %d",
	   event->request_code,
	   event->error_code,
	   last_event_type);
  return 0;
}

void 
usage(void)
{
  fprintf(stderr, "\nScwm Version %s\n", VERSION);
  fprintf(stderr, "Usage: %s [--display|-d dpy] [--debug]"
	  "[--expression|-e expression]\n"
	  "      [--file|-f rc_file] [--single-screen|-s] \n"
	  "      [--blackout|-b] [--version|-V] [--help|-h]\n"
	  , g_argv[0]);
}



void 
SetMWM_INFO(Window window)
{
/* FIXGJB: make this a per-window runtime option */
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
    DBUG("BlackoutScreen", "Blacking out screen during init...");
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
    XSync(dpy, 0);
  }
}				/* BlackoutScreen */

void 
UnBlackoutScreen()
{
  if (Blackout && (BlackoutWin != None) && !debugging) {
    DBUG("UnBlackoutScreen", "UnBlacking out screen");
    XDestroyWindow(dpy, BlackoutWin);	/* unblacken the screen */
    XSync(dpy, 0);
    BlackoutWin = None;
  }
}				/* UnBlackoutScreen */


void init_scwm_load_path()
{
  SCM *loc_load_path;
  SCM path;

  loc_load_path = SCM_CDRLOC(scm_sysintern0("%load-path"));
  path=*loc_load_path;
  path=gh_cons(gh_str02scm(SCWM_LOAD_PATH),path);
  path=scm_internal_parse_path(getenv("SCWM_LOAD_PATH"), path);
  *loc_load_path = path;
}


/*
   ** Scwm_msg: used to send output from Scwm to files and or stderr/stdout
   **
   ** type -> DBG == Debug, ERR == Error, INFO == Information, WARN == Warning
   ** id -> name of function, or other identifier
 */
void 
scwm_msg(scwm_msg_levels type, char *id, char *msg,...)
{
  char *typestr;
  va_list args;

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

  fprintf(stderr, "[Scwm][%s]: %s ", id, typestr);
  vfprintf(stderr, msg, args);
  fprintf(stderr, "\n");

  if (type == ERR) {
    char tmp[1024];		/* I hate to use a fixed length but this will do for now */

    sprintf(tmp, "[Scwm][%s]: %s ", id, typestr);
    vsprintf(tmp + strlen(tmp), msg, args);
    tmp[strlen(tmp) + 1] = '\0';
    tmp[strlen(tmp)] = '\n';
    BroadcastName(M_ERROR, 0, 0, 0, tmp);
  }
  va_end(args);
}				/* Scwm_msg */


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */

