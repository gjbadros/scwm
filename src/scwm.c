/* $Id$
 * scwm.c
 */

/****************************************************************************
 * This module has been significantly modified by Maciej Stachowiak.
 * It may be used under the terms indicated by the copyright below.
 * Changes Copyright 1997, Maciej stachowiak
 ****************************************************************************/
/****************************************************************************
 * This module is all original code 
 * by Rob Nation 
 * Copyright 1993, Robert Nation
 *     You may use this code for any purpose, as long as the original
 *     copyright remains in the source code and all documentation
 ****************************************************************************/
/***********************************************************************
 * scwm - "Scheme Configurable Window Manager"
 * based largely on fvwm2 with Guile Scheme added as the configuration 
 * language.
 ***********************************************************************/

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>
#ifdef HAVE_GETOPT_H
#include <getopt.h>
#else
#ifdef HAVE_GETOPT_LONG
struct option {
    const char *name;
    int has_flag;
    int *flag;
    int val;
};
int getopt_long(int argc, char *const argv[], const char *optstring,
                const struct option *longopts, int *longindex);
#endif /* ! HAVE_GETOPT_LONG */
/* do something here to replace getopt.h */
#endif /* END HAVE_GETOPT_H */
#include "scwm.h"
#include "menu.h"
#include "menus.h"
#include "misc.h"
#include "screen.h"
#include "window.h"
#include "Grab.h"
#include "system.h"
#include "colors.h"


#include <X11/Xproto.h>
#include <X11/Xatom.h>
/* need to get prototype for XrmUniqueQuark for XUniqueContext call */
#include <X11/Xresource.h>
#include <X11/extensions/shape.h>

#if defined (sparc) && defined (SVR4)
/* Solaris has sysinfo instead of gethostname.  */
#include <sys/systeminfo.h>
#endif

#include <guile/gh.h>
#include "scmtypes.h"
#include "scmprocs.h"
#include "miscprocs.h"
#include "binding.h"
#include "decor.h"
#include "face.h"

#define MAXHOSTNAME 255

#include "../version.h"

static char rcsid[] = "$Id$";

int master_pid;			/* process number of 1st scwm process */

ScreenInfo Scr;			/* structures for the screen */
Display *dpy;			/* which display are we talking to */

Window BlackoutWin = None;	/* window to hide window captures */

#ifdef SCWMRC
char *default_config_command = "Read " SCWMRC;

#else
char *default_config_command = "Read .scwmrc";

#endif
#define MAX_CFG_CMDS 10
char *s_cmd_config;
int interactive = 0;

static char *s_load_pre = "(load \"";
static char *s_load_post = "\")";


char *output_file = NULL;

XErrorHandler ScwmErrorHandler(Display *, XErrorEvent *);
XIOErrorHandler CatchFatal(Display *);
XErrorHandler CatchRedirectError(Display *, XErrorEvent *);
void newhandler(int sig);
void CreateCursors(void);
void ChildDied(int nonsense);
void SaveDesktopState(void);
void SetMWM_INFO(Window window);
void SetRCDefaults(void);
void StartupStuff(void);

XContext ScwmContext;		/* context for scwm windows */
XContext MenuContext;		/* context for scwm menus */

int JunkX = 0, JunkY = 0;
Window JunkRoot, JunkChild;	/* junk window */
unsigned int JunkWidth, JunkHeight, JunkBW, JunkDepth, JunkMask;

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

/***********************************************************************
 *
 *  Procedure:
 *	main - Enters scwm_main using the gh_enter convention.
 *
 ***********************************************************************
 */

void 
main(int argc, char **argv)
{
  gh_enter(argc, argv, scwm_main);
}


/***********************************************************************
 *
 *  Procedure:
 *	scwm_main - start of scwm
 *
 ***********************************************************************
 */
void 
scwm_main(int argc, char **argv)
{
  unsigned long valuemask;	/* mask for create windows */
  XSetWindowAttributes attributes;	/* attributes for create windows */
  void InternUsefulAtoms(void);
  void InitVariables(void);
  int i;
  extern int x_fd;
  int len;
#ifdef HAVE_GETOPT_LONG  
  /* getopt vars */
  int getopt_ret;
  char *getopt_opts;
  extern char *optarg;
  extern int optind, opterr, optopt;
#endif  
  char *display_string;
  char message[255];
  Bool single = False;
  Bool option_error = FALSE;

  init_scwm_types();
  init_miscprocs();
  init_menu();
  init_binding();
  init_window();
  init_face();
  init_scwm_procs();

  s_cmd_config = safemalloc(1 * sizeof(char));

  s_cmd_config[0] = '\0';

  g_argv = argv;
  g_argc = argc;

  DBUG("main", "Entered, about to parse args");

#ifdef HAVE_GETOPT_LONG  
  getopt_opts = "Dsd:f:e:hibV";
  
  while(1) {
      static struct option getopt_longopts[] =
      {
          {"debug", 0, NULL, 'D'},
	  {"single-screen", 0, NULL, 's'},
	  {"display", 1, NULL, 'd'},
	  {"file", 1, NULL, 'f'},
	  {"expression", 1, NULL, 'e'},
	  {"interactive", 0, NULL, 'i'},
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
	  s_cmd_config = realloc(s_cmd_config, sizeof(char) * 
				 (strlen(s_cmd_config) +
				  strlen(s_load_pre) +
				  strlen(optarg) +
				  strlen(s_load_post) + 1));
	  
	  s_cmd_config = strcat(s_cmd_config, s_load_pre);
	  s_cmd_config = strcat(s_cmd_config, optarg);
	  s_cmd_config = strcat(s_cmd_config, s_load_post);
	  break;
	}
      case 'e':
	if(optarg == NULL) {
	  option_error=True;
	  break;
	} else {
	  s_cmd_config=realloc(s_cmd_config, sizeof(char) *
			       (strlen(s_cmd_config) +
				strlen(optarg) + 1));
	  
	  s_cmd_config = strcat(s_cmd_config, optarg);
	  break;
	}
      case 'i':
	interactive = 1; 
	break;
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
	option_error = TRUE; 
	break;
      case '?': /* getopt's dunno return */
	option_error = TRUE; 
	break;
      default:  /* if we made an error */
	scwm_msg(ERR, "main", "Unknown option: `-%c'\n",  (char)optopt);
	option_error = TRUE; 
	break;
      }
  }
#else /* ! HAVE_GETOPT_LONG */
  /* FIXMS: perhaps it would be cleaner to provide an implementation of
     getopt_long where not available, that should be a lot easier to 
     maintain. */
    for (i = 1; i < argc; i++) {
    if ((strncasecmp(argv[i], "-D", 2) == 0) ||
	(strncasecmp(argv[i], "--debug", 7) == 0)) {
      debugging = True;
    } else if ((strncasecmp(argv[i], "-s", 2) == 0) ||
	       (strncasecmp(argv[i], "--single-screen", 15) == 0)){
      single = True;
    } else if ((strncasecmp(argv[i], "-d", 2) == 0) ||
	       (strncasecmp(argv[i], "--display", 9) == 0)) {
      if (++i >= argc) {
	scwm_msg(ERR, "main", "Missing option argument: \"%s\"\n",
		 argv[i-1]);
	option_error=True;
      } else {
	display_name = argv[i];
      }
    } else if ((strncasecmp(argv[i], "-f", 2) == 0) ||
	       (strncasecmp(argv[i], "--file", 6) == 0)) {
      if (++i >= argc) {
	scwm_msg(ERR, "main", "Missing option argument: \"%s\"\n",
		 argv[i-1]);
	option_error=True;
      } else {
	s_cmd_config=realloc(s_cmd_config, sizeof(char) *
			     (strlen(s_cmd_config) +
			      strlen(s_load_pre) +
			      strlen(argv[i]) +
			      strlen(s_load_post) + 1));

	s_cmd_config = strcat(s_cmd_config, s_load_pre);
	s_cmd_config = strcat(s_cmd_config, argv[i]);
	s_cmd_config = strcat(s_cmd_config, s_load_post);
      }
    } else if ((strncasecmp(argv[i], "-e", 2) == 0) || 
	       (strncasecmp(argv[i], "--expression", 12) == 0)) {
      if (++i >= argc) {
	scwm_msg(ERR, "main", "Missing option argument: \"%s\"\n",
		 argv[i-1]);
	option_error=True;
      } else {
	s_cmd_config=realloc(s_cmd_config, sizeof(char) *
			     (strlen(s_cmd_config) + strlen(argv[i]) + 1));
	s_cmd_config = strcat(s_cmd_config, argv[i]);
      }
    } else if ((strncasecmp(argv[i], "-i", 2) == 0) ||
	       (strncasecmp(argv[i], "--interactive",13) == 0)) {
      interactive = 1;
    } else if ((strncasecmp(argv[i], "-h", 2) == 0) ||
	       (strncasecmp(argv[i], "--help", 6) == 0)) {
      usage();
      exit(0);
    } else if ((strncasecmp(argv[i], "-b", 8) == 0) ||
	       (strncasecmp(argv[i], "--blackout", 9) == 0)) {
      Blackout = True;
    } else if ((strncasecmp(argv[i], "-V", 8) == 0) ||
               (strncasecmp(argv[i], "--version", 9) == 0)) {
      printf("Scwm Version %s compiled on %s at %s\nRCS_ID=%s",
	     VERSION, __DATE__, __TIME__, rcsid);
      exit(0);
    } else {
      scwm_msg(ERR, "main", "Unknown option:  `%s'\n", argv[i]);
      option_error = TRUE;
    }
  }
#endif

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
	    *cp = '\0';		/* truncate at display part */
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
  fd_width = GetFdWidth();

  if (fcntl(x_fd, F_SETFD, 1) == -1) {
    scwm_msg(ERR, "main", "close-on-exec failed");
    exit(1);
  }
  /*  Add a DISPLAY entry to the environment, incase we were started
   * with scwm -display term:0.0
   */
  len = strlen(XDisplayString(dpy));
  display_string = safemalloc(len + 10);
  sprintf(display_string, "DISPLAY=%s", XDisplayString(dpy));
  putenv(display_string);
  /* Add a HOSTDISPLAY environment variable, which is the same as
   * DISPLAY, unless display = :0.0 or unix:0.0, in which case the full
   * host name will be used for ease in networking . */
  /* Note: Can't free the rdisplay_string after putenv, because it
   * becomes part of the environment! */
  if (strncmp(display_string, "DISPLAY=:", 9) == 0) {
    char client[MAXHOSTNAME], *rdisplay_string;

    gethostname(client, MAXHOSTNAME);
    rdisplay_string = safemalloc(len + 14 + strlen(client));
    sprintf(rdisplay_string, "HOSTDISPLAY=%s:%s", client, &display_string[9]);
    putenv(rdisplay_string);
  } else if (strncmp(display_string, "DISPLAY=unix:", 13) == 0) {
    char client[MAXHOSTNAME], *rdisplay_string;

    gethostname(client, MAXHOSTNAME);
    rdisplay_string = safemalloc(len + 14 + strlen(client));
    sprintf(rdisplay_string, "HOSTDISPLAY=%s:%s", client,
	    &display_string[13]);
    putenv(rdisplay_string);
  } else {
    char *rdisplay_string;

    rdisplay_string = safemalloc(len + 14);
    sprintf(rdisplay_string, "HOSTDISPLAY=%s", XDisplayString(dpy));
    putenv(rdisplay_string);
  }

  Scr.Root = RootWindow(dpy, Scr.screen);
  if (Scr.Root == None) {
    scwm_msg(ERR, "main", "Screen %d is not a valid screen", (char *) Scr.screen);
    exit(1);
  }
  ShapesSupported = XShapeQueryExtension(dpy, &ShapeEventBase, &ShapeErrorBase);

  InternUsefulAtoms();

  /* Make sure property priority colors is empty */
  XChangeProperty(dpy, Scr.Root, _XA_MIT_PRIORITY_COLORS,
		  XA_CARDINAL, 32, PropModeReplace, NULL, 0);


  XSetErrorHandler((XErrorHandler) CatchRedirectError);
  XSetIOErrorHandler((XIOErrorHandler) CatchFatal);
  XSelectInput(dpy, Scr.Root,
	       LeaveWindowMask | EnterWindowMask | PropertyChangeMask |
	       SubstructureRedirectMask | KeyPressMask |
	       SubstructureNotifyMask |
	       ButtonPressMask | ButtonReleaseMask);
  XSync(dpy, 0);

  XSetErrorHandler((XErrorHandler) ScwmErrorHandler);

  BlackoutScreen();		/* if they want to hide the capture/startup */

  CreateCursors();
  InitVariables();
  InitEventHandlerJumpTable();

  Scr.gray_bitmap =
    XCreateBitmapFromData(dpy, Scr.Root, g_bits, g_width, g_height);

  init_menus();
  DBUG("main", "Setting up rc file defaults...");
  SetRCDefaults();

  DBUG("main", "Running config_commands...");

#ifndef SCWMRC
#define SCWMRC ".scwmrc"
#endif

  if (strlen(s_cmd_config) == 0) {
    gh_eval_str("(let ((home-scwmrc"
	    "       (string-append (getenv \"HOME\") \"/\" \"" SCWMRC "\"))"
		"      (system-scwmrc \"" SCWMDIR "/system" SCWMRC "\"))"
		" (if (access? home-scwmrc R_OK)"
		"     (load home-scwmrc)"
		"     (if (access? system-scwmrc R_OK)"
		"         (load system-scwmrc))))");
  } else {
    gh_eval_str(s_cmd_config);
  }

  free(s_cmd_config);

  CaptureAllWindows();
  MakeMenus();

  DBUG("main", "Done running config_commands");

  if (Scr.d_depth < 2) {
    Scr.gray_pixmap =
      XCreatePixmapFromBitmapData(dpy, Scr.Root, g_bits, g_width, g_height,
				  Scr.MenuColors.fore, Scr.MenuColors.back,
				  Scr.d_depth);
    Scr.light_gray_pixmap =
      XCreatePixmapFromBitmapData(dpy, Scr.Root, l_g_bits, l_g_width, l_g_height,
				  Scr.MenuColors.fore, Scr.MenuColors.back,
				  Scr.d_depth);
  }
  /* create a window which will accept the keyboard focus when no other 
     windows have it */
  attributes.event_mask = KeyPressMask | FocusChangeMask;
  attributes.override_redirect = True;
  Scr.NoFocusWin = XCreateWindow(dpy, Scr.Root, -10, -10, 10, 10, 0, 0,
				 InputOnly, CopyFromParent,
				 CWEventMask | CWOverrideRedirect,
				 &attributes);
  XMapWindow(dpy, Scr.NoFocusWin);

  SetMWM_INFO(Scr.NoFocusWin);

  XSetInputFocus(dpy, Scr.NoFocusWin, RevertToParent, CurrentTime);

  XSync(dpy, False);
  if (debugging)
    XSynchronize(dpy, True);

  Scr.SizeStringWidth = XTextWidth(Scr.StdFont.font,
				   " +8888 x +8888 ", 15);
  attributes.border_pixel = Scr.MenuColors.fore;
  attributes.background_pixel = Scr.MenuColors.back;
  attributes.bit_gravity = NorthWestGravity;
  valuemask = (CWBorderPixel | CWBackPixel | CWBitGravity);
  if (!(Scr.flags & MWMMenus)) {
    Scr.SizeWindow = XCreateWindow(dpy, Scr.Root,
				   0, 0,
				   (unsigned int) (Scr.SizeStringWidth +
						   SIZE_HINDENT * 2),
				   (unsigned int) (Scr.StdFont.height +
						   SIZE_VINDENT * 2),
				   (unsigned int) 0, 0,
				   (unsigned int) CopyFromParent,
				   (Visual *) CopyFromParent,
				   valuemask, &attributes);
  } else {
    Scr.SizeWindow = XCreateWindow(dpy, Scr.Root,
				   Scr.MyDisplayWidth / 2 -
				   (Scr.SizeStringWidth +
				    SIZE_HINDENT * 2) / 2,
				   Scr.MyDisplayHeight / 2 -
				   (Scr.StdFont.height +
				    SIZE_VINDENT * 2) / 2,
				   (unsigned int) (Scr.SizeStringWidth +
						   SIZE_HINDENT * 2),
				   (unsigned int) (Scr.StdFont.height +
						   SIZE_VINDENT * 2),
				   (unsigned int) 0, 0,
				   (unsigned int) CopyFromParent,
				   (Visual *) CopyFromParent,
				   valuemask, &attributes);
  }
#ifndef NON_VIRTUAL
  initPanFrames();
#endif

  XGrabServer_withSemaphore(dpy);

#ifndef NON_VIRTUAL
  checkPanFrames();
#endif
  XUngrabServer_withSemaphore(dpy);
  UnBlackoutScreen();		/* if we need to remove blackout window */
  /* set the focus to the current window if appropriate */
  CoerceEnterNotifyOnCurrentWindow();
  DBUG("main", "Entering HandleEvents loop...");
  HandleEvents();
  DBUG("main", "Back from HandleEvents loop?  Exitting...");
  return;
}



/***********************************************************************
 *
 *  Procedure:
 *      CaptureAllWindows
 *
 *   Decorates all windows at start-up and during recaptures
 *
 ***********************************************************************/

void 
CaptureAllWindows(void)
{
  int i, j;
  unsigned int nchildren;
  Window root, parent, *children;
  ScwmWindow *tmp, *next;	/* temp scwm window structure */
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
	  XFree((char *) wmhintsp);
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
    tmp = Scr.ScwmRoot.next;
    for (i = 0; i < nchildren; i++) {
      tmp = SwFromWindow(dpy,children[i]);
      if (tmp) { 
	isIconicState = DontCareState;
	if (XGetWindowProperty(dpy, tmp->w, _XA_WM_STATE, 0L, 3L, False,
			       _XA_WM_STATE, &atype, &aformat, 
			       &nitems, &bytes_remain, &prop) == Success) {
	  if (prop != NULL) {
	    isIconicState = *(long *) prop;
	    XFree(prop);
	  }
	}
	next = tmp->next;
	data[0] = (unsigned long) tmp->Desk;
	XChangeProperty(dpy, tmp->w, _XA_WM_DESKTOP, _XA_WM_DESKTOP, 32,
			PropModeReplace, (unsigned char *) data, 1);

	XSelectInput(dpy, tmp->w, 0);
	w = tmp->w;
	XUnmapWindow(dpy, tmp->frame);
	XUnmapWindow(dpy, w);
	RestoreWithdrawnLocation(tmp, True);
	Destroy(tmp);
	Event.xmaprequest.window = w;
	HandleMapRequestKeepRaised(BlackoutWin);
	tmp = next;
      }
    }
  }

  isIconicState = DontCareState;

  if (nchildren > 0)
    XFree((char *) children);

  /* after the windows already on the screen are in place,
   * don't use PPosition */
  PPosOverride = False;
  KeepOnTop();
  XUngrabServer_withSemaphore(dpy);
  XSync(dpy, 0);		/* should we do this on initial capture? */
}

/*
   ** SetRCDefaults
   **
   ** Sets some initial style values & such
 */
void 
SetRCDefaults()
{
  /* set up default colors, fonts, etc */

  gh_eval_str("(define quit scwm-quit)"
	      "(undefine scwm-quit)"
	      "(define FIXED-FONT (load-font \"fixed\"))"
	      "(define BLACK (load-color \"black\"))"
	      "(define GRAY (load-color \"gray\"))"
	      "(define SLATEGRAY (load-color \"slategray\"))"
	      "(define LIGHTGRAY (load-color \"lightgray\"))"
	      "(define DIMGRAY (load-color \"dimgray\"))"
	      "(set-rubber-band-mask! 1)"
	      "(set-menu-colors! BLACK GRAY SLATEGRAY)"
	      "(bind-event \'new-window (lambda () (set-window-colors!"
	      "                                    LIGHTGRAY DIMGRAY)"
	      "                                   (show-titlebar)))"
	   "(bind-event \'new-window-hint (lambda () (set-random-placement!"
	      "                                    #t)"
	      "                   (set-smart-placement! #t)))"
	      "(set-menu-font! FIXED-FONT)"
	      "(set-menu-mwm-style! #f)"
	      "(set-hilight-colors! BLACK GRAY)"
	      "(set-icon-font! FIXED-FONT)"
	      "(set-window-font! FIXED-FONT)"
	      "(set-title-justify! \'center)"
	      "(define default-menu (make-menu \"Default Menu\""
	      "   \'title (list \"Exit SCWM\" quit)))"
	      "(bind-mouse \'root 1 (lambda () (popup default-menu)))"
	      "(define (resize-or-raise)"
	      "    (raise-window)"
	      "    (case (mouse-event-type)"
	      "       ((motion) (interactive-resize))"
	      "       ((double-click) (lower-window))))"
	      "(define (move-or-raise)"
	      "   (raise-window)"
	      "   (case (mouse-event-type)"
	      "      ((motion) (interactive-move))"
	      "      ((double-click) (lower-window))))"
	      "(bind-mouse \'frame 1 resize-or-raise)"
	      "(bind-mouse \'(title sidebar) 1 move-or-raise)"
    );

}				/* SetRCDefaults */

/***********************************************************************
 *
 *  Procedure:
 *	MappedNotOverride - checks to see if we should really
 *		put a scwm frame on the window
 *
 *  Returned Value:
 *	TRUE	- go ahead and frame the window
 *	FALSE	- don't frame the window
 *
 *  Inputs:
 *	w	- the window to check
 *
 ***********************************************************************/

int 
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

  if (XGetWindowProperty(dpy, w, _XA_WM_STATE, 0L, 3L, False, _XA_WM_STATE,
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


/***********************************************************************
 *
 *  Procedure:
 *	InternUsefulAtoms:
 *            Dont really know what it does
 *
 ***********************************************************************
 */
Atom _XA_MIT_PRIORITY_COLORS;
Atom _XA_WM_CHANGE_STATE;
Atom _XA_WM_STATE;
Atom _XA_WM_COLORMAP_WINDOWS;
extern Atom _XA_WM_PROTOCOLS;
Atom _XA_WM_TAKE_FOCUS;
Atom _XA_WM_DELETE_WINDOW;
Atom _XA_WM_DESKTOP;
Atom _XA_MwmAtom;
Atom _XA_MOTIF_WM;

Atom _XA_OL_WIN_ATTR;
Atom _XA_OL_WT_BASE;
Atom _XA_OL_WT_CMD;
Atom _XA_OL_WT_HELP;
Atom _XA_OL_WT_NOTICE;
Atom _XA_OL_WT_OTHER;
Atom _XA_OL_DECOR_ADD;
Atom _XA_OL_DECOR_DEL;
Atom _XA_OL_DECOR_CLOSE;
Atom _XA_OL_DECOR_RESIZE;
Atom _XA_OL_DECOR_HEADER;
Atom _XA_OL_DECOR_ICON_NAME;
Atom XA_SCWM_EXECUTE;

void 
InternUsefulAtoms(void)
{
  /* 
   * Create priority colors if necessary.
   */
  _XA_MIT_PRIORITY_COLORS = XInternAtom(dpy, "_MIT_PRIORITY_COLORS", False);
  _XA_WM_CHANGE_STATE = XInternAtom(dpy, "WM_CHANGE_STATE", False);
  _XA_WM_STATE = XInternAtom(dpy, "WM_STATE", False);
  _XA_WM_COLORMAP_WINDOWS = XInternAtom(dpy, "WM_COLORMAP_WINDOWS", False);
  _XA_WM_PROTOCOLS = XInternAtom(dpy, "WM_PROTOCOLS", False);
  _XA_WM_TAKE_FOCUS = XInternAtom(dpy, "WM_TAKE_FOCUS", False);
  _XA_WM_DELETE_WINDOW = XInternAtom(dpy, "WM_DELETE_WINDOW", False);
  _XA_WM_DESKTOP = XInternAtom(dpy, "WM_DESKTOP", False);
  _XA_MwmAtom = XInternAtom(dpy, "_MOTIF_WM_HINTS", False);
  _XA_MOTIF_WM = XInternAtom(dpy, "_MOTIF_WM_INFO", False);

  _XA_OL_WIN_ATTR = XInternAtom(dpy, "_OL_WIN_ATTR", False);
  _XA_OL_WT_BASE = XInternAtom(dpy, "_OL_WT_BASE", False);
  _XA_OL_WT_CMD = XInternAtom(dpy, "_OL_WT_CMD", False);
  _XA_OL_WT_HELP = XInternAtom(dpy, "_OL_WT_HELP", False);
  _XA_OL_WT_NOTICE = XInternAtom(dpy, "_OL_WT_NOTICE", False);
  _XA_OL_WT_OTHER = XInternAtom(dpy, "_OL_WT_OTHER", False);
  _XA_OL_DECOR_ADD = XInternAtom(dpy, "_OL_DECOR_ADD", False);
  _XA_OL_DECOR_DEL = XInternAtom(dpy, "_OL_DECOR_DEL", False);
  _XA_OL_DECOR_CLOSE = XInternAtom(dpy, "_OL_DECOR_CLOSE", False);
  _XA_OL_DECOR_RESIZE = XInternAtom(dpy, "_OL_DECOR_RESIZE", False);
  _XA_OL_DECOR_HEADER = XInternAtom(dpy, "_OL_DECOR_HEADER", False);
  _XA_OL_DECOR_ICON_NAME = XInternAtom(dpy, "_OL_DECOR_ICON_NAME", False);
  XA_SCWM_EXECUTE = XInternAtom(dpy, "SCWM_EXECUTE", False);
  return;
}

/***********************************************************************
 *
 *  Procedure:
 *	newhandler: Installs new signal handler
 *
 ************************************************************************/
void 
newhandler(int sig)
{
  if (signal(sig, SIG_IGN) != SIG_IGN)
    signal(sig, SigDone);
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
  Scr.ScwmCursors[POSITION] = XCreateFontCursor(dpy, XC_top_left_corner);
  Scr.ScwmCursors[DEFAULT] = XCreateFontCursor(dpy, XC_top_left_arrow);
  Scr.ScwmCursors[SYS] = XCreateFontCursor(dpy, XC_hand2);
  Scr.ScwmCursors[TITLE_CURSOR] = XCreateFontCursor(dpy, XC_top_left_arrow);
  Scr.ScwmCursors[MOVE] = XCreateFontCursor(dpy, XC_fleur);
  Scr.ScwmCursors[MENU] = XCreateFontCursor(dpy, XC_sb_left_arrow);
  Scr.ScwmCursors[WAIT] = XCreateFontCursor(dpy, XC_watch);
  Scr.ScwmCursors[SELECT] = XCreateFontCursor(dpy, XC_dot);
  Scr.ScwmCursors[DESTROY] = XCreateFontCursor(dpy, XC_pirate);
  Scr.ScwmCursors[LEFT] = XCreateFontCursor(dpy, XC_left_side);
  Scr.ScwmCursors[RIGHT] = XCreateFontCursor(dpy, XC_right_side);
  Scr.ScwmCursors[TOP] = XCreateFontCursor(dpy, XC_top_side);
  Scr.ScwmCursors[BOTTOM] = XCreateFontCursor(dpy, XC_bottom_side);
  Scr.ScwmCursors[TOP_LEFT] = XCreateFontCursor(dpy, XC_top_left_corner);
  Scr.ScwmCursors[TOP_RIGHT] = XCreateFontCursor(dpy, XC_top_right_corner);
  Scr.ScwmCursors[BOTTOM_LEFT] = XCreateFontCursor(dpy, XC_bottom_left_corner);
  Scr.ScwmCursors[BOTTOM_RIGHT] = XCreateFontCursor(dpy, XC_bottom_right_corner);
}

/***********************************************************************
 *
 *  LoadDefaultLeftButton -- loads default left button # into 
 *		assumes associated button memory is already free
 * 
 ************************************************************************/
void 
LoadDefaultLeftButton(ButtonFace * bf, int i)
{
  bf->style = VectorButton;
  switch (i % 5) {
  case 0:
  case 4:
    bf->vector.x[0] = 22;
    bf->vector.y[0] = 39;
    bf->vector.line_style[0] = 1;
    bf->vector.x[1] = 78;
    bf->vector.y[1] = 39;
    bf->vector.line_style[1] = 1;
    bf->vector.x[2] = 78;
    bf->vector.y[2] = 61;
    bf->vector.line_style[2] = 0;
    bf->vector.x[3] = 22;
    bf->vector.y[3] = 61;
    bf->vector.line_style[3] = 0;
    bf->vector.x[4] = 22;
    bf->vector.y[4] = 39;
    bf->vector.line_style[4] = 1;
    bf->vector.num = 5;
    break;
  case 1:
    bf->vector.x[0] = 32;
    bf->vector.y[0] = 45;
    bf->vector.line_style[0] = 0;
    bf->vector.x[1] = 68;
    bf->vector.y[1] = 45;
    bf->vector.line_style[1] = 0;
    bf->vector.x[2] = 68;
    bf->vector.y[2] = 55;
    bf->vector.line_style[2] = 1;
    bf->vector.x[3] = 32;
    bf->vector.y[3] = 55;
    bf->vector.line_style[3] = 1;
    bf->vector.x[4] = 32;
    bf->vector.y[4] = 45;
    bf->vector.line_style[4] = 0;
    bf->vector.num = 5;
    break;
  case 2:
    bf->vector.x[0] = 49;
    bf->vector.y[0] = 49;
    bf->vector.line_style[0] = 1;
    bf->vector.x[1] = 51;
    bf->vector.y[1] = 49;
    bf->vector.line_style[1] = 1;
    bf->vector.x[2] = 51;
    bf->vector.y[2] = 51;
    bf->vector.line_style[2] = 0;
    bf->vector.x[3] = 49;
    bf->vector.y[3] = 51;
    bf->vector.line_style[3] = 0;
    bf->vector.x[4] = 49;
    bf->vector.y[4] = 49;
    bf->vector.line_style[4] = 1;
    bf->vector.num = 5;
    break;
  case 3:
    bf->vector.x[0] = 32;
    bf->vector.y[0] = 45;
    bf->vector.line_style[0] = 1;
    bf->vector.x[1] = 68;
    bf->vector.y[1] = 45;
    bf->vector.line_style[1] = 1;
    bf->vector.x[2] = 68;
    bf->vector.y[2] = 55;
    bf->vector.line_style[2] = 0;
    bf->vector.x[3] = 32;
    bf->vector.y[3] = 55;
    bf->vector.line_style[3] = 0;
    bf->vector.x[4] = 32;
    bf->vector.y[4] = 45;
    bf->vector.line_style[4] = 1;
    bf->vector.num = 5;
    break;
  }
}

/***********************************************************************
 *
 *  LoadDefaultRightButton -- loads default left button # into
 *		assumes associated button memory is already free
 * 
 ************************************************************************/
void 
LoadDefaultRightButton(ButtonFace * bf, int i)
{
  bf->style = VectorButton;
  switch (i % 5) {
  case 0:
  case 3:
    bf->vector.x[0] = 25;
    bf->vector.y[0] = 25;
    bf->vector.line_style[0] = 1;
    bf->vector.x[1] = 75;
    bf->vector.y[1] = 25;
    bf->vector.line_style[1] = 1;
    bf->vector.x[2] = 75;
    bf->vector.y[2] = 75;
    bf->vector.line_style[2] = 0;
    bf->vector.x[3] = 25;
    bf->vector.y[3] = 75;
    bf->vector.line_style[3] = 0;
    bf->vector.x[4] = 25;
    bf->vector.y[4] = 25;
    bf->vector.line_style[4] = 1;
    bf->vector.num = 5;
    break;
  case 1:
    bf->vector.x[0] = 39;
    bf->vector.y[0] = 39;
    bf->vector.line_style[0] = 1;
    bf->vector.x[1] = 61;
    bf->vector.y[1] = 39;
    bf->vector.line_style[1] = 1;
    bf->vector.x[2] = 61;
    bf->vector.y[2] = 61;
    bf->vector.line_style[2] = 0;
    bf->vector.x[3] = 39;
    bf->vector.y[3] = 61;
    bf->vector.line_style[3] = 0;
    bf->vector.x[4] = 39;
    bf->vector.y[4] = 39;
    bf->vector.line_style[4] = 1;
    bf->vector.num = 5;
    break;
  case 2:
    bf->vector.x[0] = 49;
    bf->vector.y[0] = 49;
    bf->vector.line_style[0] = 1;
    bf->vector.x[1] = 51;
    bf->vector.y[1] = 49;
    bf->vector.line_style[1] = 1;
    bf->vector.x[2] = 51;
    bf->vector.y[2] = 51;
    bf->vector.line_style[2] = 0;
    bf->vector.x[3] = 49;
    bf->vector.y[3] = 51;
    bf->vector.line_style[3] = 0;
    bf->vector.x[4] = 49;
    bf->vector.y[4] = 49;
    bf->vector.line_style[4] = 1;
    bf->vector.num = 5;
    break;
  case 4:
    bf->vector.x[0] = 36;
    bf->vector.y[0] = 36;
    bf->vector.line_style[0] = 1;
    bf->vector.x[1] = 64;
    bf->vector.y[1] = 36;
    bf->vector.line_style[1] = 1;
    bf->vector.x[2] = 64;
    bf->vector.y[2] = 64;
    bf->vector.line_style[2] = 0;
    bf->vector.x[3] = 36;
    bf->vector.y[3] = 64;
    bf->vector.line_style[3] = 0;
    bf->vector.x[4] = 36;
    bf->vector.y[4] = 36;
    bf->vector.line_style[4] = 1;
    bf->vector.num = 5;
    break;
  }
}

/***********************************************************************
 *
 *  LoadDefaultButton -- loads default button # into button structure
 *		assumes associated button memory is already free
 * 
 ************************************************************************/
void 
LoadDefaultButton(ButtonFace * bf, int i)
{
  int n = i / 2;

  if ((n * 2) == i) {
    if (--n < 0)
      n = 4;
    LoadDefaultRightButton(bf, n);
  } else
    LoadDefaultLeftButton(bf, n);
}

extern void FreeButtonFace(Display * dpy, ButtonFace * bf);

/***********************************************************************
 *
 *  ResetAllButtons -- resets all buttons to defaults
 *                 destroys existing buttons
 * 
 ************************************************************************/
void 
ResetAllButtons(ScwmDecor * fl)
{
  int i = 0;

  for (; i < 5; ++i) {
    int j;

    FreeButtonFace(dpy, fl->left_buttons[i].state[0]);
    FreeButtonFace(dpy, fl->right_buttons[i].state[0]);

    LoadDefaultLeftButton(fl->left_buttons[i].state[0], i);
    LoadDefaultRightButton(fl->right_buttons[i].state[0], i);

    for (j = 1; j < MaxButtonState; ++j) {
      /* FreeButtonFace(dpy, fl->left_buttons[i].state[j]); */
      /* FreeButtonFace(dpy, fl->right_buttons[i].state[j]); */

      fl->left_buttons[i].state[j] = fl->left_buttons[i].state[0];
      fl->right_buttons[i].state[j] = fl->right_buttons[i].state[0];
    }
  }
  fl->right_buttons[0].flags |= MWMButton;
}

/***********************************************************************
 *
 *  DestroyScwmDecor -- frees all memory assocated with an ScwmDecor
 *	structure, but does not free the ScwmDecor itself
 * 
 ************************************************************************/
void 
DestroyScwmDecor(ScwmDecor * fl)
{

  if (fl->tag) {
    free(fl->tag);
    fl->tag = NULL;
  }
  if (fl->HiReliefGC != NULL) {
    XFreeGC(dpy, fl->HiReliefGC);
    fl->HiReliefGC = NULL;
  }
  if (fl->HiShadowGC != NULL) {
    XFreeGC(dpy, fl->HiShadowGC);
    fl->HiShadowGC = NULL;
  }
}

/***********************************************************************
 *
 *  InitScwmDecor -- initializes an ScwmDecor structure to defaults
 * 
 ************************************************************************/
void 
InitScwmDecor(ScwmDecor * fl)
{

  fl->HiReliefGC = NULL;
  fl->HiShadowGC = NULL;

  /*  fl->tag = NULL; */
  fl->next = NULL;



}

/***********************************************************************
 *
 *  Procedure:
 *	InitVariables - initialize scwm variables
 *
 ************************************************************************/
void 
InitVariables(void)
{
  ScwmContext = XUniqueContext();
  MenuContext = XUniqueContext();

  /* initialize some lists */
  Scr.AllBindings = NULL;
  Scr.AllMenus = NULL;
  Scr.SchemeMenus = NULL;
  Scr.TheList = NULL;

  Scr.DefaultIcon = NULL;


  /* create graphics contexts */
  CreateGCs();

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

  Scr.StdFont.font = NULL;

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
    if ((XGetWindowProperty(dpy, Scr.Root, _XA_WM_DESKTOP, 0L, 1L, True,
			    _XA_WM_DESKTOP, &atype, &aformat, &nitems,
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

  /* Not the right place for this, should only be called once somewhere .. */
  /* FIXGJB: why not the right place? */
  InitPictureCMap(dpy, Scr.Root);

  return;
}

/***********************************************************************
 *
 *  Procedure:
 *	Reborder - Removes scwm border windows
 *
 ************************************************************************/
void 
Reborder(void)
{
  ScwmWindow *tmp;		/* temp scwm window structure */

  /* put a border back around all windows */
  XGrabServer_withSemaphore(dpy);

  InstallWindowColormaps(&Scr.ScwmRoot);	/* force reinstall */
  for (tmp = Scr.ScwmRoot.next; tmp != NULL; tmp = tmp->next) {
    RestoreWithdrawnLocation(tmp, True);
    XUnmapWindow(dpy, tmp->frame);
    XDestroyWindow(dpy, tmp->frame);
  }

  XUngrabServer_withSemaphore(dpy);
  XSetInputFocus(dpy, PointerRoot, RevertToPointerRoot, CurrentTime);
  XSync(dpy, 0);

}

/***********************************************************************
 *
 *  Procedure:
 *	Done - cleanup and exit scwm
 *
 ***********************************************************************
 */
void 
SigDone(int nonsense)
{
  Done(0, NULL);
  SIGNAL_RETURN;
}

void 
Done(int restart, char *command)
{

#ifndef NON_VIRTUAL
  MoveViewport(0, 0, False);
#endif


  /* Close all my pipes */

  Reborder();

  if (restart) {
    SaveDesktopState();		/* I wonder why ... */

    /* Really make sure that the connection is closed and cleared! */
    XSelectInput(dpy, Scr.Root, 0);
    XSync(dpy, 0);
    XCloseDisplay(dpy);

    {
      char *my_argv[10];
      int i, done, j;

      i = 0;
      j = 0;
      done = 0;
      while ((g_argv[j] != NULL) && (i < 8)) {
	if (strcmp(g_argv[j], "-s") != 0) {
	  my_argv[i] = g_argv[j];
	  i++;
	  j++;
	} else
	  j++;
      }
      if (strstr(command, "scwm") != NULL)
	my_argv[i++] = "-s";
      while (i < 10)
	my_argv[i++] = NULL;

      /* really need to destroy all windows, explicitly,
       * not sleep, but this is adequate for now */
      sleep(1);
      ReapChildren();
      execvp(command, my_argv);
    }
    scwm_msg(ERR, "Done", "Call of '%s' failed!!!!", command);
    execvp(g_argv[0], g_argv);	/* that _should_ work */
    scwm_msg(ERR, "Done", "Call of '%s' failed!!!!", g_argv[0]);
  } else {
    XCloseDisplay(dpy);
    exit(0);
  }
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
	  "      [--file|-f rc_file] [--single-screen|-s] [--interactive|-i]\n"
	  "      [--blackout|-b] [--version|-V] [--help|-h]\n"
	  , g_argv[0]);
}

/****************************************************************************
 *
 * Save Desktop State
 *
 ****************************************************************************/
void 
SaveDesktopState()
{
  ScwmWindow *t;
  unsigned long data[1];

  for (t = Scr.ScwmRoot.next; t != NULL; t = t->next) {
    data[0] = (unsigned long) t->Desk;
    XChangeProperty(dpy, t->w, _XA_WM_DESKTOP, _XA_WM_DESKTOP, 32,
		    PropModeReplace, (unsigned char *) data, 1);
  }

  data[0] = (unsigned long) Scr.CurrentDesk;
  XChangeProperty(dpy, Scr.Root, _XA_WM_DESKTOP, _XA_WM_DESKTOP, 32,
		  PropModeReplace, (unsigned char *) data, 1);

  XSync(dpy, 0);
}


void 
SetMWM_INFO(Window window)
{
#ifdef MODALITY_IS_EVIL
  struct mwminfo {
    long flags;
    Window win;
  } motif_wm_info;

  /* Set Motif WM_INFO atom to make motif relinquish 
   * broken handling of modal dialogs */
  motif_wm_info.flags = 2;
  motif_wm_info.win = window;

  XChangeProperty(dpy, Scr.Root, _XA_MOTIF_WM, _XA_MOTIF_WM, 32,
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
