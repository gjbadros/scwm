static char *m4_defs(Display *display, const char *host, char *m4_options, char *config_file)
{
  Screen *screen;
  Visual *visual;
  char client[MAXHOSTNAME], server[MAXHOSTNAME], *colon;
  char ostype[BUFSIZ];
  char options[BUFSIZ];
  static char tmp_name[BUFSIZ];
  struct hostent *hostname;
  char *vc;			/* Visual Class */
  FILE *tmpf;
  int fd;
  struct passwd *pwent;
  /* Generate a temporary filename.  Honor the TMPDIR environment variable,
     if set. Hope nobody deletes this file! */

  if (strlen(m4_outfile) == 0) {
    if ((vc=getenv("TMPDIR"))) {
      strcpy(tmp_name, vc);
    } else {
      strcpy(tmp_name, "/tmp");
    }
    strcat(tmp_name, "/fvwmrcXXXXXX");
    mktemp(tmp_name);
  } else {
    strcpy(tmp_name,m4_outfile);
  }

  if (*tmp_name == '\0')
  {
    perror("mktemp failed in m4_defs");
    exit(0377);
  }

  /*
  ** check to make sure it doesn't exist already, to prevent security hole
  */
  if ((fd = open(tmp_name, O_WRONLY|O_EXCL|O_CREAT, 0600)) < 0)
  {
    perror("exclusive open for output file failed in m4_defs");
    exit(0377);
  }
  close(fd);

  /*
   * Create the appropriate command line to run m4, and
   * open a pipe to the command.
   */

  if(m4_prefix)
    sprintf(options, "%s --prefix-builtins %s > %s\n",
	    m4_prog,
	    m4_options, tmp_name);
  else
    sprintf(options, "%s  %s > %s\n",
	    m4_prog,
	    m4_options, tmp_name);
  tmpf = popen(options, "w");
  if (tmpf == NULL) {
    perror("Cannot open pipe to m4");
    exit(0377);
  }
    
  mygethostname(client,MAXHOSTNAME);
  
  mygetostype  (ostype, sizeof ostype);
  
  /* Change the quoting characters, if specified */
  
  if (!m4_default_quotes)
  {
    fprintf(tmpf, "changequote(%s, %s)dnl\n", m4_startquote, m4_endquote);
  }
  
  hostname = gethostbyname(client);
  strcpy(server, XDisplayName(host));
  colon = strchr(server, ':');
  if (colon != NULL) *colon = '\0';
  if ((server[0] == '\0') || (!strcmp(server, "unix")))
    strcpy(server, client);	/* must be connected to :0 or unix:0 */
  
  /* TWM_TYPE is fvwm, for completeness */
  
  fputs(MkDef("TWM_TYPE", "fvwm"), tmpf);
  
  /* The machine running the X server */
  fputs(MkDef("SERVERHOST", server), tmpf);
  /* The machine running the window manager process */
  fputs(MkDef("CLIENTHOST", client), tmpf);
  if (hostname)
    fputs(MkDef("HOSTNAME", (char *)hostname->h_name), tmpf);
  else
    fputs(MkDef("HOSTNAME", (char *)client), tmpf);
  
  fputs(MkDef("OSTYPE", ostype), tmpf);
  
  pwent=getpwuid(geteuid());
  fputs(MkDef("USER", pwent->pw_name), tmpf);
  
  fputs(MkDef("HOME", getenv("HOME")), tmpf);
  fputs(MkNum("VERSION", ProtocolVersion(display)), tmpf);
  fputs(MkNum("REVISION", ProtocolRevision(display)), tmpf);
  fputs(MkDef("VENDOR", ServerVendor(display)), tmpf);
  fputs(MkNum("RELEASE", VendorRelease(display)), tmpf);
  screen = ScreenOfDisplay(display, Mscreen);
  visual = DefaultVisualOfScreen(screen);
  fputs(MkNum("WIDTH", DisplayWidth(display,Mscreen)), tmpf);
  fputs(MkNum("HEIGHT", DisplayHeight(display,Mscreen)), tmpf);
  
  fputs(MkNum("X_RESOLUTION",Resolution(screen->width,screen->mwidth)),tmpf);
  fputs(MkNum("Y_RESOLUTION",Resolution(screen->height,screen->mheight)),tmpf);
  fputs(MkNum("PLANES",DisplayPlanes(display, Mscreen)), tmpf);
  
  fputs(MkNum("BITS_PER_RGB", visual->bits_per_rgb), tmpf);
  fputs(MkNum("SCREEN", Mscreen), tmpf);
  
  switch(visual->class) 
  {
    case(StaticGray):
      vc = "StaticGray";
      break;
    case(GrayScale):
      vc = "GrayScale";
      break;
    case(StaticColor):
      vc = "StaticColor";
      break;
    case(PseudoColor):
      vc = "PseudoColor";
      break;
    case(TrueColor):
      vc = "TrueColor";
      break;
    case(DirectColor):
      vc = "DirectColor";
      break;
    default:
      vc = "NonStandard";
      break;
  }

  fputs(MkDef("CLASS", vc), tmpf);
  if (visual->class != StaticGray && visual->class != GrayScale) 
    fputs(MkDef("COLOR", "Yes"), tmpf);
  else 
    fputs(MkDef("COLOR", "No"), tmpf);
  fputs(MkDef("FVWM_VERSION", VERSION), tmpf);

  /* Add options together */
  *options = '\0';
#ifdef	SHAPE
  strcat(options, "SHAPE ");
#endif
#ifdef	XPM
  strcat(options, "XPM ");
#endif

  strcat(options, "M4 ");

#ifdef	NO_SAVEUNDERS
  strcat(options, "NO_SAVEUNDERS ");
#endif

  fputs(MkDef("OPTIONS", options), tmpf);

  fputs(MkDef("FVWMDIR", FVWMDIR), tmpf);
    
  /*
   * At this point, we've sent the definitions to m4.  Just include
   * the fvwmrc file now.
   */
    
  fprintf(tmpf, "%sinclude(%s%s%s)\n",
          (m4_prefix) ? "m4_": "",
          m4_startquote,
          config_file,
          m4_endquote);
  
  pclose(tmpf);
  return(tmp_name);
}
