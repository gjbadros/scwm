/* $Id$
 * xrm.c
 * X Resource manager primitives for scwm
 * (C) 1998 Greg J. Badros and Maciej Stachowiak
 */


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <pwd.h>
#include <unistd.h>
#include <signal.h>
#include <sys/types.h>
#include <X11/Xlib.h>

#include <guile/gh.h>

#include "xrm.h"

#include "scwm.h"

XrmDatabase db;
XrmDatabase dbSystem;

SCWM_PROC (X_resource_put, "X-resource-put", 2, 0, 0,
           (SCM resource, SCM value))
     /** Stores string VALUE as X resource RESOURCE (also a string).
Later, the value can be retrieved using `X-resource-get'. */
#define FUNC_NAME s_X_resource_put
{
  int iarg = 1;

  if (!gh_string_p(resource))
    scm_wrong_type_arg(FUNC_NAME, iarg++, resource);
  if (!gh_string_p(value))
    scm_wrong_type_arg(FUNC_NAME, iarg++, value);

  { /* scope */
    char *szSpecifier = gh_scm2newstr(resource,NULL);
    char *szValue = gh_scm2newstr(value,NULL);
    XrmPutStringResource(&db,szSpecifier,szValue);
    FREE(szSpecifier);
    FREE(szValue);
  }
  return SCM_UNDEFINED;
}
#undef FUNC_NAME


SCWM_PROC (X_resource_get, "X-resource-get", 1, 1, 0,
           (SCM name, SCM xclass))
     /** Get X resource specified by NAME and XCLASS from Xrm database.
Both NAME and XCLASS are strings, as is the returned value.  If
XCLASS is omitted, it defaults to the same string as NAME.
If there is no resource under the given key, #f is returned. */
#define FUNC_NAME s_X_resource_get
{
  int iarg = 1;
  SCM answer = SCM_BOOL_F;

  if (!gh_string_p(name))
    scm_wrong_type_arg(FUNC_NAME, iarg++, name);
  if (!UNSET_SCM(xclass) && !gh_string_p(xclass))
    scm_wrong_type_arg(FUNC_NAME, iarg++, xclass);

  { /* scope */
    char *szName = gh_scm2newstr(name,NULL);
    char *szClass = !UNSET_SCM(xclass)?gh_scm2newstr(xclass,NULL):strdup(szName);
    char *szType;
    XrmValue ret;
    if (XrmGetResource(db,szName,szClass,&szType,&ret) ||
        XrmGetResource(dbSystem,szName,szClass,&szType,&ret)) {
      answer = gh_str02scm(ret.addr);
    }
    FREE(szName);
    FREE(szClass);
  }
  return answer;
}
#undef FUNC_NAME


SCWM_PROC (X_resource_database_save, "X-resource-database-save", 1, 0, 0,
           (SCM filename))
     /** Save the Scwm resource database to FILENAME. 
Only the settings set or changed via `X-resource-put' go into
the file. */
#define FUNC_NAME s_X_resource_database_save
{
  int iarg = 1;
  char *szFilename = NULL;
  if (!gh_string_p(filename))
    scm_wrong_type_arg(FUNC_NAME, iarg++, filename);
  szFilename = gh_scm2newstr(filename,NULL);
  XrmPutFileDatabase(db, szFilename);
  FREE(szFilename);
  return SCM_UNDEFINED;
}
#undef FUNC_NAME




/* Modified from ORA, XLib Prging manual (X Vol 1)
   --09/10/98 gjb */
static
char *getHomeDir( char *dest )
{
  int uid;
  extern char *getenv();
  struct passwd *pw;
  register char *ptr;

  if ((ptr = getenv("HOME")) != NULL) {
    strcpy(dest, ptr);
  } else {
    if ((ptr = getenv("USER")) != NULL) {
      pw = getpwnam(ptr);
    } else {
      uid = getuid();
      pw = getpwuid(uid);
    }
    if (pw) {
      strcpy(dest, pw->pw_dir);
    } else {
      *dest = '\0';
    }
  }
  return dest;
}

void
IntegrateNewResourceManagerProperty(Display *dpy)
{
  if (XResourceManagerString(dpy) != NULL) {
    XrmDatabase serverDB = XrmGetStringDatabase(XResourceManagerString(dpy));
    XrmMergeDatabases(serverDB, &dbSystem);
  }
}

/* Modified from ORA, XLib Prging manual (X Vol 1)
   p. 477  --09/10/98 gjb */
static void
MergeResourceDatabases()
{
  XrmDatabase homeDB, serverDB, applicationDB;

  char filenamebuf[1024];
  char *filename = &filenamebuf[0];
  char *environment;
  char *classname = "Scwm";
  char name[255];

  strcpy(name, "/usr/lib/X11/app-defaults/");
  strcat(name, classname);
  /* get application defaults file, if any */
  applicationDB = XrmGetFileDatabase(name);
  XrmMergeDatabases(applicationDB, &dbSystem);
  
  /* MERGE server defaults, these are created by xrdb, loaded as a
   * property of the root window when the server initializes, and
   * loaded into the display structure on XOpenDisplay.  If not defined,
   * use .Xdefaults  */
  if (XResourceManagerString(dpy) != NULL) {
    serverDB = XrmGetStringDatabase(XResourceManagerString(dpy));
  } else {
    /* Open .Xdefaults file and merge into existing data base */
    getHomeDir(filename);
    strcat(filename, "/.Xdefaults");
    
    serverDB = XrmGetFileDatabase(filename);
  }
  XrmMergeDatabases(serverDB, &dbSystem);
  
  /* Open XENVIRONMENT file, or if not defined, the ~/.Xdefaults,
         * and merge into existing data base */
  if ((environment = getenv("XENVIRONMENT")) == NULL) {
    int len;
    environment = getHomeDir(filename);
    strcat(environment, "/.Xdefaults-");
    len = strlen(environment);
    gethostname(environment + len, 1024 - len);
  }
  homeDB = XrmGetFileDatabase(environment);
  XrmMergeDatabases(homeDB, &dbSystem);
  
  /* command line takes precedence over everything */
  /*  XrmMergeDatabases(commandlineDB, &db); 
      FIXGJB: some command line resource processing is
      done in AddWindow() */
}  


void
init_xrm()
{
#ifndef SCM_MAGIC_SNARFER
#include "xrm.x"
#endif
  MergeResourceDatabases();
}

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
