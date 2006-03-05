/* $Id$
 * Copyright (C) 1997-1999, Maciej Stachowiak and Greg J. Badros
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.GPL.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 *
 */


#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include <pwd.h>
#include <unistd.h>
#include <signal.h>
#include <sys/types.h>
#include <X11/Xlib.h>

#include "guile-compat.h"

#include "xrm.h"

#include "scwm.h"

XrmDatabase db;
XrmDatabase dbSystem;

SCM_DEFINE (X_resource_put, "X-resource-put", 2, 0, 0,
           (SCM resource, SCM value),
"Stores string VALUE as X resource RESOURCE (also a string).\n\
Later, the value can be retrieved using `X-resource-get'.")
#define FUNC_NAME s_X_resource_put
{
  char *szSpecifier;
  char *szValue;
  VALIDATE_ARG_STR_NEWCOPY(1,resource,szSpecifier);
  VALIDATE_ARG_STR_NEWCOPY(2,value,szValue);

  XrmPutStringResource(&db,szSpecifier,szValue);
  free(szSpecifier);
  free(szValue);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (X_resource_get, "X-resource-get", 1, 1, 0,
           (SCM name, SCM xclass),
"Get X resource specified by NAME and XCLASS from Xrm database.\n\
Both NAME and XCLASS are strings, as is the returned value.  If\n\
XCLASS is omitted, it defaults to the same string as NAME.\n\
If there is no resource under the given key, #f is returned.")
#define FUNC_NAME s_X_resource_get
{
  SCM answer = SCM_BOOL_F;
  char *szName;
  char *szClass;
  char *szType;
  XrmValue ret;

  VALIDATE_ARG_STR_NEWCOPY(1,name,szName);
  VALIDATE_ARG_STR_NEWCOPY_USE_NULL(2,xclass,szClass);
  if (!szClass) szClass = szName;

  if (XrmGetResource(db,szName,szClass,&szType,&ret) ||
      XrmGetResource(dbSystem,szName,szClass,&szType,&ret)) {
    answer = scm_from_locale_string(ret.addr);
  }
  free(szName);
  if (szClass != szName) free(szClass);
  return answer;
}
#undef FUNC_NAME


SCM_DEFINE (X_resource_database_save, "X-resource-database-save", 1, 0, 0,
           (SCM filename),
"Save the Scwm resource database to FILENAME. \n\
Only the settings set or changed via `X-resource-put' go into\n\
the file.")
#define FUNC_NAME s_X_resource_database_save
{
  char *szFilename;
  VALIDATE_ARG_STR_NEWCOPY(1,filename,szFilename);
  XrmPutFileDatabase(db, szFilename);
  free(szFilename);
  return SCM_UNSPECIFIED;
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
      GJB:FIXME:: some command line resource processing is
      done in AddWindow() */
}  


void
init_xrm()
{
#include "xrm.x"
  MergeResourceDatabases();
}

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

