
#if 0

#ifdef XPM
char *PixmapPath = SCWM_ICONDIR;
void setPixmapPath(XEvent *eventp,Window w,ScwmWindow *tmp_win,
                   unsigned long context, char *action,int* Module)
{
  static char *ptemp = NULL;
  char *tmp;

  if(ptemp == NULL)
    ptemp = PixmapPath;

  if((PixmapPath != ptemp)&&(PixmapPath != NULL))
    free(PixmapPath);
  tmp = stripcpy(action);
  PixmapPath = envDupExpand(tmp, 0);
  free(tmp);
}
#endif

char *IconPath = SCWM_ICONDIR;
void setIconPath(XEvent *eventp,Window w,ScwmWindow *tmp_win,
                 unsigned long context, char *action,int* Module)
{
  static char *ptemp = NULL;
  char *tmp;

  if(ptemp == NULL)
    ptemp = IconPath;

  if((IconPath != ptemp)&&(IconPath != NULL))
    free(IconPath);
  tmp = stripcpy(action);
  IconPath = envDupExpand(tmp, 0);
  free(tmp);
}

#ifdef SCWM_MODULEDIR
char *ModulePath = SCWM_MODULEDIR;
#else
char *ModulePath = SCWMDIR;
#endif
void setModulePath(XEvent *eventp,Window w,ScwmWindow *tmp_win,
                   unsigned long context, char *action,int* Module)
{
  static char *ptemp = NULL;
  char *tmp;

  if(ptemp == NULL)
    ptemp = ModulePath;

  if((ModulePath != ptemp)&&(ModulePath != NULL))
    free(ModulePath);
  tmp = stripcpy(action);
  ModulePath = envDupExpand(tmp, 0);
  free(tmp);
}

#endif
