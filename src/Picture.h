#ifndef PICTURE_H
#define PICTURE_H

/***********************************************************************
 * Stuff for dealing w/ bitmaps & pixmaps:
 ***********************************************************************/
typedef struct PictureThing
{
  struct PictureThing *next;
  char *name;
  Pixmap picture;
  Pixmap mask;
  unsigned int depth;
  unsigned int width;
  unsigned int height;
  unsigned int count;
} Picture;

void InitPictureCMap(Display*,Window);
Picture *GetPicture(Display*,Window,char *iconpath,char *pixmappath,char*);
Picture *CachePicture(Display*,Window,char *iconpath,char *pixmappath,char*);
void DestroyPicture(Display*,Picture*);

char *findIconFile(char *icon, char *pathlist, int type);

#endif

