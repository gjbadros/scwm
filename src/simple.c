/*
% uname -a
Linux merlin 2.2.16-22 #1 Tue Aug 22 16:49:06 EDT 2000 i686 unknown
# (Redhat-7.0-based)
% rpm -q gcc
gcc-2.96-69

% gcc --version
2.96

% gcc simple.c && ./a.out
frame_height = 70

% gcc -O2 simple.c && ./a.out  # THIS IS BROKEN!
frame_height = 0

% gcc -fno-defer-pop -O2 simple.c && ./a.out
frame_height = 70

*/

#include <stdio.h>

#define SET_CVALUE(psw, field, value) do { (psw)->field = (value); } while (0)

struct ScwmWindow {
  int frame_x;                  /* x position of frame */
  int frame_y;                  /* y position of frame */
  int frame_width;              /* width of frame */
  int frame_height;             /* height of frame */
  int grav_x;
  int grav_y;
};

typedef struct ScwmWindow ScwmWindow;

void ConstrainSize(ScwmWindow *psw, int xmotion, int ymotion, int *pw, int *ph)
{  return; }


void
SetScwmWindowGeometry(ScwmWindow *psw, int x, int y, int w, int h)
{
  if (psw->frame_x != x || psw->frame_y != y) {
    int oldw = w;
    int oldh = h;
    int grav_x = psw->grav_x;
    int grav_y = psw->grav_y;
    int dx, dy;
    ConstrainSize(psw, 0, 0, &w, &h);
    dx = (oldw - w) * (double) grav_x/2.0;
    dy = (oldh - h) * (double) grav_y/2.0;
    x += dx;
    y += dy;
    SET_CVALUE(psw,frame_x,x);
    SET_CVALUE(psw,frame_y,y);
    SET_CVALUE(psw,frame_width,w);
    SET_CVALUE(psw,frame_height,h);
  }
}

int main (char **argv, int argc)
{
  ScwmWindow w;
  w.frame_x = 100;
  w.frame_y = 110;
  w.frame_width = 20;
  w.frame_height = 30;
  w.grav_x = -1;
  w.grav_y = -1;
  SetScwmWindowGeometry(&w,400,420,50,70);
  printf("frame_height = %d\n",w.frame_height);
  if (w.frame_height != 70)
    exit(1);
  else
    exit(0);
}
