/* $Id$
 * scwm-constraints.h
 * Copyright (C) 1998, 1999, 2000 Greg J. Badros
 */

#ifndef SCWM_CONSTRAINTS_H__
#define SCWM_CONSTRAINTS_H__

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

struct ScwmWindow;
typedef struct ScwmWindow *PScwmWindow;
typedef const struct ScwmWindow *ConstPScwmWindow;

struct ScreenInfo;

#define ABS(x) ((x)<0?-(x):(x))

#define FRAME_X(psw) ((psw)->frame_x)
#define FRAME_Y(psw) ((psw)->frame_y)

#define FRAME_X_VIRT(psw) (FRAME_X(psw) + ((psw)->fSticky? Scr.Vx:0))
#define FRAME_Y_VIRT(psw) (FRAME_Y(psw) + ((psw)->fSticky? Scr.Vy:0))

/* VP == viewport position */
#define FRAME_X_VP(psw) (FRAME_X(psw) - ((psw)->fSticky? 0:Scr.Vx))
#define FRAME_Y_VP(psw) (FRAME_Y(psw) - ((psw)->fSticky? 0:Scr.Vy))

#define ICON_X(psw) ((psw)->icon_x_loc)
#define ICON_Y(psw) ((psw)->icon_y_loc)

#define ICON_X_VIRT(psw) (ICON_X(psw) + ((psw)->fStickyIcon? Scr.Vx:0))
#define ICON_Y_VIRT(psw) (ICON_Y(psw) + ((psw)->fStickyIcon? Scr.Vy:0))

/* VP == viewport position */
#define ICON_X_VP(psw) (ICON_X(psw) - ((psw)->fStickyIcon? 0:Scr.Vx))
#define ICON_Y_VP(psw) (ICON_Y(psw) - ((psw)->fStickyIcon? 0:Scr.Vy))


#define FRAME_WIDTH(psw) ((psw)->frame_width)
#define FRAME_HEIGHT(psw) ((psw)->frame_height)

#define SET_CVALUE(psw, field, value) do { (psw)->field = (value); } while (0)

#ifdef __cplusplus
extern "C" {
#endif

#define Bool int /* from Xlib.h */

void CassowaryInitClVarsInPsw(PScwmWindow psw);
void CassowaryInitClVarsInPscreen(struct ScreenInfo *pscreen);
void CassowaryNewWindow(PScwmWindow psw);
void CassowaryCloseWindow(PScwmWindow psw);
void CassowarySetCValuesAndSolve(PScwmWindow psw, int fSolve);
void CassowaryEditPosition(PScwmWindow psw);
void CassowaryEditSize(PScwmWindow psw);
void SuggestMoveWindowTo(PScwmWindow psw, int x, int y, Bool fOpaque);
Bool SuggestSizeWindowTo(PScwmWindow psw, int x, int y, int w, int h, Bool fOpaque);
void CassowaryEndEdit(PScwmWindow psw);
void ChangeVirtualPosition(int vx, int vy);
void CassowaryModifyOpaqueFlag(Bool *pfOpaque);

void init_constraint_primitives();

#ifdef __cplusplus
}
#endif

#endif /* SCWM_CONSTRAINTS_H__ */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

