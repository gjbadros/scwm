/* $Id$
 * scwm-constraints.h
 *
 * (C) 1998 Greg J. Badros
 */

#ifndef SCWM_CONSTRAINTS_H__
#define SCWM_CONSTRAINTS_H__

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

struct ScwmWindow;
typedef struct ScwmWindow *PScwmWindow;
typedef const struct ScwmWindow *ConstPScwmWindow;

struct ScreenInfo;

#define FRAME_X(psw) ((psw)->frame_x)
#define FRAME_Y(psw) ((psw)->frame_y)
#define ABS(x) ((x)<0?-(x):(x))
#define FRAME_X_NONVIRT(psw) (ABS((psw)->frame_x) % Scr.DisplayWidth)
#define FRAME_Y_NONVIRT(psw) (ABS((psw)->frame_y) % Scr.DisplayHeight)

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
void CassowarySetCValuesAndSolve(PScwmWindow psw, int fSolve);
void CassowaryEditPosition(PScwmWindow psw);
void CassowaryEditSize(PScwmWindow psw);
void SuggestMoveWindowTo(PScwmWindow psw, int x, int y, Bool fOpaque);
void SuggestSizeWindowTo(PScwmWindow psw, int x, int y, int w, int h, Bool fOpaque);
void CassowaryEndEdit(PScwmWindow psw);
void ChangeVirtualPosition(int vx, int vy, Bool fGrab);
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
