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

#define FRAME_X(psw) ((psw)->frame_x)
#define FRAME_Y(psw) ((psw)->frame_y)
#define FRAME_WIDTH(psw) ((psw)->frame_width)
#define FRAME_HEIGHT(psw) ((psw)->frame_height)

#define SET_CVALUE(psw, field, value) do { (psw)->field = (value); } while (0)

#ifdef __cplusplus
extern "C" {
#endif

void CassowaryInitClVarsInPsw(PScwmWindow psw);
void CassowaryNewWindow(PScwmWindow psw);
void CassowarySetCValuesAndSolve(PScwmWindow psw, int fSolve);
void CassowaryEditPosition(PScwmWindow psw);
void CassowaryEditSize(PScwmWindow psw);
void SuggestMoveWindowTo(PScwmWindow psw, int x, int y);
void SuggestSizeWindowTo(PScwmWindow psw, int x, int y, int w, int h);
void CassowaryEndEdit(PScwmWindow psw);

#ifdef __cplusplus
}
#endif

#endif /* SCWM_CONSTRAINTS_H__ */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
