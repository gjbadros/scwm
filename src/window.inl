/* $Id$
 * window.inl
 * (C) 1999 Greg J. Badros
 */

#include "scwm-guile.h"

SCWM_STATIC_INLINE int
DecorationWidth(const ScwmWindow *psw)
{
  return 2 * psw->xboundary_width;
}

SCWM_STATIC_INLINE int
DecorationXOffset(const ScwmWindow *psw)
{
  return psw->xboundary_width; 
  /* + psw->bw; GJB:FIXME:: do I need this? --10/01/99 gjb */
}

SCWM_STATIC_INLINE int
DecorationHeight(const ScwmWindow *psw)
{
  return 2 * psw->boundary_width + psw->title_height;
}

SCWM_STATIC_INLINE int
DecorationYOffset(const ScwmWindow *psw)
{
  return psw->title_height + psw->boundary_width;
  /* + psw->bw; GJB:FIXME:: do I need this? --10/01/99 gjb */
}

SCWM_STATIC_INLINE int
MaxFrameWidth(const ScwmWindow *psw)
{
  return psw->hints.max_width + 2*psw->xboundary_width;
}

SCWM_STATIC_INLINE int
MinFrameWidth(const ScwmWindow *psw)
{
  return psw->hints.min_width + 2*psw->xboundary_width;
}

SCWM_STATIC_INLINE int
MinFrameHeight(const ScwmWindow *psw)
{
  return psw->hints.min_height + 2*psw->boundary_width + psw->title_height;
}

SCWM_STATIC_INLINE int
MaxFrameHeight(const ScwmWindow *psw)
{
  return psw->hints.max_height + 2*psw->boundary_width + psw->title_height; 
}

SCWM_STATIC_INLINE int
ClientWidth(const ScwmWindow *psw)
{
  return FRAME_WIDTH(psw) - DecorationWidth(psw);
}

SCWM_STATIC_INLINE int
ClientHeight(const ScwmWindow *psw)
{
  return FRAME_HEIGHT(psw) - DecorationHeight(psw);
}
