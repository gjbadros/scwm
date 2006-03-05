;; $Id$
;; By Greg J. Badros -- 9-June-1999

#!  from /usr/X11R6/include/X11/cursorfont.h
X_cursor
arrow
based_arrow_down
based_arrow_up
boat
bogosity
bottom_left_corner
bottom_right_corner
bottom_side
bottom_tee
box_spiral
center_ptr
circle
clock
coffee_mug
cross
cross_reverse
crosshair
diamond_cross
dot
dotbox
double_arrow
draft_large
draft_small
draped_box
exchange
fleur
gobbler
gumby
hand1
hand2
heart
icon
iron_cross
left_ptr
left_side
left_tee
leftbutton
ll_angle
lr_angle
man
middlebutton
mouse
pencil
pirate
plus
question_arrow
right_ptr
right_side
right_tee
rightbutton
rtl_logo
sailboat
sb_down_arrow
sb_h_double_arrow
sb_left_arrow
sb_right_arrow
sb_up_arrow
sb_v_double_arrow
shuttle
sizing
spider
spraycan
star
target
tcross
top_left_arrow
top_left_corner
top_right_corner
top_side
top_tee
trek
ul_angle
umbrella
ur_angle
watch
xterm
!#

(set! cursor-menu (get-x-cursor "right_ptr"))
(set! cursor-kill (get-x-cursor "spider"))

(define img (load-imlib-image "/scratch/gjb/scwm/pixmaps/cursor.xpm"))

(define crsr (create-pixmap-cursor img))
(set-window-cursor! 'root-window crsr)

(set! cursor-select crsr)

(define w (get-window))

(define i (make-image "resize_br.xpm"))
(image-properties i)

(use-scwm-modules nonants)
(define*-public (image-name->cursor name #:optional (x-hotspot 8) (y-hotspot 8))
  (create-pixmap-cursor (make-image name) #f #f x-hotspot y-hotspot))
(define resize-br (image-name->cursor "resize_br.xpm"))
(define resize-tr (image-name->cursor "resize_tr.xpm"))
(define resize-h (image-name->cursor "resize_h.xpm"))
(define resize-v (image-name->cursor "resize_v.xpm"))
(set-window-cursor! (nonant-decoration w 'northwest) resize-br)
(set-window-cursor! (nonant-decoration w 'southeast) resize-br)
(set-window-cursor! (nonant-decoration w 'southwest) resize-tr)
(set-window-cursor! (nonant-decoration w 'northeast) resize-tr)
(set-window-cursor! (nonant-decoration w 'north) resize-v)
(set-window-cursor! (nonant-decoration w 'south) resize-v)
(set-window-cursor! (nonant-decoration w 'east) resize-h)
(set-window-cursor! (nonant-decoration w 'west) resize-h)
