;; $Id$
;; (C) 1999 Jeffrey W. Nichols and Greg J. Badros
;;
;; ui-constraints.scm
;;
;; A module that adds support for graphical manipulation of
;; constraints on windows.
;;

(define-module (app scwm ui-constraints))
;;  :use-module (app scwm simple-constraints))

;; (load "/scratch/gjb/scwm/scheme/ui-constraints.scm")
;; (set-current-module the-root-module)


;; Object IDs

;; These definitions specify the ID's for the two types of
;; "objects" created for the interface.  They are used 
;; primarily for type-checking.

(define-public obid-ui-constraint #(?o ?u ?i ?c))
(define-public obid-ui-constraint-class #(?o ?u ?i ?c ?c))


;; Global Lists

;; Two global lists are maintained, one for the constraint
;; classes and another for the constraint instances.  These
;; lists can be used to construct menus and to do the 
;; drawing.

(define-public global-constraint-class-list ())
(define-public global-constraint-instance-list ())


;; UI-CONSTRAINT-CLASS

;; The UI-CONSTRAINT-CLASS specifies a type of constraint.
;;
;; Constraint classes specify methods that can be used to
;; check whether an instance is satisfied, draw an instance,
;; create a new instance of a constraint in the class, and 
;; specify a UI for creating the constraint.
;;
;; The format for an UI-CONSTRAINT-CLASS object is:
;; (obid-ui-constraint-class NAME NUM-WINDOWS CTR UI-CTR DRAW-PROC SATISFIED-PROC)

;; make-ui-constraint-class

;; returns a new constraint class object based on the parameters
;; SIDE-EFFECT: adds new class obj to the global list

(define-public (make-ui-constraint-class NAME NUM-WINDOWS CTR UI-CTR DRAW-PROC SATISFIED-PROC PIXMAP-NAME)
  "CTR takes NUM-WINDOWS windows and creates a constraint of this type.
SATISFIED-PROC is a procedure that takes a single argument, the cn, and tells if it is satisfied
UI-CTR should return the arguments (as a list) for CTR to build the constraint with."
  (let* ((lst (list NAME NUM-WINDOWS CTR UI-CTR DRAW-PROC SATISFIED-PROC PIXMAP-NAME))
         (obj (cons obid-ui-constraint-class lst)))
    (set! global-constraint-class-list (cons obj global-constraint-class-list))
    obj))


;; delete-ui-constraint-class

;; Removes the UI-CONSTRAINT-CLASS permanently
;; SIDE-EFFECT: removes class object from the global list

(define-public (delete-ui-constraint-class! UI-CONSTRAINT-CLASS)
  (set! global-constraint-class-list (delq UI-CONSTRAINT-CLASS global-constraint-class-list)))


(define-public (reset-ui-constraint-classes!)
  "Empty the global list of ui-constraint-classes."
  (set! global-constraint-class-list '()))

;; ui-constraint-class?

;; returns a boolean; true if the object is a constraint class and
;; false otherwise

(define-public (ui-constraint-class? UI-CONSTRAINT-CLASS)
  (and (pair? UI-CONSTRAINT-CLASS) (eq? (car UI-CONSTRAINT-CLASS) obid-ui-constraint-class)))


;; ui-constraint-class-name

;; returns the name of the constraint class.
;; errors if UI-CONSTRAINT-CLASS is not a ui-constraint-class

(define-public (ui-constraint-class-name UI-CONSTRAINT-CLASS)
  (if (ui-constraint-class? UI-CONSTRAINT-CLASS)
      (cadr UI-CONSTRAINT-CLASS)
      (error "Argument to accessor must be a UI-CONSTRAINT-CLASS object")))


;; ui-constraint-class-num-windows

;; returns the number of windows constrainable by instances of
;;   the constraint class.
;; errors if UI-CONSTRAINT-CLASS is not a ui-constraint-class

(define-public (ui-constraint-class-num-windows UI-CONSTRAINT-CLASS)
  (if (ui-constraint-class? UI-CONSTRAINT-CLASS)
      (caddr UI-CONSTRAINT-CLASS)
      (error "Argument to accessor must be a UI-CONSTRAINT-CLASS object")))


;; ui-constraint-class-ctr

;; returns the constructor for instances of the constraint class.
;; errors if UI-CONSTRAINT-CLASS is not a ui-constraint-class

(define-public (ui-constraint-class-ctr UI-CONSTRAINT-CLASS)
  (if (ui-constraint-class? UI-CONSTRAINT-CLASS)
      (cadddr UI-CONSTRAINT-CLASS)
      (error "Argument to accessor must be a UI-CONSTRAINT-CLASS object")))


;; ui-constraint-class-ui-ctr

;; returns the UI constructor for instances of the constraint class.
;; errors if UI-CONSTRAINT-CLASS is not a ui-constraint-class

(define-public (ui-constraint-class-ui-ctr UI-CONSTRAINT-CLASS)
  (if (ui-constraint-class? UI-CONSTRAINT-CLASS)
      (cadddr (cdr UI-CONSTRAINT-CLASS))
      (error "Argument to accessor must be a UI-CONSTRAINT-CLASS object")))


;; ui-constraint-class-draw-proc

;; returns the drawing procedure for instances of the constraint class.
;; errors if UI-CONSTRAINT-CLASS is not a ui-constraint-class

(define-public (ui-constraint-class-draw-proc UI-CONSTRAINT-CLASS)
  (if (ui-constraint-class? UI-CONSTRAINT-CLASS)
      (cadddr (cddr UI-CONSTRAINT-CLASS))
      (error "Argument to accessor must be a UI-CONSTRAINT-CLASS object")))

;; ui-constraint-class-satisfied

;; returns the satisfaction checking procedure for instances of 
;;   the constraint class.
;; errors if UI-CONSTRAINT-CLASS is not a ui-constraint-class

(define-public (ui-constraint-class-satisfied-proc UI-CONSTRAINT-CLASS)
  (if (ui-constraint-class? UI-CONSTRAINT-CLASS)
      (cadddr (cdddr UI-CONSTRAINT-CLASS))
      (error "Argument to accessor must be a UI-CONSTRAINT-CLASS object")))

(define-public (ui-constraint-class-pixmap-name UI-CONSTRAINT-CLASS)
  "Return the pixmap-name of UI-CONSTRAINT-CLASS.
Errors if object is not a ui-constraint-class object."
  (if (ui-constraint-class? UI-CONSTRAINT-CLASS)
      (cadddr (cdr (cdddr UI-CONSTRAINT-CLASS)))
      (error "Argument to accessor must be a UI-CONSTRAINT-CLASS object")))


;; UI-CONSTRAINT

;; The UI-CONSTRAINT objects represents the instance of an active 
;; constraint.
;; These are essentially proxies for constraints objects in the 
;; solver.
;; 
;; The format for an UI-CONSTRAINT object is:
;; (obid-ui-constraint CN ENABLE UI-CONSTRAINT-CLASS LIST-OF-WINDOWS)


;; make-ui-constraint

;; UI-CONSTRAINT-CLASS specifies the type of constraint to be created.
;; WIN-LIST specifies the windows to constraint
;; returns a new constraint instance objects that is NOT enabled.
;; errors if UI-CONSTRAINT-CLASS is not a ui-constraint-class
;; SIDE-EFFECT: adds new instance object to the global list
;; Returned objects are (obid-ui-constraint . (CLASS CN ENABLED?))

(define-public (make-ui-constraint UI-CONSTRAINT-CLASS WIN-LIST)
  (if (ui-constraint-class? UI-CONSTRAINT-CLASS)
      (let* ((cn (apply (ui-constraint-class-ctr UI-CONSTRAINT-CLASS) WIN-LIST))
	     (uc (cons obid-ui-constraint (list UI-CONSTRAINT-CLASS cn #f))))
	(set! global-constraint-instance-list (cons uc global-constraint-instance-list))
	uc)
      (error "Argument must be a UI-CONSTRAINT-CLASS object")))


;; make-ui-constraint-interactively

;; UI-CONSTRAINT-CLASS specifies the type of constraint to be created.
;; uses the ui constructor
;; returns a new constraint instance objects that is NOT enabled.
;; errors if UI-CONSTRAINT-CLASS is not a ui-constraint-class
;; SIDE-EFFECT: adds new instance object to the global list

(define-public (make-ui-constraint-interactively UI-CONSTRAINT-CLASS)
  (if (ui-constraint-class? UI-CONSTRAINT-CLASS)
      (let ((ui-ctr (ui-constraint-class-ui-ctr UI-CONSTRAINT-CLASS)))
	(make-ui-constraint UI-CONSTRAINT-CLASS (ui-ctr)))
      (error "Argument must be a UI-CONSTRAINT-CLASS object")))


;; delete-ui-constraint

;; Removes the UI-CONSTRAINT permanently
;; SIDE-EFFECT: removes instance object from the global list

(define-public (delete-ui-constraint! UI-CONSTRAINT)
  (set! global-constraint-instance-list (delq UI-CONSTRAINT global-constraint-instance-list)))


;; ui-constraint?

;; returns #t if UI-CONSTRAINT is a ui-constraint
;; returns #f otherwise

(define-public (ui-constraint? UI-CONSTRAINT)
  (and (pair? UI-CONSTRAINT) (eq? (car UI-CONSTRAINT) obid-ui-constraint)))


;; ui-constraint-cn

;; returns the CN from the ui-constraint object UI-CONSTRAINT
;; returns #f if UI-CONSTRAINT is not an ui-constraint

(define-public (ui-constraint-cn UI-CONSTRAINT)
  (if (ui-constraint? UI-CONSTRAINT)
      (caddr UI-CONSTRAINT)
      (error "Argument must be a UI-CONSTRAINT object")))


;; ui-constraint-enabled?

;; returns the ENABLE from the ui-constraint object UI-CONSTRAINT
;; errors if UI-CONSTRAINT is not an ui-constraint

(define-public (ui-constraint-enabled? UI-CONSTRAINT)
  (if (ui-constraint? UI-CONSTRAINT)
      (cadddr UI-CONSTRAINT)
      (error "Argument must be a UI-CONSTRAINT object")))

;; ui-constraint-class

;; returns the UI-CONSTRAINT-CLASS from the ui-constraint object UI-CONSTRAINT
;; errors if UI-CONSTRAINT is not an ui-constraint

(define-public (ui-constraint-class UI-CONSTRAINT)
  (if (ui-constraint? UI-CONSTRAINT)
      (cadr UI-CONSTRAINT)
      (error "Argument must be a UI-CONSTRAINT object")))


;; ui-constraint-windows

;; returns the LIST-OF-WINDOWS from the ui-constraint object UI-CONSTRAINT
;; errors if UI-CONSTRAINT is not an ui-constraint

(define-public (ui-constraint-windows UI-CONSTRAINT)
  (if (ui-constraint? UI-CONSTRAINT)
      (cl-windows-of-constraint (ui-constraint-cn UI-CONSTRAINT))
      (error "Argument must be a UI-CONSTRAINT object")))


;; set-enable!

;; PRIVATE
;; sets the enable of a UI-CONSTRAINT object
;; Invariant: UI-CONSTRAINT *must* be a ui-constraint object
;; (test must be performed by caller)

(define (set-enable! UI-CONSTRAINT BOOL)
  (set-car! (cdddr UI-CONSTRAINT) BOOL)
  UI-CONSTRAINT)


;; enable-ui-constraint

;; enables the constraint in the constraint solver
;; errors if UI-CONSTRAINT is not a ui-constraint
;; returns the constraint otherwise

(define-public (enable-ui-constraint UI-CONSTRAINT)
  (if (ui-constraint? UI-CONSTRAINT)
      (let ((cn (ui-constraint-cn UI-CONSTRAINT)))
	(cl-add-constraint (scwm-master-solver) cn)
	(set-enable! UI-CONSTRAINT #t))
      (error "Argument must be a UI-CONSTRAINT object")))


;; disable-ui-constraint

;; disables the constraint in the constraint solver
;; errors if UI-CONSTRAINT is not a ui-constraint
;; returns the constraint otherwise

(define-public (disable-ui-constraint UI-CONSTRAINT)
  (if (ui-constraint? UI-CONSTRAINT)
      (let ((cn (ui-constraint-cn UI-CONSTRAINT)))
	(cl-remove-constraint (scwm-master-solver) cn)
	(set-enable! UI-CONSTRAINT #f))
      (error "Argument must be a UI-CONSTRAINT object")))


;; constraint-satisfied?

;; Returns the status of whether a UI-CONSTRAINT is satisfied.
;; errors if UI-CONSTRAINT is not a ui-constraint

(define-public (constraint-satisfied? UI-CONSTRAINT)
  (if (ui-constraint? UI-CONSTRAINT)
      (let* ((class (ui-constraint-class UI-CONSTRAINT))
	     (satisfied? (ui-constraint-class-satisfied class)))
	(satisfied? UI-CONSTRAINT))
      (error "Argument must be a UI-CONSTRAINT object")))


;; constrained-window-in-focus?

;; Returns #t if one of the windows in the constraint is in focus
;; Returns #f otherwise or if UI-CONSTRAINT is not a ui-constraint

(define (window-in-list-in-focus? win-list)
  (if (null? win-list) 
      #f 
      (if (equal? (car win-list) (current-window-with-focus))
	  #t
	  (window-in-list-in-focus? (cdr win-list)))))

(define-public (constrained-window-in-focus? UI-CONSTRAINT)
  (if (ui-constraint? UI-CONSTRAINT)
      (window-in-list-in-focus? (ui-constraint-windows UI-CONSTRAINT))
      (error "Argument must be a UI-CONSTRAINT object")))


;; draw variables

(define-public ui-constraint-enabled-color "blue")
(define-public ui-constraint-disabled-color "red")

(define-public ui-constraint-in-focus-width 4)
(define-public ui-constraint-no-focus-width 2)


;; do-draw-constraint

;; Calls the draw function of the UI-CONSTRAINT's class.
;; MODE indicates whether constraints should be drawn or erased
;; returns nothing
;; SIDE-EFFECT: draws the constraint representation to the screen

(define (do-draw-constraint UI-CONSTRAINT MODE)
  (if (ui-constraint? UI-CONSTRAINT)
      (let ((color (if (ui-constraint-enabled? UI-CONSTRAINT) 
		       ui-constraint-enabled-color ui-constraint-disabled-color))
	    (width (if (constrained-window-in-focus? UI-CONSTRAINT) 
		       ui-constraint-in-focus-width ui-constraint-no-focus-width))
	    (drawme (ui-constraint-class-draw-proc (ui-constraint-class UI-CONSTRAINT))))
	(drawme UI-CONSTRAINT color width MODE))
      #f))


;; draw-constraint

(define-public (draw-constraint UI-CONSTRAINT)
  (do-draw-constraint UI-CONSTRAINT #t))


;; undraw-constraint

(define-public (undraw-constraint UI-CONSTRAINT)
  (do-draw-constraint UI-CONSTRAINT #f))


;; draw-all-constraints

(define-public (draw-all-constraints)
  (map draw-constraint global-constraint-instance-list))


;; undraw-all-constraints

(define-public (undraw-all-constraints)
  (map undraw-constraint global-constraint-instance-list))


(define-public (disable-all-constraints)
  (map disable-ui-constraint global-constraint-instance-list))


(define-public (enable-all-constraints)
  (map enable-ui-constraint global-constraint-instance-list))
