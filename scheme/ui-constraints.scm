;;; $Id$
;;; Copyright (C) 1999, 2000 Jeffrey W. Nichols and Greg J. Badros
;;;
;;; ui-constraints.scm
;;;
;;; A module that adds support for graphical manipulation of
;;; constraints on windows.
;;;
;;; Constraints TODO
;;;
;;; o Use ScwmButtons instead of rolling our own
;;; o Fix visual representations for the handful of constraints w/o good ones
;;; o Use new version of get-window-nonant-interactively that provides
;;;   feedback about which nonant is selected
;;; o Improve get-window-nonant-interactively's marking of a nonant-- ideally
;;;   colorize the partial border area....
;;; o Extend ScwmButtons to permit animated icons when mouse is over button
;;; o Extend ScwmButtons to have a right-click menu for
;;;   -> turning on/off tooltips
;;;   -> changing orientation
;;;   -> changing auto-orientation

(define-module (app scwm ui-constraints)
  :use-module (app scwm base)
  :use-module (app scwm hooks)
  :use-module (app scwm constraint-animation)
  :use-module (app scwm message-window)
  :use-module (app scwm listops)
  :use-module (app scwm winops)
  :use-module (app scwm optargs)
  :use-module (cassowary constraints)
  )


;; in case we re-load
;;(reset-ui-constraint-classes!)

;; (use-scwm-modules ui-constraints)
;; (use-scwm-modules ui-constraints-classes)

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

;; (length global-constraint-instance-list)

;; UI-CONSTRAINT-CLASS

;; The UI-CONSTRAINT-CLASS specifies a type of constraint.
;;
;; Constraint classes specify methods that can be used to
;; check whether an instance is satisfied, draw an instance,
;; create a new instance of a constraint in the class, and 
;; specify a UI for creating the constraint.
;;
;; The format for an UI-CONSTRAINT-CLASS object is:
;; (obid-ui-constraint-class NAME NUM-WINDOWS CTR UI-CTR DRAW-PROC SATISFIED-PROC PIXMAP-NAME MENUNAME-PROC)
;;

;; make-ui-constraint-class

;; returns a new constraint class object based on the parameters
;; SIDE-EFFECT: adds new class obj to the global list

(define-public (make-ui-constraint-class name description num-windows ctr ui-ctr draw-proc satisfied-proc pixmap-name pixmap2-name menuname-proc)
  "Creates a new ui-constraint-class object.
CTR takes a set of arguments and installs the appropriate constraints in the solver.  A number of
windows should be included in those arguments, falling somewhere in the range of NUM-WINDOWS.  
NUM-WINDOWS is a list of '(min-win max-win) where the values specify the minimum and maximum number
of windows which may be constrained.  Max-win may optionally be specified as a non-number (preferably
as #t), in which case the constraint is taken to be able to constraint an infinite number of windows.
SATISFIED-PROC is a procedure that takes a single argument, the cn, and tells if it is satisfied
UI-CTR should return the arguments (as a list) for CTR to build the constraint with.  UI-CTR should
return #f if the user cancels the construction or fails to follow the interface correctly.  PIXMAP-NAME
is the name of the pixmap to associate with this constraint-class in the user interface.
MENUNAME-PROC is a proc that takes a UI-CONSTRAINT as an arg and returns the name that should be used
for the constraint in the toggle menu.  
This routine returns a new constraint class object based on the parameters.
SIDE-EFFECT: addes new class obj to the global class list."
  (let ((obj (vector obid-ui-constraint-class name description num-windows 
		     ctr ui-ctr draw-proc satisfied-proc pixmap-name pixmap2-name menuname-proc))
	(old (get-ui-constraint-class-by-name name)))
    (if (ui-constraint-class? old)
	(delete-ui-constraint-class! old))
    (set! global-constraint-class-list (cons obj global-constraint-class-list))
    (run-hook constraint-class-add-hook obj)
    obj))


(define-public (ui-constraint-class-creator class)
  "Return a list that contains a code snippet can create CLASS.
The resulting list can then be evaluated to create that constraint class.
Be sure to (use-scwm-modules ui-constraints-classes ui-constraints-composition) 
before evaling the list."
  `(make-ui-constraint-class
    ,(ui-constraint-class-name class)
    ,(ui-constraint-class-description class)
    ,(ui-constraint-class-num-windows class)
    ,(procedure-source (ui-constraint-class-ctr class))
    ,(procedure-source (ui-constraint-class-ui-ctr class))
    ,(procedure-source (ui-constraint-class-draw-proc class))
    ,(procedure-source (ui-constraint-class-satisfied-proc class))
    ,(ui-constraint-class-pixmap-name class)
    ,(ui-constraint-class-pixmap2-name class)
    ,(procedure-source (ui-constraint-class-menuname-proc class))))

;; (ui-constraint-class-creator (car global-constraint-class-list))
;; (procedure-source ui-constraint-class-creator)

;; delete-ui-constraint-class

;; Removes the UI-CONSTRAINT-CLASS permanently
;; SIDE-EFFECT: removes class object from the global list

(define-public (delete-ui-constraint-class! ui-constraint-class)
  "Removes UI-CONSTRAINT-CLASS from the global class list.
SIDE-EFFECT: removes class object from the global class list."
  (begin
    (set! global-constraint-class-list (delq ui-constraint-class global-constraint-class-list))
    (run-hook constraint-class-delete-hook ui-constraint-class)))


(define-public (reset-ui-constraint-classes!)
  "Empty the global list of ui-constraint-classes."
  (begin
    (for-each (lambda (class) 
		(run-hook constraint-class-delete-hook class))
	      global-constraint-class-list)
    (set! global-constraint-class-list ())))

;; ui-constraint-class?

;; returns a boolean; true if the object is a constraint class and
;; false otherwise

(define-public (ui-constraint-class? ui-constraint-class)
  "Returns a boolean: true if UI-CONSTRAINT-CLASS is a vector and starts with the
correct obid.  False otherwise."
  (and (vector? ui-constraint-class) (eq? (vector-ref ui-constraint-class 0) obid-ui-constraint-class)))


;; get-ui-constraint-class-by-name
;; returns the ui-constraint-class object with the passed in name
;; could probably generalize this to fetch a class by any property

(define (constraint-list-search name list)
  (if (null? list)
      #f
      (let* ((class (car list))
	     (cname (ui-constraint-class-name class)))
	(if (equal? name cname)
	    class
	    (constraint-list-search name (cdr list))))))

(define-public (get-ui-constraint-class-by-name name)
  (constraint-list-search name global-constraint-class-list))


;; ui-constraint-class-name

;; returns the name of the constraint class.
;; errors if UI-CONSTRAINT-CLASS is not a ui-constraint-class

(define-public (ui-constraint-class-name ui-constraint-class)
  "Returns the name of the constraint class.
Errors if UI-CONSTRAINT-CLASS is not a ui-constraint-class."
  (if (ui-constraint-class? ui-constraint-class)
      (vector-ref ui-constraint-class 1)
      (error "Argument to accessor must be a UI-CONSTRAINT-CLASS object")))


(define-public (ui-constraint-class-description ui-constraint-class)
  "Returns the description for the constraint class.  
Erors if UI-CONSTRAINT-CLASS is not a ui-constraint-class."
  (if (ui-constraint-class? ui-constraint-class)
      (vector-ref ui-constraint-class 2)
      (error "Argument to accessor must be a UI-CONSTRAINT-CLASS object")))

;; ui-constraint-class-num-windows

;; returns the number of windows constrainable by instances of
;;   the constraint class.
;; errors if UI-CONSTRAINT-CLASS is not a ui-constraint-class

(define-public (ui-constraint-class-num-windows ui-constraint-class)
  "Returns the number of windows constrainable by instances of the constraint 
class.  errors if UI-CONSTRAINT-CLASS is not a ui-constraint-class."
  (if (ui-constraint-class? ui-constraint-class)
      (vector-ref ui-constraint-class 3)
      (error "Argument to accessor must be a UI-CONSTRAINT-CLASS object")))


;; ui-constraint-class-ctr

;; returns the constructor for instances of the constraint class.
;; errors if UI-CONSTRAINT-CLASS is not a ui-constraint-class

(define-public (ui-constraint-class-ctr ui-constraint-class)
  "Returns the constructor for instance of the constraint class.  errors
if UI-CONSTRAINT-CLASS is not a ui-constraint-class."
  (if (ui-constraint-class? ui-constraint-class)
      (vector-ref ui-constraint-class 4)
      (error "Argument to accessor must be a UI-CONSTRAINT-CLASS object")))


;; ui-constraint-class-ui-ctr

;; returns the UI constructor for instances of the constraint class.
;; errors if UI-CONSTRAINT-CLASS is not a ui-constraint-class

(define-public (ui-constraint-class-ui-ctr ui-constraint-class)
  "Returns the UI constructor for instances of the constraint class.
errors if UI-CONSTRAINT-CLASS is not a ui-constraint-class"
  (if (ui-constraint-class? ui-constraint-class)
      (vector-ref ui-constraint-class 5)
      (error "Argument to accessor must be a UI-CONSTRAINT-CLASS object")))


;; ui-constraint-class-draw-proc

;; returns the drawing procedure for instances of the constraint class.
;; errors if UI-CONSTRAINT-CLASS is not a ui-constraint-class

(define-public (ui-constraint-class-draw-proc ui-constraint-class)
  "returns the drawing procedure for instances of the constraint class.
errors if UI-CONSTRAINT-CLASS is not a ui-constraint-class"
  (if (ui-constraint-class? ui-constraint-class)
      (vector-ref ui-constraint-class 6)
      (error "Argument to accessor must be a UI-CONSTRAINT-CLASS object")))

;; ui-constraint-class-satisfied

;; returns the satisfaction checking procedure for instances of 
;;   the constraint class.
;; errors if UI-CONSTRAINT-CLASS is not a ui-constraint-class

(define-public (ui-constraint-class-satisfied-proc ui-constraint-class)
  "Returns the satisfaction checking procedure for instances of the
constraint class.  errors if UI-CONSTRAINT-CLASS is not a ui-constraint-class"
  (if (ui-constraint-class? ui-constraint-class)
      (vector-ref ui-constraint-class 7)
      (error "Argument to accessor must be a UI-CONSTRAINT-CLASS object")))

;; ui-constraint-class-pixmap-name

(define-public (ui-constraint-class-pixmap-name ui-constraint-class)
  "Return the pixmap-name of UI-CONSTRAINT-CLASS.
Errors if object is not a ui-constraint-class object."
  (if (ui-constraint-class? ui-constraint-class)
      (vector-ref ui-constraint-class 8)
      (error "Argument to accessor must be a UI-CONSTRAINT-CLASS object")))

(define-public (ui-constraint-class-pixmap2-name ui-constraint-class)
  "Return the pixmap2-name of UI-CONSTRAINT-CLASS.
Errors if object is not a ui-constraint-class object."
  (if (ui-constraint-class? ui-constraint-class)
      (vector-ref ui-constraint-class 9)
      (error "Argument to accessor must be a UI-CONSTRAINT-CLASS object")))

;; ui-constraint-class-menuname-proc

(define-public (ui-constraint-class-menuname-proc ui-constraint-class)
  "Return the proc for determining the name for the constraint in
the toggle menu.  Errors if object is not a ui-constraint-class object."
  (if (ui-constraint-class? ui-constraint-class)
      (vector-ref ui-constraint-class 10)
      (error "Argument to accessor must be a UI-CONSTRAINT-CLASS object")))


;; UI-CONSTRAINT

;; The UI-CONSTRAINT objects represents the instance of an active 
;; constraint.
;; These are essentially proxies for constraints objects in the 
;; solver.
;; 
;; The format for an UI-CONSTRAINT object is:
;; (obid-ui-constraint (CN1 CN2 ...) ENABLE UI-CONSTRAINT-CLASS LIST-OF-WINDOWS)


;; make-ui-constraint

;; UI-CONSTRAINT-CLASS specifies the type of constraint to be created.
;; WIN-LIST specifies the windows to constrain
;; returns a new constraint instance objects that is NOT enabled.
;; errors if UI-CONSTRAINT-CLASS is not a ui-constraint-class
;; SIDE-EFFECT: adds new instance object to the global list
;; Returned objects are (obid-ui-constraint . (CLASS (CN) ENABLED? LIST-OF-WINDOWS OPTS))

(define*-public (make-ui-constraint ui-constraint-class arg-list #&key (visible? #t))
  "UI-CONSTRAINT-CLASS specified the type of constraint to be created.
WIN-LIST specifies the windows to be constrained.  Returns a new constraint
object that is NOT enabled.  errors if UI-CONSTRAINT-CLASS is not valid.
Returned objects are #(obid-ui-constraint CLASS CN ENABLED? LIST-OF-WINDOWS OPTS BUT DHOOKS)
The OPTS param is a spot for optional data to be specified by the ui-constraint-class 
constructor.  If data returns from that constructor in list form, the first element of
the list is assumed to be the CN and the cdr is stuck in OPTS.
BUT may contain a reference to the gtk button for the constraint instance in the 
toggle menu if that feature is in use.
DHOOKS is a list of hook procedures that should be called when the enable is changed on this
constraint.  These hook functions may only be added after an instance is created.
SIDE-EFFECT: adds new instance object to the global list."
  (if (ui-constraint-class? ui-constraint-class)
      (let* ((vars (apply (ui-constraint-class-ctr ui-constraint-class) arg-list))
	     (cn (car vars))
	     (win-list (cadr vars))
	     (opts (cddr vars))
	     (uc (vector obid-ui-constraint ui-constraint-class cn #f win-list opts #f (make-hook 1))))
	(if visible?
	    (begin
	      (set! global-constraint-instance-list (cons uc global-constraint-instance-list))
	      (run-hook constraint-add-hook uc)
	      (run-hook constraint-composition-record-hook uc arg-list)))
	uc)
      (error "make-ui-constraint first argument must be UI-CONSTRAINT-CLASS object")))


;; make-ui-constraint-interactively

;; UI-CONSTRAINT-CLASS specifies the type of constraint to be created.
;; uses the ui constructor
;; returns a new constraint instance objects that is NOT enabled.
;; errors if UI-CONSTRAINT-CLASS is not a ui-constraint-class
;; SIDE-EFFECT: adds new instance object to the global list

(define-public (make-ui-constraint-interactively ui-constraint-class)
  "Uses the UI constructor of the constraint class to prompt the user to
specify options for the constraint.  errors if UI-CONSTRAINT-CLASS is 
not a ui-constraint-class.  Calls make-ui-constraint (see above)."
  (if (ui-constraint-class? ui-constraint-class)
      (let* ((ui-ctr (ui-constraint-class-ui-ctr ui-constraint-class))
	     (arg-list (ui-ctr)))
	(if arg-list 
	    (let ((uic (make-ui-constraint ui-constraint-class arg-list)))
	      (if (ui-constraint? uic)
		  uic
		  (display-message-briefly "Error making constraint!")))
	    (display-message-briefly "Error making constraint -- no arguments!")))
      (error "make-ui-constraint-interactively first argument must be UI-CONSTRAINT-CLASS object")))


;; delete-ui-constraint

;; Removes the UI-CONSTRAINT permanently
;; SIDE-EFFECT: removes instance object from the global list

(define-public (delete-ui-constraint! ui-constraint)
  "Removes the UI-CONSTRAINT permanently.
SIDE-EFFECT: removes instance object from the global list"
  (set! global-constraint-instance-list (delq ui-constraint global-constraint-instance-list))
  (disable-ui-constraint ui-constraint)
  (run-hook constraint-delete-hook ui-constraint))


;; ui-constraint?

;; returns #t if UI-CONSTRAINT is a ui-constraint
;; returns #f otherwise

(define-public (ui-constraint? ui-constraint)
  "Returns #t if UI-CONSTRAINT is a ui-constraint.
returns #f otherwise."
  (and (vector? ui-constraint) (eq? (vector-ref ui-constraint 0) obid-ui-constraint)))


;; ui-constraint-cn

;; returns the CN from the ui-constraint object UI-CONSTRAINT
;; errors if UI-CONSTRAINT is not an ui-constraint

(define-public (ui-constraint-cn ui-constraint)
  "Returns the CN list from the ui-constraint object UI-CONSTRAINT.
errors if UI-CONSTRAINT is not a ui-constraint."
  (if (ui-constraint? ui-constraint)
      (vector-ref ui-constraint 2)
      (error "ui-constraint-cn first argument must be a UI-CONSTRAINT object")))


;; ui-constraint-enabled?

;; returns the ENABLE from the ui-constraint object UI-CONSTRAINT
;; errors if UI-CONSTRAINT is not an ui-constraint

(define-public (ui-constraint-enabled? ui-constraint)
  "Returns the ENABLE from the ui-constraint object UI-CONSTRAINT.
errors if UI-CONSTRAINT is not an ui-constraint."
  (if (ui-constraint? ui-constraint)
      (vector-ref ui-constraint 3)
      (error "ui-constraint-enabled? first argument must be a UI-CONSTRAINT object")))

;; ui-constraint-class

;; returns the UI-CONSTRAINT-CLASS from the ui-constraint object UI-CONSTRAINT
;; errors if UI-CONSTRAINT is not an ui-constraint

(define-public (ui-constraint-class ui-constraint)
  "Returns the UI-CONSTRAINT-CLASS from the ui-constraint object UI-CONSTRAINT.
errors if UI-CONSTRAINT is not an ui-constraint."
  (if (ui-constraint? ui-constraint)
      (vector-ref ui-constraint 1)
      (error "ui-constraint-class first argument must be a UI-CONSTRAINT object")))


;; ui-constraint-windows

;; returns the LIST-OF-WINDOWS from the ui-constraint object UI-CONSTRAINT
;; errors if UI-CONSTRAINT is not an ui-constraint

(define-public (ui-constraint-windows ui-constraint)
  "Returns the LIST-OF-WINDOWS from the ui-constraint object UI-CONSTRAINT.
errors if UI-CONSTRAINT is not an ui-constraint."
  (if (ui-constraint? ui-constraint)
      (vector-ref ui-constraint 4)
      (error "ui-constraint-windows argument must be a UI-CONSTRAINT object")))

;; ui-constraint-opts

;; returns the optional data list that may be added by the constraint class
;; returns #f if no such data exists

(define-public (ui-constraint-opts ui-constraint)
  "Returns the list of optional data that may be added by the constraint class.
Returns #f if no such data exists.  errors if UI-CONSTRAINT is not a ui-constraint."
  (if (ui-constraint? ui-constraint)
      (vector-ref ui-constraint 5)
      (error "ui-constraint-opts argument must be a UI-CONSTRAINT object")))


;; ui-constraint-button

(define-public (ui-constraint-button ui-constraint)
  "Returns the a reference to panel representing the UI-CONSTRAINT in the toggle menu.
Returns #f if the gtk toggle menu feature is not in use."
  (if (ui-constraint? ui-constraint)
      (vector-ref ui-constraint 6)
      (error "ui-constraint-button argument must be a UI-CONSTRAINT object")))


;; ui-constraint-set-button!

(define-public (ui-constraint-set-button! ui-constraint button)
  "Sets the reference to the gtk button for this instance in the toggle menu.
Errors if UI-CONSTRAINT is not a ui-constraint."
  (if (ui-constraint? ui-constraint)
      (vector-set! ui-constraint 6 button)
      (error "ui-constraint-set-button! first argument must be a UI-CONSTRAINT object")))



;; set-enable!

;; PRIVATE
;; sets the enable of a UI-CONSTRAINT object
;; Invariant: UI-CONSTRAINT *must* be a ui-constraint object
;; (test must be performed by caller)

(define (set-enable! ui-constraint bool)
  (vector-set! ui-constraint 3 bool)
  ui-constraint)


;; enable-ui-constraint

;; enables the constraint in the constraint solver
;; errors if UI-CONSTRAINT is not a ui-constraint
;; returns the constraint otherwise

(define-public (enable-ui-constraint ui-constraint)
  "Enables the constraint in the constraint solver.
errors if UI-CONSTRAINT is not a ui-constraint.
returns the constraint."
  (if (ui-constraint? ui-constraint)
      (let ((cn (ui-constraint-cn ui-constraint))
	    (hook (ui-constraint-enable-hooks ui-constraint)))
	(start-animating-scwm-resolves)
	(map (lambda (c) (cl-add-constraint (scwm-master-solver) c)) cn)
	(stop-animating-scwm-resolves)
	(set-enable! ui-constraint #t)
	(run-hook hook #t))
      (error "enable-ui-constraint argument must be a UI-CONSTRAINT object")))


;; disable-ui-constraint

;; disables the constraint in the constraint solver
;; errors if UI-CONSTRAINT is not a ui-constraint
;; returns the constraint otherwise

(define-public (disable-ui-constraint ui-constraint)
  "Disables the constraint in the constraint solver
errors if UI-CONSTRAINT is not a ui-constraint
returns the constraint"
  (if (ui-constraint? ui-constraint)
      (let ((cn (ui-constraint-cn ui-constraint))
	    (hook (ui-constraint-enable-hooks ui-constraint)))
	(start-animating-scwm-resolves)
	(map (lambda (c) (cl-remove-constraint (scwm-master-solver) c)) cn)
	(stop-animating-scwm-resolves)
	(set-enable! ui-constraint #f)
	(run-hook hook #f))
      (error "disable-ui-constraint argument must be a UI-CONSTRAINT object")))


;; constraint-satisfied?

;; Returns the status of whether a UI-CONSTRAINT is satisfied.
;; errors if UI-CONSTRAINT is not a ui-constraint

(define-public (constraint-satisfied? ui-constraint)
  "Returns the status of whether a UI-CONSTRAINT is satisfied.
errors if UI-CONSTRAINT is not an ui-constraint."
  (if (ui-constraint? ui-constraint)
      (let* ((class (ui-constraint-class ui-constraint))
	     (satisfied? (ui-constraint-class-satisfied class)))
	(satisfied? ui-constraint))
      (error "constraint-satisfied? argument must be a UI-CONSTRAINT object")))


;; constrained-window-in-focus?

;; Returns #t if one of the windows in the constraint is in focus
;; Returns #f otherwise or if UI-CONSTRAINT is not a ui-constraint

(define (window-in-list-in-focus? win-list)
  (if (null? win-list) 
      #f 
      (if (equal? (car win-list) (window-with-focus))
	  #t
	  (window-in-list-in-focus? (cdr win-list)))))

(define-public (constrained-window-in-focus? ui-constraint)
  "Returns #t if one of the windows in the constraint is in focus.
Returns #f otherwise or if UI-CONSTRAINT is not a ui-constraint."
  (if (ui-constraint? ui-constraint)
      (window-in-list-in-focus? (ui-constraint-windows ui-constraint))
      (error "constrained-window-in-focus? argument must be a UI-CONSTRAINT object")))


(define-public (ui-constraints-involving-window win)
  "Returns the list of ui-constraint objects that involve WIN.
The entire global-constraint-instance-list is checked."
  (filter-map 
   (lambda (c) 
     (let ((wins (ui-constraint-windows c)))
       (if (member win wins) c #f)))
       global-constraint-instance-list))


(define-public (ui-constraints-involving-two-windows win1 win2)
  "Returns the list of ui-constraint objects that involve WIN1 and WIN2.
The entire global-constraint-instance-list is checked."
  (filter-map 
   (lambda (c) 
     (let ((wins (ui-constraint-windows c)))
       (if (and (member win1 wins) (member win2 wins)) c #f)))
       global-constraint-instance-list))


(define-public (delete-ui-constraints-involving-window! win)
  "Delete all the ui-constraint objects that involve WIN.
This removes the constraints from the global-constraint-instance-list."
  (for-each delete-ui-constraint! (ui-constraints-involving-window win)))

(define-public (delete-inferred-ui-constraints-involving-window! win)
  "Delete all the ui-constraint objects that involve WIN.
This removes the constraints from the global-constraint-instance-list."
  (for-each (lambda (uic)
	      (if (object-property uic 'inferred)
		  (delete-ui-constraint! uic)))
	    (ui-constraints-involving-window win)))

;; do-draw-constraint

;; PRIVATE
;; Calls the draw function of the UI-CONSTRAINT's class.
;; MODE indicates whether constraints should be drawn or erased
;; returns nothing
;; SIDE-EFFECT: draws the constraint representation to the screen

(define (do-draw-constraint ui-constraint mode)
  (if (ui-constraint? ui-constraint)
      (let ((enable (ui-constraint-enabled? ui-constraint)) 
	    (focus (constrained-window-in-focus? ui-constraint))
	    (drawme (ui-constraint-class-draw-proc (ui-constraint-class ui-constraint))))
	(drawme ui-constraint enable focus mode))
      (error "do-draw-constraint first argument must be a UI-CONSTRAINT object")))


;; draw-constraint

(define-public (draw-constraint ui-constraint)
  "Draw the UI-CONSTRAINT.  error if UI-CONSTRAINT is not
an ui-constraint."
  (do-draw-constraint ui-constraint #t))


;; undraw-constraint

(define-public (undraw-constraint ui-constraint)
  "Undraw the UI-CONSTRAINT.  error if UI-CONSTRAINT is not
an ui-constraint."
  (do-draw-constraint ui-constraint #f))


;; helpers for the following draw many constraint functions.

(define (draw-enabled cn)
  (if (ui-constraint-enabled? cn)
      (draw-constraint cn)))

(define (undraw-enabled cn)
  (if (ui-constraint-enabled? cn)
      (undraw-constraint cn)))

;; (use-scwm-modules optargs)

;; draw-constraints-of-window

(define*-public (draw-constraints-of-window win #&key (draw-disabled #t))
  "Draw all constraints associated with WIN.
If WIN is not specified, the user is prompted to select a window."
  (for-each (if draw-disabled draw-constraint draw-enabled) (ui-constraints-involving-window win)))


;; undraw-constraints-of-window

(define*-public (undraw-constraints-of-window win #&key (draw-disabled #t))
  "Undraw all constraints associated with WIN.
If WIN is not specified, the user is prompted to select a window."
  (for-each (if draw-disabled undraw-constraint undraw-enabled) (ui-constraints-involving-window win)))


;; draw-all-constraints

(define*-public (draw-all-constraints #&key (draw-disabled #t))
  "Draw all constraints in the global instance list."
  (for-each (if draw-disabled draw-constraint draw-enabled) global-constraint-instance-list))


;; undraw-all-constraints

(define*-public (undraw-all-constraints #&key (draw-disabled #t))
  "Undraw all constraints in the global instance list."
  (for-each (if draw-disabled undraw-constraint undraw-enabled) global-constraint-instance-list))


;; disable-all-constraints

(define-public (disable-all-constraints)
  "Disable all constraints in the global instance list."
  (for-each disable-ui-constraint global-constraint-instance-list))

;; enable-all-constraints

(define-public (enable-all-constraints)
  "Enable all constraints in the global instance list."
  (for-each enable-ui-constraint global-constraint-instance-list))

;; delete-all-constraints
(define-public (delete-all-constraints)
  "Delete all constraints in the global instance list."
  (for-each delete-ui-constraint! global-constraint-instance-list))


;; HOOKS

;; This code handles the hook functions which are called when 
;; a constraint is added or deleted.

;;----------------------------------------------
;; Constraint Class Hooks

;; The lists of hook functions

(define-scwm-hook constraint-class-add-hook 1
  "Run when a new constraint class is added.
Invoked as (proc ADDED-CONSTRAINT-CLASS-OBJECT).")

(define-scwm-hook constraint-class-delete-hook 1
  "Run when a constraint class is deleted.
Invoked as (proc DELTED-CONSTRAINT-CLASS-OBJECT).")


;; Constraint Instance Hooks

;; The lists of hook functions

(define-scwm-hook constraint-add-hook 1
  "Run when a constraint is added.
Invoked as (proc ADDED-CONSTRAINT-OBJECT).")

(define-scwm-hook constraint-delete-hook 1
  "Run when a constraint is deleted.
Invoked as (proc DELETD-CONSTRAINT-OBJECT).")

(define-scwm-hook constraint-composition-record-hook 2
  "Run when a constraint is added during composition recording.
Invoked as (proc ADDED-CONSTRAINT-OBJECT ARGUMENTS).")

;; Constraint Instance (Un)Enable Hooks

;; ui-constraint-add-enable-hook

(define-public (ui-constraint-add-enable-hook ui-constraint hook)
  "Adds a HOOK proc which will be called when UI-CONSTRAINT enable state changes.
HOOK should take one argument which is the new state of the UI-CONSTRAINT.
Errors if UI-CONSTRAINT is not a ui-constraint object."
  (let ((hlist (ui-constraint-enable-hooks ui-constraint)))
      (add-hook! hlist hook)))
    
;; ui-constraint-remove-enable-hook

(define-public (ui-constraint-remove-enable-hook ui-constraint hook)
  "Removes a HOOK proc from the list in UI-CONSTRAINT.
Errors if UI-CONSTRAINT is not a ui-constraint object."
  (let ((hlist (ui-constraint-enable-hooks ui-constraint)))
    (remove-hook! hlist hook)))

;; ui-constraint-enable-hooks

(define-public (ui-constraint-enable-hooks ui-constraint)
  "Returns a list of the enable-hook functions added to the UI-CONSTRAINT.
Errors if UI-CONSTRAINT is not a ui-constraint object."
  (if (ui-constraint? ui-constraint)
      (vector-ref ui-constraint 7)
      (error "ui-constraint-enable-hooks argument must be a UI-CONSTRAINT object")))

(add-hook! window-close-hook delete-ui-constraints-involving-window!)

(define*-public (move-after-deleting-constraints #&optional (win (get-window)))
  "Move WIN after deleting all constraints that involve it.
See also `delete-ui-constraints-involving-window!'."
  (interactive)
  (delete-ui-constraints-involving-window! win)
  (interactive-move win))

(define*-public (move-after-deleting-inferred-constraints #&optional (win (get-window)))
  "Move WIN after deleting all inferred constraints that involve it.
See also `delete-inferred-ui-constraints-involving-window!'."
  (interactive)
  (delete-inferred-ui-constraints-involving-window! win)
  (interactive-move win))
