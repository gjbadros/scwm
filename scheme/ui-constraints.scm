;; $Id$
;; Copyright (C) 1999 Jeffrey W. Nichols and Greg J. Badros
;;
;; ui-constraints.scm
;;
;; A module that adds support for graphical manipulation of
;; constraints on windows.
;;

(define-module (app scwm ui-constraints)
  :use-module (app scwm base)
  :use-module (app scwm optargs)
;;  :use-module (cassowary constraints))
  )

;; (use-modules (app scwm ui-constraints))
;; (use-modules (app scwm ui-constraints-classes))
;; (load "/home/gjb/scwm/scheme/constraints.scm")
;; (load "/home/gjb/scwm/scheme/ui-constraints.scm")
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
;; (obid-ui-constraint-class NAME NUM-WINDOWS CTR UI-CTR DRAW-PROC SATISFIED-PROC PIXMAP-NAME MENUNAME-PROC)
;;

;; make-ui-constraint-class

;; returns a new constraint class object based on the parameters
;; SIDE-EFFECT: adds new class obj to the global list

(define-public (make-ui-constraint-class name num-windows ctr ui-ctr draw-proc satisfied-proc pixmap-name menuname-proc)
  "CTR takes NUM-WINDOWS windows and creates a constraint of this type.
SATISFIED-PROC is a procedure that takes a single argument, the cn, and tells if it is satisfied
UI-CTR should return the arguments (as a list) for CTR to build the constraint with.  UI-CTR should
return #f if the user cancels the construction or fails to follow the interface correctly.  PIXMAP-NAME
is the name of the pixmap to associate with this constraint-class in the user interface.
MENUNAME-PROC is a proc that takes a UI-CONSTRAINT as an arg and returns the name that should be used
for the constraint in the toggle menu.  
This routine returns a new constraint class object based on the parameters.
SIDE-EFFECT: addes new class obj to the global class list."
  (let ((obj (vector obid-ui-constraint-class name num-windows ctr ui-ctr draw-proc satisfied-proc pixmap-name menuname-proc)))
    (set! global-constraint-class-list (cons obj global-constraint-class-list))
    (call-hook-procedures constraint-class-add-hook-list (list obj))
    obj))


;; delete-ui-constraint-class

;; Removes the UI-CONSTRAINT-CLASS permanently
;; SIDE-EFFECT: removes class object from the global list

(define-public (delete-ui-constraint-class! ui-constraint-class)
  "Removes UI-CONSTRAINT-CLASS from the global class list.
SIDE-EFFECT: removes class object from the global class list."
  (begin
    (set! global-constraint-class-list (delq ui-constraint-class global-constraint-class-list))
    (call-hook-procedures constraint-class-delete-hook-list (list ui-constraint-class))))


(define-public (reset-ui-constraint-classes!)
  "Empty the global list of ui-constraint-classes."
  (begin
    (for-each (lambda (class) 
		(call-hook-procedures constraint-class-remove-hook-list (list class))) 
	      global-constraint-class-list)
    (set! global-constraint-class-list '())))

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
  "Returns the name of the constraint class.  errors if UI-CONSTRAINT-CLASS
is not a ui-constraint-class."
  (if (ui-constraint-class? ui-constraint-class)
      (vector-ref ui-constraint-class 1)
      (error "Argument to accessor must be a UI-CONSTRAINT-CLASS object")))


;; ui-constraint-class-num-windows

;; returns the number of windows constrainable by instances of
;;   the constraint class.
;; errors if UI-CONSTRAINT-CLASS is not a ui-constraint-class

(define-public (ui-constraint-class-num-windows ui-constraint-class)
  "Returns the number of windows constrainable by instances of the constraint 
class.  errors if UI-CONSTRAINT-CLASS is not a ui-constraint-class."
  (if (ui-constraint-class? ui-constraint-class)
      (vector-ref ui-constraint-class 2)
      (error "Argument to accessor must be a UI-CONSTRAINT-CLASS object")))


;; ui-constraint-class-ctr

;; returns the constructor for instances of the constraint class.
;; errors if UI-CONSTRAINT-CLASS is not a ui-constraint-class

(define-public (ui-constraint-class-ctr ui-constraint-class)
  "Returns the constructor for instance of the constraint class.  errors
if UI-CONSTRAINT-CLASS is not a ui-constraint-class."
  (if (ui-constraint-class? ui-constraint-class)
      (vector-ref ui-constraint-class 3)
      (error "Argument to accessor must be a UI-CONSTRAINT-CLASS object")))


;; ui-constraint-class-ui-ctr

;; returns the UI constructor for instances of the constraint class.
;; errors if UI-CONSTRAINT-CLASS is not a ui-constraint-class

(define-public (ui-constraint-class-ui-ctr ui-constraint-class)
  "Returns the UI constructor for instances of the constraint class.
errors if UI-CONSTRAINT-CLASS is not a ui-constraint-class"
  (if (ui-constraint-class? ui-constraint-class)
      (vector-ref ui-constraint-class 4)
      (error "Argument to accessor must be a UI-CONSTRAINT-CLASS object")))


;; ui-constraint-class-draw-proc

;; returns the drawing procedure for instances of the constraint class.
;; errors if UI-CONSTRAINT-CLASS is not a ui-constraint-class

(define-public (ui-constraint-class-draw-proc ui-constraint-class)
  "returns the drawing procedure for instances of the constraint class.
errors if UI-CONSTRAINT-CLASS is not a ui-constraint-class"
  (if (ui-constraint-class? ui-constraint-class)
      (vector-ref ui-constraint-class 5)
      (error "Argument to accessor must be a UI-CONSTRAINT-CLASS object")))

;; ui-constraint-class-satisfied

;; returns the satisfaction checking procedure for instances of 
;;   the constraint class.
;; errors if UI-CONSTRAINT-CLASS is not a ui-constraint-class

(define-public (ui-constraint-class-satisfied-proc ui-constraint-class)
  "Returns the satisfaction checking procedure for instances of the
constraint class.  errors if UI-CONSTRAINT-CLASS is not a ui-constraint-class"
  (if (ui-constraint-class? ui-constraint-class)
      (vector-ref ui-constraint-class 6)
      (error "Argument to accessor must be a UI-CONSTRAINT-CLASS object")))

;; ui-constraint-class-pixmap-name

(define-public (ui-constraint-class-pixmap-name ui-constraint-class)
  "Return the pixmap-name of UI-CONSTRAINT-CLASS.
Errors if object is not a ui-constraint-class object."
  (if (ui-constraint-class? ui-constraint-class)
      (vector-ref ui-constraint-class 7)
      (error "Argument to accessor must be a UI-CONSTRAINT-CLASS object")))

;; ui-constraint-class-menuname-proc

(define-public (ui-constraint-class-menuname-proc ui-constraint-class)
  "Return the proc for determining the name for the constraint in
the toggle menu.  Errors if object is not a ui-constraint-class object."
  (if (ui-constraint-class? ui-constraint-class)
      (vector-ref ui-constraint-class 8)
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
	     (uc (vector obid-ui-constraint ui-constraint-class cn #f win-list opts #f '())))
	(if visible?
	    (begin
	      (set! global-constraint-instance-list (cons uc global-constraint-instance-list))
	      (call-hook-procedures constraint-add-hook-list (list uc))
	      (call-hook-procedures constraint-composition-record-hook-list (list uc arg-list))))
	uc)
      (error "Argument must be a UI-CONSTRAINT-CLASS object")))


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
	(if arg-list (make-ui-constraint ui-constraint-class arg-list)))
      (error "Argument must be a UI-CONSTRAINT-CLASS object")))


;; delete-ui-constraint

;; Removes the UI-CONSTRAINT permanently
;; SIDE-EFFECT: removes instance object from the global list

(define-public (delete-ui-constraint! ui-constraint)
  "Removes the UI-CONSTRAINT permanently.
SIDE-EFFECT: removes instance object from the global list"
  (set! global-constraint-instance-list (delq ui-constraint global-constraint-instance-list))
  (disable-ui-constraint ui-constraint)
  (call-hook-procedures constraint-delete-hook-list (list ui-constraint)))


;; ui-constraint?

;; returns #t if UI-CONSTRAINT is a ui-constraint
;; returns #f otherwise

(define-public (ui-constraint? ui-constraint)
  "returns #t if UI-CONSTRAINT is a ui-constraint.
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
      (error "Argument must be a UI-CONSTRAINT object")))


;; ui-constraint-enabled?

;; returns the ENABLE from the ui-constraint object UI-CONSTRAINT
;; errors if UI-CONSTRAINT is not an ui-constraint

(define-public (ui-constraint-enabled? ui-constraint)
  "Returns the ENABLE from the ui-constraint object UI-CONSTRAINT.
errors if UI-CONSTRAINT is not an ui-constraint."
  (if (ui-constraint? ui-constraint)
      (vector-ref ui-constraint 3)
      (error "Argument must be a UI-CONSTRAINT object")))

;; ui-constraint-class

;; returns the UI-CONSTRAINT-CLASS from the ui-constraint object UI-CONSTRAINT
;; errors if UI-CONSTRAINT is not an ui-constraint

(define-public (ui-constraint-class ui-constraint)
  "Returns the UI-CONSTRAINT-CLASS from the ui-constraint object UI-CONSTRAINT.
errors if UI-CONSTRAINT is not an ui-constraint."
  (if (ui-constraint? ui-constraint)
      (vector-ref ui-constraint 1)
      (error "Argument must be a UI-CONSTRAINT object")))


;; ui-constraint-windows

;; returns the LIST-OF-WINDOWS from the ui-constraint object UI-CONSTRAINT
;; errors if UI-CONSTRAINT is not an ui-constraint

(define-public (ui-constraint-windows ui-constraint)
  "Returns the LIST-OF-WINDOWS from the ui-constraint object UI-CONSTRAINT.
errors if UI-CONSTRAINT is not an ui-constraint."
  (if (ui-constraint? ui-constraint)
      (vector-ref ui-constraint 4)
      (error "Argument must be a UI-CONSTRAINT object")))

;; ui-constraint-opts

;; returns the optional data list that may be added by the constraint class
;; returns #f if no such data exists

(define-public (ui-constraint-opts ui-constraint)
  "Returns the list of optional data that may be added by the constraint class.
Returns #f if no such data exists.  errors if UI-CONSTRAINT is not a ui-constraint."
  (if (ui-constraint? ui-constraint)
      (vector-ref ui-constraint 5)
      (error "Argument must be a UI-CONSTRAINT object")))


;; ui-constraint-button

(define-public (ui-constraint-button ui-constraint)
  "Returns the a reference to panel representing the UI-CONSTRAINT in the toggle menu.
Returns #f if the gtk toggle menu feature is not in use."
  (if (ui-constraint? ui-constraint)
      (vector-ref ui-constraint 6)
      (error "Argument must be a UI-CONSTRAINT object")))


;; ui-constraint-set-button!

(define-public (ui-constraint-set-button! ui-constraint button)
  "Sets the reference to the gtk button for this instance in the toggle menu.
Errors if UI-CONSTRAINT is not a ui-constraint."
  (if (ui-constraint? ui-constraint)
      (vector-set! ui-constraint 6 button)
      (error "Argument must be a UI-CONSTRAINT object")))



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
	    (hooks (ui-constraint-enable-hooks ui-constraint)))
	(map (lambda (c) (cl-add-constraint (scwm-master-solver) c)) cn)
	(set-enable! ui-constraint #t)
	(call-hook-procedures hooks '(#t)))
      (error "Argument must be a UI-CONSTRAINT object")))


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
	    (hooks (ui-constraint-enable-hooks ui-constraint)))
	(map (lambda (c) (cl-remove-constraint (scwm-master-solver) c)) cn)
	(set-enable! ui-constraint #f)
	(call-hook-procedures hooks '(#f)))
      (error "Argument must be a UI-CONSTRAINT object")))


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

(define-public (constrained-window-in-focus? ui-constraint)
  "Returns #t if one of the windows in the constraint is in focus.
Returns #f otherwise or if UI-CONSTRAINT is not a ui-constraint."
  (if (ui-constraint? ui-constraint)
      (window-in-list-in-focus? (ui-constraint-windows ui-constraint))
      (error "Argument must be a UI-CONSTRAINT object")))


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
      (error "Argument must be a UI-CONSTRAINT object")))


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


;; draw-all-constraints

(define-public (draw-all-constraints)
  "Draw all constraints in the global instance list."
  (map draw-constraint global-constraint-instance-list))


;; undraw-all-constraints

(define-public (undraw-all-constraints)
  "Undraw all constraints in the global instance list."
  (map undraw-constraint global-constraint-instance-list))

;; disable-all-constraints

(define-public (disable-all-constraints)
  "Disable all constraints in the global instance list."
  (map disable-ui-constraint global-constraint-instance-list))

;; enable-all-constraints

(define-public (enable-all-constraints)
  "Enable all constraints in the global instance list."
  (map enable-ui-constraint global-constraint-instance-list))


;; HOOKS

;; This code handles the hook functions which are called when 
;; a constraint is added or deleted.

;;----------------------------------------------
;; Constraint Class Hooks

;; The lists of hook functions

(define-public constraint-class-add-hook-list '())
(define-public constraint-class-delete-hook-list '())


;; add a hook procedures

(define-public (add-constraint-class-add-hook! hook)
  "Add a procedure HOOK to be called when a constraint class is added.
HOOK should be a procedure which takes a ui-constraint-class as
an argument."
  (set! constraint-class-add-hook-list (cons hook constraint-class-add-hook-list)))

(define-public (add-constraint-class-delete-hook! hook)
  "Add a procedure HOOK to be called when a constraint class is deleted.
HOOK should be a procedure which takes a ui-constraint-class as
an argument."
  (set! constraint-class-delete-hook-list (cons hook constraint-class-delete-hook-list)))


;; remove a hook procedures
  
(define-public (remove-constraint-class-add-hook! hook)
  "Remove a procedure HOOK from the list of constraint-class-add hooks."
  (set! constraint-class-add-hook-list (delq hook constraint-class-add-hook-list)))

(define-public (remove-constraint-class-delete-hook! hook)
  "Remove a procedure HOOK from the list of constraint-class-delete hooks."
  (set! constraint-class-delete-hook-list (delq hook constraint-class-delete-hook-list)))

;;----------------------------------------------
;; Constraint Instance Hooks

;; The lists of hook functions

(define-public constraint-add-hook-list '())
(define-public constraint-delete-hook-list '())
(define-public constraint-composition-record-hook-list '())

;; Constraint Instance Construction/Destruction Hooks

;; add a hook procedures

(define-public (add-constraint-add-hook! hook)
  "Add a procedure HOOK to be called when a constraint is added.
HOOK should be a procedure which takes a ui-constraint-instance as
an argument."
  (set! constraint-add-hook-list (cons hook constraint-add-hook-list)))

(define-public (add-constraint-delete-hook! hook)
  "Add a procedure HOOK to be called when a constraint is deleted.
HOOK should be a procedure which takes a ui-constraint-instance as
an argument."
  (set! constraint-delete-hook-list (cons hook constraint-delete-hook-list)))


;; remove a hook procedures
  
(define-public (remove-constraint-add-hook! hook)
  "Remove a procedure HOOK from the list of constraint-add hooks."
  (set! constraint-add-hook-list (delq hook constraint-add-hook-list)))

(define-public (remove-constraint-delete-hook! hook)
  "Remove a procedure HOOK from the list of constraint-delete hooks."
  (set! constraint-delete-hook-list (delq hook constraint-delete-hook-list)))


;; Constraint Instance (Un)Enable Hooks

;; ui-constraint-add-enable-hook

(define-public (ui-constraint-add-enable-hook ui-constraint hook)
  "Adds a HOOK proc which will be called when UI-CONSTRAINT enable state changes.
HOOK should take one argument which is the new state of the UI-CONSTRAINT.
Errors if UI-CONSTRAINT is not a ui-constraint object."
  (let ((hlist (ui-constraint-enable-hooks ui-constraint)))
      (vector-set! ui-constraint 7 (cons hook hlist))))
    
;; ui-constraint-remove-enable-hook

(define-public (ui-constraint-remove-enable-hook ui-constraint hook)
  "Removes a HOOK proc from the list in UI-CONSTRAINT.
Errors if UI-CONSTRAINT is not a ui-constraint object."
  (let ((hlist (ui-constraint-enable-hooks ui-constraint)))
    (vector-set! ui-constraint 7 (delq hook hlist))))

;; ui-constraint-enable-hooks

(define-public (ui-constraint-enable-hooks ui-constraint)
  "Returns a list of the enable-hook functions added to the UI-CONSTRAINT.
Errors if UI-CONSTRAINT is not a ui-constraint object."
  (if (ui-constraint? ui-constraint)
      (vector-ref ui-constraint 7)
      (error "Argument must be a UI-CONSTRAINT object")))



;; Constraint Instance Composition Recording Hooks

;; These hooks may be of limited usefulness outside of the 
;; ui-constraints-composition framework.

(define-public (add-constraint-composition-record-hook! hook)
  "Add a procedure HOOK to be called when a constraint is added.
HOOK should be a procedure which takes a ui-constraint object and
the list of arguments passed to the constructor for that 
ui-constraint object.  It is used by the ui-constraint-composition
system."
  (set! constraint-composition-record-hook-list 
	(cons hook constraint-composition-record-hook-list)))

(define-public (remove-constraint-composition-record-hook! hook)
  "Remove a procedure HOOK from the list of constraint-composition-record list."
  (set! constraint-composition-record-hook-list 
	(delq hook constraint-composition-record-hook-list)))
