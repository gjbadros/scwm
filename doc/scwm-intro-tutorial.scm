;; $Id$ -*- scwm -*-
;;

;; Scwm embeds a scheme interpreter for configurability
;; and programming dynamic behaviours.

;; Your $HOME/.scwmrc file is run inside Scwm's Scheme
;; interpreter, so code such as:

;; (set-hilight-foreground! "white")
;; (set-hilight-background! "navyblue")

;; in your .scwmrc specifies the colors you'd like the
;; window with the focus to use.

;; But the scheme interpreter is a *lot* more powerful
;; than the static configuration language that other
;; window managers like fvwm2, [vc]twm, Enlightenment,
;; WindowMaker, IceWM, and others use.  Arbitrary
;; scheme code (including all the primitives defined
;; by scwm to do window management) can be executed
;; at will, after Scwm has started up.

;; In general, the ability to dynamically execute Scheme
;; code in the Scwm Scheme interpreter makes it unusual
;; to need to restart scwm after changing your $HOME/.scwmrc;
;; you can simply make the changes in your .scwmrc, and also
;; have Scwm evaluate those changes right now, interactively.

;; You can interact with the scheme interpreter in many
;; ways.  The simplest (but not most convenient) is via the
;; program "scwmexec" from a shell.

;; Try entering this at a shell (in an xterm, for example):
;; scwmexec '(display-message-briefly "Scwm says hello")'

;; The single argument to the scwmexec program is the
;; expression (or "S-expression") to evaluate.

;; Using emacs's scwm-mode, we can ask Scwm's scheme interpreter to
;; evaluate the preceding S-expression (sexp) very easily.  If you're
;; not reading this file in Emacs, start an Emacs (or XEmacs) and find
;; this file using C-x C-f (if you do not know how to use [X]Emacs,
;; first run the tutorial by typing "C-h t"; i.e. press and hold the
;; "Control" key and hit "h", then release both those keys and press
;; "t").

;; After loading this file, Emacs should set the editing mode to use
;; "scwm-mode", a major mode specifically designed to make it easy to
;; interact with the Scheme interpreter embedded in Scwm.  Your
;; modeline (at the bottom of your screen) should display "(Scwm)" in
;; the list of modes.  Also, "C-h v major-mode" (describe-variable
;; "major-mode") should report "scwm-mode".  If it does not, you
;; need to do:

;; M-x load-file /path/to/scwm.el
;; (e.g., M-x load-file /usr/local/lib/emacs/site-lisp/scwm.el
;;  the scwm-mode.el file is included with the scwm distribution
;;  as utilities/emacs/scwm.el;  it should be installed by default
;;  when you or your system administrator do a "make install")
;; This will make available a new Emacs command "scwm-mode",
;; which we now need to run via:
;; M-x scwm-mode

;; Now you should be editing this buffer using scwm-mode,
;; as indicated by "(Scwm)" in your Emacs modeline.

;; scwm-mode provides numerous Emacs keybindings.  To see the
;; entire list, use "C-h m".   The most important among these
;; is C-j, which runs scwm-eval-print.  Using "C-h f scwm-eval-print"
;; to retrieve the Emacs documentation for this emacs command,
;; we can see that this Emacs function will:

;; "Evaluate the last SEXP and insert the result into the current buffer."

;; Let's try re-running the Scwm `display-message-briefly' command 
;; using C-j.  Since C-j will change this buffer, we first need
;; to be sure you're allowed to edit the buffer. The easiest way to 
;; do this is to save this buffer as a new file in your home
;; directory.  Do this with:

;; M-x write-file ~/scwm-intro.scm

;; And a copy of this file will be put in your home directory.
;; (substitute a different path if you'd prefer).

;; Now let's get back to using the Scwm interpreter from Emacs.
;; First position your cursor at the end of the below
;; line, and then press "C-j"

(+ 1 2)
;;     ^ cursor here when you press C-j

;; If Scwm and scwm-mode are properly installed, you should have
;; had "3" get inserted into the buffer just after the "(+ 1 2)" line.
;; (If you received an error, consult with your local Scwm expert or
;; system administrator, or ask the kind folks at scwm-discuss@mit.edu)

;; So what happened?  The C-j keystroke (bound to the Emacs command
;; scwm-eval-print, as mentioned earlier) caused Emacs to find the
;; full S-expression preceding the point (the cursor position) by
;; matching parentheses.  It then sent that SEXP to Scwm for it to
;; evaluate in its Scheme interpreter.  The Scwm Scheme interpreter
;; got the string "(+ 1 2)" and evaluated it as a Scheme expression.
;; The result from that evaluation, along with any errors and output,
;; was then inserted into the buffer just after where you pressed C-j.
;; In this case, the expression evaluated to the number 3, and the
;; printable form of the number ("3") was inserted into the buffer.

;; As you've probably guessed by now, the expression "(+ 1 2)" 
;; means add 1 and 2.  This is a bit different from how most of us
;; are used to expressing addition.  Though it is more common
;; for us to write "1 + 2", using "infix" notation where the operator
;; appears in-between the two operands, Scheme uses "prefix" notation,
;; where the operator appears first, before all of its arguments.
;; In Scheme, parentheses indicate a list, and a list is evaluated
;; by applying the procedure specified by the first argument to 
;; the arguments given by the remaining arguments.

;; Thus "(+ 1 2)" is evaluated by applying the procedure "+" (a 
;; built-in primitive procedure) to the arguments "1" and "2".
;; This can be written more explicitly in Scheme using the
;; procedure `apply'.  Evaluate the below SEXP:

(apply + (list 1 2))


;; Again, you'll receive the result "3".  In this last example,
;; I used the procedure `list' which just returns its arguments
;; in a list.  So:

(list 1 2 (+ 1 2))

;; Gives us "1 2 3".  It's important to realize that Scheme evaluates
;; all of the arguments before invoking a procedure on those
;; arguments.  Thus, in the above, the `+' procedure finishes
;; executing and evaluates to a 3 before the `list' procedure
;; is invoked on the arguments "1" "2" and "3".

;; Though procedures evaluate their arguments,
;; two other scheme constructs, special forms and macros,
;; permit control over how their arguments are evaluated.
;; We'll learn more about them later.

;; Sometimes we want to suppress evaluation of a list entirely.
;; Scheme provides a special syntax, the "'" character (single forward
;; quote, or tic), to do this.  Evaluate each of the two below SEXPs
;; and compare:

(+ 1 2)
;; evaluates and gives us "3"

'(+ 1 2)
;; "'" suppresses the evaluation of the list, and the
;; result is the list containing 3 objects, "(+ 1 2)"



;; Scwm uses a Scheme interpreter called "Guile Scheme" that
;; is part of the GNU project.  See:
;; http://www.red-bean.com/guile
;; Guile Scheme provides all of the standard scheme functionality
;; of R4RS, and if you have the R4RS Scheme info pages installed
;; on your system, you can browse that documentation using
;; "C-h i" (or "M-x info") then selected "R4RS Scheme".

;; All of the extensions to Standard Scheme that guile provides are
;; also available.  This functionality is documented (a bit less
;; well) in the guile info page, also available via the info
;; browsing system under Emacs.  That functionality includes
;; such things as regular expression matching, modules, file
;; manipulation, and lots more.

;; Finally, Scwm also implements hundreds of new window-manager
;; related "primitives" (procedures written in Scheme coding providing
;; fundamental new capabilities) and procedures [a primitive is a
;; procedure, but not necessarily vice-versa].  The Emacs scwm-mode
;; provides an easy way to access the documentation for these
;; procedures:  "C-h C-s" is bound to "scwm-documentation".
;; Try it below to find out more about the Scwm procedure below:

select-window-interactively
;;                         ^ hit "C-h C-s" here, and press return

;; The Emacs scwm-documentation command looks at the symbol
;; at the cursor and suggests that as the default to look up
;; in the documentation.  You can always enter a different symbol
;; in by hand, but often it's easier to move the point (the
;; Emacs cursor) to the symbol you want to learn more about,
;; and press "C-h C-s" so that the default is correct.  You'll
;; always need to press RETURN to confirm your selection.

;; If the documentation for Scwm is properly installed, your 
;; Emacs frame (what Scwm calls a window) will split into
;; two Emacs windows (not the same thing as what Scwm calls
;; windows -- Emacs windows are all inside the single Emacs frame
;; which is the only "window" in the Scwm sense of the term.
;; The new Emacs window should look something like this:


;;;;  SCWM documentation for `select-window-interactively':
;;;;  
;;;;   value:
;;;;  
;;;;   #<primitive-procedure select-window-interactively>
;;;;  
;;;;   documentation:
;;;;  
;;;;  (select-window-interactively  #&optional msg)
;;;;  - Built-in Primitive
;;;;  Returns a window selected interactively while displaying MSG.
;;;;  Returns #f if no window was selected. Display no message if MSG not given.
;;;;  [From scwm/window.c:809]

;; The most important lines are those following the "documentation:"
;; heading.  The "(select-window-interactively  #&optional msg)"
;; shows the arguments that the procedure takes.  According to this,
;; `select-window-interactively' takes a single optional argument,
;; a "msg".  The description will describe further the purpose of
;; the argument, and it tells us that the MSG given is displayed
;; while permitting the user to interactively select a window.
;; The description also tells us what the return value of the
;; procedure is:  `select-window-interactively' returns the
;; selected window if one was selected, or otherwise "#f".
;; (#f is the printable form of the boolean object that represents
;;  FALSE;  #t is the analogous object for TRUE.)

;; Let's try using the `select-window-interactively' procedure.
;; Evaluate the below line:

(select-window-interactively "Pick a window")
;;                                           ^ C-j here

;; Your X11 cursor (mouse pointer) will change into a circle, and you
;; should move it and left click on a window.  After doing so, text
;; similar to #<window 46137358: "xterm"> will be inserted in the
;; buffer.  The procedure returned a window object, and that window's
;; printable representation (as text) is the text that was inserted.
;; For all Scwm objects, the printable representation starts with "#<"
;; followed by the kind of object.  The remaining information in the
;; printable representation depends on what type of object it is;
;; for window objects, it consists of the X11 window identifier number
;; followed by the string which is the window title.

;; You should be careful to not use the printable form of an object
;; to find out information about that object, as the printable form
;; is subject to change.  There are accessor procedures for all object
;; types that will answer any question you might have about an object.
;; E.g., to find out the window-title of a window, you can use the
;; `window-title' accessor function.  First use "C-h C-s" to ask
;; Emacs to tell give you the documentation for this procedure,
;; and then evaluate the entire S-expression using C-j at the
;; end of the line:

(window-title (select-window-interactively "Pick a window") )
;;           ^ C-h C-s here for docs        C-j here to eval ^

;; In this example, the returned object is a string, not the
;; window object.  `select-window-interactively' returned a
;; window object which was then the argument to the `window-title'
;; procedure which returns the title string from a given window
;; object.

;; Since window objects are used everywhere in Scwm, there are
;; many procedures that operate on window objects.  Many of
;; these procedures have "window" in their name, and we can
;; ask Emacs's scwm-mode to provide us with a list of those
;; procedures using "C-h C-a" (or "M-x scwm-apropos").  Similar
;; to "C-h C-s" for looking up specific functions, scwm-apropos
;; looks near the point (the emacs cursor) for a symbol that you
;; may be interested in finding out more about.  Try it below:

window
;;    ^ C-h C-a 

;; This will popup a new Emacs window (again, different from
;; Scwm's notion of windows-- an Emacs window still lives inside
;; the single Emacs frame [Emacs frame == Scwm window]) that
;; lists all the symbols with "window" in their name.  This
;; "apropos" functionality is a good way to learn more about
;; a certain aspect of Scwm.  From the "*Apropos*" buffer,
;; you can press return on any of the symbols to learn more
;; about it.  E.g., move down to `window-id' (perhaps use Emacs's
;; incremental search feature, "C-s window-id", to find the text),
;; and press return on that line.  Let's use `window-id':

(window-id (select-window-interactively))
;;                                       ^ C-j here

;; This is very similar to `window-title', but returns the X11 window
;; id instead.  Window IDs are especially useful for communicating
;; with external processes about specific windows on the X display.
;; (e.g., from a shell, run "xev -id " and then the number that was
;; just returned when you evaluated to above SEXP.  This will start
;; the X11 Event Viewer program to watch events on that window that
;; you chose from within Scwm.  Or:

;; FROM A SHELL:
;; xev -id `scwmexec '(window-id (select-window-interactively))'`

;;                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;;                 This SEXP gets evaluated
;;
;;      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;;      and we get something like "46137358" here
;;      because the printable representation of the return value
;;      is replaced via the shell's command substitution mechanism
;;
;; Finally, "xev -id 46137358" is run

