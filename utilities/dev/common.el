(defconst *swarm-modules* '(activity
                            analysis
                            collections
                            defobj
                            gui
                            objectbase
                            random
                            (random . "generators.h")
                            (random . "distributions.h")
                            simtools
                            simtoolsgui
                            space))

(defun swarm-modules ()
  (loop for module-sym in *swarm-modules*
        unless (consp module-sym)
        collect module-sym))

(defun get-swarmsrcdir ()
  (concat
   (let ((swarmsrcdir-env (getenv "SWARMSRCDIR")))
     (if swarmsrcdir-env
         swarmsrcdir-env
         (if (> (length command-line-args 1))
             (car (last command-line-args))
             (error "Can't find SWARMSRCDIR"))))
   "/"))

(defun get-swarmdocs ()
  (let ((swarmdocs-env (getenv "SWARMDOCS")))
    (if swarmdocs-env
        swarmdocs-env
        (concat (get-swarmsrcdir) "../swarmdocs"))))

(defun header-filename-for-module-sym (module-sym)
  (concat (symbol-name module-sym) ".h"))

(defun pathname-for-module-sym (module-sym &optional filename)
  (let ((module-name (symbol-name module-sym)))
    (concat (get-swarmsrcdir) "src/" module-name "/" 
            (if filename
                filename
                (header-filename-for-module-sym module-sym)))))

(defun pathname-for-swarmdocs (module-sym filename)
  (let ((module-name (symbol-name module-sym)))
    (concat (get-swarmdocs) 
            (case module-sym
              ((refbook set installbook overbook) (concat module-name "/" filename))
              (otherwise (concat "refbook/" module-name "/" filename))))))

(defun get-swarmdocs-build-area ()
    (getenv "SWARMDOCS_BUILD_AREA"))

(defun pathname-for-swarmdocs-pages-output (module-sym)
  (let ((module-name (symbol-name module-sym)))
    (concat (get-swarmdocs-build-area)
            "refbook/"
            module-name
            "/"
            module-name
            "pages.sgml")))

(defun pathname-for-swarmdocs-revision-output (module-sym)
  (let ((module-name (symbol-name module-sym)))
    (case module-sym
      ((refbook set installbook overbook) 
       (concat (get-swarmdocs-build-area)
               module-name "/" module-name "revhistory.sgml"))
      (otherwise (concat (get-swarmdocs-build-area)
                         "refbook/"
                         module-name
                         "/"
                         module-name
                         "revhistory.sgml")))))

(defun insert-text (text)
  (let ((beg (point)))
    (insert text)
    (let ((end (point)))
      (save-excursion
        (save-restriction
          (narrow-to-region beg end)
          (goto-char (point-min))
          (save-excursion
            (while (search-forward "<" nil t)
              (replace-match "&lt;")))
          (goto-char (point-min))
          (save-excursion
            (while (search-forward ">" nil t)
              (replace-match "&gt;")))
          (goto-char (point-min))
          (save-excursion
            (while (search-forward "\t" nil t)
              (replace-match ""))))))))

(defun strip-regexp (str strip-str)
  (with-output-to-string
    (with-temp-buffer
      (let ((beg (point)))
        (insert str)
        (let ((end (point)))
          (save-excursion
            (save-restriction
              (narrow-to-region beg end)
              (goto-char (point-min))
              (while (re-search-forward strip-str nil t)
                (replace-match ""))))))
      (princ (buffer-string)))))

(defun cook-id (id)
  (strip-regexp id "_"))

