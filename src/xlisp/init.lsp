; get some more memory
(expand 1)

; some fake definitions for Common Lisp pseudo compatiblity
(setq symbol-function symbol-value)
(setq fboundp boundp)
(setq first car)
(setq second cadr)
(setq rest cdr)

; some more cxr functions
(defun caddr (x) (car (cddr x)))
(defun cadddr (x) (cadr (cddr x)))

; (when test code...) - execute code when test is true
(defmacro when (test &rest code)
          `(cond (,test ,@code)))

; (unless test code...) - execute code unless test is true
(defmacro unless (test &rest code)
          `(cond ((not ,test) ,@code)))

; (makunbound sym) - make a symbol be unbound
(defun makunbound (sym) (setq sym '*unbound*) sym)

; (objectp expr) - object predicate
(defun objectp (x) (eq (type x) 'OBJ))

; (filep expr) - file predicate
(defun filep (x) (eq (type x) 'FPTR))

; (unintern sym) - remove a symbol from the oblist
(defun unintern (sym) (cond ((member sym *oblist*)
                             (setq *oblist* (delete sym *oblist*))
                             t)
                            (t nil)))

; (mapcan ...)
(defmacro mapcan (&rest args) `(apply #'nconc (mapcar ,@args)))

; (mapcon ...)
(defmacro mapcon (&rest args) `(apply #'nconc (maplist ,@args)))

; (save fun) - save a function definition to a file
(defun save (fun)
       (let* ((fname (strcat (symbol-name fun) ".lsp"))
              (fp (openo fname)))
             (cond (fp (print (cons (if (eq (car (eval fun)) 'lambda)
                                        'defun
                                        'defmacro)
                                    (cons fun (cdr (eval fun)))) fp)
                       (close fp)
                       fname)
                   (t nil))))

; (debug) - enable debug breaks
(defun debug ()
       (setq *breakenable* t))

; (nodebug) - disable debug breaks
(defun nodebug ()
       (setq *breakenable* nil))

; initialize to enable breaks but no trace back
(setq *breakenable* t)
(setq *tracenable* nil)
