; initialization file for XLISP 1.6

; get some more memory
(expand 1)

; some fake definitions for Common Lisp pseudo compatiblity
(setq first  car)
(setq second cadr)
(setq rest   cdr)

; (when test code...) - execute code when test is true
(defmacro when (test &rest code)
          `(cond (,test ,@code)))

; (unless test code...) - execute code unless test is true
(defmacro unless (test &rest code)
          `(cond ((not ,test) ,@code)))

; (makunbound sym) - make a symbol be unbound
(defun makunbound (sym) (setq sym '*unbound*) sym)

; (objectp expr) - object predicate
(defun objectp (x) (eq (type-of x) :OBJECT))

; (filep expr) - file predicate
(defun filep (x) (eq (type-of x) :FILE))

; (mapcan fun list [ list ]...)
(defmacro mapcan (&rest args) `(apply #'nconc (mapcar ,@args)))

; (mapcon fun list [ list ]...)
(defmacro mapcon (&rest args) `(apply #'nconc (maplist ,@args)))

; (set-macro-character ch fun [ tflag ])
(defun set-macro-character (ch fun &optional tflag)
    (setf (aref *readtable ch) (cons (if tflag :tmacro :nmacro) fun))
    t)

; (get-macro-character ch)
(defun get-macro-character (ch)
  (if (consp (aref *readtable* ch))
    (cdr (aref *readtable* ch))
    nil))

; (save fun) - save a function definition to a file
(defmacro save (fun)
         `(let* ((fname (strcat (symbol-name ',fun) ".lsp"))
                 (fval (car ,fun))
                 (fp (openo fname)))
                (cond (fp (print (cons (if (eq (car fval) 'lambda)
                                           'defun
                                           'defmacro)
                                       (cons ',fun (cdr fval))) fp)
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


