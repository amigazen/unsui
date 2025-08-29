;+
;               PP 1.0 : (C) Copyright 1985 by Gregory Frascadore
;
;   This software may be copied, modified, and distributed to others as long
;   as it is not sold for profit, and as long as this copyright notice is
;   retained intact. For further information contact the author at:
;               frascado%umn-cs.CSNET   (on CSNET)
;               75106,662               (on CompuServe)
;-
 
;+
;                               PP 1.0
; DESCRIPTION
;   PP is a function for producing pretty-printed XLISP code. Version 1.0
;   works with XLISP 1.4 and may work with other versions of XLISP or other
;   lisp systems.
;
; UPDATE HISTORY
;   Version 1.0 - Original version, 11 April 1985 by Gregory Frascadore.
;
;-
 
;+
; pp
;   This function pretty-prints an s-expression.
;
; format
;   (pp <expr> [<sink>] )
;
;       <expr>  the expression to print.
;       <sink>  optional. the sink to print to. defaults to
;                   *standard-output*
;       <maxlen> the threshold that pp uses to determine when an expr
;                   should be broken into several lines. The smaller the
;                   value, the more lines are used. Defaults to 45 which
;                   seems reasonable and works well too.
;-

(let ((pp-stack* nil)
      (pp-istack* nil)
      (pp-currentpos* nil)
      (pp-sink* nil)
      (pp-maxlen* nil))
 
(defun pp (*expr &optional *sink *maxlen)
   (setq pp-stack* nil
         pp-istack* '(0)
         pp-currentpos* 0
         pp-sink* *sink
         pp-maxlen* *maxlen)

   (if (null pp-sink*) (setq pp-sink* *standard-output*))
   (if (null pp-maxlen*) (setq pp-maxlen* 45))
 
   (pp-expr *expr)
   (pp-newline)
   t)
 
 
(defun pp-expr (*expr)
   (cond ((consp *expr)
            (pp-list *expr) )
 
         (t (pp-prin1 *expr)) ) )
 
 
;+
; pp-list
;   Pretty-print a list expression.
;       IF <the flatsize length of *expr is less than pp-maxlen*>
;           THEN print the expression on one line,
;       ELSE
;       IF <the car of the expression is an atom>
;           THEN print the expression in the following form:
;                   "(atom <item1>
;                          <item2>
;                           ...
;                          <itemn> )"
;       ELSE
;       IF <the car of the expression is a list>
;           THEN print the expression in the following form:
;                   "(<list1>
;                     <item2>
;                       ...
;                     <itemn> )"
;
;-
 
(defun pp-list (*expr)
   (cond ((< (flatsize *expr) pp-maxlen*)
            (pp-prin1 *expr) )
 
         ((atom (car *expr))
            (pp-start)
            (pp-prin1 (car *expr))
            (pp-princ " ")
            (pp-pushmargin)
            (pp-rest (cdr *expr))
            (pp-popmargin)
            (pp-finish) )
 
         (t (pp-start)
            (pp-pushmargin)
            (pp-rest *expr)
            (pp-popmargin)
            (pp-finish) ) ) )
 
;+
; pp-rest
;   pp-expr each element of a list and do a pp-newline after every call to
;   pp-expr except the last.
;-
 
(defun pp-rest (*rest)
   (do* ((item* *rest (cdr item*)))
        ((null item*))
            (pp-expr (car item*))
            (if (not (null (cdr item*))) (pp-newline)) ) )
 
;+
; pp-newline
;   Print out a newline character and indent to the current margin setting
;   which is maintained at the top of the pp-istack. Note that is the
;   current top of the pp-stack* is a ")" we push a " " so that we will know
;   to print a space before closing any parenthesis which were started on a
;   different line from the one they are being closed on.
;-
 
(defun pp-newline ()
   (if (eql ")" (pp-top pp-stack*)) (pp-push " " pp-stack*))
 
   (terpri pp-sink*)
   (spaces (pp-top pp-istack*) pp-sink*)
   (setq pp-currentpos* (pp-top pp-istack*)) )
 
;+
; pp-finish
;   Print out the closing ")". If the top of the pp-stack* has a " " on it,
;   then print out the space, then the ")" , and then pop both off the stack.
;-
 
(defun pp-finish ()
   (cond ((eql ")" (pp-top pp-stack*))
            (pp-princ ")") )
 
         (t
            (pp-princ " )")
            (pp-pop pp-stack*) ) )
 
   (pp-pop pp-stack*) )
 
 
;+
; pp-start
;   Start printing a list. ie print the "(" and push a ")" on the pp-stack*
;   so that pp-finish knows to print a ")" when closing an list.
;-
 
(defun pp-start ()
   (pp-princ "(")
   (pp-push ")" pp-stack*) )
 
;+
; pp-princ
;   Prints out an expr without any quotes and updates the pp-currentpos*
;   pointer so that we know where on the line the cursor is at.
;-
 
(defun pp-princ (*expr)
    (setq pp-currentpos* (+ pp-currentpos* (flatc *expr)))
    (princ *expr pp-sink*) )
 
;+
; pp-prin1
;   Does the same thing as pp-prin1, except that the expr is printed with
;   quotes if needed. Hence pp-prin1 uses flatsize to calc expr length instead
;   of flatc.
;-
 
(defun pp-prin1 (*expr)
    (setq pp-currentpos* (+ pp-currentpos* (flatsize *expr)))
    (prin1 *expr pp-sink*) )
 
(defmacro pp-push (*item *stack)
   `(setq ,*stack (cons ,*item ,*stack)) )
 
 
(defmacro pp-pop (*stack)
   `(let ((top* (car ,*stack)))
 
        (setq ,*stack (cdr ,*stack))
        top*) )
 
 
(defun pp-top (*stack) (car *stack))
 
 
(defun pp-pushmargin ()
   (pp-push pp-currentpos* pp-istack*) )
 
 
(defun pp-popmargin ()
   (pp-pop pp-istack*) )

(defun spaces (n f)
    (dotimes (x n) (write-char 32 f)))

)






















