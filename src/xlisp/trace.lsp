(setq *tracelist* nil)

(defun evalhookfcn (expr &aux val)
       (if (and (consp expr) (member (car expr) *tracelist*))
           (progn (princ ">>> ") (print expr)
                  (setq val (evalhook expr evalhookfcn nil))
                  (princ "<<< ") (print val))
           (evalhook expr evalhookfcn nil)))

(defun trace (fun)
       (if (not (member fun *tracelist*))
	   (progn (setq *tracelist* (cons fun *tracelist*))
                  (setq *evalhook* evalhookfcn)))
       *tracelist*)

(defun untrace (fun)
       (if (null (setq *tracelist* (delete fun *tracelist*)))
           (setq *evalhook* nil))
       *tracelist*)
