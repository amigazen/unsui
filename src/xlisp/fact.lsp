(defun factorial (n)
       (cond ((= n 1) 1)
	     (t (* n (factorial (- n 1))))))
