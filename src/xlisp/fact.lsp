(defun fact (n)
       (if (< n 2)
           1
	   (* n (fact (1- n)))))


