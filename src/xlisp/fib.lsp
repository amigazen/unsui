(defun fib (x)
       (if (< x 2)
           x
           (+ (fib (1- x)) (fib (- x 2)))))


