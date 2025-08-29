; -*-Lisp-*-
;
; Jwahar R. Bammi
; A simple description of hardware objects using xlisp
; Mix and match instances of the objects to create your
; organization.
; Needs:
; - busses and connection and the Design
;   Class that will have the connections as instance vars.
; - Print method for each object, that will display
;   the instance variables in an human readable form.
; Some day I will complete it.
;
;
;
; utility functions


; function to calculate 2^n

(defun pow2 (n)
	(pow2x n 1))

(defun pow2x (n sum)
       (cond((equal n 0) sum)
	    (t (pow2x (- n 1) (* sum 2)))))


; hardware objects

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;The class areg

(setq areg (Class :new '(value nbits max_val min_val)))

; methods

; initialization method
; when a new instance is called for the user supplies
; the parameter nbits, from which the max_val & min_val are derived

(areg :answer :isnew '(n)
	  '((self :init n)
	    	self))

(areg :answer :init '(n)
	  '((setq value ())
	    (setq nbits n)
	    (setq max_val (- (pow2 (- n 1)) 1))
	    (setq min_val (- (- 0 max_val) 1))))

; load areg

(areg :answer :load '(val)
	  '((cond ((> val max_val) (princ (list "The max value a "nbits" bit register can hold is "max_val"\n")))
		  ((< val min_val) (princ (list "The min value a "nbits" bit register can hold is "min_val"\n")))
		  (t (setq value val)))))

; see areg

(areg :answer :see '()
      '((cond ((null value) (princ "Register does not contain a value\n"))
	      (t value))))
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The class creg ( a register that can be cleared and incremented)
; subclass of a reg

(setq creg (Class :new '() '() areg))

; it inherites all the instance vars & methods of a reg
; in addition to them it has the following methods

(creg :answer :isnew '(n)
      '((self :init n)
	self))

(creg :answer :init '(n)
      '((setq value ())
	(setq nbits n)
	(setq max_val (- (pow2 n) 1))
	(setq min_val 0)))

(creg :answer :clr '()
      '((setq value 0)))

(creg :answer :inc '()
      '((cond ((null value) (princ "Register does not contain a value\n"))
	      (t (setq value (rem (+ value 1) (+ max_val 1)))))))

;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Register bank
; contains n areg's n_bits each

(setq reg_bank (Class :new '(regs n_regs curr_reg)))

;methods

(reg_bank :answer :isnew '(n n_bits)
	  '((self :init n n_bits)
	    self))

(reg_bank :answer :init '(n n_bits)
	  '((setq regs ())
	    (setq n_regs (- n 1))
	    (self :initx n n_bits)))

(reg_bank :answer :initx '(n n_bits)
	  '((cond ((equal n 0) t)
	          (t (list (setq regs (cons (areg :new n_bits) regs))
		  (self :initx (setq n (- n 1)) n_bits))))))

(reg_bank :answer :load '(reg val)
	  '((cond((> reg n_regs) (princ (list "Only "(+ 1 n_regs)" registers instantiated\n")))
		 (t (setq curr_reg (nth (+ reg 1) regs))
		    (curr_reg :load val)))))

(reg_bank :answer :see '(reg)
	  '((cond((> reg n_regs) (princ (list "Only "(+ 1 n_regs)" registers instantiated\n")))
		 (t (setq curr_reg (nth (+ reg 1) regs))
		    (curr_reg :see)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The Class alu

;alu - an n bit alu

(setq alu (Class :new '(n_bits maxs_val mins_val maxu_val minu_val nf zf vf cf)))

; methods

(alu :answer :isnew '(n)
     '((self :init n)
       self))

(alu :answer :init '(n)
     '((setq n_bits n)
       (setq maxu_val (- (pow2 n) 1))
       (setq maxs_val (- (pow2 (- n 1)) 1))
       (setq mins_val (- (- 0 maxs_val) 1))
       (setq minu_val 0)
       (setq nf 0)
       (setq zf 0)
       (setq vf 0)
       (setq cf 0)))

(alu :answer :check_arith '(a b)
     '((cond ((and (self :arith_range a) (self :arith_range b)) t)
	     (t ()))))

(alu :answer :check_logic '(a b)
     '((cond ((and (self :logic_range a) (self :logic_range b)) t)
	     (t ()))))

(alu :answer :arith_range '(a)
     '((cond ((< a mins_val) (princ (list "Operand out of Range "a"\n")))
	     ((> a maxs_val) (princ (list "Operand out of range "a"\n")))
             (t t))))

(alu :answer :logic_range '(a)
     '((cond ((< (abs a) minu_val) (princ (list "Operand out of Range "a"\n")))
             (t t))))

(alu :answer :set_flags '(a b r)
     '((if (equal 0 r) ((setq zf 1)))
       (if (< r 0) ((setq nf 1)))
       (if (or (and (and (< a 0) (< 0 b)) (>= r 0))
		  (and (and (>= a 0) (>= b 0)) (< r 0))) ((setq vf 1)))
       (if (or (or (and (< a 0) (< b 0)) (and (< a 0) (>= r 0)))
		  (and (>= r 0) (< b 0))) ((setq cf 1)))))
       
(alu :answer :+ '(a b &aux result)
     '((cond ((null (self :check_arith a b)) ())
	    (t (self :clear_flags)
	       (setq result (+ a b))
	       (if (> result maxs_val) ((setq result (+ (- (rem result maxs_val) 1) mins_val))))
		   (if (< result mins_val) ((setq result (+ (rem result mins_val) (+ maxs_val 1)))))
	       (self :set_flags a b result)
	       result))))

(alu :answer :& '(a b &aux result)
     '((cond ((null (self :check_logic a b)) ())
	    (t (self :clear_flags)
	       (setq result (bit-and a b))
	       (self :set_flags a b result)
	       result))))

(alu :answer :| '(a b &aux result)
     '((cond ((null (self :check_logic a b)) ())
	    (t (self :clear_flags)
	       (setq result (bit-ior a b))
	       (self :set_flags a b result)
	       result))))

(alu :answer :~ '(a  &aux result)
     '((cond ((null (self :check_logic a 0)) ())
	    (t (self :clear_flags)
	       (setq result (bit-not a))
	       (self :set_flags a 0 result)
	       result))))	       

(alu :answer :- '(a b)
     '((self '+ a (- 0 b))))

(alu :answer :passa '(a)
     '(a))

(alu :answer :zero '()
     '(0))

(alu :answer :com '(a)
     '((self :- 0 a)))

(alu :answer :status '()
     '((princ (list "NF "nf"\n"))
       (princ (list "ZF "zf"\n"))
       (princ (list "CF "cf"\n"))
       (princ (list "VF "vf"\n"))))

(alu :answer :clear_flags '()
     '((setq nf 0)
       (setq zf 0)
       (setq cf 0)
       (setq vf 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The class Memory
;

(setq memory (Class :new '(nabits ndbits maxu_val maxs_val mins_val max_addr undef memry)))

; methods

(memory :answer :isnew '(addr_bits data_bits)
     '((self :init addr_bits data_bits)
       self))

(memory :answer :init '(addr_bits data_bits)
     '((setq nabits addr_bits)
       (setq ndbits data_bits)
       (setq maxu_val (- (pow2 data_bits) 1))
       (setq max_addr (- (pow2 addr_bits) 1))
       (setq maxs_val (- (pow2 (- data_bits 1)) 1))
       (setq mins_val (- 0 (pow2 (- data_bits 1))))
       (setq undef (+ maxu_val 1))
       (setq memry (array :new max_addr undef))))


(memory :answer :load '(loc val)
     '((cond ((> (abs loc) max_addr) (princ (list "Address "loc" out of range\n")))
	     ((< val 0) (princ (list "Cant store "val" in "ndbits" bits\n")))
	     ((> val maxu_val) (princ (list "Cant store "val" in "ndbits" bits\n")))
	     (t (memry :load loc val)))))

(memory :answer :write '(loc val)
     '((cond ((> (abs loc) max_addr) (princ (list "Address "loc" out of range\n")))
	     ((> val maxs_val) (princ (list "Cant store "val" in "ndbits" bits\n")))
	     ((< val mins_val) (princ (list "Cant store "val" in "ndbits" bits\n")))
	     (t (memry :load loc val)))))


(memory :answer :read '(loc &aux val)
     '((cond ((> (abs loc) max_addr) (princ (list "Address "loc" out of range\n")))
	     (t (setq val (memry :see loc))
		(cond ((equal undef val) (princ (list "Address "loc" read before write\n")))
		      (t val))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The class array

(setq array (Class :new '(arry)))

; methods

(array :answer :isnew '(n val)
       '((self :init n val)
	 self))

(array :answer :init '(n val)
	'((cond ((< n 0) t)
	      (t (setq arry (cons val arry))
		 (self :init (- n 1) val)))))

(array :answer :see '(n)
	       '((nth (+ n 1) arry)))


(array :answer :load '(n val &aux left right temp)
       '((setq left (self :left_part n arry temp))
	 (setq right (self :right_part n arry))
	 (setq arry (append left (list val)))
	 (setq arry (append arry right))
	 val))

(array :answer :left_part '(n ary left)
       '((cond ((equal n 0) (reverse left))
	       (t (setq left (cons (car ary) left))
		  (self :left_part (- n 1) (cdr ary) left)))))

(array :answer :right_part '(n ary &aux right)
       '((cond ((equal n 0) (cdr ary))
	       (t (self :right_part (- n 1) (cdr ary))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
