(global-set-key "\C-z" 'amiga-iconify)
(setq amiga-map (make-keymap))
(global-set-key "\C-x\C-^" amiga-map)

(load "amiga-mouse")
(load "amiga-menu")

(define-key amiga-map "A" 'previous-line)
(define-key amiga-map "B" 'next-line)
(define-key amiga-map "D" 'backward-char)
(define-key amiga-map "C" 'forward-char)
(define-key amiga-map "?~" 'info)
(define-key amiga-map "T" 'scroll-down)
(define-key amiga-map "S" 'scroll-up)
(define-key amiga-map " @" 'forward-word)
(define-key amiga-map " A" 'backward-word)
(define-key amiga-map "\M-A" 'beginning-of-buffer)
(define-key amiga-map "\M-B" 'end-of-buffer)
(define-key amiga-map "\M-D" 'beginning-of-line)
(define-key amiga-map "\M-C" 'end-of-line)
(define-key amiga-map "\M- \M-@" 'forward-sexp)
(define-key amiga-map "\M- \M-A" 'backward-sexp)
(define-key amiga-map "\M-T" 'scroll-down-1)
(define-key amiga-map "\M-S" 'scroll-up-1)
; Keypad sequences are handled like normal ones
(define-key amiga-map "K" 'do-nothing)

(defun do-nothing () (interactive))

(defun scroll-down-1 ()
  "Move up one line on screen"
  (interactive)
  (scroll-down 1))

(defun scroll-up-1 ()
  "Move down one line on screen"
  (interactive)
  (scroll-up 1))

;; ARexx stuff

;;; This function needs to be re-written to handle rexx returned results.
;;;
(setq amiga-arexx-processing nil)
(setq amiga-arexx-errors nil)

(defvar amiga-arexx-failat 5
  "Return level from which arexx commands returns cause errors")

;;
;; process incoming rexx messages
;;
(defun amiga-arexx-process ()
  (interactive)
  (if (not amiga-arexx-processing)
      (progn
	(setq amiga-arexx-processing t)
	(condition-case nil ; Avoid blocking of processing in case of bugs
	    (let (arexxcmd)
	      (while (setq arexxcmd (amiga-arexx-get-next-msg))
		(let ((rc 0) result)
		  (condition-case err ; detect errors in arexx command
		      (let ((expr (car (read-from-string arexxcmd))))
			(setq result (prin1-to-string (eval expr))))
		    (error (progn
			     (setq rc 20)
			     (setq result (prin1-to-string err)))))
		  (amiga-arexx-reply rc result))))
	  (error nil))
	(setq amiga-arexx-processing nil))))

(defun amiga-arexx-wait-command (id)
  "Waits for a pending ARexx commands (MSGID) to complete.
Also processes any pending ARexx requests during this interval.
returns the result list associated with this id, which takes the
form: (msgid result-code error-or-string)
``error-or-string'' depends on ``result-code''.
if ``result-code'' is 0 the command finished successfully and
``error-or-string'' will be a string or nil, otherwise the command
returned with an error and ``error-or-string'' will be an interger
that is the secondary error code of the arexx command."
  (amiga-arexx-process)
  (while (not (amiga-arexx-check-command id))
    (amiga-arexx-wait)
    (amiga-arexx-process))
  (amiga-arexx-get-msg-results id))

(defconst amiga-arexx-error-messages
["No cause"
"Program not found"
"Execution halted"
"Insufficient memory"
"Invalid character"
"Unmatched quote"
"Unterminated comment"
"Clause too long"
"Invalid token"
"Symbol or string too long"
"Invalid message packet"
"Command string error"
"Error return from function"
"Host environment not found"
"Requested library not found"
"Function not found"
"Function did not return value"
"Wrong number of arguments"
"Invalid argument to function"
"Invalid PROCEDURE"
"Unexpected THEN or WHEN"
"Unexpected ELSE or OTHERWISE"
"Unexpected BREAK, LEAVE or ITERATE"
"Invalid statement in SELECT"
"Missing or multiple THEN"
"Missing OTHERWISE"
"Missing or unexpected END"
"Symbol mismatch"
"Invalid DO syntax"
"Incomplete IF or SELECT"
"Label not found"
"Symbol expected"
"Symbol or string expected"
"Invalid keyword"
"Required keyword missing"
"Extraneous characters"
"Keyword conflict"
"Invalid template"
"Invalid TRACE request"
"Unitialized variable"
"Invalid variable name"
"Invalid expression"
"Unbalanced parentheses"
"Nesting limit exceeded"
"Invalid expression result"
"Expression required"
"Boolean value not 0 or 1"
"Arithmetic conversion error"
"Invalid operand"
]
"The arexx error messages, sorted by number")

(defun amiga-arexx-do-command (str as-file)
  "Sends ARexx command STR (like amiga-arexx-send-command).
If AS-FILE is true, STR is an arexx command, otherwise it is a file name.
Waits for the command to return.  If the arexx command fails an error will
be caused.

If you would like to get result strings and errors (ie. not cause
a lisp error) use: (amiga-arexx-do-command-with-results)"
  (interactive "sARexx command:
P")
  (let ((id (amiga-arexx-send-command str as-file)))
    (if (not id)
	(error "Failed to send arexx command.")
      (let ((reslist (amiga-arexx-wait-command id)))
	(let ((rc (nth 1 reslist)) (second (nth 2 reslist)))
	  (if (> rc 0)
	      (progn			; error
		(let ((error-message
		       (if (< second (length amiga-arexx-error-messages))
			 (aref amiga-arexx-error-messages second)
			 (format nil "Unknown error %d" second))))
		  (error "Arexx command failed, level %d, cause %s" rc error-message))
		reslist)
	  second))))))

(defun amiga-arexx-do-command-with-results (str as-file)
  "Sends ARexx command STR (like amiga-arexx-do-command).
If AS-FILE is true, STR is an arexx command, otherwise it is a file name.
Waits for the command to return.

The return value is one of three things:
 - the command executed succesfully: nil or a result string.
 - the command failed: a list of the form (RC ERROR-CODE)
   where RC is the severity and ERROR-CODE is the secondary error."
  (interactive "sARexx command:
P")
  (let ((id (amiga-arexx-send-command str as-file)))
    (if (not id)
	(error "Failed to send arexx command.")
      (let ((reslist (amiga-arexx-wait-command id)))
	(let ((rc (nth 1 reslist)) (second (nth 2 reslist)))
	  (if (and rc (> rc 0))
	      (list rc second)
	    second))))))

(define-key amiga-map "X" 'amiga-arexx-process)
(setq amiga-arexx-initialized t) ;; ARexx commands can now be processed.

(defun amiga-wb-process ()
  "Process all pending workbench events, ie load all files requested"
  (interactive)
  (let (file)
    (condition-case nil
	(while (setq file (amiga-get-wb-event t))
	  (condition-case nil
	      (find-file file)
	    (error nil)))
      (error nil))))

(define-key amiga-map "W" 'amiga-wb-process)
(setq amiga-wb-initialized t) ;; WB events can now be processed.

(setq completion-ignore-case t)
;; Default is no numbered versions on Amiga, because directory searches are too
;; slow.
(setq version-control 'never)
