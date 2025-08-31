;; Mouse support for Amiga Intuition window system.
;; Copyright (C) 1985, 1987 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

(provide 'amiga-mouse)

(defconst amiga-button-right (char-to-string 0))
(defconst amiga-button-middle (char-to-string 1))
(defconst amiga-button-left (char-to-string 2))

(defconst amiga-button-right-up (char-to-string 4))
(defconst amiga-button-middle-up (char-to-string 5))
(defconst amiga-button-left-up (char-to-string 6))

(defconst amiga-button-s-right (char-to-string 16))
(defconst amiga-button-s-middle (char-to-string 17))
(defconst amiga-button-s-left (char-to-string 18))

(defconst amiga-button-s-right-up (char-to-string 20))
(defconst amiga-button-s-middle-up (char-to-string 21))
(defconst amiga-button-s-left-up (char-to-string 22))

(defconst amiga-button-m-right (char-to-string 32))
(defconst amiga-button-m-middle (char-to-string 33))
(defconst amiga-button-m-left (char-to-string 34))

(defconst amiga-button-m-right-up (char-to-string 36))
(defconst amiga-button-m-middle-up (char-to-string 37))
(defconst amiga-button-m-left-up (char-to-string 38))

(defconst amiga-button-c-right (char-to-string 64))
(defconst amiga-button-c-middle (char-to-string 65))
(defconst amiga-button-c-left (char-to-string 66))

(defconst amiga-button-c-right-up (char-to-string 68))
(defconst amiga-button-c-middle-up (char-to-string 69))
(defconst amiga-button-c-left-up (char-to-string 70))

(defconst amiga-button-m-s-right (char-to-string 48))
(defconst amiga-button-m-s-middle (char-to-string 49))
(defconst amiga-button-m-s-left (char-to-string 50))

(defconst amiga-button-m-s-right-up (char-to-string 52))
(defconst amiga-button-m-s-middle-up (char-to-string 53))
(defconst amiga-button-m-s-left-up (char-to-string 54))

(defconst amiga-button-c-s-right (char-to-string 80))
(defconst amiga-button-c-s-middle (char-to-string 81))
(defconst amiga-button-c-s-left (char-to-string 82))

(defconst amiga-button-c-s-right-up (char-to-string 84))
(defconst amiga-button-c-s-middle-up (char-to-string 85))
(defconst amiga-button-c-s-left-up (char-to-string 86))

(defconst amiga-button-c-m-right (char-to-string 96))
(defconst amiga-button-c-m-middle (char-to-string 97))
(defconst amiga-button-c-m-left (char-to-string 98))

(defconst amiga-button-c-m-right-up (char-to-string 100))
(defconst amiga-button-c-m-middle-up (char-to-string 101))
(defconst amiga-button-c-m-left-up (char-to-string 102))

(defconst amiga-button-c-m-s-right (char-to-string 112))
(defconst amiga-button-c-m-s-middle (char-to-string 113))
(defconst amiga-button-c-m-s-left (char-to-string 114))

(defconst amiga-button-c-m-s-right-up (char-to-string 116))
(defconst amiga-button-c-m-s-middle-up (char-to-string 117))
(defconst amiga-button-c-m-s-left-up (char-to-string 118))

(defmacro cadr (x) (list 'car (list 'cdr x)))
(defmacro caddr (x) (list 'car (list 'cdr (list 'cdr x))))
(defmacro cadddr (x) (list 'car (list 'cdr (list 'cdr (list 'cdr x)))))

(defun coordinates-in-window-p (arg w)
  (let ((x (car arg))
	(y (cadr arg))
	(edges (window-edges w)))
    (and (>= x (car edges)) (< x (caddr edges))
	 (>= y (cadr edges)) (< y (cadddr edges))
	 (list (- x (car edges)) (- y (cadr edges))))))

(defvar amiga-process-mouse-hook nil
  "Hook to run after each mouse event is processed.  Should take two
arguments; the first being a list (XPOS YPOS) corresponding to character
offset from top left of screen and the second being a specifier for the
buttons/keys.

This will normally be set on a per-buffer basis.")

(defun amiga-flush-mouse-queue () 
  "Process all queued mouse events."
  ;; A mouse event causes a special character sequence to be given
  ;; as keyboard input.  That runs this function, which process all
  ;; queued mouse events and returns.
  (interactive)
  (while (> (amiga-mouse-events) 0)
    (amiga-proc-mouse-event)
    (and (boundp 'amiga-process-mouse-hook)
	 (symbol-value 'amiga-process-mouse-hook)
	 (funcall amiga-process-mouse-hook amiga-mouse-pos amiga-mouse-item))))

(defun amiga-mouse-select (arg)
  "Select Emacs window the mouse is on."
  (let ((start-w (selected-window))
	(done nil)
	(w (selected-window))
	(rel-coordinate nil))
    (if (eq start-w (minibuffer-window))
	(setq rel-coordinate (coordinates-in-window-p arg w))
	(while (and (not done)
		    (null (setq rel-coordinate
				(coordinates-in-window-p arg w))))
	  (setq w (next-window w))
	  (if (eq w start-w)
	      (setq done t))))
    (select-window w)
    rel-coordinate))

(defun amiga-mouse-keep-one-window (arg)
  "Select Emacs window mouse is on, then kill all other Emacs windows."
  (if (amiga-mouse-select arg)
      (delete-other-windows)))

(defun amiga-mouse-select-and-split (arg)
  "Select Emacs window mouse is on, then split it vertically in half."
  (if (amiga-mouse-select arg)
      (split-window-vertically nil)))


(defun amiga-mouse-set-point (arg)
  "Select Emacs window mouse is on, and move point to mouse position."
  (let* ((relative-coordinate (amiga-mouse-select arg))
	 margin-column
	 (rel-x (car relative-coordinate))
	 (rel-y (car (cdr relative-coordinate))))
    (if relative-coordinate
	(let ((prompt-width (if (eq (selected-window) (minibuffer-window))
				minibuffer-prompt-width 0)))
	  (move-to-window-line rel-y)
	  (setq margin-column
		(if (or truncate-lines (> (window-hscroll) 0))
		    (current-column)
		  ;; If we are using line continuation,
		  ;; compensate if first character on a continuation line
		  ;; does not start precisely at the margin.
		  (- (current-column)
		     (% (current-column) (1- (window-width))))))
	  (move-to-column (+ rel-x (1- (max 1 (window-hscroll)))
			     (if (= (point) 1)
				 (- prompt-width) 0)
			     margin-column))))))

(defun amiga-mouse-set-mark (arg)
  "Select Emacs window mouse is on, and set mark at mouse position.
Display cursor at that position for a second."
  (if (amiga-mouse-select arg)
      (let ((point-save (point)))
	(unwind-protect
	    (progn (amiga-mouse-set-point arg)
		   (push-mark nil t)
		   (sit-for 1))
	  (goto-char point-save)))))

(defun amiga-mouse-cut (arg)
  "Select Emacs window mouse is on, and set mark at mouse position. 
Display cursor at that position for a second. Then cut."
  (if (amiga-mouse-select arg)
      (let ((point-save (point)))
	(unwind-protect
	    (progn (amiga-mouse-set-point arg)
		   (push-mark nil t)
		   (kill-region point-save (point))
		   (sit-for 1))
	  (goto-char point-save)))))

(defun amiga-mouse-copy (arg)
  "Select Emacs window mouse is on, and set mark at mouse position. 
Display cursor at that position for a second. Then copy."
  (if (amiga-mouse-select arg)
      (let ((point-save (point)))
	(unwind-protect
	    (progn (amiga-mouse-set-point arg)
		   (push-mark nil t)
		   (copy-region-as-kill point-save (point))
		   (sit-for 1))
	  (goto-char point-save)))))

(defun amiga-mouse-paste (arg)
  "Move point to mouse position (and select window), then paste."
  (if (amiga-mouse-select arg)
      (progn
	(amiga-mouse-set-point arg)
	(yank))))

(defun amiga-mouse-iconify (arg) (amiga-iconify))

(defun amiga-mouse-ignore (arg)
  "Don't do anything.")

; Prevent beeps. on button-up.  If the button isn't bound to anything, it
(define-key mouse-map amiga-button-right 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-middle 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-left 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-right-up 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-middle-up 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-left-up 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-s-right 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-s-middle 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-s-left 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-s-right-up 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-s-middle-up 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-s-left-up 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-m-right 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-m-middle 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-m-left 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-m-right-up 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-m-middle-up 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-m-left-up 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-c-right 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-c-middle 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-c-left 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-c-right-up 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-c-middle-up 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-c-left-up 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-m-s-right 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-m-s-middle 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-m-s-left 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-m-s-right-up 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-m-s-middle-up 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-m-s-left-up 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-c-s-right 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-c-s-middle 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-c-s-left 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-c-s-right-up 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-c-s-middle-up 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-c-s-left-up 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-c-m-right 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-c-m-middle 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-c-m-left 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-c-m-right-up 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-c-m-middle-up 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-c-m-left-up 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-c-m-s-right 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-c-m-s-middle 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-c-m-s-left 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-c-m-s-right-up 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-c-m-s-middle-up 'amiga-mouse-ignore)
(define-key mouse-map amiga-button-c-m-s-left-up 'amiga-mouse-ignore)

; Define a few events
(define-key mouse-map amiga-button-left 'amiga-mouse-set-point)
(define-key mouse-map amiga-button-s-left 'amiga-mouse-set-mark)
(define-key mouse-map amiga-button-c-left 'amiga-mouse-cut)
(define-key mouse-map amiga-button-m-left 'amiga-mouse-copy)
(define-key mouse-map amiga-button-middle 'amiga-mouse-paste)
(define-key mouse-map amiga-button-s-middle 'amiga-mouse-iconify)

(define-key amiga-map "M" 'amiga-flush-mouse-queue)
(setq amiga-mouse-initialized t)  ;; Mouse commands can now be processed.
