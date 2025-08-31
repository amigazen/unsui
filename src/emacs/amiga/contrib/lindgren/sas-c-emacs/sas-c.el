;;;
;;; FILE
;;;	sas-c.el V0.1
;;;
;;;	Copyright (C) 1993 by Anders Lindgren.
;;;
;;;	This file is NOT part of GNU Emacs.
;;;
;;; DISTRIBUTION
;;;	sas-c.el is free software; you can redistribute it and/or modify
;;;	it under the terms of the GNU General Public License as published 
;;;	by the Free Software Foundation; either version 1, or (at your 
;;;	option) any later version.
;;;
;;;	GNU Emacs is distributed in the hope that it will be useful,
;;;	but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;	GNU General Public License for more details.
;;;
;;;	You should have received a copy of the GNU General Public
;;;	License along with GNU Emacs; see the file COPYING.  If not,
;;;	write to the Free Software Foundation, 675 Mass Ave, Cambridge,
;;;	MA 02139, USA.
;;;
;;; AUTHOR
;;;	Anders Lindgren, d91ali@csd.uu.se
;;;
;;; HISTORY
;;;	93-Mar-17 ALi * Created this file
;;;	93-Jun-09 ALi * Emacs now handles resultcodes from arexx-calls,
;;;			consequently, my old "bazoka" method were removed.
;;;	93-Jul-08 ALi * A typo in sas-c-scmsg-num fixed.
;;;			The delcomp-functions uncommented.
;;;

(defvar sas-c-mode nil
  "Variable indicating if the sas-c-mode is active.")

(defvar sas-c-compile-command "sc:c/smake"
  "The command to run when a sas-c-build is executed.")

(defun sas-c-mode (arg)
  "Minor mode which enables Emacs to communicate with SCMSG,
the error handler from SAS/C. If the function is called without args
the mode is toggled, a positive integer switchen it on and a negative off.

The following keys are added to the current local map:

C-c C-a		Display the alternative file, if any.
C-c C-c		Build a project (normally execute smake).
C-c C-d		Delete the current message, and display next
C-c C-h		Hide the SCMSG window.
C-c C-l		Redisplay current message.
C-c C-n		Display next error.
C-c <down>	dito
C-c C-p		Display previous error.
C-c C-q		Remove all messages for a certain primary file.
C-c C-s		Show the SCMSG window.
C-c <up>	dito
C-c <		Go to the first error message.
C-c <sh. up>	dito
C-c >		Go to the last error message.
C-c <sh. down>	dito

When sas-c-mode is switched on, the hook sas-c-hook is called.

If a key shall be defined, the best way is to use a hook and the
sas-c-define-key function. This way the keys are removed and the
original values are restored when sas-c-mode is disabled.

For example:
 (setq sas-c-mode-hook '(lambda ()
		    (sas-c-define-key \"\\C-ca\" 'your-favorite-fnk)
		    (sas-c-define-key \"\\C-cb\" 'another-function)
		    ))"
  (interactive "P")
  (make-local-variable 'sas-c-mode)
  (make-local-variable 'sas-c-original-keys)
  (let ((sas-c-mode-orig sas-c-mode))
    (setq sas-c-mode
	  (if (null arg) (not sas-c-mode)
	    (> (prefix-numeric-value arg) 0)))
    (or (assq 'sas-c-mode minor-mode-alist)
	(setq minor-mode-alist
	      (cons '(sas-c-mode " SAS/C") minor-mode-alist)))
    (cond ((and sas-c-mode (not sas-c-mode-orig))
	   ;; turning on sas-c-mode
	   (setq sas-c-original-keys '())
	   (sas-c-define-key "\C-c\C-a"      'sas-c-display-altfile)
	   (sas-c-define-key "\C-c\C-c"	     'sas-c-build)
	   (sas-c-define-key "\C-c\C-d"      'sas-c-delete)
	   (sas-c-define-key "\C-c\C-h"      'sas-c-hide)
	   (sas-c-define-key "\C-c\C-l"      'sas-c-display-error)
	   (sas-c-define-key "\C-c\C-n"      'sas-c-next)
	   (sas-c-define-key "\C-c\C-x\C-^B" 'sas-c-next)
	   (sas-c-define-key "\C-c\C-p"      'sas-c-prev)
	   (sas-c-define-key "\C-c\C-x\C-^A" 'sas-c-prev)
	   (sas-c-define-key "\C-c\C-q"      'sas-c-delcomp)
	   (sas-c-define-key "\C-c\C-Q"	     'sas-c-delfile)
	   (sas-c-define-key "\C-c\C-s"      'sas-c-show)
	   (sas-c-define-key "\C-c<"         'sas-c-top)
	   (sas-c-define-key "\C-c\C-x\C-^T" 'sas-c-top)
	   (sas-c-define-key "\C-c>"         'sas-c-bottom)
	   (sas-c-define-key "\C-c\C-x\C-^S" 'sas-c-bottom)
	   (run-hooks 'sas-c-mode-hook))
	  ((and (not sas-c-mode) sas-c-mode-orig)
	   ;; turning off sas-c-mode
	   (sas-c-undef-keys)))))

(defun sas-c-define-key (key fnk)
  "Make a keybinding which can be undone."
  (setq sas-c-original-keys (cons (cons key (local-key-binding key)) 
				  sas-c-original-keys))
  (local-set-key key fnk))

(defun sas-c-undef-keys ()
  "Unmake the keybindings made by sas-c-mode
and restore the keys previous values."
  (while sas-c-original-keys
    (let ((fnk (cdr (car sas-c-original-keys)))
	  (key (car (car sas-c-original-keys))))
      (if (numberp fnk)
	  (local-unset-key key)
	(local-set-key key fnk)))
    (setq sas-c-original-keys (cdr sas-c-original-keys))))

(defun sas-c-delete ()
  "Delete the current error message and move to the next."
  (interactive)
  (sas-c-scmsg "delete")
  (sas-c-display-error))

(defun sas-c-next ()
  "Move to the nest error message."
  (interactive)
  (sas-c-scmsg "next")
  (sas-c-display-error))

(defun sas-c-prev ()
  "Move to the prevous error message."
  (interactive)
  (sas-c-scmsg "prev")
  (sas-c-display-error))

(defun sas-c-top ()
  "Move to the first error message."
  (interactive)
  (sas-c-scmsg "top")
  (sas-c-display-error))

(defun sas-c-bottom ()
  "Move to the last error message."
  (interactive)
  (sas-c-scmsg "bottom")
  (sas-c-display-error))

(defun sas-c-delcomp-current ()
  "Delete all messages for the primary file of the current error."
  (sas-c-scmsg "delcomp"))

(defun sas-c-delcomp (filename)
  "Delete all messages with the specified filename as primary filename."
  (interactive "fFilename (Press return for current file): ")
  (sas-c-scmsg (format "delcomp \"%s\"" filename)))

(defun sas-c-delfile-current ()
  "Delete all messages for the secondary file of the current error."
  (sas-c-scmsg "delfile"))

(defun sas-c-delfile (filename)
  "Delete all messages with the specified filename as secondary filename."
  (interactive "fFilename (Press return for current file): ")
  (sas-c-scmsg (format "delfile \"%\"s" filename)))

(defun sas-c-show (& optional arg)
  "Show the scmsg window.
If called with arguments the window gets unactivated."
  (interactive "P")
  (sas-c-scmsg (if arg "show" "show activate")))

(defun sas-c-hide ()
  "Show the scmsg window."
  (interactive)
  (sas-c-scmsg "hide"))

(defun sas-c-build ()
  "Build with SAS/C. The command sas-c-compile-command is executed
and the output is places in the buffer *compilation*"
  (interactive)
  (compile sas-c-compile-command))

(defun sas-c-display-error ()
"Display the current error in SCMSG."
  (interactive)
  (let ((file (sas-c-scmsg-str "file")))
    (if (equal file "")
	(message "No more errors")
      (sas-c-view-message file 
			  (sas-c-scmsg-num "line")
			  (sas-c-scmsg-str "text")
			  (sas-c-scmsg-str "class")
			  (sas-c-scmsg-str "errnum")))))

(defun sas-c-display-altfile ()
"Display the secondary file. (Same as C-u sas-c-display-error.)"
  (interactive)
  (let ((file (sas-c-scmsg-str "altfile")))
    (if (equal file "")
	(message "No alternate file")
      (sas-c-view-message file 
			  (sas-c-scmsg-num "altline")
			  (sas-c-scmsg-str "text")
			  (sas-c-scmsg-str "class")
			  (sas-c-scmsg-str "errnum")))))

(defun sas-c-view-message (file line text class errnum)
  (sas-c-get-file file)
  (set-mark (point))
  (goto-line line)
  (let ((isalt (string-match "; See line [0-9]* file \".*\"" text)))
    (if isalt (setq text (substring text 0 isalt)))
    (message (format "%s %s%s: %s" class 
		                   errnum 
				   (if isalt " (Alt)" "") 
				   text))))

(defun sas-c-get-file (file)
  "Get the file requested into a visiable buffer."
  (let ((buf (get-file-buffer file)))
    (if buf
	(let ((win (get-buffer-window buf)))
	  (if win
	      (select-window win)
	    (switch-to-buffer buf)))
      (find-file file))))

;;;
;;; Low level ARexx communication routines.
;;;  

(defun sas-c-scmsg (command)
  "Sends a command to SCMSG."
  (amiga-arexx-do-command
    (concat "address 'SC_SCMSG' '" command "'") 
    t))

(defun sas-c-scmsg-str (command)
  "Sends a command to SCMSG and returns the result string."
  (amiga-arexx-do-command
    (concat "options results; address 'SC_SCMSG' '"
	    command
	    "'; return result")
    t))

(defun sas-c-scmsg-num (command)
  "Sends a command to SCMSG and returns the resulting number."
  (string-to-int (amiga-arexx-do-command
		  (concat "options results; address 'SC_SCMSG' '"
			  command
			  "'; return result")
		  t)))
