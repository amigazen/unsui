;;;
;;;  FILE
;;;	xshell.el
;;;
;;;  DESCRIPTION
;;;	Filter for removing those pesky ctrl-O:s my shell (WShell)
;;;	outputs.
;;;
;;;	Place the following line in your s:.emacs file:
;;;
;;;	(autoload 'xshell "xshell.el" nil t)
;;;
;;;  AUTHOR
;;; 	Anders Lindgren, d91ali@csd.uu.se
;;;
;;;  STATUS
;;;	This file is in the public domain.
;;;

(provide 'xshell)

(defvar explicit-shell-file-name "xsh")
(defvar explicit-xsh-args '())

(defun xshell ()
  (interactive)
  (shell)
  (set-process-filter (get-process "shell") 'strip-ctrl-o-filter))


(defun strip-ctrl-o-filter (proc string)
  (let ((old-buffer (current-buffer)))
    (unwind-protect
	(let (moving)
	  (set-buffer (process-buffer proc))
	  (setq moving (= (point) (process-mark proc)))
	  (save-excursion
	    (goto-char (process-mark proc))
	    (insert (strip-ctrl-o string 0))
	    (set-marker (process-mark proc) (point)))
	  (if moving (goto-char (process-mark proc))))
      (set-buffer old-buffer))))

(defun strip-ctrl-o (string start)
  (let ((idx (string-match "\C-O" string start)))
    (cond ((eq idx nil) (substring string start))
	  (t (concat (substring string start idx) 
		     (strip-ctrl-o string (1+ idx)))))))
    






