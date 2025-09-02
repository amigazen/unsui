;(provide 'amiga-menu)

(defconst amiga-menu-pick (char-to-string 3))
(defconst amiga-menu-help (char-to-string 7))

(defvar amiga-menus-description nil
  "Variable containing the menus setup for Emacs")

(defun amiga-menus-set (menus)
  "Setup menus for emacs (parameter as for amiga-menus)"
  (define-key mouse-map amiga-menu-pick 'amiga-menus-dispatch)
  (define-key mouse-map amiga-menu-help 'amiga-menus-help)
  (setq amiga-menus-description menus)
  (amiga-menus menus))

(defun amiga-menus-dispatch (selection)
  (let* ((menu (car selection))
	 (item (cadr selection))
	 (code (cadr (nth item (cadr (nth menu amiga-menus-description))))))
    (if (and (listp code) (eq (car code) 'call-interactively)
	     (listp (cadr code)) (eq (car (cadr code)) 'quote))
	(setq this-command (cadr (cadr code))))
    (eval code)))

(defun amiga-menus-help (selection)
  (let* ((menu (car selection))
	 (item (cadr selection))
	 (cmd (cadr (nth item (cadr (nth menu amiga-menus-description))))))
    (if (and (listp cmd) (eq (car cmd) 'call-interactively)
	     (listp (car (cdr cmd))) (eq (car (car (cdr cmd))) 'quote))
	(describe-function (car (cdr (car (cdr cmd)))))
	(error "Don't know how to describe %s" cmd))))

(defun make-explicit-string (str)
  (if (and (>= (length str) 2) (= (elt str 0) 27) (< (elt str 1) 128))
      (key-description (concat (char-to-string (+ 128 (elt str 1)))
			       (substring str 2)))
      (key-description str)))

(defun make-command-name (command str width)
  (let ((keys (where-is-internal command nil t))
	(string (if str str (symbol-name command))))
    (if keys
	(format (if width (format "%%-%ds%%s" (+ width 2)) "%s (%s)")
		string (make-explicit-string keys))
	string)))

(defun menu-items (commands proportional)
  (let* ((width (if proportional nil 0))
	 (names (mapcar
		 (function (lambda (cmd)
			     (if cmd
				 (let* ((name (if (symbolp cmd)
						  (symbol-name cmd)
						  (car cmd)))
					(len (length name)))
				   (if (and (not proportional) (> len width))
				       (setq width len))
				   name))))
		 commands)))
    (mapcar
     (function (lambda (cmd)
		 (let ((name (car names)))
		   (setq names (cdr names))
		   (if cmd
		       (let ((command (if (symbolp cmd) cmd (cadr cmd))))
			 (list (make-command-name command name width)
			       (list 'call-interactively (list 'quote command))
			       (caddr cmd)))))))
     commands)))

(defun convert-menu-buffer (proportional)
  "Convert the current buffer into a loadable menu file for emacs.\n\
If PROPORTIONAL is true (or if a prefix arg is given), assume menu is in a \n\
proportional font & present it differently."
  (interactive "P")
  (save-buffer)
  (widen)
  (goto-char 1)
  (let ((menu-spec (reverse (read (current-buffer))))
	menu-code)
    (while menu-spec
      (let ((menu-item (car menu-spec)))
	(setq menu-code
	      (cons (list (car menu-item)
			  (menu-items (cdr menu-item) proportional))
		    menu-code))
	(setq menu-spec (cdr menu-spec))))
    (let ((new-buf
	   (find-file (concat
		       (substring (buffer-file-name) 0
				  (string-match "\\.menu$" (buffer-file-name)))
		       ".el"))))
      (erase-buffer)
      (prin1 (list 'amiga-menus-set (list 'quote menu-code)) (current-buffer))
      (beginning-of-buffer))))

(defvar menu-mode-syntax-table nil
  "Syntax table used while in menu mode.")

(defvar menu-mode-abbrev-table nil
  "Abbrev table used while in menu mode.")
(define-abbrev-table 'menu-mode-abbrev-table ())

(if menu-mode-syntax-table
    ()
  (setq menu-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?' "w   " menu-mode-syntax-table))

(defvar menu-mode-map ())
(if menu-mode-map
    ()
  (setq menu-mode-map (make-sparse-keymap))
  (define-key menu-mode-map "\t" 'indent-relative)
  (define-key menu-mode-map "\C-c\C-c" 'convert-menu-buffer))

(defun menu-mode ()
  "Major mode for editing menus intended for humans to read.
Indentation works like in indented-text-mode. This could be improved.\\{menu-mode-map}
Turning on menu-mode calls the value of the variable menu-mode-hook,
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map menu-mode-map)
  (define-abbrev-table 'menu-mode-abbrev-table ())
  (setq local-abbrev-table menu-mode-abbrev-table)
  (set-syntax-table menu-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'indent-relative-maybe)
  (use-local-map menu-mode-map)
  (setq mode-name "Menu")
  (setq major-mode 'menu-mode)
  (run-hooks 'menu-mode-hook))

(setq auto-mode-alist (cons '("\\.menu$" . menu-mode) auto-mode-alist))
