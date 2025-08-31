;; Code contributed by :
;;   Michel Schinz     | INTERNET: Week-end: schinz@guano.alphanet.ch
;;   Epinettes 10a     |           Week    : schinz@di.epfl.ch
;;   CH-2013 COLOMBIER | FIDONET:  2:302/562 (Michel Schinz)


   (defun amiga-arexx-add-lib (lib entry)
     "Adds the specified ARexx library, with given entry point."
     (interactive "sLibrary name: \nnOffset: ")
     (amiga-arexx-do-command (concat "CALL ADDLIB('" lib "',0," entry ")") t))

   (defun amiga-reqtools-find-file ()
     "Like find-file, but with RexxReqTools' file requester."
     (interactive)
     (amiga-arexx-add-lib "rexxreqtools.library" -30)
     (amiga-arexx-do-command (concat "f = rtFileRequest('" default-directory
				     "',,'Find file')\n"
				     "IF f ~= '' THEN"
				     "   '(find-file \"'||f||'\")'") t))

   (defun amiga-reqtools-insert-file ()
     "Like insert-file, but with RexxReqTools' file requester."
     (interactive)
     (amiga-arexx-add-lib "rexxreqtools.library" -30)
     (amiga-arexx-do-command (concat "f = rtFileRequest('" default-directory
				     "',,'Insert file')\n"
				     "IF f ~= '' THEN"
				     "   '(insert-file \"'||f||'\")'") t))

   (defun amiga-reqtools-write-file ()
     "Like write-file, but with RexxReqTools' file requester."
     (interactive)
     (let ((dir (file-name-directory buffer-file-name))
	   (file (file-name-nondirectory buffer-file-name)))
       (amiga-arexx-add-lib "rexxreqtools.library" -30)
       (amiga-arexx-do-command (concat "f = rtFileRequest('" dir "','" file
				       "','Write file',,"
				       "'rtfi_flags=freqf_save')\n"
				       "IF f ~= '' THEN"
				       "   '(write-file \"'||f||'\")'") t)))

   ;;; amiga-update-version-string
   ;;; Michel Schinz
   ;;; $VER: amiga_verstring.el 1.0 (7.05.1993)

   (defun amiga-update-version-string ()
     "Create/update a version string (CBM style) on the current line.
   If no version string is present on the current line, create a new one
   that looks like `$VER: <buffer-name> 1.0 (<current_date>)' at the
   point position. If a version string is already present on the current
   line, the date is ... updated and the revision is incremented.
   Please note that currently, no spaces are allowed in the `name' field."
     (interactive)
     (save-excursion
       (let (end-of-line-pos
	     name version revision date
	     (old-point-pos (point-marker)))
	 (let* ((date-string (current-time-string))
		(garbage (string-match
			  " \\([A-Z][a-z][a-z]\\) *\\([0-9]*\\) .* \\([0-9]*\\)$"
			  date-string))
		(day (substring date-string (match-beginning 2) (match-end 2)))
		(month
		 (cdr (assoc
		       (substring date-string (match-beginning 1) (match-end 1))
		       '(("Jan" . "01") ("Feb" . "02") ("Mar" . "03") ("Apr" . "04")
			 ("May" . "05") ("Jun" . "06") ("Jul" . "07") ("Aug" . "08")
			 ("Sep" . "09") ("Oct" . "10") ("Nov" . "11") ("Dec" . "12")))))
		(year (substring date-string (match-beginning 3) (match-end 3))))
	   (setq date (concat day "." month "." year)))
	 (end-of-line)
	 (setq end-of-line-pos (point))
	 (beginning-of-line)
	 (if (search-forward "$VER: " end-of-line-pos t)
	     (let ((start-of-version-string (- (point) 6)))
	       (re-search-forward
		" *\\([^ ]*\\) *\\([0-9]*\\)\.\\([0-9]*\\) *([0-9.]*)" end-of-line-pos t)
	       (setq name (buffer-substring (match-beginning 1) (match-end 1))
		     version (buffer-substring (match-beginning 2) (match-end 2))
		     revision (+ 1 (string-to-int
				    (buffer-substring (match-beginning 3) (match-end 3)))))
	       (delete-region start-of-version-string (point)))
	   (progn (setq name (file-name-nondirectory (buffer-name))
			version 1
			revision 0)
		  (goto-char old-point-pos)))
	 (insert (concat "$VER: " name " " version "." revision " (" date ")")))))

