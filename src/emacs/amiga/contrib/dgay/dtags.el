;; Dtags facility for Emacs.
;; Copyright (C) 1985, 1986, 1988 Free Software Foundation, Inc.

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


(provide 'dtags)
(load "tags")

(defvar dtag-table-files nil
  "List of file names covered by current dtag table.
nil means it has not been computed yet; do (dtag-table-files) to compute it.")

(defvar dtags-file-name "docs:DTAGS"
  "Where to find the doc tags, use visit-dtags-table to change it.")

(defun visit-dtags-table (file)
  "Tell dtags commands to use tag table file FILE.
FILE should be the name of a file created with the `etags' program.
A directory name is ok too; it means file TAGS in that directory."
  (interactive (list (read-file-name "Visit tags table: (default TAGS) "
				     default-directory
				     (concat default-directory "TAGS")
				     t)))
  (setq file (expand-file-name file))
  (if (file-directory-p file)
      (setq file (concat file "DTAGS")))
  (setq dtag-table-files nil
	dtags-file-name file))

(defun visit-dtags-table-buffer ()
  "Select the buffer containing the current tag table.
This is a file whose name is in the variable tags-file-name."
  (or dtags-file-name
      (call-interactively 'visit-dtags-table))
  (set-buffer (or (get-file-buffer dtags-file-name)
		  (progn
		    (setq dtag-table-files nil)
		    (find-file-noselect dtags-file-name))))
  (or (verify-visited-file-modtime (get-file-buffer dtags-file-name))
      (cond ((yes-or-no-p "Dtags file has changed, read new contents? ")
	     (revert-buffer t t)
	     (setq dtag-table-files nil))))
  (or (eq (char-after 1) ?\^L)
      (error "File %s not a valid dtag table" dtags-file-name)))

(defun dtag-table-files ()
  "Return a list of files in the current dtag table.
File names returned are absolute."
  (save-excursion
   (visit-dtags-table-buffer)
   (or dtag-table-files
       (let (files)
	(goto-char (point-min))
	(while (not (eobp))
	  (forward-line 1)
	  (end-of-line)
	  (skip-chars-backward "^,\n")
	  (setq prev (point))
	  (setq size (read (current-buffer)))
	  (goto-char prev)
	  (setq files (cons (expand-file-name
			     (buffer-substring (1- (point))
					       (save-excursion
						 (beginning-of-line)
						 (point)))
			     (file-name-directory dtags-file-name))
			    files))
	  (forward-line 1)
	  (forward-char size))
	(setq dtag-table-files (nreverse files))))))

(defun doc-tag (tagname &optional next)
  "Find tag (in current tag table) whose name contains TAGNAME.
 Selects the buffer that the tag is contained in
and puts point at its definition.
 If TAGNAME is a null string, the expression in the buffer
around or before point is used as the tag name.
 If second arg NEXT is non-nil (interactively, with prefix arg),
searches for the next tag in the tag table
that matches the tagname used in the previous find-tag.

See documentation of variable tags-file-name."
  (interactive (if current-prefix-arg
		   '(nil t)
		 (find-tag-tag "Find tag: ")))
  (let (buffer file linebeg startpos)
    (save-excursion
     (visit-dtags-table-buffer)
     (if (not next)
	 (goto-char (point-min))
       (setq tagname last-tag))
     (setq last-tag tagname)
     (while (progn
	      (if (not (search-forward tagname nil t))
		  (error "No %sentries containing %s"
			 (if next "more " "") tagname))
	      (not (looking-at "[^\n\177]*\177"))))
     (search-forward "\177")
     (setq file (expand-file-name (file-of-tag)
				  (file-name-directory dtags-file-name)))
     (setq linebeg
	   (buffer-substring (1- (point))
			     (save-excursion (beginning-of-line) (point))))
     (search-forward ",")
     (setq startpos (read (current-buffer))))
    (let ((win (selected-window)))
      (find-file-other-window file)
      (widen)
      (push-mark)
      (let ((offset 1000)
	    found
	    (pat (concat (regexp-quote linebeg) "[ \t]")))
	(or startpos (setq startpos (point-min)))
	(while (and (not found)
		    (progn
		      (goto-char (- startpos offset))
		      (not (bobp))))
	  (setq found
		(re-search-forward pat (+ startpos offset) t))
	  (setq offset (* 3 offset)))
	(or found
	    (re-search-forward pat nil t)
	    (error "%s not found in %s" pat file)))
      (beginning-of-line)
      (recenter 0)
      (select-window win)))
  (setq tags-loop-form '(doc-tag nil t))
  ;; Return t in case used as the tags-loop-form.
  t)

(defun dtags-search (regexp)
  "Search through all files listed in tag table for match for REGEXP.
Stops when a match is found.
To continue searching for next match, use command \\[tags-loop-continue].

See documentation of variable tags-file-name."
  (interactive "sTags search (regexp): ")
  (if (and (equal regexp "")
	   (eq (car tags-loop-form) 're-search-forward))
      (tags-loop-continue nil)
    (setq tags-loop-form
	  (list 're-search-forward regexp nil t))
    (setq next-file-list (dtag-table-files))
    (next-file)
    (tags-loop-continue)))

(defun list-dtags (string)
  "Display list of dtags in file FILE.
FILE should not contain a directory spec
unless it has one in the dtag table."
  (interactive "sList dtags (in file): ")
  (with-output-to-temp-buffer "*Dtags List*"
    (princ "Dtags in file ")
    (princ string)
    (terpri)
    (save-excursion
     (visit-dtags-table-buffer)
     (goto-char 1)
     (search-forward (concat "\f\n" string ","))
     (forward-line 1)
     (while (not (or (eobp) (looking-at "\f")))
       (princ (buffer-substring (point)
				(progn (skip-chars-forward "^\177")
				       (point))))
       (terpri)
       (forward-line 1)))))

(defun dtags-apropos (string)
  "Display list of all dtags in dtag table REGEXP matches."
  (interactive "sDtag apropos (regexp): ")
  (with-output-to-temp-buffer "*Dtags List*"
    (princ "Dtags matching regexp ")
    (prin1 string)
    (terpri)
    (save-excursion
     (visit-dtags-table-buffer)
     (goto-char 1)
     (while (re-search-forward string nil t)
       (beginning-of-line)
       (princ (buffer-substring (point)
				(progn (skip-chars-forward "^\177")
				       (point))))
       (terpri)
       (forward-line 1)))))

(defun make-dtags (file)
  (interactive "FDoc file: ")
  (setq file (expand-file-name file))
  (save-excursion
    (find-file-noselect dtags-file-name)
    (let ((buf-doc (get-buffer-create "*make-doc-temp*"))
	  (buf-tags (get-buffer-create dtags-file-name)))
      ; First, create the header for the tags
      (set-buffer buf-tags)
      (goto-char (point-max))
      (insert ?\f ?\n file ?,)
      (let ((size-pos (point)))
	(insert ?\n)
	; Now, we prepare the doc file
	(set-buffer buf-doc)
        (erase-buffer)
	(insert-file file)
	(goto-char (point-min))
        ; We are now ready to find entries
	(while (and (search-forward "\f" nil t)
		    (progn
		      (skip-chars-forward "\n")
		      (not (eobp))))
	  (let ((beg (point)))
	    (skip-chars-forward "^ \t")
	    (let ((fn (buffer-substring beg (point)))
		  (line (1+ (count-lines 1 beg))))
	      (set-buffer buf-tags)
	      (insert fn 127 (int-to-string line) ?, (int-to-string beg) ?\n)
	      (set-buffer buf-doc))))
	(set-buffer buf-tags)
	(let ((size (- (point) size-pos 1)))
	  (goto-char size-pos)
	  (insert (int-to-string size))))))
  (pop-to-buffer (get-buffer dtags-file-name) t))

