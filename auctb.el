;;; auctb.el --- toolbar icons on AUC-TeX in GNU emacs and XEmacs

;; Copyright (C) 2004 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;; Author: Miguel V. S. Frasson <frasson@math.leidenuniv.nl>
;; Keywords: tool-bar

;;; Use of this preliminary version

;; Requirements for the images: 1) you should have a folder called
;; "symb-pics" with the pics of the symbols (xpm format is a good
;; one), and the *parent* of this folder should be in `load-path'; 2)
;; each image file is named after the command that it represents in
;; the following rules: the base name is the name of the command
;; without the escape character "\", like \delta -> "delta.xpm";
;; however, since in some OS filenames are case insensitive, all
;; occurences of capital letter should be replaced by the letter plus
;; a dash: \Rightarrow -> "R-ightarrow.xpm" (just apply
;; `auctb-img-filename' to "Rightarrow".)

;; This package also needs `toolbarx.el' and `latex.el'

;;; Code

(eval-when-compile
  (require 'toolbarx)
  (require 'latex))


;;; Symbol toolbar
(defun auctb-img-filename (str)
  (let ((str-list (append str nil))
	(str-result))
    (dolist (i str-list)
      (cond 
       ;; capital letter -> letter + "-"
       ((and (>= i ?A) (<= i ?Z))
	(setq str-result (cons ?- (cons i str-result))))
       ;; lowercase letter -> letter
       ((and (>= i ?a) (<= i ?z))
        (setq str-result (cons i str-result)))
       ;; open curly brackets `{' -> "ocb--"
       ((eq i ?{)
	(setq str-result (cons ?o str-result))
	(setq str-result (cons ?c str-result))
	(setq str-result (cons ?b str-result))
	(setq str-result (cons ?- str-result))
	(setq str-result (cons ?- str-result)))
       ;; close curly brackets `}' -> "ccb--"
       ((eq i ?})
	(setq str-result (cons ?c str-result))
	(setq str-result (cons ?c str-result))
	(setq str-result (cons ?b str-result))
	(setq str-result (cons ?- str-result))
	(setq str-result (cons ?- str-result)))
       ;; vertical bar `|' -> "v--"
       ((eq i ?|)
	(setq str-result (cons ?v str-result))
	(setq str-result (cons ?- str-result))
	(setq str-result (cons ?- str-result)))
       ;; slash `/' -> "s--"
       ((eq i ?/)
	(setq str-result (cons ?s str-result))
	(setq str-result (cons ?- str-result))
	(setq str-result (cons ?- str-result)))))
    (concat (nreverse str-result))))

(let* ((menu-strings-buttons-alist
	;; make a alist os strings with the symbol classes and store it in
	;; `menu-strings-alist'
	(let* ((menu-strings-alist-temp))
	  (dolist (item-external (cdr LaTeX-math-menu)
				 (nreverse menu-strings-alist-temp))
	    (when (listp item-external)
	      ;; if first element is vector, I am supposing that all are
	      ;; vectors as well
	      (if (vectorp (cadr item-external))
		  (let* ((menu-str (car item-external))
			 (menu-buttons))
		    (dolist (button (cdr item-external))
		      (setq menu-buttons
			    (cons (list (intern (auctb-img-filename
						 (aref button 0)))
					:image
					(concat "symb-pics/"
						(auctb-img-filename
						 (aref button 0)))
					:help (aref button 0)
					:command (aref button 1))
				  menu-buttons)))
		    (setq menu-buttons (nreverse menu-buttons))
		    (setq menu-strings-alist-temp
			  (cons (cons menu-str (list menu-buttons))
				menu-strings-alist-temp)))
		;; if another list (therefore, up to second level menu)
		(let ((parent-str (concat (car item-external) " ")))
		  (dolist (item-internal (cdr item-external))
		    (unless (equal (car item-internal) "Special")
		      (let* ((menu-str (concat parent-str
					       (car item-internal)))
			     (menu-buttons))
			(dolist (button (cdr item-internal))
			  (setq menu-buttons
				(cons (list (intern (aref button 0))
					    :image
					    (concat "symb-pics/"
						    (auctb-img-filename
						     (aref button 0)))
					    :help (aref button 0)
					    :command (aref button 1))
				      menu-buttons)))
			(setq menu-buttons (nreverse menu-buttons))
			(setq menu-strings-alist-temp
			      (cons (cons menu-str (list menu-buttons))
				    menu-strings-alist-temp)))))))))))
       (list-strings (let* ((list-str-temp))
		       (dolist (i menu-strings-buttons-alist
				  (nreverse list-str-temp))
			 (setq list-str-temp (cons (car i)
						   list-str-temp))))))
  (defvar LaTeX-symbols-toolbar-visible-flag nil
    "Non-nil means that the LaTeX symbols on toolbar are visible.")
  (defconst latex-symbols-toolbar-switch-contents
    `(;; the on-off switch button
      (latex-symbols-switch 
       :image (lambda nil (if LaTeX-symbols-toolbar-visible-flag
			      "ltx-symb-turn-off"
			    "ltx-symb-turn-on"))
       :command (progn
		  (setq LaTeX-symbols-toolbar-visible-flag
			(not LaTeX-symbols-toolbar-visible-flag))
		  (toolbarx-refresh))
       ;; help message depends on if symb-toolbar is on or off, and in
       ;; the name of the current class of symbols
       :help (lambda nil
	       (concat "Turn "
		       (if LaTeX-symbols-toolbar-visible-flag
			   "off "
			 "on ")
		       "the toolbar of LaTeX symbols (current class: "
		       (nth (1- LaTeX-symbols-active-menuitem)
			    (quote ,list-strings))
		       ")")))
      ;; the dropdown button, that also switch on the symbols
      ,(append '(:dropdown-group)
	       list-strings
	       '(:variable
		 LaTeX-symbols-active-menuitem
		 :save offer
		 :dropdown-prepend-command
		 (setq LaTeX-symbols-toolbar-visible-flag t)
		 :dropdown-help "Select a class of symbols to be displayed"))))
  (defconst latex-symbols-toolbar-contents
    (let* ((ltx-symb)
	   (count 0))
      (dolist (i menu-strings-buttons-alist 
		 (append (nreverse ltx-symb) 
			 '(:insert
			   LaTeX-symbols-toolbar-visible-flag
			   :toolbar (bottom . top))))
	(setq count (1+ count))
	(setq ltx-symb
	      (cons (append (cdr i)
			    `(:insert (eq LaTeX-symbols-active-menuitem
					  ,count)))
		    ltx-symb))))))

;;; instalation of toolbar (still primitive)
(defun install-auctex-toolbar ()
  (interactive)
  (toolbarx-install-toolbar '(:eval-group latex-symbols-toolbar-switch-contents
					  latex-symbols-toolbar-contents)))

(provide 'auctb)

;;; auctb.el ends here.
