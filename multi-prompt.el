;;; multi-prompt.el --- completing read of multiple strings.

;; Copyright (C) 1996, 1997 Per Abrahamsen.

;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Keywords: extensions
;; Version: 0.2
;; Bogus-Bureaucratic-Cruft: How 'bout ESR and the LCD people agreed
;; 	on a common format?

;; LCD Archive Entry:
;; multi-prompt|Per Abrahamsen|abraham@dina.kvl.dk|
;; completing read of multiple strings|
;; 1996-08-31|0.1|~/functions/multi-prompt.el.Z|

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This package is written for use in emacs lisp programs, where the
;; user is prompted for a string of the form:
;;
;;   FOO,BAR,BAZ
;;
;; where FOO, BAR, and BAZ are elements of some table.  The function
;; `multi-prompt' is a replacement `completing-read' that will allow
;; the user to enter a string like the above, yet get completion on
;; both FOO, BAR, and BAZ.

;;; Change Log:
;;
;; Sat Feb 15 17:58:31 MET 1997
;;      * Version 0.2 released.
;;        Renamed predicate to `mp-predicate'.
;; Sat Aug 31 18:32:20 MET DST 1996
;;      * Version 0.1 released.
;;        Fixed `predicate' bug.  
;;        Added provide.
;;        Added `multi-prompt-found' variable.
;; Sat Aug 31 16:29:14 MET DST 1996
;;      * Created.

;;; Code:

(provide 'multi-prompt)

(defvar multi-prompt-found nil
  "List of entries currently added during a `multi-prompt'.")

(defun multi-prompt (separator
		     unique prompt table
		     &optional mp-predicate require-match initial history)
  "Completing prompt for a list of strings.  
The first argument SEPARATOR should be the string (of length 1) to
separate the elements in the list.  The second argument UNIQUE should
be non-nil, if each element must be unique.  The remaining elements
are the arguments to `completing-read'.  See that."
  (let ((old-map (if require-match
		     minibuffer-local-must-match-map
		   minibuffer-local-completion-map))
	(new-map (make-sparse-keymap)))
    (if (fboundp 'set-keymap-parent)
	;; `set-keymap-parent' was introduced in Emacs 19.32.
	(set-keymap-parent new-map old-map)
      (setq new-map (copy-keymap old-map)))
    (define-key new-map separator (if require-match
				      'multi-prompt-next-must-match
				    'multi-prompt-next))
    (define-key new-map "\C-?" 'multi-prompt-delete)
    (let* ((minibuffer-local-completion-map new-map)
	   (minibuffer-local-must-match-map new-map)
	   (multi-prompt-found nil)
	   (done nil)
	   (filter (cond (unique
			  (lambda (x)
			    (and (not (member (car x) multi-prompt-found))
				 (or (null mp-predicate)
				     (funcall mp-predicate x)))))
			 (mp-predicate)))
	   (answer (catch 'multi-prompt-exit
		     (while t
		       (let ((extra (catch 'multi-prompt-next
				      (throw 'multi-prompt-exit
					     (completing-read prompt 
							      table
							      filter
							      require-match
							      initial
							      history)))))
			 (cond ((eq extra 'back)
				(when multi-prompt-found
				  (setq prompt (substring 
						prompt 0 
						(- 0 (length separator)
						   (length
						    (car multi-prompt-found))))
					initial (car multi-prompt-found))
				  (setq multi-prompt-found 
					(cdr multi-prompt-found))))
			       (t
				(setq prompt (concat prompt extra separator)
				      initial nil)
				(setq multi-prompt-found
				      (cons extra multi-prompt-found)))))))))
      (if answer 
	  (nreverse (cons answer multi-prompt-found))
	multi-prompt-found))))

(defun multi-prompt-delete ()
  (interactive)
  (if (bobp)
      (throw 'multi-prompt-next 'back)
    (call-interactively 'backward-delete-char)))

(defun multi-prompt-next ()
  (interactive)
  (throw 'multi-prompt-next
	 (buffer-substring-no-properties (point-min) (point-max))))

(defun multi-prompt-next-must-match ()
  (interactive)
  (if (call-interactively 'minibuffer-complete)
      (throw 'multi-prompt-next
	     (buffer-substring-no-properties (point-min) (point-max)))))

;;; multi-prompt.el ends here
