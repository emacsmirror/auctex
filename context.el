;;; context.el --- Support for ConTeXt documents.
;; 
;; Maintainer: Patrick Gundlach <pg@levana.de>
;; Version: 11.14
;; Keywords: wp
;; X-URL: http://www.gnu.org/software/auctex/
;; Copyright 2003 Free Software Foundation

;; Last Change: 05/03/03 19:45:38

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


;;; Notes:

;; This is the very basic context support for AUCTeX. It will be
;; extended in the near future. 

;; AUCTeX is closely intervowen with LaTeX. We have to split up
;; things without breaking 'em. 

;;; Code:

;; some parts are stolen from latex.el and adapted to ConTeXt

(require 'tex)

(defgroup ConTeXt-macro nil
  "Special support for ConTeXt macros in AUCTeX."
  :prefix "TeX-"
  :group 'ConTeXt
  :group 'TeX-macro)


(defun TeX-ConTeXt-sentinel (process name)
  "Cleanup TeX output buffer after running ConTeXt."
  (cond ((TeX-TeX-sentinel-check process name))
	((save-excursion 
	   ;; in a full ConTeXt run there will multiple texutil
	   ;; outputs. Just looking for "another run needed" would
	   ;; find the first occurence
	   (goto-char (point-max))
	   (re-search-backward "TeXUtil " nil t)
	   (re-search-forward "another run needed" nil t))
	 (message (concat "You should run ConTeXt again "
			  "to get references right, "
                          (TeX-current-pages)))
	 (setq TeX-command-next TeX-command-default))
	((re-search-forward "removed files :" nil t)
	 (message "sucessfully cleaned up"))
	((re-search-forward "^ TeX\\(Exec\\|Util\\)" nil t) ;; strange regexp --pg
	 (message (concat name ": successfully formatted "
			  (TeX-current-pages)))
	 (setq TeX-command-next TeX-command-Show))
	(t
	 (message (concat name ": problems after "
			  (TeX-current-pages)))
	 (setq TeX-command-next TeX-command-default))))


;;; Environments
(setq ConTeXt-known-interfaces '("nl" "en" "de" "cz" "it" "ro"))

(defvar ConTeXt-default-interface "en"
  "Default interface to be used when running ConTeXt.")

(defvar ConTeXt-current-interface "en"
  "Interface to be used for inserting macros and ConTeXt run")
(make-variable-buffer-local 'ConTeXt-current-interface)

(defgroup ConTeXt-environment nil
  "Environments in AUCTeX."
  :group 'ConTeXt-macro)

;; todo: interface awareness
(defcustom ConTeXt-default-environment "itemize"
  "*The default environment when creating new ones with `ConTeXt-environment'."
  :group 'ConTeXt-environment
  :type 'string)
(make-variable-buffer-local 'ConTeXt-default-environment)

;; now we have ConTeXt-en-add-environment... ConTeXt-de-add-environment
(mapcar '(lambda (interface)
	   (TeX-auto-add-type "environment" (concat "ConTeXt-" interface)))
	ConTeXt-known-interfaces)

(defmacro ConTeXt-environment-list ()
  "Calls ConTeXt-XX-environment-list where XX is the current interface."
  (quote (funcall (intern (concat "ConTeXt-" 
				  ConTeXt-current-interface
				  "-environment-list")))))

;; is it safe to return nil? --pg
(defmacro ConTeXt-add-environments (&rest args)
  "Calls ConTeXt-XX-add-environments where XX is the current interface."
  `(apply (intern (concat "ConTeXt-" 
			 ConTeXt-current-interface
			 "-add-environments"))
	 (mapcar 'eval args))
  nil)


(defvar ConTeXt-environment-history nil)

(defun ConTeXt-environment-start-name ()
  "Return the \\start translated to the language in current interface."
  ;; it is "inizia", others are "start"
  (cond ((equal ConTeXt-current-interface "it")
	 "inizia")
	((member ConTeXt-current-interface ConTeXt-known-interfaces)
	 "start")
	(t
	 ;; this should not happen
	 (error "unknown interface: " ConTeXt-current-interface))))

(defun ConTeXt-environment-stop-name ()
  "Return the \\stop translated to the language in current interface."
  ;; it is "termina", others are "stop"
  (cond ((equal ConTeXt-current-interface "it")
	 "termina")
	((member ConTeXt-current-interface ConTeXt-known-interfaces)
	 "stop")
	(t
	 ;; this should not happen
	 (error "unknown interface: " ConTeXt-current-interface))))
	

(defun ConTeXt-environment (arg)
  "Make ConTeXt environment (\\start...-\\stop... pair).
With optional ARG, modify current environment."
  (interactive "*P")
  (let ((environment (completing-read (concat "Environment type: (default "
					      (if (TeX-near-bobp)
						  "text"
						ConTeXt-default-environment)
					      ") ")
				      (ConTeXt-environment-list)
				      nil nil nil
				      'ConTeXt-environment-history)))
    ;; Get default
    (cond ((and (zerop (length environment))
                (TeX-near-bobp))
           (setq environment "text"))
          ((zerop (length environment))
           (setq environment ConTeXt-default-environment))
          (t
           (setq ConTeXt-default-environment environment)))
 
    (let ((entry (assoc environment (ConTeXt-environment-list))))
      (when (null entry)
	(ConTeXt-add-environments (list environment)))
      (if arg
	  (ConTeXt-modify-environment environment)
	(ConTeXt-environment-menu environment)))))

;; taken from latex.el
(defun ConTeXt-modify-environment (environment)
  "Modify current environment."
  (save-excursion
    (ConTeXt-find-matching-stop)
    (re-search-backward (concat (regexp-quote TeX-esc)
				(ConTeXt-environment-stop-name)
				" *\\([a-zA-Z]*\\)")
			(save-excursion (beginning-of-line 1) (point)))
    (replace-match 
     (concat TeX-esc (ConTeXt-environment-stop-name) environment) t t)
    (beginning-of-line 1)
    (ConTeXt-find-matching-start)
    (re-search-forward (concat (regexp-quote TeX-esc)
			       (ConTeXt-environment-start-name)
			       " *\\([a-zA-Z]*\\)")
		       (save-excursion (end-of-line 1) (point)))
    (replace-match 
     (concat TeX-esc (ConTeXt-environment-start-name) environment) t t)))


(defun ConTeXt-environment-menu (environment)
  "Insert ENVIRONMENT around point or region."
  (let ((entry (assoc environment (ConTeXt-environment-list))))
    (cond ((not (and entry (nth 1 entry)))
	   (ConTeXt-insert-environment environment))
	  ((numberp (nth 1 entry))
	   (let ((count (nth 1 entry))
		 (args ""))
	     (while (> count 0)
	       (setq args (concat args TeX-grop TeX-grcl))
	       (setq count (- count 1)))
	     (ConTeXt-insert-environment environment args)))
	  ((stringp (nth 1 entry))
	   (let ((prompts (cdr entry))
		 (args ""))
	     (while prompts
	       (setq args (concat args
				  TeX-grop
				  (read-from-minibuffer
				   (concat (car prompts) ": "))
				  TeX-grcl))
	       (setq prompts (cdr prompts)))
	     (ConTeXt-insert-environment environment args)))
	  (t
	   (apply (nth 1 entry) environment (nthcdr 2 entry))))))

(defun ConTeXt-close-environment ()
  "Insert \\stop... to match the current environment."
  (interactive "*")
  (if (> (point)
	 (save-excursion
	   (beginning-of-line)
	   (skip-chars-forward " \t")
	   (point)))
      (insert "\n"))
  (insert  TeX-esc (ConTeXt-environment-stop-name)
	   (ConTeXt-current-environment))
  (indent-according-to-mode)
  (if (not (looking-at "[ \t]*$"))
      (insert "\n")
    (let ((next-line-add-newlines t))
      (next-line 1)
      (beginning-of-line)))
  (indent-according-to-mode))

;; ??? this looks very LaTeXish --pg
(defun ConTeXt-insert-environment (environment &optional extra)
  "Insert ENVIRONMENT of type ENV, with optional argument EXTRA."
  (if (and (TeX-active-mark)
	   (not (eq (mark) (point))))
      (progn
	(if (< (mark) (point))
	    (exchange-point-and-mark))
	(or (TeX-looking-at-backward "^[ \t]*")
	    (newline))
	(insert TeX-esc (ConTeXt-environment-start-name) environment)
	(indent-according-to-mode)
	(if extra (insert extra))
	(newline)
	(goto-char (mark))
	(or (TeX-looking-at-backward "^[ \t]*")
	    (newline))
	(insert TeX-esc (ConTeXt-environment-start-name) environment)
	(or (looking-at "[ \t]*$")
	    (save-excursion (newline-and-indent)))
	(indent-according-to-mode)
	(end-of-line 0)
	;; (or (assoc environment ConTeXt-indent-environment-list)
	;; 	    (ConTeXt-fill-environment nil))
	)
    (or (TeX-looking-at-backward "^[ \t]*")
	(newline))
    (insert TeX-esc (ConTeXt-environment-start-name) environment)
    (indent-according-to-mode)
    (if extra (insert extra))
    (newline-and-indent)
    (newline)
    (insert TeX-esc (ConTeXt-environment-stop-name) environment)
    (or (looking-at "[ \t]*$")
	(save-excursion (newline-and-indent)))
    (indent-according-to-mode)
    (end-of-line 0)))


;; with the following we can call a function on an environment. Say
;; you have metapost stuff within your TeX file, go to the environment
;; and run ConTeXt-work-on-environment (suggested Key: C-c !). AUCTeX
;; sees that you are inside e.g. \startMPpage....\stopMPpage and
;; looks in ConTeXt-environment-helper for a function to be called. 

;; % so pressing C-c ! inside the following ...
;;\startuseMPgraphic{Logo}{Scale}
;; % Top rectangle
;; filldraw (0,0)--(2cm,0)--(2cm,1cm)--(0,1cm)--cycle withcolor blue ;
;; % Bottom black rectangle
;; drawfill (0,0)--(2cm,0)--(2cm,-1cm)--(0,-1cm)--cycle withcolor black;
;; % White Text
;; draw btex \bf AB etex withcolor white ;
;; % resize to size
;; currentpicture := currentpicture scaled \MPvar{Scale} ;
;; \stopuseMPgraphic

;; % ...should give you a "new buffer" (currently narrowed to region
;; % and switched to metapost-mode and recursive-edit)

;; % Top rectangle
;; filldraw (0,0)--(2cm,0)--(2cm,1cm)--(0,1cm)--cycle withcolor blue ;
;; % Bottom black rectangle
;; drawfill (0,0)--(2cm,0)--(2cm,-1cm)--(0,-1cm)--cycle withcolor black;
;; % White Text
;; draw btex \bf AB etex withcolor white ;
;; % resize to size
;; currentpicture := currentpicture scaled \MPvar{Scale} ;


(defvar ConTeXt-environment-helper 
  '(("useMPgraphic" . ConTeXt-mp-region)
    ("MPpage" . ConTeXt-mp-region))
  "alist that holds functions to call for working on regions. An entry
  looks like: (\"environment\" . function)")

(defun ConTeXt-mp-region ()
  "Edit region in metapost-mode."
  (ConTeXt-mark-environment t)
  (narrow-to-region (mark) (point))
  (metapost-mode)
  (message "Type `M-x exit-recursive-edit' to get back")
  (recursive-edit)
  (context-mode)
  (widen))

;; find smarter name. Suggestions welcome
(defun ConTeXt-work-on-environment ()
  "Takes current environment and does something on it (todo: documentation)"
  (interactive)
  (let ((fun (cdr (assoc (ConTeXt-current-environment)
			 ConTeXt-environment-helper))))
    (when (functionp fun) 
      (funcall fun))))


;; duplicate?!?
;; (defun ConTeXt-current-environment (&optional arg)
;;   "Return the name (a string) of the enclosing ConTeXt environment.
;; With optional ARG>=1, find that outer level."
;;   (setq arg (if arg (if (< arg 1) 1 arg) 1))
;;   (save-excursion
;;     (while (and
;; 	    (/= arg 0)
;; 	    (re-search-backward
;; 	     (concat (regexp-quote TeX-esc) (ConTeXt-environment-start-name)
;; 		     "\\|"
;; 		     (regexp-quote TeX-esc) (ConTeXt-environment-stop-name))
;; 	     nil t 1))
;;       (cond ((TeX-in-comment)
;; 	     (beginning-of-line 1))
;; 	    ((looking-at (concat (regexp-quote TeX-esc) 
;; 				 (ConTeXt-environment-stop-name)))
;; 	     (setq arg (1+ arg)))
;; 	    (t
;; 	     (setq arg (1- arg)))))
;;     (if (/= arg 0)
;; 	"text" ; todo: translate
;;       (search-forward (concat TeX-esc (ConTeXt-environment-start-name)))  
;;       (let ((beg (point)))
;; 	;; we can have \startitemize[n][stopper=)]
;; 	(re-search-forward "[^\\a-zA-Z]")
;; 	(backward-char 1)
;; 	(buffer-substring beg (point))))))

(defun ConTeXt-current-environment ()
  "Return the name of the current environment."
  ;; don't make this interactive.
  (let ((beg)
	(end))
    
    (save-excursion 
      (re-search-backward (concat (regexp-quote TeX-esc)
				  (ConTeXt-environment-start-name)))
      (goto-char (match-end 0))
      (setq beg (point))
      (skip-chars-forward "a-zA-Z")
      (buffer-substring beg (point)))))

(defun ConTeXt-mark-environment (&optional inner)
  "Set mark to end of current environment (\\start...-\\stop...) and
  point to the matching begin. If optional INNER is not 
nil, include \\start... and \\stop, otherwise only the
contents."
  (interactive)
  (let ((cur (point)))
    (ConTeXt-find-matching-stop inner)
    (set-mark (point))
    (goto-char cur)
    (ConTeXt-find-matching-start inner)
    (TeX-activate-region)))

(defun ConTeXt-find-matching-stop (&optional inner)
  "Find end of current \\start...\\stop-Pair. If INNER is non
nil, go to the point just past before \\stop... macro. Otherwise goto
the point just past \\stop..."
  (interactive)
  (let ((regexp (concat (regexp-quote TeX-esc)
			"\\("
			(ConTeXt-environment-start-name)
			"\\|"
			(ConTeXt-environment-stop-name)
			"\\)"
			))
	(level 1)
	(pos))
    ;;jump over the \start... when at the beginning of it.
    (when (looking-at (concat (regexp-quote TeX-esc)
			      (ConTeXt-environment-start-name)))
      (re-search-forward regexp nil t))
    (while (and (> level 0) 
		(re-search-forward regexp nil t)
		(goto-char (1- (match-beginning 1)))
		(cond ((looking-at (concat (regexp-quote TeX-esc)
					   (ConTeXt-environment-start-name)))
		       (re-search-forward regexp nil t)
		       (setq level (1+ level)))
		      ((looking-at (concat (regexp-quote TeX-esc)
					   (ConTeXt-environment-stop-name)))
		       (re-search-forward regexp nil t)
		       (setq level (1- level))))))
    ;; now we have to look if we want to start behind the \start... macro
    (if inner
	(beginning-of-line)
      (skip-chars-forward "a-zA-Z"))))
  
(defun ConTeXt-find-matching-start (&optional inner)
  "Find beginning of current \\start...\\stop-Pair. If INNER is non
nil, go to the point just past the \\start... macro."
  (interactive)
  (let ((regexp (concat (regexp-quote TeX-esc)
			"\\("
			(ConTeXt-environment-start-name)
			"\\|"
			(ConTeXt-environment-stop-name)
			"\\)"
			))
	(level 1)
	(pos))
    (while (and (> level 0) 
		(re-search-backward regexp nil t)
		(cond ((looking-at (concat (regexp-quote TeX-esc)
					   (ConTeXt-environment-stop-name)))
		       (setq level (1+ level)))
		      ((looking-at (concat (regexp-quote TeX-esc)
					   (ConTeXt-environment-start-name)))
		       (setq level (1- level))))))
    ;; now we have to look if we want to start behind the \start... macro
    (when inner
      ;; \startfoo can have 0 or more {} and [] pairs. I assume that
      ;; skipping all those parens will be smart enough. It fails when
      ;; the first part in the \start-\stop-environment is { or [, like
      ;; in \startquotation   {\em important} \stopquotation. There is
      ;; yet another pitfall: \startsetups SomeSetup foo bar
      ;; \stopsetups will use SomeSetup as the argument and the
      ;; environment 
      (skip-chars-forward "\\\\a-zA-Z")
      (save-excursion
	(while (progn 
		 (skip-chars-forward "\t\n ")
		 (forward-comment 1)
		 (skip-chars-forward "\t\n ")
		 (looking-at "\\s\("))
	  (forward-list 1)
	  (setq pos (point))))
      (when pos
	(goto-char pos))
      (unless (bolp)
	(forward-line)))))

;;; items

(defun ConTeXt-insert-item ()
  "Insert a new item. "
  (interactive "*")
  (newline)
  (TeX-insert-macro "item")
  (indent-according-to-mode))


(defvar ConTeXt-mode-map
  (let ((map (copy-keymap TeX-mode-map)))
    
    (define-key map "\e\C-a"  'ConTeXt-find-matching-start)
    (define-key map "\e\C-e"  'ConTeXt-find-matching-stop)
;; likely to change in the future    
    (define-key map "\C-c!"    'ConTeXt-work-on-environment)
    (define-key map "\C-c\C-e" 'ConTeXt-environment)
    (define-key map "\C-c\n"   'ConTeXt-insert-item)
    (or (key-binding "\e\r")
	(define-key map "\e\r"    'ConTeXt-insert-item)) ;*** Alias
    (define-key map "\C-c]" 'ConTeXt-close-environment)
    map)
  "Keymap used in `ConTeXt-mode'.")


;;; Menu
(easy-menu-define ConTeXt-mode-command-menu
    ConTeXt-mode-map
    "Command menu used in ConTeXt mode."
    (list TeX-command-menu-name
          :filter (lambda (&rest ignored)
                    (TeX-mode-specific-command-menu 'context-mode))
          "Bug."))


;;; Mode
(defun context-mode ()
  "Major mode for editing files of input for ConTeXt.

Special commands:
\\{ConTeXt-mode-map}
 
Entering context-mode calls the value of text-mode-hook,
then the value of TeX-mode-hook, and then the value
of context-mode-hook."
  (interactive)
  (plain-TeX-common-initialization)
  (use-local-map ConTeXt-mode-map)

  ;; Menu
  (easy-menu-add ConTeXt-mode-command-menu ConTeXt-mode-map)

  ;; initializations
  (ConTeXt-en-add-environments
   "itemize" "text" "typing" "linenumbering"
   ;; some metapost environments
   "MPpositiongraphic" "useMPgraphic" "MPcode" "reusableMPgraphic"
   "uniqueMPgraphic"
   "buffer" "narrower" "tabulate")

  ;; todo: make interface aware! 
  (TeX-add-symbols
   '("item" (TeX-arg-literal "  ")))
  (setq mode-name "ConTeXt") 
  (setq major-mode 'context-mode) 
  (setq TeX-command-default "ConTeXt")
  (setq TeX-sentinel-default-function 'TeX-ConTeXt-sentinel)
  (run-hooks 'text-mode-hook 'TeX-mode-hook 'ConTeXt-mode-hook))

;;; context.el ends here