;; texmathp.el -- Code to check if point is inside LaTeX math environment
;; Copyright (c) 1998 Carsten Dominik

;; $Id: texmathp.el,v 5.1 1998-06-10 09:25:40 abraham Exp $

;; Author: Carsten Dominik <dominik@strw.LeidenUniv.nl>
;; Keywords: tex
;;
;; This file is not part of GNU Emacs
;;
;; COPYRIGHT NOTICE
;;
;; This program is free software you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If you did not, write to the Free Software Foundation,
;; Inc., 675 Mass Ave., Cambridge, MA 02139, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This code provides a function to determine if point in a buffer is
;;  inside a (La)TeX math environment.  This is not trivial since many
;;  different ways are used to switch between the two.  Examples:
;;
;;    \begin{equation}  ... \end{equation}
;;    $ ... $
;;    $$ ... $$
;;    \[ ... \]
;;    \ensuremath{...}
;;    \mbox{...}
;;
;;  To install, put this file on your load-path and compile it.
;;
;;  To use this in your lisp program, do
;;
;;     (require 'texmathp)
;;
;;  You can then write code like this:
;;    
;;     (if (texmathp) ...)
;;
;;  The call to `texmathp' leaves some extra information in the
;;  variable `texmathp-why'.  It's value is a cons cell (MATCH . POSITION),
;;  specifying which command at what position is responsible for math
;;  mode being on or off.
;;
;;  To configure which macros and environments influence LaTeX math mode,
;;  customize the variable `texmathp-tex-commands'.  By default
;;  it recognizes the LaTeX core as well as AMS-LaTeX.
;;
;;  To try out the code interactively, use `M-x texmathp RET'.
;;
;;  Of course, in order to work this function assumes that the
;;  LaTeX above point is syntactically correct.  In particular:
;;
;;  o The different math delimiters are paired correctly.  Thus if
;;    you do things like "\begin{equation} $"  or "\[ ... \)"
;;    the result of (texmathp) is undefined.  It is in fact possible
;;    in LaTeX to pair \[ with $$ and \( with $, but this will confuse
;;    texmathp (and human readers as well).
;;
;;  o However, texmathp will correctly work with nested delimiters,
;;    e.g. something like this will be parsed correctly at any point:
;;
;;       \begin{equation}
;;          x = y \mbox{abc \ensuremath{\alpha} cba $2^3$}
;;       \end{equation}
;;
;;  o texmathp is somewhat forgiving if you have an empty line inside
;;    the current math environment, which is not legal in TeX but may
;;    easily happen during editing.  Depending upon the variable
;;    `texmathp-search-n-paragraphs' we check several paragraphs,
;;    backwards, by default 2.  Paragraph here means something limited
;;    by an emty line.

;;; Code:

(provide 'texmathp)

;; Safety-check for the old customize package.
(eval-and-compile
  (condition-case () (require 'custom) (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
      nil
    (defmacro defgroup (&rest args) nil)
    (defmacro defcustom (var value doc &rest args) 
      (` (defvar (, var) (, value) (, doc))))))

(defgroup texmathp nil
  "Testing TeX and LaTeX documents for math mode."
  :tag "Test For TeX and LaTeX Math Mode"
  :prefix "texmathp-"
  :group 'tex)

(defvar texmathp-tex-commands)  ;; Just to silence the compiler...
(defvar texmathp-environments nil)
(defvar texmathp-macros nil)
(defvar texmathp-onoff-regexp nil)
(defvar texmathp-toggle-regexp nil)

(defun texmathp-compile (&optional symbol value)
  "Compile the value of `texmathp-tex-commands' into the internal lists.
Call this when you have changed the value of that variable without using
customize (customize calls it when setting the variable)."

  (interactive)

  ;; If this is called with args, customize wants us to set the variable.
  (and symbol (set-default symbol value))

  ;; Extract lists and regexp.
  (setq texmathp-macros nil texmathp-environments nil)
  (put 'texmathp-tex-commands 'texmathp-compiled t)
  (let ((list (reverse texmathp-tex-commands))
	var entry type switches togglers)
    (while (setq entry (car list))
      (setq type (nth 1 entry)
	    list (cdr list)
	    var (cond ((memq type '(env-on env-off)) 'texmathp-environments)
		      ((memq type '(arg-on arg-off)) 'texmathp-macros)
		      ((memq type '(sw-on sw-off))   'switches)
		      ((memq type '(sw-toggle))      'togglers)))
      (set var (cons (car entry) (symbol-value var))))
    (setq texmathp-onoff-regexp
	  (concat "[^\\\\]\\(" 
		  (mapconcat 'regexp-quote switches "\\|")
		  "\\)")
	  texmathp-toggle-regexp
	  (concat "[^\\\\\\$]\\("
		  (mapconcat 'regexp-quote togglers "\\|")
		  "\\)"))))

(defcustom texmathp-tex-commands
  '(;; Standard LaTeX
    ("equation"      env-on)      ("equation*"     env-on)
    ("eqnarray"      env-on)      ("eqnarray*"     env-on)
    ("displaymath"   env-on)
    ("\\mbox"        arg-off)
    ("\\("           sw-on)       ("\\)"           sw-off)
    ("\\["           sw-on)       ("\\]"           sw-off)
    ("$$"            sw-toggle)   ("$"             sw-toggle)
    ;; AMS-LaTeX
    ("align"         env-on)      ("align*"        env-on)
    ("gather"        env-on)      ("gather*"       env-on)
    ("multline"      env-on)      ("multline*"     env-on)
    ("flalign"       env-on)      ("flalign*"      env-on)
    ("alignat"       env-on)      ("alignat*"      env-on)
    ("xalignat"      env-on)      ("xalignat*"     env-on)
    ("xxalignat"     env-on)      ("xxalignat*"    env-on)
    ("\\ensuremath"  arg-on)
    ("\\text"        arg-off)     ("\\intertext"   arg-off))
  "List of environments and macros influencing (La)TeX math mode.

The structure of each entry is (NAME TYPE)

- The first item in each entry is the name of an environment or macro.
  If it's a macro, include the backslash.

- The second item is a symbol indicating how the command works: 
    `env-on'     Environment, turns math mode for its body  on
    `env-off'    Environment: turns math mode for its body  off
    `arg-on'     Command: turns math mode for its arguments on
    `arg-off'    Command: turns math mode for its arguments off
    `sw-on'      Switch: turns math-mode of following text  on
    `sw-off'     Switch: turns math-mode of following text  off
    `sw-toggle'  Switch: toggles math mode of following text"
  :group 'texmathp
  :set 'texmathp-compile
  :type
  '(repeat
    (list :value ("" env-on)
     (string  :tag "Name")
     (choice  :tag "Type"
      (const :tag "Environment: turns math mode for its body on" env-on)
      (const :tag "Environment: turns math mode for its body off" env-off)
      (const :tag "Command: turns math mode for its argument on" arg-on)
      (const :tag "Command: turns math-mode for its argument off" arg-off)
      (const :tag "Switch: turns math-mode of following text on" sw-on)
      (const :tag "Switch: turns math-mode of following text off" sw-off)
      (const :tag "Switch: toggles math mode of following text" sw-toggle)))))

(defcustom texmathp-search-n-paragraphs 2
  "*Number of paragraphs to check before point.
Normally, you cannot have an empty line in a math environment in (La)TeX.
Therefore, the fastest method to test for math mode is limiting the
search backward to the nearest empty line.
However, during editing it happens that such lines exist temporarily.
Therefore we look a little further.  This variable determines how many 
empty lines we go back to fix the search limit."
  :group 'texmathp
  :type 'number)

(defvar texmathp-why nil
  "After a call to `texmathp' this variable shows why math-mode is on or off.
The value is a cons cell (MATCH . POSITION).  
MATCH is a string like a car of an entry in `texmathp-tex-commands', e.q.
\"equation\" or \"\\ensuremath\" or \"\\[\" or \"$\".
POSITION is the buffer position of the match.  If there was no match,
it points to the limit used for searches, usually two paragraphs up.")

(defun texmathp ()
  "Determine if point is inside (La)TeX math mode. 
Returns t or nil.  Additional info is placed into `texmathp-why'.
The functions assumes that you have (almost) syntactically correct (La)TeX in
the buffer.
See the variable `texmathp-tex-commands' about which commands are checked."
  (interactive)
  (let* ((pos (point)) math-on $-match sw-match
	 (bound (save-excursion
		  (re-search-backward "[\n\t][ \t]*[\n\r]"
				      nil 1 texmathp-search-n-paragraphs)
		  (match-beginning 0)))
	 (env-match (texmathp-match-environment bound))
	 (mac-match (texmathp-match-macro bound))
	 (match (cons nil bound)))

    ;; Select the nearer match
    (and env-match (setq match env-match))
    (and mac-match (> (cdr mac-match) (cdr match)) (setq match mac-match))
    (setq math-on (memq (nth 1 (assoc (car match) texmathp-tex-commands))
			'(env-on arg-on)))

    ;; Check for switches
    (and (not math-on)
	 (setq sw-match (texmathp-match-switch bound))
	 (> (cdr sw-match) (cdr match))
	 (eq (nth 1 (assoc (car sw-match) texmathp-tex-commands)) 'sw-on)
	 (setq match sw-match math-on t))

    ;; Check for togglers
    (if (not math-on)
	;; There still might be dollars and other switches - check for these.
	(save-excursion
	  (goto-char (cdr match))
	  (while (re-search-forward texmathp-toggle-regexp pos t)
	    (if (setq math-on (not math-on))
		(setq $-match (cons (match-string 1) (match-beginning 1)))
	      (setq $-match nil)))
	  (and math-on $-match (setq match $-match))))

    ;; Store info, show as message when interactive, and return
    (setq texmathp-why match)
    (and (interactive-p)
	 (message "math-mode is %s: %s begins at buffer position %d"
		  (if math-on "on" "off")
		  (or (car match) "new paragraph") (cdr match)))
    math-on))

(defun texmathp-match-macro (bound)
  ;; Find out if point is within the arguments of any of the Math macros.
  ;; Limit searches to BOUND.  The return value is like ("\\macro" . (point)).
  (catch 'exit
    (and (null texmathp-macros) (throw 'exit nil))
    (let (pos cmd)
      (save-restriction
	(save-excursion
	  (narrow-to-region (max 1 bound) (point))
	  ;; Move back out of the current parenthesis
	  (while (condition-case nil (progn (up-list -1) t) (error nil))
	    ;; Move back over any touching sexps (actually also non-touching)
	    (while 
	      (and (cond
		    ((memq (preceding-char) '(?\] ?\})))
		    ((re-search-backward 
		      "[]}][ \t]*[\n\r]?\\([ \t]*%[^\n\r]*[\n\r]\\)*[ \t]*\\="
		      bound t)
		     (goto-char (1+ (match-beginning 0))) t))
		   (condition-case nil (progn (backward-sexp) t) (error nil))))
	    (setq pos (point))
	    (and (memq (following-char) '(?\[ ?\{))
		 (re-search-backward "\\\\[*a-zA-Z]+\\=" nil t)
		 (setq cmd (buffer-substring-no-properties
			    (match-beginning 0) (match-end 0)))
		 (member cmd texmathp-macros)
		 (throw 'exit (cons cmd (point))))
	    (goto-char pos))
	  nil)))))

(defun texmathp-match-environment (bound)
  ;; Find out if point is inside any of the math environments.
  ;; Limit searched to BOUND.  The return value is like ("equation" . (point)).
  (catch 'exit
    (save-excursion
      (and (null texmathp-environments) (throw 'exit nil))
      (let (end-list env)
	(while (re-search-backward "\\\\\\(begin\\|end\\){\\([^}]+\\)}"
				   bound t)
	  (setq env (buffer-substring-no-properties
		     (match-beginning 2) (match-end 2)))
	  (cond ((string= (match-string 1) "end")
		 (add-to-list 'end-list env))
		((equal env (car end-list))
		 (setq end-list (cdr end-list)))
		((member env texmathp-environments)
		 (throw 'exit (cons env (point))))))
	nil))))

(defun texmathp-match-switch (bound)
  ;; Search backward for any of the math switches.
  ;; Limit searched to BOUND.  The return value is like ("\\(" . (point)).
  (save-excursion
    (if (re-search-backward texmathp-onoff-regexp bound t)
	(cons (buffer-substring-no-properties (match-beginning 1) (match-end 1))
	      (match-beginning 1))
      nil)))

;; Compile the configuration variable if not already done.
(or (get 'texmathp-tex-commands 'texmathp-compiled) (texmathp-compile))

;;; texmathp.el end here
