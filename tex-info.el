;;; @ tex-info.el - Support for editing TeXinfo source.
;;;
;;; $Id: tex-info.el,v 5.1 1993-08-17 16:54:16 amanda Exp $

(provide 'tex-info)
(require 'tex-misc)
(require 'texinfo)
(require 'easymenu)

;;; @@ Copyright
;;;
;;; Copyright (C) 1993 Per Abrahamsen 
;;; 
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; @@ Environments

(defvar TeXinfo-environment-list
  '(("cartouche")
    ("defcv")
    ("deffn") ("defivar") ("defmac")
    ("defmethod") ("defop") ("defopt") ("defspec") ("deftp")
    ("deftypefn") ("deftypefun") ("deftypevar") ("deftypevr")
    ("defun") ("defvar") ("defvr") ("description") ("display")
    ("enumerate") ("example") ("ifset") ("ifclear") ("flushleft")
    ("flushright") ("format") ("ftable") ("iftex") ("itemize")
    ("lisp") ("quotation") ("smallexample") ("smalllisp") ("table")
    ("tex") ("titlepage") ("vtable")) 
  "Alist of TeXinfo environments.")

(defconst texinfo-environment-regexp
  ;; Overwrite version from `texinfo.el'.
  (concat "^@\\("
	  (mapconcat 'car TeXinfo-environment-list "\\|")
	  "\\|end\\)")
  "Regexp for environment-like TexInfo list commands.
Subexpression 1 is what goes into the corresponding `@end' statement.")

(defun TeXinfo-insert-environment (env)
  "Insert TeXinfo environment ENV.
When called interactively, prompt for an environment."
  (interactive (list (completing-read "Environment: "
				      TeXinfo-environment-list)))
  (insert "@" env "\n\n@end " env "\n")
  (if (null (cdr-safe (assoc "defcv" TeXinfo-environment-list)))
      (forward-line -2)
    ;; apply arguments
    ))

;;; @@ Keymap

(defvar TeXinfo-mode-map nil
  "Keymap for TeXinfo mode.")

(if TeXinfo-mode-map
    ()
  (setq TeXinfo-mode-map (make-sparse-keymap))

  (easy-iflemacs (define-key TeXinfo-mode-map 'button3 'LaTeX-mode-menu))

  ;; From texinfo.el

  ;; bindings for updating nodes and menus
  (define-key TeXinfo-mode-map "\C-c\C-um"      'texinfo-master-menu)
  (define-key TeXinfo-mode-map "\C-c\C-u\C-m"   'texinfo-make-menu)
  (define-key TeXinfo-mode-map "\C-c\C-u\C-n"   'texinfo-update-node)
  (define-key TeXinfo-mode-map "\C-c\C-u\C-e"   'texinfo-every-node-update)
  (define-key TeXinfo-mode-map "\C-c\C-u\C-a"   'texinfo-all-menus-update)

  ;; From TeX-mode

  ;; Standard
  (define-key TeXinfo-mode-map "\177"     'backward-delete-char-untabify)
  (define-key TeXinfo-mode-map "\C-c}"    'up-list)
  (define-key TeXinfo-mode-map "\C-c#"    'TeX-normal-mode)
  (define-key TeXinfo-mode-map "\C-c\C-n" 'TeX-normal-mode)
  (define-key TeXinfo-mode-map "\C-c?"    'describe-mode)
  
  ;; From tex-misc.el
  (define-key TeXinfo-mode-map "\C-c{"    'TeX-insert-braces)
  (define-key TeXinfo-mode-map "\C-c\C-f" 'TeX-font)

  (define-key TeXinfo-mode-map "\C-c;"    'TeX-comment-region)
  (define-key TeXinfo-mode-map "\C-c%"    'TeX-comment-paragraph)

  (define-key TeXinfo-mode-map "\C-c'"    'TeX-comment-out-paragraph) ;*** Old way
  (define-key TeXinfo-mode-map "\C-c:"    'TeX-un-comment-region) ;*** Old way
  (define-key TeXinfo-mode-map "\C-c\""   'TeX-un-comment) ;*** Old way

  ;; From tex-cpl.el
  (define-key TeXinfo-mode-map "\C-c\C-m" 'TeX-insert-macro)

  ;; From tex-buf.el
  (define-key TeXinfo-mode-map "\C-c\C-r" 'TeX-command-region)
  (define-key TeXinfo-mode-map "\C-c\C-c" 'TeX-command-master)
  (define-key TeXinfo-mode-map "\C-c\C-k" 'TeX-kill-job)
  (define-key TeXinfo-mode-map "\C-c\C-l" 'TeX-recenter-output-buffer)
  (define-key TeXinfo-mode-map "\C-c^" 'TeX-home-buffer)

  ;; From tex-dbg.el
  (define-key TeXinfo-mode-map "\C-c`"    'TeX-next-error)
  (define-key TeXinfo-mode-map "\C-c\C-w" 'TeX-toggle-debug-boxes)

  ;; From tex.cpl.el
  (define-key TeXinfo-mode-map "\e\t"   'TeX-complete-symbol) ;*** Emacs 19 way
  (define-key TeXinfo-mode-map "\C-c\t"   'TeX-complete-symbol)

  ;; Simulating LaTeX-mode

  (define-key TeXinfo-mode-map "\C-c\C-e" 'TeXinfo-insert-environment)
  (define-key TeXinfo-mode-map "\C-c\n"   'texinfo-insert-@item)
  (define-key TeXinfo-mode-map "\C-c\C-s" 'texinfo-insert-@node)
  (define-key TeXinfo-mode-map "\C-c]" 'texinfo-insert-@end))

(easy-menu-define TeXinfo-mode-menu
    TeXinfo-mode-map
    "Menu used in TeXinfo mode."
  (list "AUC TeX"
	["Environment..." TeXinfo-insert-environment t]
	["Node..." texinfo-insert-@node t]
	["Macro..." TeX-insert-macro t]
	["Complete" TeX-complete-symbol t]
	["Item" texinfo-insert-@item t]
	(list "Insert Font"
	      ["Emphasize"  (TeX-font nil ?\C-e) "  C-c C-f C-e"]
	      ["Bold"       (TeX-font nil ?\C-b) "  C-c C-f C-b"]
	      ["Typewriter" (TeX-font nil ?\C-t) "  C-c C-f C-t"]
	      ["Small Caps" (TeX-font nil ?\C-c) "  C-c C-f C-c"]
	      ["Italic"     (TeX-font nil ?\C-i) "  C-c C-f C-i"]
	      ["Sample"    (TeX-font nil ?\C-s) "  C-c C-f C-s"]
	      ["Roman"      (TeX-font nil ?\C-r) "  C-c C-f C-r"])
	(list "Insert Font around Region"
	      ["Emphasize"
	       (progn (kill-region (point) (mark)) (TeX-font nil ?\C-e) (yank))
	       "  C-w C-c C-f C-e C-y"]
	      ["Bold"
	       (progn (kill-region (point) (mark)) (TeX-font nil ?\C-b) (yank))
	       "  C-w C-c C-f C-b C-y"]
	      ["Typewriter"
	       (progn (kill-region (point) (mark))
		      (TeX-font nil ?\C-t)
		      (yank))
	       "  C-w C-c C-f C-t C-y"]
	      ["Small Caps"
	       (progn (kill-region (point) (mark)) 
		      (TeX-font nil ?\C-c)
		      (yank))
	       "  C-w C-c C-f C-c C-y"]
	      ["Italic"
	       (progn (kill-region (point) (mark))
		      (TeX-font nil ?\C-i)
		      (yank))
	       "  C-w C-c C-f C-i C-y"]
	      ["Sample"
	       (progn (kill-region (point) (mark))
		      (TeX-font nil ?\C-s)
		      (yank))
	       "  C-w C-c C-f C-s C-y"]
	      ["Roman"
	       (progn (kill-region (point) (mark))
		      (TeX-font nil ?\C-r)
		      (yank))
	       "  C-w C-c C-f C-r C-y"])
	(list "Change Font"
	      ["Emphasize"  (TeX-font t ?\C-e) "  C-u C-c C-f C-e"]
	      ["Bold"       (TeX-font t ?\C-b) "  C-u C-c C-f C-b"]
	      ["Typewriter" (TeX-font t ?\C-t) "  C-u C-c C-f C-t"]
	      ["Small Caps" (TeX-font t ?\C-c) "  C-u C-c C-f C-c"]
	      ["Italic"     (TeX-font t ?\C-i) "  C-u C-c C-f C-i"]
	      ["Sample"    (TeX-font t ?\C-s) "  C-u C-c C-f C-s"]
	      ["Roman"      (TeX-font t ?\C-r) "  C-u C-c C-f C-r"])
	"-"
	(TeX-command-menu "Command on Master File" 'TeX-master-file)
	(TeX-command-menu "Command on Region" 'TeX-region-file)
	["Next Error" TeX-next-error t]
	(list "TeX Output"
	      ["Kill Job" TeX-kill-job t]
	      ["Toggle debug of boxes" TeX-toggle-debug-boxes t]
	      ["Switch to original file" TeX-home-buffer t]
	      ["Recenter Output Buffer" TeX-recenter-output-buffer t])
	"--"
	["Create Master Menu" 'texinfo-master-menu t]
	["Create Menu" 'texinfo-make-menu t]
	["Update Node" 'texinfo-update-node t]
	["Update Every Node" texinfo-every-node-update t]
	["Update All Menus" 'texinfo-all-menus-update t]
	["Uncomment Region" TeX-un-comment-region t]
	["Comment Region" TeX-comment-region t]
	["Switch to Master file" TeX-home-buffer t]
	["Reset Buffer" TeX-normal-mode t]
	["Reset AUC TeX" (TeX-normal-mode t) "  C-u C-c C-n"]))

;;; @@ The Mode

(defun texinfo-mode ()
  "Major mode for editing files of input for TeXinfo.

Special commands:
\\{TeXinfo-mode-map}

Entering TeXinfo mode calls the value of text-mode-hook,
then the value of TeX-mode-hook, and then the value of
TeXinfo-mode-hook."  
  (interactive)
  (VirTeX-mode "TEXINFO"))

(fset 'TeXinfo-mode 'texinfo-mode)

;;; @@ Emacs

(run-hooks 'TeX-after-tex-info-hook)

;;; Local Variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: ";;; @+\\|(......"
;;; End:
