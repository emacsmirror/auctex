;;; auc-menu.el - Easy menu support for GNU Emacs 19 and XEmacs.
;; 
;; $Id: auc-menu.el,v 5.7 1994-11-28 01:41:22 amanda Exp $
;;
;; LCD Archive Entry:
;; auc-menu|Per Abrahamsen|abraham@iesd.auc.dk|
;; Easy menu support for GNU Emacs 19 and XEmacs|
;; $Date: 1994-11-28 01:41:22 $|$Revision: 5.7 $|~/misc/auc-menu.el.gz|

;; Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.
;; Copyright (C) 1994 Per Abrahamsen <abraham@iesd.auc.dk>
;;
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

;; Commentary:
;;
;; Easymenu allows you to define menus for both Emacs 19 and XEmacs.
;; The advantages of using easymenu are:
;;
;; - Easier to use than either the Emacs 19 and XEmacs menu syntax.
;;
;; - Common interface for Emacs 18, Emacs 19, and XEmacs.  
;;   (The code does nothing when run under Emacs 18).
;;
;; The public functions are:
;; 
;; - Function: easy-menu-define SYMBOL MAPS DOC MENU
;;     SYMBOL is both the name of the variable that holds the menu and
;;            the name of a function that will present a the menu.
;;     MAPS is a list of keymaps where the menu should appear in the menubar.
;;     DOC is the documentation string for the variable.
;;     MENU is an XEmacs style menu description.  
;;
;;     See the documentation for easy-menu-define for details.
;;
;; - Function: easy-menu-change PATH NAME ITEMS
;;     Change an existing menu.
;;     The menu must already exist an be visible on the menu bar.
;;     PATH is a list of strings used for locating the menu on the menu bar. 
;;     NAME is the name of the menu.  
;;     ITEMS is a list of menu items, as defined in `easy-menu-define'.
;;
;; - Function: easy-menu-add MENU [ MAP ]
;;     Add MENU to the current menubar in MAP.
;;
;; - Function: easy-menu-remove MENU
;;     Remove MENU from the current menubar.
;;
;; GNU Emacs 19 never uses `easy-menu-add' or `easy-menu-remove',
;; menus automatically appear and disappear when the keymaps
;; specified by the MAPS argument to `easy-menu-define' are
;; activated.
;;
;; XEmacs will bind the map to button3 in each MAPS, but you must
;; explicitly call `easy-menu-add' and `easy-menu-remove' to add and
;; remove menus from the menu bar.

;; auc-menu.el define the easymenu API included in Emacs 19.29 and
;; later.  In fact, the Emacs 19 specific code should be identical.

;;; Code:

;;;###autoload
(defmacro easy-menu-define (symbol maps doc menu)
  "Define a menu bar submenu in maps MAPS, according to MENU.
The arguments SYMBOL and DOC are ignored; they are present for
compatibility only.  SYMBOL is not evaluated.  In other Emacs versions
these arguments may be used as a variable to hold the menu data, and a
doc string for that variable.

The first element of MENU must be a string.  It is the menu bar item name.
The rest of the elements are menu items.

A menu item is usually a vector of three elements:  [NAME CALLBACK ENABLE]

NAME is a string--the menu item name.

CALLBACK is a command to run when the item is chosen,
or a list to evaluate when the item is chosen.

ENABLE is an expression; the item is enabled for selection
whenever this expression's value is non-nil.

Alternatively, a menu item may have the form: 

   [ NAME CALLBACK [ KEYWORD ARG ] ... ]

Where KEYWORD is one of the symbol defined below.

   :keys KEYS

KEYS is a string; a complex keyboard equivalent to this menu item.

   :active ENABLE

ENABLE is an expression; the item is enabled for selection
whenever this expression's value is non-nil.

   :suffix NAME

NAME is a string; the name of an argument to CALLBACK.

   :style STYLE
   
STYLE is a symbol describing the type of menu item.  The following are
defined:  

toggle: A checkbox.  
        Currently just prepend the name with the string \"Toggle \".
radio: A radio button. 
nil: An ordinary menu item.

   :selected SELECTED

SELECTED is an expression; the checkbox or radio button is selected
whenever this expression's value is non-nil.
Currently just disable radio buttons, no effect on checkboxes.

A menu item can be a string.  Then that string appears in the menu as
unselectable text.  A string consisting solely of hyphens is displayed
as a solid horizontal line.

A menu item can be a list.  It is treated as a submenu.
The first element should be the submenu name.  That's used as the
menu item in the top-level menu.  The cdr of the submenu list
is a list of menu items, as above."
  (` (progn
       (defvar (, symbol) nil (, doc))
       (easy-menu-do-define (quote (, symbol)) (, maps) (, doc) (, menu)))))

(cond 

;;; Emacs 18

((< (string-to-int emacs-version) 19)

(defun easy-menu-do-define (symbol maps doc menu)
  (fset symbol (symbol-function 'ignore)))

(defun easy-menu-remove (menu))

(defun easy-menu-add (menu &optional map))

(defun easy-menu-change (path name items))

)					;Emacs 18

;;; XEmacs

((string-match "XEmacs\\|Lucid" emacs-version)

(defun easy-menu-do-define (symbol maps doc menu)
  (set symbol menu)
  (fset symbol (list 'lambda '(e)
		     doc
		     '(interactive "@e")
		     '(run-hooks 'activate-menubar-hook)
		     '(setq zmacs-region-stays 't)
		     (list 'popup-menu symbol)))
  (mapcar (function (lambda (map) (define-key map 'button3 symbol)))
	  (if (keymapp maps) (list maps) maps)))

(fset 'easy-menu-change (symbol-function 'add-menu))

(defun easy-menu-add (menu &optional map)
  "Add MENU to the current menu bar."
  (cond ((null current-menubar)
	 ;; Don't add it to a non-existing menubar.
	 nil)
	((assoc (car menu) current-menubar)
	 ;; Already present.
	 nil)
	((equal current-menubar '(nil))
	 ;; Set at left if only contains right marker.
	 (set-buffer-menubar (list menu nil)))
	(t
	 ;; Add at right.
	 (set-buffer-menubar (copy-sequence current-menubar))
	 (add-menu nil (car menu) (cdr menu)))))

(defun easy-menu-remove (menu)
  "Remove MENU from the current menu bar."
  (and current-menubar
       (assoc (car menu) current-menubar)
       (delete-menu-item (list (car menu)))))

)					;XEmacs

;;; GNU Emacs 19

(t

(defun easy-menu-do-define (symbol maps doc menu)
  ;; We can't do anything that might differ between Emacs dialects in
  ;; `easy-menu-define' in order to make byte compiled files
  ;; compatible.  Therefore everything interesting is done in this
  ;; function. 
  (set symbol (easy-menu-create-keymaps (car menu) (cdr menu)))
  (fset symbol (` (lambda (event) (, doc) (interactive "@e")
		    (easy-popup-menu event (, symbol)))))
  (mapcar (function (lambda (map) 
	    (define-key map (vector 'menu-bar (intern (car menu)))
	      (cons (car menu) (symbol-value symbol)))))
	  (if (keymapp maps) (list maps) maps)))

(defvar easy-menu-item-count 0)

;; Return a menu keymap corresponding to a XEmacs style menu list
;; MENU-ITEMS, and with name MENU-NAME.
(defun easy-menu-create-keymaps (menu-name menu-items)
  (let ((menu (make-sparse-keymap menu-name)))
    ;; Process items in reverse order,
    ;; since the define-key loop reverses them again.
    (setq menu-items (reverse menu-items))
    (while menu-items
      (let* ((item (car menu-items))
	     (callback (if (vectorp item) (aref item 1)))
	     command enabler name)
	(cond ((stringp item)
	       (setq command nil)
	       (setq name (if (string-match "^-+$" item) "" item)))
	      ((consp item)
	       (setq command (easy-menu-create-keymaps (car item) (cdr item)))
	       (setq name (car item)))
	      ((vectorp item)
	       (setq command (make-symbol (format "menu-function-%d"
						  easy-menu-item-count)))
	       (setq easy-menu-item-count (1+ easy-menu-item-count))
	       (setq name (aref item 0))
	       (let ((keyword (aref item 2)))
		 (if (and (symbolp keyword)
			  (= ?: (aref (symbol-name keyword) 0)))
		     (let ((count 2)
			   style selected active keys
			   arg)
		       (while (> (length item) count)
			 (setq keyword (aref item count))
			 (setq arg (aref item (1+ count)))
			 (setq count (+ 2 count))
			 (cond ((eq keyword ':keys)
				(setq keys arg))
			       ((eq keyword ':active)
				(setq active arg))
			       ((eq keyword ':suffix)
				(setq name (concat name " " arg)))
			       ((eq keyword ':style)
				(setq style arg))
			       ((eq keyword ':selected)
				(setq selected arg))))
		       (if keys
			   (setq name (concat name "  (" keys ")")))
		       (if (eq style 'toggle)
			   ;; Simulate checkboxes.
			   (setq name (concat "Toggle " name)))
		       (if active 
			   (put command 'menu-enable active)
			 (and (eq style 'radio)
			      selected
			      ;; Simulate radio buttons with menu-enable.
			      (put command 'menu-enable
				   (list 'not selected)))))))	       
	       (if (keymapp callback)
		   (setq name (concat name " ...")))
	       (if (symbolp callback)
		   (fset command callback)
		 (fset command (list 'lambda () '(interactive) callback)))))
	(if (null command)
	    ;; Handle inactive strings specially--allow any number
	    ;; of identical ones.
	    (setcdr menu (cons (list nil name) (cdr menu)))
	  (if name 
	      (define-key menu (vector (intern name)) (cons name command)))))
      (setq menu-items (cdr menu-items)))
    menu))

(defun easy-menu-change (path name items)
  "Change menu found at PATH as item NAME to contain ITEMS.
PATH is a list of strings for locating the menu containing NAME in the
menu bar.  ITEMS is a list of menu items, as in `easy-menu-define'.
These items entirely replace the previous items in that map.

Call this from `activate-menubar-hook' to implement dynamic menus."
  (let ((map (key-binding (apply 'vector
				 'menu-bar
				 (mapcar 'intern (append path (list name)))))))
    (if (keymapp map)
	(setcdr map (cdr (easy-menu-create-keymaps name items)))
      (error "Malformed menu in `easy-menu-change'"))))

(defun easy-menu-remove (menu))

(defun easy-menu-add (menu &optional map))

)					;GNU Emacs 19

)					;cond

(provide 'easymenu)
(provide 'auc-menu)

;;; auc-menu.el ends here
