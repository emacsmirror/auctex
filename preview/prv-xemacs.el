;;; prv-xemacs.el --- XEmacs support for preview-latex

;; Copyright (C) 2001  Free Software Foundation, Inc.

;; Author: David Kastrup <David.Kastrup@t-online.de>
;; Keywords: convenience, tex, wp

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; 

;;; Code:

(eval-when-compile (require 'overlay))

(eval-and-compile
  (defvar preview-compatibility-macros nil
    "List of macros only present when compiling/loading.")

  (defmacro preview-defmacro (name &rest rest)
    (unless (fboundp name)
      (push name preview-compatibility-macros)
      `(defmacro ,name ,@rest)))
  (push 'preview-defmacro preview-compatibility-macros))

(preview-defmacro propertize (string &rest properties)
		  `(let ((res (copy-sequence ,string)))
		     (add-text-properties 0 (length res) ,@properties res)
		     res))

(preview-defmacro assoc-default (key alist test)
		  `(cdr (assoc* ,key ,alist
				:test #'(lambda(a b) (funcall ,test b a)))))

(preview-defmacro display-mm-height () '(device-mm-height))
(preview-defmacro display-mm-width () '(device-mm-width))
(preview-defmacro display-pixel-height () '(device-pixel-height))
(preview-defmacro display-pixel-width () '(device-pixel-width))
(preview-defmacro face-attribute (face attr)
		  (unless (eq attr :height)
		    (error "Know only how to fake face-height"))
		  `(face-height ,face))
(preview-defmacro line-beginning-position () '(point-at-bol))
(preview-defmacro line-end-position () '(point-at-eol))

(unless (fboundp 'find-image)
  (defun find-image (specs)
    "Find an image, choosing one of a list of image specifications.

SPECS is a list of image specifications.

Each image specification in SPECS is a property list.  The contents of
a specification are image type dependent.  All specifications must at
least contain the properties `:type TYPE' and either `:file FILE' or
`:data DATA', where TYPE is a symbol specifying the image type,
e.g. `xbm', FILE is the file to load the image from, and DATA is a
string containing the actual image data.  The specification whose TYPE
is supported, and FILE exists, is used to construct the image
specification to be returned.  Return nil if no specification is
satisfied.

The image is looked for first on `load-path' and then in `data-directory'."
    (let (image)
      (while (and specs (null image))
	(let* ((spec (car specs))
	       (type (plist-get spec :type))
	       (data (plist-get spec :data))
	       (file (plist-get spec :file))
	       found)
	  (when (if (fboundp 'valid-image-instantiator-format-p)
		    (valid-image-instantiator-format-p type)
		  (image-type-available-p type))
	    (cond ((stringp file)
		   (let ((path load-path))
		     (while (and (not found) path)
		       (let ((try-file (expand-file-name file (car path))))
			 (when (file-readable-p try-file)
			   (setq found try-file)))
		       (setq path (cdr path)))
		     (unless found
		       (let ((try-file (expand-file-name file data-directory)))
			 (if (file-readable-p try-file)
			     (setq found try-file))))
		     (if found
			 (setq image
			       (cons 'image (plist-put (copy-sequence spec)
						       :file found))))))
		  ((not (null data))
		   (setq image (cons 'image spec)))))
	  (setq specs (cdr specs))))
      image)))
  
(preview-defmacro defimage (symbol specs &optional doc)
  "Define SYMBOL as an image.

SPECS is a list of image specifications.  DOC is an optional
documentation string.

Each image specification in SPECS is a property list.  The contents of
a specification are image type dependent.  All specifications must at
least contain the properties `:type TYPE' and either `:file FILE' or
`:data DATA', where TYPE is a symbol specifying the image type,
e.g. `xbm', FILE is the file to load the image from, and DATA is a
string containing the actual image data.  The first image
specification whose TYPE is supported, and FILE exists, is used to
define SYMBOL.

Example:

   (defimage test-image ((:type xpm :file \"~/test1.xpm\")
                         (:type xbm :file \"~/test1.xbm\")))"
      `(defvar ,symbol (find-image ',specs) ,doc))

(preview-defmacro make-temp-file (prefix dir-flag)
		  (if (not dir-flag)
		      (error "Can only fake make-temp-file for directories"))
		  `(let (file)
		     (while (condition-case ()
				(progn
				  (setq file
					(make-temp-name ,prefix))
				  (make-directory file)
				  nil)
			      (file-already-exists t))
		       nil)
		     file))

(provide 'prv-xemacs)
;;; prv-xemacs.el ends here
