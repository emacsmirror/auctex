;;; prv-install.el --- Complicated install-time magic for preview-latex.

;; Copyright (C) 2002  Free Software Foundation, Inc.

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

;; This contains package-building stuff and other install-time magic.
;; It may well contain Emacs-version-specific code, but certain
;; functions here should be *callable* from any Emacs version.

;;; Code:

(require 'preview)

(defun preview-make-package ()
  "Do anything required to make a package in this version of Emacs,
other than actually copying the Lisp files.

Takes arguments on the comamnd line: the package directory and any
number of Lisp files to generate autoloads from.

Does nothing in Emacsen that do not support a package system."
(if (string-match "XEmacs" (emacs-version))
    (preview-make-package-xemacs)))

(defun preview-make-package-xemacs ()
  "Do anything required to make a package in XEmacs,
other than actually copying the Lisp files.

Generates auto-autoloads, custom-loads, a manifest, and a package
metadata file in the right locations.

Takes arguments on the comamnd line: the package directory and any
number of Lisp files to generate autoloads from."
  (let* ((package-dir (pop command-line-args-left))
         (lisp-dir (expand-file-name "lisp/preview/" package-dir))
         (metadata (expand-file-name "_pkg.el" lisp-dir))
         (custom-load (expand-file-name "custom-load.el" lisp-dir))
         (generated-autoload-file (expand-file-name "auto-autoloads.el"
                                                    lisp-dir))
         (els command-line-args-left)
         (icons (directory-files (expand-file-name "images/") nil nil nil t))
         (si:message (symbol-function 'message))
            manifest make-backup-files noninteractive)
    ; Delete and regenerate the custom-load file.
    (when (file-exists-p custom-load)
      (delete-file custom-load))
    (when (file-exists-p (concat custom-load "c"))
      (delete-file (concat custom-load "c")))
    (Custom-make-dependencies lisp-dir)
    (when (file-exists-p custom-load)
      (require 'cus-load)
      (byte-compile-file custom-load)
      (push "custom-load.el" els))
    ; Delete and regenerate the package metadata file.
    ; There is no compiled form of this file.
    (message "Updating metadata for the directory %s..." lisp-dir)
    (with-temp-file metadata
      (insert
       (concat ";;;###autoload\n"
               "(package-provide 'preview\n"
               "                 :version "
               (if (string-match "^CVS-" preview-version)
                   (concat "0." (substring preview-version 4))
                 preview-version) "\n"
               "                 :type 'regular)\n")))
    ; Delete and regenerate the auto-autoloads file.
    (message "Updating autoloads for the directory %s..." lisp-dir)
    (when (file-exists-p generated-autoload-file)
      (delete-file generated-autoload-file))
    (when (file-exists-p (concat generated-autoload-file "c"))
      (delete-file (concat generated-autoload-file "c")))
    (defun message (fmt &rest args)
      "Ignore useless messages while generating autoloads."
      (cond ((and (string-equal "Generating autoloads for %s..." fmt)
                  (file-exists-p (file-name-nondirectory (car args))))
             (funcall si:message
                      fmt (file-name-nondirectory (car args))))
            ((string-equal "No autoloads found in %s" fmt))
            ((string-equal "Generating autoloads for %s...done" fmt))
            (t (apply si:message fmt args))))
    (unwind-protect
        (update-autoloads-from-directory lisp-dir)
      (fset 'message si:message))
    (when (file-exists-p generated-autoload-file)
      (byte-compile-file generated-autoload-file)
      (push "auto-autoloads.el" els))
    ; Some people delete the pkginfo directory; this should not break
    ; anything other than package autoupgrading, so cater for that.
    (when (file-directory-p (expand-file-name "pkginfo/" package-dir))
      (setq manifest (expand-file-name "pkginfo/MANIFEST.preview" package-dir))
      (message "Generating %s..." manifest)
      (with-temp-file manifest
        (insert "pkginfo/MANIFEST.preview\n")
        (insert "lisp/preview/ChangeLog\n")
        (dolist (el els)
          (insert "lisp/preview/" el "\n")
          (insert "lisp/preview/" el "c\n"))
        (dolist (icon icons)
          (insert "etc/preview/" icon "\n"))))
    (message "Generating %s...done" manifest)))

;;; prv-install.el ends here
