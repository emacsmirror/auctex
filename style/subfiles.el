;;; subfiles.el --- AUCTeX style for the subfiles package.

;; Copyright (C) 2016 Free Software Foundation, Inc.

;; Author: Uwe Brauer <oub@mat.ucm.es>
;; Created: 07 Nov 2016
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; AUCTeX is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;; Acknowledgements
;; Mosè Giordano  <mose@gnu.org>
;; Arash Esbati <arash.esbati+ml@gmail.com>

;;; Commentary:

;; This file adds support for the subfiles package.

;;; Code:

(defvar LaTeX-subfiles-package-options nil
  "Package options for the subfiles package.")

(TeX-add-style-hook
 "subfiles"
 (lambda ()
   ;; The following code will fontify `\subfile{}' like  include.
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("subfile" "{"))
                              'reference))
   ;; The following code will run `TeX-run-style-hooks' on the subfile master file.
   ;; Thanks to Mosè Giordano <mose@gnu.org> for presenting a better solution using `assoc'.
   (TeX-run-style-hooks
    (file-name-base (cadr (assoc "subfiles" LaTeX-provided-class-options))))
   (TeX-add-symbols
    '("subfile" TeX-arg-file)))
 LaTeX-dialect)


;;; subfiles.el ends here
