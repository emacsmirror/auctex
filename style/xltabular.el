;;; xltabular.el --- AUCTeX style for `xltabular.sty' (v0.05)

;; Copyright (C) 2017 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2017-11-03
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

;;; Commentary:

;; This file adds support for `xltabular.sty' (v0.05) from 2017/10/26.
;; `xltabular.sty' is part of TeXLive.

(TeX-add-style-hook
 "xltabular"
 (lambda ()
   ;; ltablex loads both tabularx and longtable
   (TeX-run-style-hooks "ltablex")

   ;; `LaTeX-env-longtable' is provided by `longtable.el':
   (LaTeX-add-environments
    '("xltabular" LaTeX-env-longtable))

   ;; Use the enhanced table formatting.  Append to
   ;; `LaTeX-indent-environment-list' in order not to override custom settings.
   (add-to-list (make-variable-buffer-local 'LaTeX-indent-environment-list)
		'("xltabular" LaTeX-indent-tabular) t)

   ;; Append xltabular to `LaTeX-label-alist', in order not to
   ;; override possible custome values.
   (add-to-list 'LaTeX-label-alist '("xltabular" . LaTeX-table-label) t)

   ;; Append xltabular to `LaTeX-item-list' with `LaTeX-item-longtable'
   (add-to-list 'LaTeX-item-list '("xltabular" . LaTeX-item-longtable) t)

   ;; Tell RefTeX -- This is the same entry as for "longtable" in
   ;; `reftex-label-alist-builtin':
   (when (fboundp 'reftex-add-label-environments)
     (reftex-add-label-environments
      '(("xltabular" ?t nil nil caption)))))
 LaTeX-dialect)

(defvar LaTeX-xltabular-package-options nil
  "Package options for the xltabular package.")

;;; xltabular.el ends here
