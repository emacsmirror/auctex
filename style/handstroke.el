;;; handstroke.el --- AUCTeX style for `handstroke.sty' (v0.3.0)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2026-07-13
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any
;; later version.

;; AUCTeX is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file adds support for `handstroke.sty' (v0.3.0) from 2026-07-12.
;; `handstroke.sty' is part of TeXLive.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords "font-latex" (keywords class))
(declare-function LaTeX-xcolor-definecolor-list "xcolor" nil)

(TeX-add-style-hook
 "handstroke"
 (lambda ()
   (when (or (LaTeX-provided-package-options-member "handstroke" "colored")
             (LaTeX-provided-package-options-member "handstroke" "colored=true"))
     (TeX-run-style-hooks "xcolor"))

   (TeX-add-symbols
    `("handstroke"
      (TeX-arg-conditional
          (or (LaTeX-provided-package-options-member "handstroke" "colored")
              (LaTeX-provided-package-options-member "handstroke" "colored=true"))
          ([TeX-arg-key-val (("color" ,(mapcar #'car (LaTeX-xcolor-definecolor-list))))])
        ())
      t))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("handstroke" "[{"))
                              'underline-command)))
 TeX-dialect)

(defun LaTeX-handstroke-package-options-list ()
  "Return package options for the handstroke package."
  (let (cols)
    (unless (member "xcolor" TeX-active-styles)
      ;; Don't run the style hook for `xcolor' unconditionally: If the
      ;; package isn't loaded yet, use a temp-buffer to get the possible
      ;; colors.
      (with-temp-buffer
        (LaTeX-mode)
        (TeX-run-style-hooks "xcolor")
        (setq cols (mapcar #'car (LaTeX-xcolor-definecolor-list)))))
    `(("colored" ("true" "false"))
      ("color" ,(or cols (mapcar #'car (LaTeX-xcolor-definecolor-list)))))))

(defun LaTeX-handstroke-package-options ()
  "Read the handstroke package options from user."
  (TeX-read-key-val t (LaTeX-handstroke-package-options-list)))

;;; handstroke.el ends here
