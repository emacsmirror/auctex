;;; texmathp-test.el --- tests for texmathp  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  Free Software Foundation, Inc.

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

;;; Code:

(require 'ert)
(require 'latex)
(require 'texmathp)

;; bug#41559
(ert-deftest texmathp-bob ()
  "Test math expressions beginning at BOB are identified correctly."
  (should (with-temp-buffer
            (insert "\\(")
            (LaTeX-mode)
            (texmathp)))

  (should (with-temp-buffer
            (insert "\\[")
            (LaTeX-mode)
            (texmathp)))

  (should (with-temp-buffer
            (insert "\\ensuremath{")
            (LaTeX-mode)
            (texmathp)))

  (should (with-temp-buffer
            (insert "$")
            (LaTeX-mode)
            (texmathp))))

;; bug#61410
(ert-deftest texmathp-verbatim ()
  "Test for math command inside verbatim which is ignored."
  (let ((TeX-install-font-lock #'font-latex-setup))
    (should-not (with-temp-buffer
                  (insert "a $b$ \\verb|$| c ")
                  (LaTeX-mode)
                  (font-lock-ensure)
                  (texmathp)))

    (should-not (with-temp-buffer
                  (insert "\
a $b$

\\begin{verbatim}
$
\\end{verbatim}
c")
                  (LaTeX-mode)
                  (font-lock-ensure)
                  (texmathp)))))

;;; texmathp-test.el ends here
