;;; navigation.el --- tests for navigation function in TeX buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2023  Free Software Foundation, Inc.

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
(require 'tex)

(defun TeX-check-f-m-e-h (string &optional position)
  "Check whether `TeX-find-macro-end-helper' works for exceptional case."
  (erase-buffer)
  (insert string)
  (should (= (or position (point-max))
             (TeX-find-macro-end-helper (point-min)))))

(ert-deftest TeX-find-macro-end-helper ()
  (with-temp-buffer
    (LaTeX-mode)

    ;; single macro ending at EOB
    (TeX-check-f-m-e-h "\\foo")

    ;; curly braces ending at EOB
    (TeX-check-f-m-e-h "\\foo{bar}")

    ;; curly brace failing to close at EOB
    (TeX-check-f-m-e-h "\\foo{bar")

    ;; square brackets ending at EOB
    (TeX-check-f-m-e-h "\\foo{bar}[baz]")

    ;; square bracket failing to close at EOB
    (TeX-check-f-m-e-h "\\foo{bar}[baz" (1+ (length "\\foo{bar}")))))

(defun TeX-check-f-m-b (string &optional chars)
  "Check whether `TeX-find-macro-boundaries' works for exceptional case."
  (erase-buffer)
  (insert string)
  (if chars (backward-char chars))
  (let ((result (TeX-find-macro-boundaries)))
    (should (= (point-min)
               (car result)))
    (should (= (point-max)
               (cdr result)))))

(ert-deftest TeX-find-macro-boundaries-detached-arg ()
  (with-temp-buffer
    ;; necessary to set comment syntax properly
    (LaTeX-mode)

    ;; argument separated by newline
    (TeX-check-f-m-e-h "\\foo{bar}
{baz}")

    (TeX-check-f-m-e-h "\\foo{bar}
  {baz}")

    (TeX-check-f-m-e-h "\\foo{bar} % comment
  {baz}")

    (TeX-check-f-m-b "\\foo{bar}
{baz}" 2)

    (TeX-check-f-m-b "\\foo{bar}
  {baz}" 2)

    (TeX-check-f-m-b "\\foo{bar} % comment
  {baz}" 2)

    (TeX-check-f-m-b "\\foo{bar}% comment
  {baz}" 2)))

;;; navigation.el ends here
