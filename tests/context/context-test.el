;;; context-test.el --- tests for ConTeXt mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2021  Free Software Foundation, Inc.

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
(require 'context)

;; We need to ensure that font-lock has put the syntax properties
;; already which won't happen in batch mode.  So trigger font-lock
;; immediately.
(define-advice ConTeXt-mode-common-initialization (:after ())
  (font-lock-ensure))

(AUCTeX-set-ert-path
 'ConTeXt-indent-test/in
 "context-indentation-in.tex"
 'ConTeXt-indent-test/out
 "context-indentation-out.tex")

(ert-deftest ConTeXt-indent ()
  (should (string=
           (with-temp-buffer
             (insert-file-contents ConTeXt-indent-test/in)
             (ConTeXt-mode)
             (indent-region (point-min) (point-max))
             (buffer-string))
           (with-temp-buffer
             (insert-file-contents ConTeXt-indent-test/out)
             (buffer-string)))))

;;; context-test.el ends here
