;;; utility.el --- tests for AUCTeX utility functions -*- lexical-binding: t; -*-

;; Copyright (C) 2017, 2021 Free Software Foundation, Inc.

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

(ert-deftest TeX-infinite-loop ()
  "Check whether functions don't fall into infinite loop."
  (should (TeX-delete-duplicate-strings '("nil")))
  (should (TeX-delete-dups-by-car '(("nil" . 1)))))

;; `TeX-add-to-alist' needs dynamic scope variable as its first
;; argument.
(defvar TeX-dummy-alist nil)

(ert-deftest TeX-adding-to-alist ()
  "Check whether `TeX-add-to-alist' works as expected."
  (TeX-add-to-alist 'TeX-dummy-alist '((a 1)))
  (should (equal TeX-dummy-alist '((a 1))))

  (TeX-add-to-alist 'TeX-dummy-alist '((b 2 3)))
  (should (equal TeX-dummy-alist '((a 1) (b 2 3))))

  ;; Append new value(s) of the same key. The target cons is moved to
  ;; the last of the alist.
  (TeX-add-to-alist 'TeX-dummy-alist '((a 4)))
  (should (equal TeX-dummy-alist '((b 2 3) (a 1 4))))

  ;; Adding the same value again should not create duplicated
  ;; elements.
  (TeX-add-to-alist 'TeX-dummy-alist '((a 1)))
  (should (equal TeX-dummy-alist '((b 2 3) (a 1 4))))

  ;; A value which is the same as the key should be included in the
  ;; result.
  (TeX-add-to-alist 'TeX-dummy-alist '((a a)))
  (should (equal TeX-dummy-alist '((b 2 3) (a 1 4 a)))))

;;; utility.el ends here
