;;; path-expansion.el --- tests for path expansion

;; Copyright (C) 2017 Free Software Foundation, Inc.

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

(ert-deftest TeX-variable-truncation ()
  "Check whether list variable is not truncated as side effect."
  (let ((var '("str1" "str2"))
	(TeX-kpathsea-path-delimiter nil)
	(TeX-search-files-type-alist
	 '((abc "${dummy}" ("str2" var) TeX-file-extensions))))
    (TeX-search-files-by-type 'abc 'global)
    (should (equal var '("str1" "str2")))))

(ert-deftest TeX-kpathsea-delimiter-w32 ()
  "Check whether `TeX-kpathsea-path-delimiter' is set to \";\" on w32 platform."
  ;; This test is meaningful only on w32 platform.
  (skip-unless (eq system-type 'windows-nt))
  ;; Provide `TeX-tree-expand' with output which doesn't begin with
  ;; dos drive letter.
  (let ((TeX-kpathsea-path-delimiter
	 (eval (car (get 'TeX-kpathsea-path-delimiter 'standard-value)))))
    (TeX-tree-expand '(".") nil)
    (should (equal TeX-kpathsea-path-delimiter ";")))
  ;; Provide `TeX-search-files-kpathsea' with output of only one
  ;; component (thus without a separator ";".)
  (let ((TeX-kpathsea-path-delimiter
	 (eval (car (get 'TeX-kpathsea-path-delimiter 'standard-value))))
	;; Let's hope that no other files in the temp directory have
	;; such a bizarre extension.
	(temp-file (make-temp-file "TeX-path-expansion" nil ".xxyyzz")))
    (unwind-protect
	(TeX-search-files-kpathsea (file-name-directory temp-file)
				   '("xxyyzz") nil nil nil)
      (delete-file temp-file))
    (should (equal TeX-kpathsea-path-delimiter ";"))))

;;; command-expansion.el ends here
