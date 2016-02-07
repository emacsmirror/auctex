;;; error-parsing.el --- tests for error parsing

;; Copyright (C) 2016 Free Software Foundation, Inc.

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
(require 'tex-buf)

(defun AUCTeX-set-ert-path (&rest sym-val)
  "Set first element of SYM-VAL to the next one, and so on.

The value is the path to the test file, make sure it is expanded
in the right directory even when the ERT test is run from the
command line and from another directory."
  (while sym-val
    (set (pop sym-val)
	 (expand-file-name (pop sym-val)
			   (when load-file-name
			     (file-name-directory load-file-name))))))

(AUCTeX-set-ert-path
 'TeX-test-compilation-log
 "compilation-log.txt")

(ert-deftest TeX-error-parsing ()
  "Test error parsing functions."
  (should (equal
	   (with-temp-buffer
	     (setq TeX-debug-warnings t)
             (insert-file-contents TeX-test-compilation-log)
             (TeX-parse-all-errors)
	     TeX-error-list)
	   '((warning "./test.tex" nil "Package foo Warning: This is a warning!"
		      0 "Package foo Warning: This is a warning!\n"
		      nil nil nil 170)))))

;;; error-parsing.el ends here
