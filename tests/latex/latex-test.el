;;; latex-test.el --- tests for LaTeX mode

;; Copyright (C) 2014, 2015 Free Software Foundation, Inc.

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

(defun AUCTeX-set-ert-path (&rest sym-val)
  "Set first element of SYM-VAL to the next one, and so on.

The value is the path to the test file, make sure it is expanded
in the right directory even when the ERT test from the command
line and from another directory."
  (while sym-val
    (set (pop sym-val)
	 (expand-file-name (pop sym-val)
			   (when load-file-name
			     (file-name-directory load-file-name))))))

(AUCTeX-set-ert-path
 'LaTeX-indent-tabular-test/in
 "tabular-in.tex"
 'LaTeX-indent-tabular-test/out
 "tabular-out.tex"
 'LaTeX-filling/in
 "latex-filling-in.tex"
 'LaTeX-filling/out
 "latex-filling-out.tex")

(ert-deftest LaTeX-indent-tabular ()
  (should (string=
           (with-temp-buffer
             (insert-file-contents LaTeX-indent-tabular-test/in)
             (LaTeX-mode)
             (indent-region (point-min) (point-max))
             (buffer-string))
           (with-temp-buffer
             (insert-file-contents LaTeX-indent-tabular-test/out)
             (buffer-string)))))

;; Test LaTeX code with math modes is indented as expected.  This has mostly to
;; do with the value of `LaTeX-fill-break-at-separators' and how
;; `LaTeX-fill-move-to-break-point' handles it.  If the test fails, try to look
;; there.
(ert-deftest LaTeX-filling ()
  (should (string=
           (with-temp-buffer
             (insert-file-contents LaTeX-filling/in)
             (LaTeX-mode)
	     (let ((fill-column 70))
	       (fill-paragraph))
             (buffer-string))
           (with-temp-buffer
             (insert-file-contents LaTeX-filling/out)
             (buffer-string)))))

;;; latex-test.el ends here
