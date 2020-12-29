;;; font-latex-test.el --- tests for font-latex

;; Copyright (C) 2020 Free Software Foundation, Inc.

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
(require 'font-latex)

(ert-deftest font-latex-three-dollars ()
  "Test three consecutive dollar is ignored."
  ;; When the function `font-latex-match-dollar-math' encounters three
  ;; or more consecutive dollar signs which have no special meaning,
  ;; it should not stop there and return nil, but instead should
  ;; ignore them and search another occurence of $. That is the
  ;; behavior expected for MATCHER function of `font-lock-keywords'.
  (should (let ((TeX-install-font-lock 'font-latex-setup))
            (with-temp-buffer
              (insert "% $$$ $$$
$a$")
              (LaTeX-mode)
              (goto-char (point-min))
              (setq font-latex--updated-region-end (point-max))
              (font-latex-match-dollar-math (point-max))))))

(ert-deftest font-latex-extend-region-backwards-quotation ()
  "Test f-l-e-r-b-q doesn't extend region too eagerly."
  (with-temp-buffer
    (let ((TeX-install-font-lock 'font-latex-setup)
          (font-latex-quotes 'french)
          font-lock-beg font-lock-end)
      (LaTeX-mode)

      ;; Test 1: Double prime in math expression doesn't cause region
      ;; extension.
      (setq font-lock-beg (point))
      (insert "$f''(x)=x^{3}$")
      (setq font-lock-end (point))
      (should-not (font-latex-extend-region-backwards-quotation))

      (erase-buffer)
      (insert "abc ``def ghi'' jkl ")
      (setq font-lock-beg (point))
      (insert "$f''(x)=x^{3}$")
      (setq font-lock-end (point))
      (should-not (font-latex-extend-region-backwards-quotation))

      ;; Test 2: open-close pair before '' in math expression is
      ;; picked up.
      (erase-buffer)
      (insert "abc ``def ")
      (setq font-lock-beg (point))
      (insert "ghi'' jkl $f''(x)=x^{3}$")
      (setq font-lock-end (point))
      (should (font-latex-extend-region-backwards-quotation))
      (should (= font-lock-beg 5))

      (erase-buffer)
      (insert "abc <<def ")
      (setq font-lock-beg (point))
      (insert "ghi>> jkl $f''(x)=x^{3}$")
      (setq font-lock-end (point))
      (should (font-latex-extend-region-backwards-quotation))
      (should (= font-lock-beg 5)))))

;;; font-latex-test.el ends here
