;;; latex-modify-math-test.el --- tests for LaTeX-make-inline  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Free Software Foundation, Inc.

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
(require 'cl-lib)
(require 'latex)

(defmacro latex-modify-test--with-temp-buffer (contents &rest body)
  "Create a temporary LaTeX buffer with CONTENTS and execute BODY.
This macro is used to set up a test environment for `LaTeX-modify-math'."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (LaTeX-mode)
     (insert ,contents)
     (goto-char (point-min))
     (cl-letf (((symbol-function 'preview-clearout-at-point) #'ignore))
       ,@body)))

(ert-deftest LaTeX-modify-math-inline-bracket-period ()
  "Convert \\=\\[...\\=\\] to $..$ and keep trailing period."
  (latex-modify-test--with-temp-buffer
      "We have\n\\[ a+b = c. \\]"
    (search-forward "b")
    (LaTeX-make-inline)
    (should (equal (buffer-string) "We have $a+b = c$."))))

(ert-deftest LaTeX-modify-math-inline-double-dollar ()
  "Convert $$..$$ to $..$."
  (latex-modify-test--with-temp-buffer
      "$$x!$$"
    (search-forward "x")
    (LaTeX-make-inline)
    (should (equal (buffer-string) "$x$!"))))

(ert-deftest LaTeX-modify-math-inline-electric-math ()
  "Respect `TeX-electric-math'."
  (let ((TeX-electric-math '("\\(" . "\\)")))
    (latex-modify-test--with-temp-buffer
        "\\[ x \\]"
      (search-forward "x")
      (LaTeX-make-inline)
      (should (equal (buffer-string) "\\(x\\)")))))

(ert-deftest LaTeX-modify-math-inline-equation-env ()
  "Convert equation environment, drop \\label, keep comma."
  (latex-modify-test--with-temp-buffer
      "Hi.\n\nWe have\n\\begin{equation}\n\\label{l}x+y,\n\\end{equation}\n"
    (search-forward "x")
    (let ((TeX-electric-math '("\\(" . "\\)")))
      (LaTeX-make-inline)
      (should (equal (buffer-string) "Hi.\n\nWe have \\(x+y\\),\n")))))

(ert-deftest LaTeX-modify-math-inline-noop ()
  "Call inside inline math leaves buffer unchanged."
  (latex-modify-test--with-temp-buffer
      "Already $z$ inline."
    (search-forward "z")
    (LaTeX-make-inline)
    (should (equal (buffer-string) "Already $z$ inline."))))

(ert-deftest LaTeX-modify-math-inline-paren-to-dollar ()
  "Convert \\(...\\) to $...$."
  (latex-modify-test--with-temp-buffer
      "Text \\(a + b\\) more text."
    (search-forward "a")
    (let ((TeX-electric-math nil))
      (LaTeX-make-inline)
      (should (equal (buffer-string) "Text $a + b$ more text.")))))

(ert-deftest LaTeX-modify-math-inline-multiline-equation ()
  "Convert multiline equation environment to inline, removing labels."
  (latex-modify-test--with-temp-buffer
      "Before\n\\begin{equation}\n  x + y = z\n  \\label{eq:test}\n\\end{equation}\nAfter"
    (search-forward "x")
    (LaTeX-make-inline)
    (should (equal (buffer-string) "Before $x + y = z$ After"))))

(ert-deftest LaTeX-modify-math-inline-punctuation-semicolon ()
  "Move semicolon outside inline math."
  (latex-modify-test--with-temp-buffer
      "\\[ x + y; \\]"
    (search-forward "x")
    (LaTeX-make-inline)
    (should (equal (buffer-string) "$x + y$;"))))

(ert-deftest LaTeX-modify-math-inline-multiple-punctuation ()
  "Handle multiple punctuation marks."
  (latex-modify-test--with-temp-buffer
      "\\[ result?! \\]"
    (search-forward "result")
    (LaTeX-make-inline)
    (should (equal (buffer-string) "$result$?!"))))

(ert-deftest LaTeX-modify-math-inline-whitespace-preservation ()
  "Preserve surrounding whitespace appropriately."
  (latex-modify-test--with-temp-buffer
      "Text   \\[   a + b   \\]   more."
    (search-forward "a")
    (LaTeX-make-inline)
    (should (equal (buffer-string) "Text   $a + b$   more."))))

(ert-deftest LaTeX-modify-math-inline-empty-lines ()
  "Remove empty lines from display math when converting."
  (latex-modify-test--with-temp-buffer
      "\\[\n\n  x = y  \n\n\\]"
    (search-forward "x")
    (LaTeX-make-inline)
    (should (equal (buffer-string) "$x = y$"))))

(ert-deftest LaTeX-modify-math-dollar-to-bracket ()
  "Convert $...$ to \\=\\[...\\=\\]."
  (latex-modify-test--with-temp-buffer
      "Text $x + y$ more."
    (search-forward "+")
    (LaTeX-modify-math "\\[")
    (should (equal (buffer-string) "Text\n\\[\n  x + y\n\\]\nmore."))))

(ert-deftest LaTeX-modify-math-paren-to-double-dollar ()
  "Convert \\(...\\) to $$...$$."
  (latex-modify-test--with-temp-buffer
      "Text \\(a = b\\) end."
    (search-forward "a")
    (LaTeX-modify-math "$$")
    (should (equal (buffer-string) "Text\n$$\na = b\n$$\nend."))))

(ert-deftest LaTeX-modify-math-bracket-to-equation ()
  "Convert \\=\\[...\\=\\] to equation environment."
  (latex-modify-test--with-temp-buffer
      "\\[ f(x) = x^2 \\]"
    (search-forward "f")
    (LaTeX-modify-math "equation")
    (should (equal (buffer-string) "\\begin{equation}\n  f(x) = x^2\n\\end{equation}"))))

(ert-deftest LaTeX-modify-math-point-inline-to-display-after-content ()
  "Point after inline content preserved after display conversion."
  (latex-modify-test--with-temp-buffer
      "A $x+y$ B"
    (search-forward "y")
    (LaTeX-modify-math "\\[")
    (should (looking-back "y" (1- (point))))
    (should (looking-at "\n[[:space:]]*\\\\\\]"))))

(ert-deftest LaTeX-modify-math-point-inline-to-display-before-content ()
  "Point before inline content preserved after display conversion."
  (latex-modify-test--with-temp-buffer
      "A $x+y$ B"
    (search-forward "$")
    (LaTeX-modify-math "\\[")
    (looking-at "x")
    (should (looking-at "x"))))

(ert-deftest LaTeX-modify-math-point-display-to-inline-after-content ()
  "Point after display content preserved after inline conversion."
  (latex-modify-test--with-temp-buffer
      "\\[\n  x + y\n\\]"
    (goto-char (point-min))
    (re-search-forward "y")
    (LaTeX-make-inline)
    (should (looking-back "y" (1- (point))))
    (should (looking-at "\\$"))))

(ert-deftest LaTeX-modify-math-point-display-to-inline-before-content ()
  "Point before display content preserved after inline conversion."
  (latex-modify-test--with-temp-buffer
      "\\[\n  x + y\n\\]"
    (goto-char (point-min))
    (re-search-forward "x")
    (forward-char -1)
    (LaTeX-make-inline)
    (should (looking-at "x"))))

(ert-deftest LaTeX-modify-math-point-multiline-roundtrip ()
  "Point before/after content preserved for round-trip conversion."
  (latex-modify-test--with-temp-buffer
      "foo $x+y$ bar"
    (search-forward "y")
    (backward-char)
    (LaTeX-modify-math "\\[")
    (should (looking-at "y"))
    (LaTeX-make-inline)
    (should (looking-at "y"))))

;;; latex-modify-math-test.el ends here
