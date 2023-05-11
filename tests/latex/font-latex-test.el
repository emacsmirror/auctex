;;; font-latex-test.el --- tests for font-latex  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023  Free Software Foundation, Inc.

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
(defvar font-lock-beg)
(defvar font-lock-end)

;; We need to ensure that font-lock has put the syntax properties
;; already which won't happen in batch mode.  So trigger font-lock
;; immediately.
(define-advice LaTeX-common-initialization (:after ())
  (font-lock-ensure))

(ert-deftest font-latex-three-dollars ()
  "Test three consecutive dollar is ignored."
  ;; When the function `font-latex-match-dollar-math' encounters three
  ;; or more consecutive dollar signs which have no special meaning,
  ;; it should not stop there and return nil, but instead should
  ;; ignore them and search another occurence of $. That is the
  ;; behavior expected for MATCHER function of `font-lock-keywords'.
  (should (let ((TeX-install-font-lock #'font-latex-setup))
            (with-temp-buffer
              (insert "% $$$ $$$
$a$")
              (LaTeX-mode)
              (goto-char (point-min))
              (setq font-latex--updated-region-end (point-max))
              (font-latex-match-dollar-math (point-max))))))

(ert-deftest font-latex-unclosed-dollars ()
  "Test unclosed dollar doesn't cause error."
  (let ((TeX-install-font-lock #'font-latex-setup))
    (with-temp-buffer
      (LaTeX-mode)

      (insert "a$")
      (goto-char (point-min))
      (setq font-latex--updated-region-end (point-max))
      (should (not (font-latex-match-dollar-math (point-max))))

      (erase-buffer)
      (insert "a$$")
      (goto-char (point-min))
      (setq font-latex--updated-region-end (point-max))
      (should (not (font-latex-match-dollar-math (point-max)))))))

(ert-deftest font-latex-extend-region-backwards-quotation ()
  "Test f-l-e-r-b-q doesn't extend region too eagerly."
  (with-temp-buffer
    (let ((TeX-install-font-lock #'font-latex-setup)
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

(ert-deftest font-latex-general-fontification ()
  "Test general fontification in a LaTeX file."
  (with-temp-buffer
    (let ((TeX-install-font-lock #'font-latex-setup)
          (font-latex-fontify-sectioning 'color))
      (insert "\
\\documentclass[10pt]{article}
\\begin{document}

\\section{Macros}
Inline verbatim test:  \\verb|x|
Inline math test:      $x$, \\(x\\)
Marginpar test:        \\marginpar{x}
Sedate macro test:     \\sedate
\\section{Font Specifiers}
Roman face test:       \\textrm{x}
Bold face test:        \\textbf{x}
Italic face test:      \\textit{x}
Bold italic face test: \\textit{\\textbf{x}}
Emphasize face test:   \\emph{x}
Declarations test:     \\ttfamily x {\\ttfamily x}
                       \\itshape  x {\\itshape x}
                       \\bfseries x {\\bfseries x}

\\section{Environments}
\\subsection{Math}

\\begin{math}
  x
\\end{math}

\\[
  x
\\]

\\[x\\]

\\begin{displaymath}
  x
\\end{displaymath}

\\begin{equation}
  x
\\end{equation}

\\subsection{Misc.}

\\begin{verbatim}
x
\\end{verbatim}

\\begin{description}
\\item[x] x
\\end{description}

\\section{Box commands}

\\newsavebox\\mysavebox
\\savebox{\\mysavebox}[30mm][r]{This is my box}
\\savebox{\\mysavebox}(0,0)[l]{This is my box}

\\parbox[m][3cm][c]{2cm}{Some Text}

\\end{document}

%%% Local Variables:
%%% mode: latex
%%% TeX-master: t
%%% End:\n")
      (LaTeX-mode)
      (font-lock-ensure)
      (goto-char (point-min))

      ;; Test for \documentclass:
      (re-search-forward "\\\\document\\(?1:c\\)lass\\[")
      (should (font-latex-faces-present-p 'font-lock-keyword-face
                                          (match-beginning 1)))
      (forward-char)
      ;; Optional argument
      (should (font-latex-faces-present-p 'font-lock-variable-name-face))
      (search-forward "{")
      (forward-char)
      ;; Mandatory argument:
      (should (font-latex-faces-present-p 'font-lock-function-name-face))
      (end-of-line)

      ;; Test for \section macro itself:
      (re-search-forward "\\\\sec\\(?1:t\\)ion{")
      (should (font-latex-faces-present-p 'font-lock-keyword-face
                                          (match-beginning 1)))
      (goto-char (match-end 0))
      (forward-char)
      ;; Test for the argument of \section:
      (should (font-latex-faces-present-p 'font-lock-type-face))
      (end-of-line)

      ;; Test for inline verb:
      (search-forward "\\verb|")
      (should (font-latex-faces-present-p 'font-latex-verbatim-face))
      (end-of-line)

      ;; Test for inline math:
      (search-forward "$")
      (should (font-latex-faces-present-p 'font-latex-math-face))
      (search-forward "\\(")
      (should (font-latex-faces-present-p 'font-latex-math-face))
      (end-of-line)

      ;; Test for marginpar:
      (search-forward "\\marginpar{")
      (should (font-latex-faces-present-p 'font-lock-constant-face))
      (end-of-line)

      ;; Test for unfontified macro:
      (search-forward "\\seda")
      (should (font-latex-faces-present-p 'font-latex-sedate-face))
      (end-of-line)

      ;; Test for font specifiers:
      (search-forward "\\textrm{")
      (should (font-latex-faces-present-p 'font-lock-type-face))
      (end-of-line)
      (search-forward "\\textbf{")
      (should (font-latex-faces-present-p 'font-latex-bold-face))
      (end-of-line)
      (search-forward "\\textit{")
      (should (font-latex-faces-present-p 'font-latex-italic-face))
      (end-of-line)
      (search-forward "\\textit{\\textbf{")
      (should (font-latex-faces-present-p '(font-latex-italic-face
                                            font-latex-bold-face)))
      (end-of-line)
      (search-forward "\\emph{")
      (should (font-latex-faces-present-p 'font-latex-italic-face))
      (end-of-line)
      (search-forward "\\ttfam")
      (should (font-latex-faces-present-p 'font-lock-type-face))
      (search-forward "\\ttfamily ")
      (should (font-latex-faces-present-p 'font-lock-type-face))
      (end-of-line)
      (search-forward "\\itsha")
      (should (font-latex-faces-present-p 'font-latex-italic-face))
      (search-forward "\\itshape ")
      (should (font-latex-faces-present-p 'font-latex-italic-face))
      (end-of-line)
      (search-forward "\\bfseri")
      (should (font-latex-faces-present-p 'font-latex-bold-face))
      (search-forward "\\bfseries ")
      (should (font-latex-faces-present-p 'font-latex-bold-face))
      (end-of-line)

      ;; Test for math environments:
      (re-search-forward "\\\\be\\(?1:g\\)in{ma\\(?2:t\\)h}")
      (should (font-latex-faces-present-p 'font-lock-keyword-face
                                          (match-beginning 1)))
      (should (font-latex-faces-present-p 'font-lock-function-name-face
                                          (match-beginning 2)))
      (forward-line)
      (skip-chars-forward "[:blank:]")
      (should (font-latex-faces-present-p 'font-latex-math-face))
      (LaTeX-find-matching-end)

      (search-forward "\\[")
      (forward-line)
      (skip-chars-forward "[:blank:]")
      (should (font-latex-faces-present-p 'font-latex-math-face))

      (search-forward "\\[")
      (should (font-latex-faces-present-p 'font-latex-math-face))
      (end-of-line)

      (search-forward "\\begin{displaymath}")
      (forward-line)
      (skip-chars-forward "[:blank:]")
      (should (font-latex-faces-present-p 'font-latex-math-face))
      (LaTeX-find-matching-end)

      (search-forward "\\begin{equation}")
      (forward-line)
      (skip-chars-forward "[:blank:]")
      (should (font-latex-faces-present-p 'font-latex-math-face))
      (LaTeX-find-matching-end)

      ;; Test for misc. environments:
      (search-forward "\\begin{verbatim}")
      (forward-line)
      (should (font-latex-faces-present-p 'font-latex-verbatim-face))
      (LaTeX-find-matching-end)

      ;; Check the fontification of \item macro itself:
      (re-search-forward "\\\\it\\(?1:e\\)m\\[")
      (should (font-latex-faces-present-p 'font-lock-keyword-face
                                          (match-beginning 1)))
      ;; Now for the optional argument:
      (should (font-latex-faces-present-p 'font-lock-variable-name-face))
      (LaTeX-find-matching-end)

      ;; Test for boxing commands
      (re-search-forward "\\\\news\\(?1:a\\)vebox")
      (should (font-latex-faces-present-p 'font-lock-keyword-face
                                          (match-beginning 1)))
      (re-search-forward "\\\\mys\\(?1:a\\)vebox")
      (should (font-latex-faces-present-p 'font-lock-function-name-face
                                          (match-beginning 1)))
      ;; Test for the fontification in braces '{\mysavebox}':
      (re-search-forward "{\\\\mys\\(?1:a\\)ve")
      (should (font-latex-faces-present-p 'font-lock-function-name-face
                                          (match-beginning 1)))
      ;; Now for the optionals arguments of '\savebox':
      (re-search-forward "\\[\\(?1:[^]]+\\)\\]\\[\\(?2:[^]]+\\)\\]{")
      (should (font-latex-faces-present-p 'font-lock-variable-name-face
                                          (match-beginning 1)))
      (should (font-latex-faces-present-p 'font-lock-variable-name-face
                                          (match-beginning 2)))
      (should (font-latex-faces-present-p 'font-lock-function-name-face
                                          (match-end 0)))
      (end-of-line)

      ;; Test for the optional arguments of '\savebox' which look
      ;; different for 'picture' environments:
      (re-search-forward "(\\(?1:.\\),\\(?2:.\\))\\[\\(?3:[^]]+\\)\\]{")
      (should (font-latex-faces-present-p 'font-lock-variable-name-face
                                          (match-beginning 1)))
      (should (font-latex-faces-present-p 'font-lock-variable-name-face
                                          (match-beginning 2)))
      (should (font-latex-faces-present-p 'font-lock-variable-name-face
                                          (match-beginning 3)))
      (should (font-latex-faces-present-p 'font-lock-function-name-face
                                          (match-end 0)))

      ;; Test for \parbox and its arguments:
      (re-search-forward
       "\\\\p\\(?1:a\\)rbox\\[\\(?2:[^]]+\\)\\]\\[\\(?3:[^]]+\\)\\]\\[\\(?4:[^]]+\\)\\]{")
      (should (font-latex-faces-present-p 'font-lock-keyword-face
                                          (match-beginning 1)))
      (should (font-latex-faces-present-p 'font-lock-variable-name-face
                                          (match-beginning 2)))
      (should (font-latex-faces-present-p 'font-lock-variable-name-face
                                          (match-beginning 3)))
      (should (font-latex-faces-present-p 'font-lock-variable-name-face
                                          (match-beginning 4)))
      (should (font-latex-faces-present-p 'font-lock-function-name-face
                                          (match-end 0)))  )))

(ert-deftest font-latex-shortvrb-chars ()
  "Test fontification within delimiters defined by `LaTeX-shortvrb-chars'."
  (with-temp-buffer
    (let ((TeX-install-font-lock #'font-latex-setup)
          (LaTeX-shortvrb-chars '(?| ?\"))
          (TeX-parse-self t))
      (insert "\
\\documentclass{article}
\\usepackage{shortvrb}
\\begin{document}
foo |xyz\\| bar
foo \"xyz\\\" bar
\\end{document}")
      (LaTeX-mode)
      (TeX-update-style t)
      ;; See https://lists.gnu.org/archive/html/auctex-devel/2023-04/msg00011.html
      (syntax-ppss-flush-cache (point-min))
      (font-lock-ensure)
      (goto-char (point-min))
      (re-search-forward "^f" nil t)
      (should-not (get-text-property (point) 'face))
      (search-forward "|" nil t)
      ;; This is the `|' char:
      (should (font-latex-faces-present-p 'font-latex-verbatim-face
                                          (1- (point))))
      ;; This is the `x' char:
      (should (font-latex-faces-present-p 'font-latex-verbatim-face))
      (search-forward "|" nil t)
      ;; This is the `\' char:
      (should (font-latex-faces-present-p 'font-latex-verbatim-face
                                          (- (point) 2)))
      ;; This is the `|' char:
      (should (font-latex-faces-present-p 'font-latex-verbatim-face
                                          (1- (point))))
      (search-forward "ba" nil t)
      (should-not (get-text-property (point) 'face))

      (re-search-forward "^f" nil t)
      (should-not (get-text-property (point) 'face))
      (search-forward "\"" nil t)
      ;; This is the `"' char:
      (should (font-latex-faces-present-p 'font-latex-verbatim-face
                                          (1- (point))))
      ;; This is the `x' char:
      (should (font-latex-faces-present-p 'font-latex-verbatim-face))
      (search-forward "\"" nil t)
      ;; This is the `\' char:
      (should (font-latex-faces-present-p 'font-latex-verbatim-face
                                          (- (point) 2)))
      ;; This is the `"' char:
      (should (font-latex-faces-present-p 'font-latex-verbatim-face
                                          (1- (point))))
      (search-forward "ba" nil t)
      (should-not (get-text-property (point) 'face)))))

(ert-deftest font-latex-verb-macros-with-braces ()
  "Test fontification for verb macros with argument in braces."
  (with-temp-buffer
    (let ((TeX-install-font-lock #'font-latex-setup)
          (TeX-parse-self t))
      (insert "\
\\documentclass{article}
\\usepackage{fvextra}
\\usepackage{hyperref}
\\begin{document}
foo \\Verb[commandchars=\\\\\\{\\}]{Pre \fbox{Middle} Post} bar
foo \\Verb{w{o}r{k}s} bar
foo \\Verb{b{r}eak{s}} bar
foo \\href[ismap=false]{text \\cmd{test} text}{more text} bar
foo \\path{C:\\path\\to\\} bar
\\end{document}")
      (LaTeX-mode)
      (TeX-update-style t)
      (syntax-ppss-flush-cache (point-min))
      (font-lock-ensure)
      (goto-char (point-min))

      (re-search-forward "^f" nil t)
      (should-not (get-text-property (point) 'face))
      (search-forward "commandc")
      (should (font-latex-faces-present-p 'font-lock-variable-name-face))
      (search-forward "Mid")
      (should (font-latex-faces-present-p 'font-latex-verbatim-face))
      (search-forward "Po")
      (should (font-latex-faces-present-p 'font-latex-verbatim-face))
      (search-forward "ba")
      (should-not (get-text-property (point) 'face))

      (re-search-forward "^f" nil t)
      (should-not (get-text-property (point) 'face))
      (search-forward "k")
      (should (font-latex-faces-present-p 'font-latex-verbatim-face))
      (search-forward "ba")
      (should-not (get-text-property (point) 'face))

      (re-search-forward "^f" nil t)
      (should-not (get-text-property (point) 'face))
      (search-forward "s")
      (should (font-latex-faces-present-p 'font-latex-verbatim-face))
      (search-forward "ba")
      (should-not (get-text-property (point) 'face))

      (re-search-forward "^f" nil t)
      (should-not (get-text-property (point) 'face))
      (search-forward "ismap")
      (should (font-latex-faces-present-p 'font-lock-variable-name-face))
      (search-forward "text")
      (should (font-latex-faces-present-p 'font-latex-verbatim-face))
      (search-forward "cmd{t")
      (should (font-latex-faces-present-p 'font-latex-verbatim-face))
      (search-forward "text")
      (should (font-latex-faces-present-p 'font-latex-verbatim-face))
      (search-forward "more")
      (should (font-latex-faces-present-p 'font-lock-constant-face))
      (search-forward "ba")
      (should-not (get-text-property (point) 'face))

      (re-search-forward "^f" nil t)
      (should-not (get-text-property (point) 'face))
      (search-forward "C:")
      (should (font-latex-faces-present-p 'font-latex-verbatim-face))
      (search-forward "to")
      (should (font-latex-faces-present-p 'font-latex-verbatim-face))
      (search-forward "ba")
      (should-not (get-text-property (point) 'face)))))

;;; font-latex-test.el ends here
