;;; latex-test.el --- tests for LaTeX mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2022  Free Software Foundation, Inc.

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

;; We need to ensure that font-lock has put the syntax properties
;; already which won't happen in batch mode.  So trigger font-lock
;; immediately.
(define-advice LaTeX-common-initialization (:after ())
  (font-lock-ensure))

(AUCTeX-set-ert-path
 'LaTeX-indent-tabular-test/in
 "tabular-in.tex"
 'LaTeX-indent-tabular-test/out
 "tabular-out.tex"
 'LaTeX-filling/in
 "latex-filling-in.tex"
 'LaTeX-filling/out
 "latex-filling-out.tex"
 'LaTeX-comment-filling/in
 "latex-comment-filling-in.tex"
 'LaTeX-comment-filling/out
 "latex-comment-filling-out.tex"
 'LaTeX-math-indent/in
 "math-indent-in.tex"
 'LaTeX-math-indent/out
 "math-indent-out.tex"
 'tabular-count-ampersands/in
 "tabular-count-ampersands-in.tex"
 'tabular-count-ampersands/out
 "tabular-count-ampersands-out.tex"
 'LaTeX-conditionals-indent/in
 "conditionals-indent-in.tex"
 'LaTeX-conditionals-indent/out
 "conditionals-indent-out.tex"
 'LaTeX-nested-indent/in
 "nested-indent-in.tex"
 'LaTeX-nested-indent/out
 "nested-indent-out.tex"
 'docTeX/in
 "doctex-indent-in.dtx"
 'docTeX/out
 "doctex-indent-out.dtx")

;; Test for detecting \& in a table cell added; see
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=26010
;; Test for missing & in row added; see
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=26032
(ert-deftest LaTeX-indent-tabular ()
  (should (string=
           (with-temp-buffer
             (insert-file-contents LaTeX-indent-tabular-test/in)
             (LaTeX-mode)
             (let ((TeX-parse-self t))
               (TeX-update-style t))
             (indent-region (point-min) (point-max))
             (buffer-string))
           (with-temp-buffer
             (insert-file-contents LaTeX-indent-tabular-test/out)
             (buffer-string)))))

;; Another test for indentation, but for math mode, see
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=20227 Let's keep those tests
;; separated so it would be easier to find the culprit of a future failure.
(ert-deftest LaTeX-math-indent ()
  (should (string=
           (with-temp-buffer
             (insert-file-contents LaTeX-math-indent/in)
             (LaTeX-mode)
             (indent-region (point-min) (point-max))
             (buffer-string))
           (with-temp-buffer
             (insert-file-contents LaTeX-math-indent/out)
             (buffer-string)))))

;; Test for flush-left type indentation.  The begin/end line of
;; verbatim-like environments and comment-like environments (provided
;; by comment.sty) must be flush left.
;; We also test the indent inside these environments and after them.
(ert-deftest LaTeX-flush-left-indent ()
  (with-temp-buffer
    (LaTeX-mode)
    (let ((LaTeX-verbatim-environments
           '("verbatim" "verbatim*" "filecontents" "filecontents*"))
          (LaTeX-comment-env-list '("comment")))

      ;; Test 1: verbatim environment
      (insert "\
\\begin{itemize}
\\item abc")
      (LaTeX-insert-environment "verbatim")
      ;; Check indent inside verbatim env.
      (should (= (current-column) 0))
      ;; Check indent of verbatim env itself.
      (should (string= "\
\\begin{itemize}
\\item abc
\\begin{verbatim}

\\end{verbatim}"
                       (buffer-string)))
      ;; Check indent after verbatim env.
      (goto-char (point-max))
      (newline-and-indent)
      (should (= (current-column) 2))

      ;; Test 2: comment environment
      (erase-buffer)
      (insert "\
\\begin{itemize}
\\item abc")
      (LaTeX-insert-environment "comment")
      ;; Check indent inside comment env.
      (should (= (current-column) 2))
      ;; Check indent of comment env itself.
      (should (string= "\
\\begin{itemize}
\\item abc
\\begin{comment}
\s\s
\\end{comment}"
                       (buffer-string)))
      ;; Check indent after comment env.
      (goto-char (point-max))
      (newline-and-indent)
      (should (= (current-column) 2)))))

;; bug#48518 Test indent in nested environments
(ert-deftest LaTeX-indent-nested-envs ()
  (should (string=
           (with-temp-buffer
             (insert-file-contents LaTeX-nested-indent/in)
             (LaTeX-mode)
             (let ((TeX-parse-self t))
               (TeX-update-style t))
             (search-forward "a^2 + b^2 = c^2")
             (set-mark (match-beginning 0))
             (activate-mark)
             (LaTeX-insert-environment "equation")
             (buffer-string))
           (with-temp-buffer
             (insert-file-contents LaTeX-nested-indent/out)
             (buffer-string)))))

;; Test LaTeX code with math modes is indented as expected.  This has mostly to
;; do with the value of `LaTeX-fill-break-at-separators' and how
;; `LaTeX-fill-move-to-break-point' handles it.  If the test fails, try to look
;; there.  The second part of the test looks for unambiguousness of
;; macros starting a paragraph
;; (https://lists.gnu.org/archive/html/auctex/2017-03/msg00009.html)
(ert-deftest LaTeX-filling ()
  (should (string=
           (with-temp-buffer
             (insert-file-contents LaTeX-filling/in)
             (LaTeX-mode)
             (let ((fill-column 70)
                   (LaTeX-shortvrb-chars '(?\"))
                   (TeX-parse-self t))
               (TeX-update-style t)
               (search-forward "Lorem")
               (fill-paragraph)

               (let ((cmds '("captionsetup" "caption"
                             "parencite"    "par")))
                 (dolist (cmd cmds)
                   (search-forward (concat "\\" cmd))
                   (save-excursion
                     (end-of-line 0)
                     (fill-paragraph))))

               (while (search-forward "% bug#" nil t)
                 (TeX-forward-comment-skip 1)
                 (fill-paragraph)))
             (buffer-string))
           (with-temp-buffer
             (insert-file-contents LaTeX-filling/out)
             (buffer-string)))))

;; Test for comment filling, especially with
;; `LaTeX-syntactic-comments' which is t by default.
(ert-deftest LaTeX-comment-filling ()
  (should (string=
           (with-temp-buffer
             (insert-file-contents LaTeX-comment-filling/in)
             (LaTeX-mode)
             (let ((fill-column 70)
                   (code-comment-test nil))
               (fill-paragraph)
               (while (= 0 (forward-line 1))
                 (when (looking-at "% Code comments.")
                   (setq code-comment-test t))
                 (when code-comment-test
                   (LaTeX-back-to-indentation 'inner))
                 (fill-paragraph)))
             (buffer-string))
           (with-temp-buffer
             (insert-file-contents LaTeX-comment-filling/out)
             (buffer-string)))))

;; Test for bug#19281 (https://debbugs.gnu.org/cgi/bugreport.cgi?bug=19281):
;; make sure AUCTeX is able to insert and modify an environment containing a
;; TeX-esc and braces in its name.
(ert-deftest LaTeX-change-environment-with-esc ()
  (should (string=
           (with-temp-buffer
             (LaTeX-mode)
             (LaTeX-insert-environment (concat TeX-esc "foo{bar}"))
             (LaTeX-modify-environment "foobar")
             (buffer-string))
           (with-temp-buffer
             (LaTeX-mode)
             (LaTeX-insert-environment "foobar")
             (buffer-string)))))

;; Test for inserting &'s with `M-RET' in various tabular environment.
;; Following styles are loaded: tabularx, tabulary, longtable,
;; dcolumn, siunitx
(ert-deftest LaTeX-count-ampersands-inserted-in-tabular ()
  (should (string=
           (with-temp-buffer
             (insert-file-contents tabular-count-ampersands/in)
             (setq TeX-parse-self t)
             (LaTeX-mode)
             (goto-char (point-min))
             ;; Do not ask for opt. argument in (TeX-insert-macro "\\"):
             (let ((TeX-insert-macro-default-style 'mandatory-args-only))
               (while (search-forward "LaTeX-insert-item" nil t)
                 (LaTeX-insert-item)))
             (buffer-string))
           (with-temp-buffer
             (insert-file-contents tabular-count-ampersands/out)
             (LaTeX-mode)
             (buffer-string)))))

(ert-deftest LaTeX-addbibresource ()
  "Check parsing of bibliography files added with addbibresource.

In particular, make sure dots are treated correctly and only the
last extension is stripped."
  (should
   (equal
    (with-temp-buffer
      (insert "\\addbibresource{../foo-1.bar_2.qux3.ext}")
      (LaTeX-mode)
      (let ((TeX-parse-self t))
        (TeX-update-style t))
      (LaTeX-bibliography-list))
    '(("../foo-1.bar_2.qux3")))))

(ert-deftest LaTeX-auto-class-regexp ()
  "Check parsing optional argument with comment correctly.

Test against RequirePackage."
  (with-temp-buffer
    (insert "\\RequirePackage[
backend=biber % here is a comment
]{biblatex}
")
    (latex-mode)
    (let ((TeX-parse-self t))
      (TeX-update-style t))
    (should (member "biblatex" (TeX-style-list)))
    (should (LaTeX-provided-package-options-member
             "biblatex" "backend=biber"))))

(ert-deftest LaTeX-includegraphics-extensions ()
  "Check correct extensions are generated accoding to `TeX-engine'."
  ;; Emacs 26.1 has a bug in byte compile optimization, which makes
  ;; compiled `LaTeX-includegraphics-extensions-list' to return wrong
  ;; value when `TeX-engine' is neither `default', `xetex' nor
  ;; `luatex'.
  ;; c.f. https://debbugs.gnu.org/cgi/bugreport.cgi?bug=31718
  :expected-result (if (and (= emacs-major-version 26)
                            (= emacs-minor-version 1))
                       :failed
                     :passed)
  (with-temp-buffer
    (LaTeX-mode)
    (TeX-load-style "graphicx")
    (let (TeX-engine TeX-PDF-mode TeX-PDF-from-DVI
                     TeX-PDF-via-dvips-ps2pdf TeX-DVI-via-PDFTeX)
      ;; tests for default engine
      (setq TeX-engine 'default)
      ;; default 1
      (setq TeX-PDF-mode t
            TeX-PDF-from-DVI nil
            TeX-DVI-via-PDFTeX nil)
      (should
       (equal (sort (LaTeX-includegraphics-extensions-list) #'string<)
              (sort '("png" "pdf" "jpe?g" "jbig2" "jb2" "mps"
                      "PNG" "PDF" "JPE?G" "JBIG2" "JB2" "eps") #'string<)))
      ;; default 2
      (setq TeX-PDF-mode t
            TeX-PDF-from-DVI "Dvips"
            TeX-DVI-via-PDFTeX nil)
      (should
       (equal (sort (LaTeX-includegraphics-extensions-list) #'string<)
              (sort '("eps" "mps" "EPS") #'string<)))
      ;; default 3
      (setq TeX-PDF-mode nil
            TeX-PDF-from-DVI nil
            TeX-DVI-via-PDFTeX nil)
      (should
       (equal (sort (LaTeX-includegraphics-extensions-list) #'string<)
              (sort '("eps" "mps" "EPS") #'string<)))
      ;; default 4
      (setq TeX-PDF-mode nil
            TeX-PDF-from-DVI nil
            TeX-DVI-via-PDFTeX t)
      (should
       (equal (sort (LaTeX-includegraphics-extensions-list) #'string<)
              (sort '("png" "pdf" "jpe?g" "jbig2" "jb2" "mps"
                      "PNG" "PDF" "JPE?G" "JBIG2" "JB2" "eps") #'string<)))
      ;; default 5
      (setq TeX-PDF-mode t
            TeX-PDF-from-DVI "Dvipdfmx"
            TeX-DVI-via-PDFTeX nil)
      (should
       (equal (sort (LaTeX-includegraphics-extensions-list) #'string<)
              (sort '("eps" "mps" "EPS" "jpe?g" "pdf" "png") #'string<)))

      ;; tests for luatex engine
      (setq TeX-engine 'luatex)
      ;; luatex 1
      (setq TeX-PDF-mode t)
      (should
       (equal (sort (LaTeX-includegraphics-extensions-list) #'string<)
              (sort '("png" "pdf" "jpe?g" "jbig2" "jb2" "mps"
                      "PNG" "PDF" "JPE?G" "JBIG2" "JB2" "eps") #'string<)))
      ;; luatex 2
      (setq TeX-PDF-mode nil)
      (should
       (equal (sort (LaTeX-includegraphics-extensions-list) #'string<)
              (sort '("eps" "mps" "EPS") #'string<)))

      ;; test for xetex engine
      (setq TeX-engine 'xetex)
      (should
       (equal (sort (LaTeX-includegraphics-extensions-list) #'string<)
              (sort '("pdf" "eps" "mps" "ps" "png" "jpe?g" "jp2" "jpf"
                      "PDF" "EPS" "MPS" "PS" "PNG" "JPE?G" "JP2" "JPF"
                      "bmp" "pict" "psd" "mac" "tga" "gif" "tif" "tiff"
                      "BMP" "PICT" "PSD" "MAC" "TGA" "GIF" "TIF" "TIFF")
                    #'string<)))

      ;; test for other engine
      (setq TeX-engine 'omega)
      ;; other 1
      (setq TeX-PDF-mode t
            TeX-PDF-from-DVI "Dvipdfmx")
      (should
       (equal (sort (LaTeX-includegraphics-extensions-list) #'string<)
              (sort '("eps" "mps" "EPS" "jpe?g" "pdf" "png") #'string<)))
      ;; other 2
      (setq TeX-PDF-mode nil
            TeX-PDF-from-DVI nil)
      (should
       (equal (sort (LaTeX-includegraphics-extensions-list) #'string<)
              (sort '("eps" "jpe?g" "pdf" "png") #'string<))))))

(ert-deftest LaTeX-style-hook-with-class-option ()
  "Check style hooks associated with class option are processed."
  (with-temp-buffer
    (let ((TeX-parse-self t))
      ;; test for dvips option
      ;; This depends on the following code in latex.el:
      ;; (TeX-add-style-hook "dvips"
      ;;                      (lambda ()
      ;;                        (setq TeX-PDF-from-DVI "Dvips"))
      ;;                      :classopt)
      (insert "\\documentclass[dvips]{article}\n")
      (latex-mode)
      (TeX-update-style)
      (should (equal (TeX-PDF-from-DVI) "Dvips"))
      (should (not (member "dvips" TeX-active-styles)))

      ;; test for dvipdfmx option
      (erase-buffer)
      ;; This depends on the following code in latex.el:
      ;; (TeX-add-style-hook "dvipdfmx"
      ;;                      (lambda ()
      ;;                        (TeX-PDF-mode-on)
      ;;                        ;; XeLaTeX normally don't use dvipdfmx
      ;;                        ;; explicitly.
      ;;                        (unless (eq TeX-engine 'xetex)
      ;;                          (setq TeX-PDF-from-DVI "Dvipdfmx")))
      ;;                      :classopt)
      (insert "\\documentclass[dvipdfmx]{article}\n")
      (latex-mode)
      (TeX-update-style)
      (should TeX-PDF-mode)
      (should (equal (TeX-PDF-from-DVI) "Dvipdfmx"))
      (should (not (member "dvipdfmx" TeX-active-styles)))

      ;; dvipdfmx option should not trigger `TeX-PDF-from-DVI' for
      ;; XeLaTeX document
      (latex-mode)
      (let ((TeX-engine 'xetex))
        (TeX-update-style))
      (should TeX-PDF-mode)
      (should (not (TeX-PDF-from-DVI)))
      (should (not (member "dvipdfmx" TeX-active-styles)))

      ;; test for pdftricks option
      (erase-buffer)
      ;; This depends on the following code in latex.el:
      ;; (TeX-add-style-hook "pdftricks" #'TeX-PDF-mode-on :classopt)
      (insert "\\documentclass[pdftricks]{article}\n")
      (latex-mode)
      (TeX-update-style)
      (should TeX-PDF-mode)
      (should (not (member "pdftricks" TeX-active-styles)))

      ;; test for psfrag option
      (erase-buffer)
      ;; This depends on the following code in latex.el:
      ;; (TeX-add-style-hook "psfrag" #'TeX-PDF-mode-off :classopt)
      (insert "\\documentclass[psfrag]{article}\n")
      (latex-mode)
      (TeX-update-style)
      (should (not TeX-PDF-mode))
      (should (not (member "psfrag" TeX-active-styles))))))

(ert-deftest LaTeX-insert-environment-with-active-region ()
  "Check environment is inserted correctly with active region."
  ;; The former codes of `LaTeX-insert-environment' had problems about
  ;; the management of the point and the mark, which sometimes
  ;; resulted in additional empty line, spurious insertion of comment
  ;; prefix, or both.
  (with-temp-buffer
    (let ((transient-mark-mode t)
          (LaTeX-insert-into-comments t))
      (latex-mode)
      (auto-fill-mode 1)

      ;; test 1: for bug#35284
      ;; test 1-1
      (insert "\\begin{document}
% This is a comment
\\def\\foo#1{foo}
% another comment
\\end{document}
")
      (set-mark (line-beginning-position 0)) ; just before \end{document}.
      (goto-char (point-min))
      (beginning-of-line 2) ; just before the first "%".
      (LaTeX-insert-environment "verbatim")
      (should (string=
               (buffer-string)
               "\\begin{document}
% \\begin{verbatim}
% This is a comment
\\def\\foo#1{foo}
% another comment
% \\end{verbatim}
\\end{document}
"))

      ;; test 1-2
      (erase-buffer)
      (insert "\\begin{document}
% This is a comment
\\def\\foo#1{foo}
% another comment
\\end{document}
")
      (set-mark (line-end-position -1)) ; just after "another comment"
      (goto-char (point-min))
      (beginning-of-line 2) ; just before the first "%".
      (LaTeX-insert-environment "verbatim")
      (should (string=
               (buffer-string)
               "\\begin{document}
% \\begin{verbatim}
% This is a comment
\\def\\foo#1{foo}
% another comment
% \\end{verbatim}
\\end{document}
"))

      (setq LaTeX-insert-into-comments nil)

      ;; test 1-3
      (erase-buffer)
      (insert "\\begin{document}
% This is a comment
\\def\\foo#1{foo}
% another comment
\\end{document}
")
      (set-mark (line-beginning-position 0)) ; just before \end{document}
      (goto-char (point-min))
      (beginning-of-line 2) ; just before the first "%".
      (LaTeX-insert-environment "verbatim")
      (should (string=
               (buffer-string)
               "\\begin{document}
\\begin{verbatim}
% This is a comment
\\def\\foo#1{foo}
% another comment
\\end{verbatim}
\\end{document}
"))
      ;; test 2: for
      ;; https://lists.gnu.org/archive/html/auctex/2019-11/msg00009.html

      (setq LaTeX-insert-into-comments t)

      ;; test 2-1
      (erase-buffer)
      (insert "abc def ghi")
      (set-mark 5) ; just before "def"
      (goto-char 8) ; just after "def"
      (LaTeX-insert-environment "center")
      (should (string=
               (buffer-string)
               "abc
\\begin{center}
  def
\\end{center}
ghi"))

      ;; test 2-2
      (erase-buffer)
      (insert "abc
def
ghi")
      (beginning-of-line 0) ; just before "def"
      (set-mark (line-end-position)) ; just after "def"
      (LaTeX-insert-environment "center")
      (should (string=
               (buffer-string)
               "abc
\\begin{center}
  def
\\end{center}
ghi"))

      ;; test 2-3
      (erase-buffer)
      (insert "\\begin{quote}
  % abc
  % def
  % ghi
\\end{quote}
")
      (set-mark (line-beginning-position 0)) ; just before \end{quote}
      (goto-char (point-min))
      (beginning-of-line 2) ; just before the first "%"
      (LaTeX-insert-environment "center")
      (should (string=
               (buffer-string)
               "\\begin{quote}
  % \\begin{center}
  %   abc def ghi
  % \\end{center}
\\end{quote}
"))

      ;; test 2-4
      (erase-buffer)
      (insert "\\begin{quote}
  %\s
  % abc
  % def
  % ghi
  %\s
\\end{quote}
")
      (set-mark (line-end-position -1)) ; just after the last "% "
      (goto-char (point-min))
      (beginning-of-line 2) ; just before the first "  %"
      (LaTeX-insert-environment "center")
      (should (string=
               (buffer-string)
               "\\begin{quote}
  % \\begin{center}
  %   abc def ghi
  % \\end{center}
\\end{quote}
"))

      ;; test 2-5
      (erase-buffer)
      (insert "\\begin{quote}
  %\s
  % abc
  % def
  % ghi
  %\s
\\end{quote}
")
      (set-mark (1- (line-end-position -1))) ; just after the last "%"
      (goto-char (point-min))
      (beginning-of-line 2)
      (forward-char 2) ; just before the first "%"
      (LaTeX-insert-environment "center")
      (should (string=
               (buffer-string)
               "\\begin{quote}
  % \\begin{center}
  %   abc def ghi
  % \\end{center}
\\end{quote}
"))

      (setq LaTeX-insert-into-comments nil)

      ;; test 2-6
      (erase-buffer)
      (insert "\\begin{quote}
  % abc
  % def
  % ghi
\\end{quote}
")
      (set-mark (line-beginning-position 0)) ; just before \end{quote}
      (goto-char (point-min))
      (beginning-of-line 2) ; just before the first "  %"
      (LaTeX-insert-environment "center")
      (should (string=
               (buffer-string)
               "\\begin{quote}
  \\begin{center}
    % abc def ghi
  \\end{center}
\\end{quote}
")))))

(ert-deftest LaTeX-electric-pair-interaction ()
  "Whether `LaTeX-insert-left-brace' is compatible with `electric-pair-mode'."
  (require 'elec-pair)
  (let ((LaTeX-electric-left-right-brace t)
        (orig-mode electric-pair-mode))
    (unwind-protect
        (with-temp-buffer
          ;; Temporally enable electric pair mode, if not enabled
          ;; already.
          (or orig-mode
              (electric-pair-mode 1))
          (latex-mode)

          ;; When `LaTeX-insert-left-brace' supplies right brace,
          ;; `electric-pair-mode' shouldn't come into play.
          (setq last-command-event ?\()
          (LaTeX-insert-left-brace nil)
          (should (string= "()" (buffer-string)))

          (erase-buffer)
          ;; When there is a prefix argument, `LaTeX-insert-left-brace'
          ;; just calls `self-insert-command' and `electric-pair-mode'
          ;; should work.
          (setq last-command-event ?\()
          (LaTeX-insert-left-brace 2)
          (should (string= "(()" (buffer-string))))
      ;; Restore electric pair mode.
      (or orig-mode
          (electric-pair-mode -1)))))

(ert-deftest LaTeX-conditionals-indent ()
  "Test if conditionals are indented correctly.
The code inside the test is randomely taken from source2e.  This
test also sets the variables `TeX-indent-open-delimiters' and
`TeX-indent-close-delimiters' to opening and closing brackets to
check the indentation for optional argument of \\usepackage."
  (should (string=
           (with-temp-buffer
             (insert-file-contents LaTeX-conditionals-indent/in)
             (LaTeX-mode)
             (let ((TeX-indent-open-delimiters "[")
                   (TeX-indent-close-delimiters "]")
                   (TeX-parse-self t))
               (TeX-update-style t)
               (indent-region (point-min) (point-max))
               (buffer-string)))
           (with-temp-buffer
             (insert-file-contents LaTeX-conditionals-indent/out)
             (buffer-string)))))

(ert-deftest docTeX-indentation ()
  "Test if content in docTeX-mode is indented correctly."
  (should (string=
           (with-temp-buffer
             (insert-file-contents docTeX/in)
             (docTeX-mode)
             (let ((TeX-parse-self t))
               (TeX-update-style t)
               (indent-region (point-min) (point-max))
               (whitespace-cleanup)
               (buffer-string)))
           (with-temp-buffer
             (insert-file-contents docTeX/out)
             (buffer-string)))))

;;; latex-test.el ends here
