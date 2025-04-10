;;; longtable.el --- AUCTeX style for `longtable.sty'.  -*- lexical-binding: t; -*-

;; Copyright (C) 2013--2025  Free Software Foundation, Inc.

;; Maintainer: auctex-devel@gnu.org
;; Author: Mosè Giordano <mose@gnu.org>
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any
;; later version.

;; AUCTeX is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file adds support for `longtable.sty'.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(defvar LaTeX-longtable-skipping-regexp
  (regexp-opt '("[l]" "[r]" "[c]" ""))
  "Regexp matching between \\begin{longtable} and column specification.
For longtable environments only.")

(defun LaTeX-item-longtable (&optional suppress)
  "Insert line break macro on the last line and suitable number of &'s.
For longtable environments.  If SUPPRESS is non-nil, do not
insert line break macro."
  (unless suppress
    (save-excursion
      (end-of-line 0)
      (just-one-space)
      (TeX-insert-macro "\\")))
  (LaTeX-insert-ampersands
   LaTeX-longtable-skipping-regexp #'LaTeX-array-count-columns))

(defun LaTeX-env-longtable (environment)
  "Insert a longtable-like ENVIRONMENT with caption and label."
  (let* ((pos (and LaTeX-default-position ; `LaTeX-default-position'
                                        ; can be nil, i.e. no prompt
                   (completing-read (TeX-argument-prompt t nil "Position")
                                    '("l" "r" "c")
                                    nil nil LaTeX-default-position)))
         (fmt (TeX-read-string
               (format-prompt "Format" LaTeX-default-format)
               nil nil
               (if (string= LaTeX-default-format "")
                   nil
                 LaTeX-default-format)))
         (caption (TeX-read-string "Caption: "))
         (short-caption (when (>= (length caption) LaTeX-short-caption-prompt-length)
                          (TeX-read-string "(Optional) Short caption: "))))
    (setq LaTeX-default-position pos
          LaTeX-default-format   fmt)
    (LaTeX-insert-environment environment
                              (concat
                               (unless (zerop (length pos))
                                 (concat LaTeX-optop pos LaTeX-optcl))
                               (concat TeX-grop fmt TeX-grcl)))
    ;; top caption -- do nothing if user skips caption
    (unless (zerop (length caption))
      ;; insert `\caption[short-caption]{caption':
      (insert TeX-esc "caption")
      (when (and short-caption (not (string= short-caption "")))
        (insert LaTeX-optop short-caption LaTeX-optcl))
      (insert TeX-grop caption)
      ;; ask for a label and insert it
      (LaTeX-label environment 'environment)
      ;; the longtable `\caption' is equivalent to a `\multicolumn',
      ;; so it needs a `\\' at the end of the line.  Prior to that,
      ;; add } to close `\caption{' and a space:
      (insert TeX-grcl)
      (just-one-space)
      (insert "\\\\")
      ;; fill the caption
      (when auto-fill-function (LaTeX-fill-paragraph))
      ;; Insert a new line and indent
      (LaTeX-newline)
      (indent-according-to-mode))
    ;; Insert suitable number of &'s, suppress line break
    (LaTeX-item-longtable t)))

(TeX-add-style-hook
 "longtable"
 (lambda ()
   (LaTeX-add-environments
    '("longtable" LaTeX-env-longtable))

   (TeX-add-symbols
    ;; Commands to end table rows
    '("endhead" 0)
    '("endfirsthead" 0)
    '("endfoot" 0)
    '("endlastfoot" 0)
    ;; Caption commands
    '("caption*" 1))

   ;; longtable.sty v4.21 provides the macro \LTcaptype (from
   ;; ltcaption.sty) as well.  So check here if ltcaption.el is not
   ;; loaded before adding the entry as well:
   (unless (member "ltcaption" (TeX-style-list))
     (TeX-add-symbols "LTcaptype"))

   ;; These parameters are set with \setlength
   (LaTeX-add-lengths
    "LTleft" "LTright" "LTpre" "LTpost" "LTcapwidth")

   ;; This parameter is set with \setcounter
   (LaTeX-add-counters "LTchunksize")

   ;; Append longtable to `LaTeX-label-alist', in order not to override
   ;; possible custome values.
   (add-to-list 'LaTeX-label-alist '("longtable" . LaTeX-table-label) t)

   ;; Append longtable to `LaTeX-item-list' with `LaTeX-item-longtable'
   (add-to-list 'LaTeX-item-list '("longtable" . LaTeX-item-longtable) t)

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     ;; Actually, `\caption*{}' macro takes only one mandatory argument,
     ;; not an optional one, the following is a workaround to fontify
     ;; correctly also the standard `\caption[]{}' macro.
     (font-latex-add-keywords '(("caption" "*[{"))
                              'textual)))
 TeX-dialect)

;; `longtable.sty' has two options "errorshow" and "pausing", both for
;; debugging purposes.  We ignore them both in order to make package
;; loading faster in a buffer.
(defvar LaTeX-longtable-package-options nil
  "Package options for the longtable package.")

;; longtable.el ends here
