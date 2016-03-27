;;; tikz.el --- AUCTeX style for `tikz.sty'

;; Copyright (C) 2016 Free Software Foundation, Inc.

;; Author: Matthew Leach <matthew@mattleach.net>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2016-22-03
;; Keywords: tex tikz

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

;;; Commentary:

;; This file adds some support for `tikz.sty'

;;; Code:

(defun TeX-TikZ-get-opt-arg-string (arg &optional open close)
  "Return a string for optional arguments.
If ARG is nil or \"\", return \"\".  Otherwise return \"OPEN ARG
CLOSE\". If OPEN or CLOSE are nil, set them to `LaTeX-optop' and
`LaTeX-optcl' respectively."
  (unless (or open close)
    (setq open LaTeX-optop)
    (setq close LaTeX-optcl))
  (if (and arg (> (length arg) 0))
      (concat open arg close)
    ""))

(defun TeX-TikZ-arg-rect-point (_ignored)
  "Prompt the user for a point on the Cartesian plane.
Ask the user for an X and Y coordinate, and return the string
\"(X,Y)\"."
  (let ((x (TeX-read-string (TeX-argument-prompt nil nil "X-coordinate")))
        (y (TeX-read-string (TeX-argument-prompt nil nil "Y-coordinate"))))
   (concat " (" x ", " y") ")))

(defun TeX-TikZ-arg-polar-point (_ignored)
  "Prompt the user for a point on the polar plane.
Ask the user for r and theta values, and return the string
\"(THETA:R)\"."
  (let ((r (TeX-read-string (TeX-argument-prompt nil nil "R")))
        (theta (TeX-read-string (TeX-argument-prompt nil nil "Theta"))))
   (concat " (" theta ":" r ") ")))

(defun TeX-TikZ-arg-options (optional)
  "Prompt the user for options to a TikZ macro.
If OPTIONAL is non-nil, always return `LaTeX-optop' and
`LaTeX-optcl', even if the user doesn't provide any input."
  (let ((options (TeX-read-string (TeX-argument-prompt optional nil "Options" ))))
    (if optional
        (TeX-TikZ-get-opt-arg-string options)
      (concat LaTeX-optop options LaTeX-optcl))))

(defun TeX-TikZ-arg-name (optional)
  "Prompt the user for a TikZ name.
If OPTIONAL is non-nil, always return \"()\", even if the user
doesn't provide any input."
  (let ((name (TeX-read-string (TeX-argument-prompt optional nil "Name" ))))
    (if optional
        (TeX-TikZ-get-opt-arg-string name "(" ")")
      (concat "(" name ")"))))

(defun TeX-TikZ-arg-text (optional)
  "Prompt the user for TikZ text.
If OPTIONAL is non-nil always return `TeX-grop' and `TeX-grcl',
even if the user doesn't provide any input."
  (let ((text (TeX-read-string (TeX-argument-prompt optional nil "Text" ))))
    (if optional
        (TeX-TikZ-get-opt-arg-string text TeX-grop TeX-grcl)
      (concat TeX-grop text TeX-grcl))))

(defun TeX-TikZ-arg-node (_ignored)
  "Prompt the user for the deatils of a node.
Ask the user for the name and text for a node and return the
string \"node[OPTIONS](NAME){TEXT}\"."
  (let ((options (TeX-TikZ-arg-options t))
        (name (TeX-TikZ-arg-name t))
        (text (TeX-TikZ-arg-text nil)))
    (concat "node" options name text " ")))

(defun TeX-TikZ-get-arg-type (types &optional prompt)
  "Prompt the user for an argument type.
TYPES is a list of possible types that the user can specify.  If
PROMPT is non-nil use that prompt instead."
  (let ((prompt (if prompt
                    prompt
                  "Next argument type (RET to finish): ")))
    (completing-read prompt types nil t)))

(defun TeX-TikZ-single-macro-arg (function-alist &optional prompt)
  "Prompt the user for a single argument to compose a TikZ macro.
FUNCTION-ALIST is a mapping of argument-types to functions.  The
user is prompted for the argument type, the chosen function is
then called and the value returned.  PROMPT is used as the prompt
for the argument type."
  (let* ((argument-types (mapcar 'car function-alist))
         (argument-type (TeX-TikZ-get-arg-type argument-types prompt)))
    (funcall
     (cadr (assoc argument-type function-alist))
     argument-type)))


(defun TeX-TikZ-macro-arg (function-alist)
  "Prompt the user for arguments to compose a TikZ macro.
FUNCTION-ALIST is a mapping of argument-types to functions.  The
user is repeatedly prompted for the next argument-type; they can
choose form the cars in FUNCTION-ALIST and the appropriate
function is then called.  If the user enters \"\", then the macro
is finished."
  (let* ((options (TeX-TikZ-arg-options t))
         ;; For the iterative version, we need to add "" to the
         ;; function-alist, allowing the user to end the macro.
         (function-alist-iterative `(,@function-alist ("" identity)))
         (string-to-insert (TeX-TikZ-single-macro-arg function-alist-iterative)))

    ;; Insert the macro options.
    (insert options " ")

    ;; Iteratively prompt the user for TikZ's arguments until "" is
    ;; returned.
    (while (not (string= string-to-insert ""))
      (insert string-to-insert)
      (setq string-to-insert
            (TeX-TikZ-single-macro-arg function-alist-iterative)))

    ;; Finish the macro.
    (insert ";")))

(defconst TeX-TikZ-draw-arg-function-map
  '(("Rect Point" TeX-TikZ-arg-rect-point)
    ("Polar Point" TeX-TikZ-arg-polar-point)
    ("Node" TeX-TikZ-arg-node)
    ("--" identity)
    ("-+" identity))
  "An alist of argument type names to their respecitve functions
  for TikZ's \draw macro.")

(defun TeX-TikZ-draw-arg (optional)
  (TeX-TikZ-macro-arg TeX-TikZ-draw-arg-function-map))

(TeX-add-style-hook
 "tikz"
 (lambda ()
   (TeX-add-symbols
    '("draw" (TeX-TikZ-draw-arg)))
   (LaTeX-add-environments
    '("tikzpicture"))))
