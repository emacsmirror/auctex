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

(defun TeX-TikZ-arg-node (_ignored)
  "Prompt the user for the deatils of a node.
Ask the user for the name and text for a node and return the
string \"node[OPTIONS](NAME){TEXT}\"."
  (let ((options (TeX-read-string (TeX-argument-prompt t nil "Options" )))
        (name (TeX-read-string (TeX-argument-prompt t nil "Name")))
        (text (TeX-read-string (TeX-argument-prompt nil nil "Text"))))
    (concat "node"
            (TeX-TikZ-get-opt-arg-string options)
            (TeX-TikZ-get-opt-arg-string name "(" ")")
            TeX-grop text TeX-grcl " ")))

(defun TeX-TikZ-get-arg-type (types)
  "Prompt the user for the next argument type.
TYPES is a list of possible types that the user can specify."
  (completing-read "Next argument type (RET to finish): " types nil t))

(defun TeX-TikZ-macro-arg (function-alist)
  "Prompt the user for arguments to compose a TikZ macro.
FUNCTION-ALIST is a mapping of argument-types to functions.  The
user is repeatedly prompted for the next argument-type; they can
choose form the cars in FUNCTION-ALIST and the appropriate
function is then called.  If the user enters \"\", then the macro
is finished."
  (let* ((options (TeX-read-string (TeX-argument-prompt t nil "Options")))
         (argument-types `("" ,@(mapcar 'car function-alist)))
         (argument-type (TeX-TikZ-get-arg-type argument-types)))

    ;; Insert the macro options.
    (insert (TeX-TikZ-get-opt-arg-string options)
            " ")

    ;; Iteratively prompt the user for TikZ's arguments until "" is
    ;; returned.
    (while (not (string= argument-type ""))
      (insert (funcall
               (cadr (assoc argument-type TeX-TikZ-draw-arg-function-map))
               argument-type))
      (setq argument-type (TeX-TikZ-get-arg-type argument-types)))

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
