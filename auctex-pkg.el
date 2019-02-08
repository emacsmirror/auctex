;;; auctex-pkg.el --- The ELPA package definition for GNU AUCTeX

;; Copyright (C) 2019 Free Software Foundation, Inc.

;; URL: http://www.gnu.org/software/auctex/
;; Maintainer: auctex-devel@gnu.org
;; Notifications-To: auctex-diffs@gnu.org
;; Keywords: tex

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

(define-package "auctex" "12.1.2" "Integrated environment for *TeX*"
  '((emacs "24.1")
    (cl-lib "0.5"))
  :url "https://www.gnu.org/software/auctex/"
  :keywords '("TeX" "LaTeX" "Texinfo" "ConTeXt" "docTeX" "preview-latex"))

(setq byte-compile-warnings '(not unresolved))
