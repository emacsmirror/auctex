;;; swedish.el --- Setup AUCTeX for editing Swedish text.  -*- lexical-binding: t; -*-

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

;; Apparently the Swedes use ''this style'' quotations.

(require 'tex)
(require 'latex)

(TeX-add-style-hook
 "swedish"
 (lambda ()
   (unless (eq (car TeX-quote-language) 'override)
     (setq TeX-quote-language
           `("swedish" "''" ,TeX-close-quote ,TeX-quote-after-quote)))
   (setq LaTeX-babel-hyphen-language "swedish")
   (run-hooks 'TeX-language-sv-hook))
 TeX-dialect)
