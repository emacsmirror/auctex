;;; ngerman.el --- Setup AUCTeX for editing German text.  -*- lexical-binding: t; -*-

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

;; This file contains specific code for `ngerman' language option
;; provided by `babel-german' bundle.

;;; Code:

(require 'tex)
(require 'latex)

(TeX-add-style-hook
 "ngerman"
 (lambda ()
   (unless (eq (car TeX-quote-language) 'override)
     (setq TeX-quote-language '("ngerman" "\"`" "\"'" t)))
   (setq LaTeX-babel-hyphen-language "ngerman"))
 TeX-dialect)

;;; ngerman.el ends here
