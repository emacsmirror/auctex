;;; textcomp.el --- AUCTeX style for `textcomp.sty' (v2.0n)  -*- lexical-binding: t; -*-

;; Copyright (C) 2014, 2017, 2020 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2014-10-25
;; Keywords: tex

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any
;; later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file adds support for `textcomp.sty' (v2.0n) from 2020/02/02.
;; With this version, the package mainly no-op as the macros are moved
;; into LaTeX kernel.  `textcomp.sty' is a standard LaTeX package and
;; part of TeXLive.

;;; Code:

(defvar LaTeX-textcomp-package-options
  '("full" "almostfull" "euro" "safe" "error"
    "warn" "info" "quiet" "force")
  "Package options for the textcomp package.")

;;; textcomp.el ends here
