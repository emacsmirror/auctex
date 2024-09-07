;;; lstautogobble.el --- AUCTeX style for `lstautogobble.sty'  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2024-08-21
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

;;; Commentary:

;; This file adds support for `lstautogobble.sty' v0.1 from 2012-05-03.

;;; Code:

(require 'tex)

(TeX-add-style-hook
 "lstautogobble"
 (lambda ()
   (TeX-run-style-hooks "listings"))
 TeX-dialect)

(defvar LaTeX-lstautogobble-package-options nil
  "Package options for the lstautogobble package.")

;;; lstautogobble.el ends here
