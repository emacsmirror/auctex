;;; This file is only used for installing AUCTeX.  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2021  Free Software Foundation, Inc.

;;; It is not a part of AUCTeX itself.

;; Make sure we get the right files.

(let ((auctex-dir (file-name-directory load-file-name)))
  (add-to-list 'load-path auctex-dir)
  (add-to-list 'load-path (expand-file-name "style" auctex-dir))
  (setq TeX-lisp-directory "<none>"
        TeX-auto-global "<none>"))
