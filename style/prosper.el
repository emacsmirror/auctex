;; This file is not part of Emacs

;; Author: Phillip Lord<p.lord@russet.org.uk>
;; Maintainer: Phillip Lord<p.lord@russet.org.uk>
;; Keywords: auctex, style file, prosper.

;; Copyright (C) 2001 by Phillip Lord

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This is a style for which allows auctex (http://auctex.sunsite.dk)
;; to work better with prosper (http://prosper.sourceforge.net).

;;; Installation:
;;
;; For this file to work you need to have a working installation of
;; AucTeX. After that installtion is simple. Put this file into one of
;; the directories specified in `TeX-style-path', with the name
;; "style" rather than "auto" as it might get over written in the
;; latter. 
;;
;; Then stick the current for into your .emacs
;; (eval-after-load "latex"
;;   '(add-to-list 'LaTeX-style-list '("prosper")))
;; 
;; And that should be it. You check whether it's worked or not by
;; opening a prosper document, and trying `LaTeX-environment'. "slide"
;; should be available by tab completion and it should ask you about
;; overlays. 

(TeX-add-style-hook
 "prosper"
 (function
  (lambda()
    (LaTeX-add-environments
     '("slide" LaTeX-prosper-insert-slide)
     '("Itemize" LaTeX-env-item)
     '("itemstep" LaTeX-env-item)   
     )
    (TeX-add-symbols
     '("PDFtransition" TeX-arg-transition-style)
     '("DefaultTransition" TeX-arg-transition-style)
     '("fromSlide" TeX-arg-literal "Number" t)
     '("onlySlide" TeX-arg-literal "Number" t)
     '("untilSlide" TeX-arg-literal "Number" t)
     '("FromSlide" TeX-arg-literal "Number")
     '("OnlySlide" TeX-arg-literal "Number")
     '("UntilSlide" TeX-arg-literal "Number")
     '("overlays" TeX-arg-literal "Number"  t)))))


(defun LaTeX-prosper-insert-slide (environment)
  (if (y-or-n-p "Surround with overlay ?")
      (progn (TeX-insert-macro "overlays")
             (if (search-backward "{" 0 t)
                 (progn
                   (goto-char (+ 1 (point)))
                   (insert "%")))))
  (let ((title (read-input "Title: ")))
    (LaTeX-insert-environment "slide" (concat "{" title "}"))))


(defun TeX-arg-transition-style (environment)
  (TeX-argument-insert
   (completing-read 
    (TeX-argument-prompt nil "Transition" t)
    '(("Split") ("Blinds") ("Box") ("Wipe") 
      ("Dissolve") ("Glitter") ("Replace")))
   nil))

;;; prosper.el ends here
