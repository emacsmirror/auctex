;;; context.el --- Support for ConTeXt documents.
;; 
;; Maintainer: Patrick Gundlach <pg@levana.de>
;; Version: 11.14
;; Keywords: wp
;; X-URL: http://www.nongnu.org/auctex/

;; Copyright 2003 Patrick Gundlach
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;; Notes:

;;; This is the very basic context support for auctex. It will be
;;; extended in the near future. 

;; Auctex is closely intervowen with LaTeX. We have to split up
;; things without breaking 'em. 

;;; Code:

(defun context-mode ()
  "Major mode for editing files of input for ConTeXt.

Special commands:
\\{TeX-mode-map}
 
Entering context-mode calls the value of text-mode-hook,
then the value of TeX-mode-hook, and then the value
of context-mode-hook."
  (interactive)
  (plain-TeX-common-initialization)
  (setq mode-name "ConTeXt") 
  (setq major-mode 'context-mode) 
  (setq TeX-command-default "ConTeXt Interactive")
  (setq TeX-sentinel-default-function 'TeX-ConTeXt-sentinel)
  (run-hooks 'text-mode-hook 'TeX-mode-hook 'ConTeXt-mode-hook))

(defun TeX-ConTeXt-sentinel (process name)
  "Cleanup TeX output buffer after running ConTeXt."
  (cond ((TeX-TeX-sentinel-check process name))
	((save-excursion 
	   ;; in a full ConTeXt run there will multiple texutil
	   ;; outputs. Just looking for "another run needed" would
	   ;; find the first occurence
	   (goto-char (point-max))
	   (re-search-backward "TeXUtil " nil t)
	   (re-search-forward "another run needed" nil t))
	 (message (concat "You should run ConTeXt again "
			  "to get references right, "
                          (TeX-current-pages)))
	 (setq TeX-command-next TeX-command-default))
	((re-search-forward "removed files :" nil t)
	 (message "sucessfully cleaned up"))
	((re-search-forward "^ TeX\\(Exec\\|Util\\)" nil t) ;; strange regexp --pg
	 (message (concat name ": successfully formatted "
			  (TeX-current-pages)))
	 (setq TeX-command-next TeX-command-Show))
	(t
	 (message (concat name ": problems after "
			  (TeX-current-pages)))
	 (setq TeX-command-next TeX-command-default))))
