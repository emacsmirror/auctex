;;; @ TEX.el - Special code for TeX mode.
;;;
;;; $Id: TEX.el,v 1.1 1993-03-15 18:12:49 amanda Exp $

(require 'tex-init)
(require 'tex-misc)

;;; @@ Hook

(TeX-add-style-hook "TEX"
 (function
  (lambda ()
    (use-local-map TeX-mode-map)
    (setq mode-name "TeX")
    (setq major-mode 'plain-TeX-mode)
    (set-syntax-table TeX-mode-syntax-table)
    (setq paragraph-start
	  (concat
	   "\\(^[ \t]*$"
	   "\\|" (regexp-quote TeX-esc) "par\\|" 
	   "^[ \t]*"
	   (regexp-quote TeX-esc)
	   "\\("
	   "begin\\|end\\|part\\|chapter\\|"
	   "section\\|subsection\\|subsubsection\\|"
	   "paragraph\\|include\\|includeonly\\|"
	   "tableofcontents\\|appendix\\|label\\|caption\\|"
	   "\\[\\|\\]" ; display math delimitors
	   "\\)"
	   "\\|"
	   "^[ \t]*\\$\\$" ; display math delimitor
	   "\\)" ))
    (setq paragraph-separate
	  (concat
	   "\\("
	   (regexp-quote TeX-esc)
	   "par\\|"
	   "^[ \t]*$\\|"
	   "^[ \t]*"
	   (regexp-quote TeX-esc)
	   "\\("
	   "begin\\|end\\|label\\|caption\\|part\\|chapter\\|"
	   "section\\|subsection\\|subsubsection\\|"
	   "paragraph\\|include\\|includeonly\\|"
	   "tableofcontents\\|appendix\\|" (regexp-quote TeX-esc)
	   "\\)"
	   "\\)"))
    (setq comment-start-skip
	  (concat
	   "\\(\\(^\\|[^\\]\\)\\("
	   (regexp-quote TeX-esc)
	   (regexp-quote TeX-esc)
	   "\\)*\\)\\(%+ *\\)"))
    (setq TeX-header-end (regexp-quote "%**end of header"))
    (setq TeX-trailer-start (regexp-quote (concat TeX-esc "bye")))
    (setq TeX-command-default TeX-command-TeX)
    (run-hooks 'plain-TeX-mode-hook))))

;;; @@ Emacs

;;; Local Variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: ";;; @+\\|(......"
;;; End:
