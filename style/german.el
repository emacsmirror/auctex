;;; german.el --- Setup AUCTeX for editing German text.

;;; Commentary:
;;
;; Cater for some specialities of `(n)german.sty', e.g. special quote
;; and hyphen strings or that `"' makes the following letter an
;; umlaut.

;;; Code:

(defvar LaTeX-german-mode-syntax-table
  (copy-syntax-table LaTeX-mode-syntax-table)
  "Syntax table used in LaTeX mode when using `german.sty'.")

(modify-syntax-entry ?\"  "w"  LaTeX-german-mode-syntax-table)
(define-key LaTeX-mode-map "-" 'LaTeX-german-insert-hyphen)

(defvar LaTeX-german-hyphen-internal nil)
(make-variable-buffer-local 'LaTeX-german-hyphen-internal)
(defun LaTeX-german-insert-hyphen (force)
  "Insert a hyphen string.
The string can be either a normal hyphen or the string specified
in `LaTeX-german-hyphen'.  Wether one or the other is chosen
depends on the value of `LaTeX-german-hyphen-after-hyphen' and
the buffer context.
If prefix argument FORCE is non-nil, always insert a regular hyphen."
  (interactive "*P")
  (if (or force (zerop (length LaTeX-german-hyphen-internal)))
      (call-interactively 'self-insert-command)
    ;; FIXME: It would be nice to check for verbatim constructs in the
    ;; non-font-locking case, but things like `LaTeX-current-environment'
    ;; are rather expensive in large buffers.
    (if (or (and (fboundp 'font-latex-faces-present-p)
		 (font-latex-faces-present-p '(font-latex-verbatim-face
					       font-latex-math-face
					       font-lock-comment-face)))
	    (texmathp)
	    (TeX-in-comment))
	(call-interactively 'self-insert-command)
      (let ((hyphen-length (length LaTeX-german-hyphen)))
	(cond
	 ((and (> hyphen-length 1)
	       (not LaTeX-german-hyphen-after-hyphen)
	       (string= (buffer-substring (- (point) hyphen-length) (point))
			LaTeX-german-hyphen))
	  (delete-backward-char hyphen-length)
	  (call-interactively 'self-insert-command))
	 ((and (> hyphen-length 1)
	       LaTeX-german-hyphen-after-hyphen
	       (eq (char-before) ?-))
	  (delete-backward-char 1)
	  (insert LaTeX-german-hyphen))
	 (LaTeX-german-hyphen-after-hyphen
	  (call-interactively 'self-insert-command))
	 (t (insert LaTeX-german-hyphen)))))))

(TeX-add-style-hook
 "german"
 (lambda ()
   (set-syntax-table LaTeX-german-mode-syntax-table)
   (unless (local-variable-p 'TeX-open-quote (current-buffer))
     (make-local-variable 'TeX-open-quote)
     (setq TeX-open-quote LaTeX-german-open-quote))
   (unless (local-variable-p 'TeX-close-quote (current-buffer))
     (make-local-variable 'TeX-close-quote)
     (setq TeX-close-quote LaTeX-german-close-quote))
   (unless (local-variable-p 'TeX-quote-after-quote (current-buffer))
     (make-local-variable 'TeX-quote-after-quote)
     (setq TeX-quote-after-quote LaTeX-german-quote-after-quote))
   (run-hooks 'TeX-language-de-hook)
   (setq LaTeX-german-hyphen-internal LaTeX-german-hyphen)))

;;; german.el ends here
