;; This file contains the autoload of preview-latex
;;
;; The ideal place for this file is in the `site-lisp.d' directory.
(autoload 'LaTeX-preview-setup "preview")
(add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
