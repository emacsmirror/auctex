(defvar LaTeX-indent-tabular-test/in  (expand-file-name "tabular-in.tex"))
(defvar LaTeX-indent-tabular-test/out (expand-file-name "tabular-out.tex"))

(ert-deftest LaTeX-indent-tabular ()
  (should (string=
           (with-temp-buffer
             (insert-file-contents LaTeX-indent-tabular-test/in)
             (LaTeX-mode)
             (indent-region (point-min) (point-max))
             (buffer-string))
           (with-temp-buffer
             (insert-file-contents LaTeX-indent-tabular-test/out)
             (buffer-string)))))
