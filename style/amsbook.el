;;; amsbook.el --- AMS book style hook.

;;; Code:

(TeX-add-style-hook "amsbook"
 (function
  (lambda ()
    (TeX-run-style-hooks "amstex"))))

;;; amsbook.el ends here.
