;;; amsart.el --- AMS article style hook.

;;; Code:

(TeX-add-style-hook "amsart"
 (function
  (lambda ()
    (TeX-run-style-hooks "amstex"))))

;;; amsart.el ends here.
