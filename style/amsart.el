;;; amsart.el --- AMS article style hook.

;;; Code:

(TeX-add-style-hook "amsart"
 (function
  (lambda ()
    (TeX-add-symbols
   '("eqref" TeX-arg-label)))))

;;; amsart.el ends here.
