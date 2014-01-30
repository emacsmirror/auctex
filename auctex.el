;;; auctex.el --- Integrated environment for *TeX*

;; Version: 11.87.2
;; URL: http://www.gnu.org/software/auctex/

;;; Commentary:

;; This can be used for starting up AUCTeX.  The following somewhat
;; strange trick causes tex-site.el to be loaded in a way that can be
;; safely undone using (unload-feature 'tex-site).

;;; Code:

(autoload 'TeX-load-hack
  (expand-file-name "tex-site.el"
                    (file-name-directory load-file-name)))
(TeX-load-hack)
