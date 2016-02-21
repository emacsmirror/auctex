;;; error-parsing.el --- tests for error parsing

;; Copyright (C) 2016 Free Software Foundation, Inc.

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; AUCTeX is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Code:

(require 'ert)
(require 'tex-buf)

(defun AUCTeX-set-ert-path (&rest sym-val)
  "Set first element of SYM-VAL to the next one, and so on.

The value is the path to the test file, make sure it is expanded
in the right directory even when the ERT test is run from the
command line and from another directory."
  (while sym-val
    (set (pop sym-val)
	 (expand-file-name (pop sym-val)
			   (when load-file-name
			     (file-name-directory load-file-name))))))

(AUCTeX-set-ert-path
 'TeX-test-compilation-log
 "compilation-log.txt")

(ert-deftest TeX-error-parsing ()
  "Test error parsing functions."
  (should (equal
	   (with-temp-buffer
	     (setq TeX-debug-warnings t
		   TeX-debug-bad-boxes t)
             (insert-file-contents TeX-test-compilation-log)
             (TeX-parse-all-errors)
	     TeX-error-list)
	   '((warning
	      "./nice-class.cls" 32
	      "Package nice-class Warning: ******************************************"
	      0
	      "Package nice-class Warning: ******************************************
(nice-class)                * THIS IS JUST A WARNING WITH A PESKY
(nice-class)                * UNMATCHED CLOSED PARENTHESIS :-)
(nice-class)                ****************************************** on input line 32.\n"
	      nil 32 nil 376 nil)
	     (error
              "./test.tex" 2
              "Class nice-class Error: ***********************************" 0
              "\n(nice-class)              * This is a very bad error!
(nice-class)              ************************************.

See the suftesi class documentation for explanation.
Type  H <return>  for immediate help.
 ...                                              
                                                  
l.2 \\begin{document}

(/other/packages/loaded.sty)
ABD: EveryShipout initializing macros"
              "\\begin{document}\n\n(/other/packages/loaded.sty)" nil nil 971 nil)
	     (warning "./test.tex" 3
              "Package foo Warning: This is a warning! on input line 3." 0
	      "Package foo Warning: This is a warning! on input line 3.\n"
	      nil 3 nil 1030 nil)
	     (bad-box
	      "./secondary-file.tex" 131
	      "Underfull \\hbox (badness 6608) in paragraph at lines 131--132"
	      0 "\n[]|\\T1/jkpl/m/n/10.95 (+20) Something bla" "bla"
	      132 10 1268 nil)
             (warning "./test.tex" 4
              "LaTeX Warning: Reference `wrong' on page 1 undefined on input line 4."
              0
              "LaTeX Warning: Reference `wrong' on page 1 undefined on input line 4.\n"
              "wrong" 4 nil 1351 nil)
             (warning "./test.tex" 4 "LaTeX Warning: There were undefined references."
              0 "LaTeX Warning: There were undefined references.\n" nil 4 nil 1482 nil)))))

;;; error-parsing.el ends here
