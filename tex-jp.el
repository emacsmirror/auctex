;;; tex-jp.el --- Support for Japanese TeX.

;; Copyright (C) 1999, 2001 Hidenobu Nabetani <nabe@debian.or.jp>
;; Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007 Free Software Foundation

;; Author:     KOBAYASHI Shinji <koba@flab.fujitsu.co.jp>
;; Maintainer: Masayuki Ataka <masayuki.ataka@gmail.com>
;; Keywords: tex

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

;;; Commentary:

;; This file was written by KOBAYASHI Shinji <koba@flab.fujitsu.co.jp>
;; based on many patches developed by Japanese NetNews community.
;; Japanese message translation by MATUI Takao <mat@nuis.ac.jp>.

;;; Code:

(require 'latex)

;;; Customization

(defgroup AUCTeX-jp nil
  "Japanese support in AUCTeX."
  :group 'AUCTeX)

(defcustom japanese-TeX-engine-default 'ptex
  "Default TeX engine for Japanese TeX."
  :group 'AUCTeX-jp
  :type '(choice (const :tag "pTeX" ptex)
		 (const :tag "jTeX" jtex)
		 (const :tag "upTeX" uptex)))

(setq TeX-engine-alist-builtin
      (append TeX-engine-alist-builtin
             '((ptex "pTeX" "ptex %(kanjiopt)" "platex %(kanjiopt)" "eptex")
               (jtex "jTeX" "jtex" "jlatex" nil)
               (uptex "upTeX" "euptex" "uplatex" "euptex"))))

;; 順調に行けば不要になる。
(defcustom japanese-TeX-command-list
  ;; Changed to double quotes for Windows afflicted people.  I don't
  ;; use the %(latex) and %(tex) shorthands here because I have not
  ;; clue whether Omega-related versions exist.  --dak
  '(("jTeX" "%(PDF)jtex %`%S%(PDFout)%(mode)%' %t"
     TeX-run-TeX nil (plain-tex-mode) :help "Run NTT jTeX")
    ("jLaTeX" "%(PDF)jlatex %`%S%(PDFout)%(mode)%' %t"
     TeX-run-TeX nil (latex-mode) :help "Run NTT jLaTeX")
    ("pTeX" "%(PDF)ptex %(kanjiopt)%`%S%(PDFout)%(mode)%' %t"
     TeX-run-TeX nil (plain-tex-mode) :help "Run ASCII pTeX")
    ("pLaTeX" "%(PDF)platex %(kanjiopt)%`%S%(PDFout)%(mode)%' %t"
     TeX-run-TeX nil (latex-mode) :help "Run ASCII pLaTeX")
    ("Mendex" "mendex %(mendexkopt)%s" TeX-run-command nil t :help "Create index file with mendex")
    ("jBibTeX" "jbibtex %s" TeX-run-BibTeX nil t :help "Run jBibTeX")
    ("pBibTeX" "pbibtex %(kanjiopt)%s" TeX-run-BibTeX nil t :help "Run pBibTeX"))
  "Additional list of commands, especially for Japanese.
For detail, see `TeX-command-list', to which this list is appended."
  :group 'AUCTeX-jp
  :type '(repeat (group :value ("" "" TeX-run-command nil t)
			(string :tag "Name")
			(string :tag "Command")
			(choice :tag "How"
				:value TeX-run-command
				(function-item TeX-run-command)
				(function-item TeX-run-format)
				(function-item TeX-run-TeX)
				(function-item TeX-run-interactive)
				(function-item TeX-run-BibTeX)
				(function-item TeX-run-compile)
				(function-item TeX-run-shell)
				(function-item TeX-run-discard)
				(function-item TeX-run-background)
				(function-item TeX-run-silent)
				(function-item TeX-run-discard-foreground)
				(function-item TeX-run-function)
				(function-item TeX-run-discard-or-function)
				(function :tag "Other"))
			(boolean :tag "Prompt")
			(choice :tag "Modes"
				(const :tag "All" t)
				(set (const :tag "Plain TeX" plain-tex-mode)
				     (const :tag "LaTeX" latex-mode)
				     (const :tag "DocTeX" doctex-mode)
				     (const :tag "ConTeXt" context-mode)
				     (const :tag "Texinfo" texinfo-mode)
				     (const :tag "AmSTeX" ams-tex-mode)))
			(repeat :tag "Menu elements" :inline t sexp))))

;; 順調に行けば不要になる。
(setq TeX-command-list
      (append japanese-TeX-command-list
	      '(("-" "" ignore nil t)) ;; separator for command menu
	      TeX-command-list))

;; 暫定処置。tex.el に取り込んでもらえるとよい。
(setcar (cdr (assoc "BibTeX" TeX-command-list)) "%(bibtex) %s")
(setcar (cdr (assoc "Index" TeX-command-list)) "%(makeindex) %s")

;; 暫定処置。tex.el に取り込んでもらえるとよい。
(setq TeX-expand-list
      (append
       TeX-expand-list
       '(
        ;; -kanji オプションの文字列を作る。
        ("%(kanjiopt)" (lambda ()
                         (if (and
                              ;; non-mule な emacsen はそもそも日本語
                              ;; 文書を typeset することは考えなくても
                              ;; いいだろう、とは思うけど一応…。
                              (featurep 'mule)
                              japanese-TeX-use-kanji-opt-flag)
                             (let ((str (japanese-TeX-get-encoding-string)))
                               (if str (format " -kanji=%s " str) ""))
                           "")))
        ;; pbibtex, jbibtex, upbibtex, bibtex の中から適切なものを選択する。
        ("%(bibtex)" (lambda ()
                       (cond
                        ((eq TeX-engine 'ptex)
                         ;; pLaTeX 用日本語 BibTeX が pbibtex になった
                         ;; のは比較的最近なので、まだ jbibtex の人もそ
                         ;; れなりにいるだろう。
                         (if (executable-find "pbibtex")
                             "pbibtex %(kanjiopt)" "jbibtex"))
                        ((eq TeX-engine 'jtex) "jbibtex")
                        ((eq TeX-engine 'uptex) "upbibtex")
                        (t "bibtex"))))
        ;; mendex と makeindex の適切な方を選択する。
        ("%(makeindex)" (lambda ()
                          (if (memq TeX-engine '(ptex uptex))
                              "mendex %(mendexkopt)" "makeindex")))
        ;; mendex 用日本語コードオプション。
        ("%(mendexkopt)" (lambda ()
                           (if (and (featurep 'mule)
                                    japanese-TeX-use-kanji-opt-flag)
                               (let ((str (japanese-TeX-get-encoding-string)))
                                 ;; １文字目を大文字に。
                                 (if str (format " -%c " (upcase (aref str 0)))
                                   ""))
                             "")))
        ;; pxdvi と %(o?)xdvi の適切な方を選択する。
        ("%(xdvi)" (lambda ()
                     ;; pxdvi は ptex, jtex 共用なので、
                     ;; japanese mode かどうかで判定すれば OK。
                     (if (and japanese-TeX-mode (executable-find "pxdvi"))
                         "pxdvi" "%(o?)xdvi"))))))

;;; Viewing (new implementation)

(unless (get 'TeX-view-predicate-list 'saved-value)
  (setq TeX-view-predicate-list
       '((paper-a4
          (TeX-match-style
           "\\`\\(a4j\\|a4paper\\|a4dutch\\|a4wide\\|sem-a4\\)\\'"))
         (paper-a5
          (TeX-match-style
           "\\`\\(a5j\\|a5paper\\|a5comb\\)\\'"))
         ;; jarticle などだと b4paper, b5paper は JIS B 系列。
         ;; j-article などの方には a4j, b5j といったオプションはない。
         (paper-b5    ; ISO B5
          (and (TeX-match-style "\\`b5paper\\'")
               (not (memq TeX-engine '(ptex uptex)))))
         (paper-b5jis ; JIS B5
          (or (TeX-match-style "\\`b5j\\'")
              (and (TeX-match-style "\\`b5paper\\'")
                   (memq TeX-engine '(ptex uptex)))))
         ;; article などには b4paper というオプションはない。
         ;; b4paper というオプションがあったら JIS B4 と見なす。
         (paper-b4jis
          (TeX-match-style "\\`\\(b4j\\|b4paper\\)\\'")))))
;; jsarticle だと他にももっと判型のオプションがあるが、
;; 全部面倒見てるとキリがないので、これくらいでいいだろう。
;; jsarticle.el や jsbook.el で追加分の処理を仕込めばいいのかも知れない。

;; 暫定処置。tex.el に取り込んでもらえるとよい。
(unless (get 'TeX-view-program-list 'saved-value)
  (setq TeX-view-program-list
       (cond
        ;; http://oku.edu.mie-u.ac.jp/~okumura/texwiki/?AUCTeX
        ;; を参考にしてみた。
        ((eq system-type 'windows-nt)
         '(("Dviout" ("dviout -1 "
                      ((paper-a4 paper-portrait) " -y=A4 ")
                      ((paper-a4 paper-landscape) " -y=A4L ")
                      ((paper-a5 paper-portrait) " -y=A5 ")
                      ((paper-a5 paper-landscape) " -y=A5L ")
                      ((paper-b5 paper-portrait) " -y=E5 ")
                      ((paper-b5 paper-landscape) " -y=E5L ")
                      ((paper-b4jis paper-portrait) " -y=B4 ")
                      ((paper-b4jis paper-landscape) " -y=B4L ")
                      ((paper-b5jis paper-portrait) " -y=B5 ")
                      ((paper-b5jis paper-landscape) " -y=B5L ")
                      (paper-legal " -y=Legal ")
                      (paper-letter " -y=Letter ")
                      (paper-executive " -y=Exective ")
                      "%o" (mode-io-correlate " \"# %n '%b'\"")))
           ("TeXworks" "TeXworks %o")
           ("SumatraPDF" "SumatraPDF -reuse-instance %o"
            (mode-io-correlate " -forward-search \"%b\" %n"))
           ("MuPDF" "mupdf %o")))
        ;; これでいいのかどうかは不安。
        ((eq system-type 'darwin)
         '(("Preview" "open -a Preview.app %o")
           ("TeXShop" "open -a TeXShop.app %o")
           ("TeXworks" "open -a TeXworks.app %o")
           ("Skim" "open -a Skim.app %o")
           ("displayline" "displayline %n %o %b")
           ("PictPrinter" "open -a PictPrinter.app %d")
           ("Mxdvi" "open -a Mxdvi.app %d")
           ("open" "open %o")))
        (t
         (setcar (cadr (assoc "xdvi" TeX-view-program-list-builtin))
                 "%(xdvi) -unique")
         '(("TeXworks" "texworks %o")
           ("zathura" "zathura %o")
           ("MuPDF" "mupdf %o"))))))

;; これは tex.el に取り入れてもらうのは難しいか？
;; tex-jp.el が読み込まれるだけで、dvi viewer のデフォルトが dviout に
;; なってしまうのは抵抗が大きいかも。
(unless (get 'TeX-view-program-selection 'saved-value)
  (setq TeX-view-program-selection
       (append
        (cond
         ((eq system-type 'windows-nt)
          '((output-dvi "Dviout")
            (output-pdf "TeXworks")))
         ((eq system-type 'darwin)
          '((output-pdf "Preview")))
         (t
          nil))
        TeX-view-program-selection)))

(mapc (lambda (dir) (add-to-list 'TeX-macro-global dir t))
      (or (TeX-tree-expand
	   '("$SYSTEXMF" "$TEXMFLOCAL" "$TEXMFMAIN" "$TEXMFDIST")
	   "platex" '("/ptex/" "/pbibtex/bst/"))
	  '("/usr/share/texmf/ptex/" "/usr/share/texmf/pbibtex/bst/")))

(mapc (lambda (dir) (add-to-list 'TeX-macro-global dir t))
      (or (TeX-tree-expand
	   '("$SYSTEXMF" "$TEXMFLOCAL" "$TEXMFMAIN" "$TEXMFDIST")
	   "jlatex" '("/jtex/" "/jbibtex/bst/"))
	  '("/usr/share/texmf/jtex/" "/usr/share/texmf/jbibtex/bst/")))

;; 順調に行けば不要になる。
(setq LaTeX-command-style
      (append '(("\\`u[jt]\\(article\\|report\\|book\\)\\'\\|\\`uplatex\\'"
                "%(PDF)uplatex %(kanjiopt)%S%(PDFout)")
               ("\\`[jt]s?\\(article\\|report\\|book\\)\\'"
                "%(PDF)platex %(kanjiopt)%S%(PDFout)")
               ("\\`j-\\(article\\|report\\|book\\)\\'"
                "%(PDF)jlatex %(kanjiopt)%S%(PDFout)"))
	      LaTeX-command-style))

(defcustom japanese-TeX-error-messages t
  "*If non-nil, explain TeX error messages in Japanese."
  :group 'AUCTeX-jp
  :type 'boolean)

(when (featurep 'mule)

;; FIX-ME (2007-02-09) The default coding system in recent Unix (like Fedora and
;; Ubuntu) is utf-8.  But Japanese TeX system does not support utf-8 yet
;; (platex-utf is under development, may be alpha phase).  So,
;; process-coding-system for Japanese TeX is not defined from
;; default-coding-system.  When platex-utf is out, we should look this setting,
;; again.

(defcustom TeX-japanese-process-input-coding-system
  (cond ((memq system-type '(windows-nt ms-dos cygwin)) 'shift_jis-dos)
	((memq system-type '(mac darwin)) 'shift_jis-mac)
	(t 'euc-jp-unix))
  "TeX-process' coding system with standard input."
  :group 'AUCTeX-jp
  :type 'coding-system)

(defcustom TeX-japanese-process-output-coding-system
  (cond ((memq system-type '(windows-nt ms-dos cygwin)) 'shift_jis-dos)
	((memq system-type '(mac darwin)) 'shift_jis-mac)
	(t 'euc-jp-unix))
  "TeX-process' coding system with standard output."
  :group 'AUCTeX-jp
  :type 'coding-system)

)

;; 順調に行けば不要になる。
(defcustom japanese-TeX-command-default "pTeX"
  "*The default command for `TeX-command' in the japanese-TeX mode."
  :group 'AUCTeX-jp
  :type 'string)
  (make-variable-buffer-local 'japanese-TeX-command-default)

;; 順調に行けば不要になる。
(defcustom japanese-LaTeX-command-default "LaTeX"
  "*The default command for `TeX-command' in the japanese-LaTeX mode."
  :group 'AUCTeX-jp
  :type 'string)
  (make-variable-buffer-local 'japanese-LaTeX-command-default)

(defcustom japanese-LaTeX-default-style "jarticle"
  "*Default when creating new Japanese documents."
  :group 'AUCTeX-jp
  :type 'string)

(defcustom japanese-LaTeX-style-list
  '(("j-article")
    ("j-report")
    ("j-book")
    ("jslides")
    ("jarticle")
    ("jreport")
    ("jbook")
    ("tarticle")
    ("treport")
    ("tbook")
    ("jsarticle")
    ("jsbook"))
  "*List of Japanese document styles."
  :group 'AUCTeX-jp
  :type '(repeat (group (string :format "%v"))))

(setq LaTeX-style-list
      (append japanese-LaTeX-style-list LaTeX-style-list))

;;; Coding system

(when (featurep 'mule)

(defun japanese-TeX-set-process-coding-system (process)
  "Set proper coding system for japanese TeX PROCESS."
  (if (with-current-buffer TeX-command-buffer japanese-TeX-mode)
      (set-process-coding-system process
				 TeX-japanese-process-output-coding-system
				 TeX-japanese-process-input-coding-system)))
(setq TeX-after-start-process-function
      'japanese-TeX-set-process-coding-system)

(defcustom japanese-TeX-use-kanji-opt-flag t
  "Add kanji option to Japanese pTeX family if non-nil."
  :group 'AUCTeX-jp
  :type 'boolean)

(defun japanese-TeX-coding-ejsu (coding-system)
  "Convert japanese CODING-SYSTEM to mnemonic string.
euc-jp:    \"euc\"
jis:       \"jis\"
shift_jis: \"sjis\"
utf-8:     \"utf8\"
Return nil otherwise."
  (let ((base (coding-system-base coding-system)))
    (cdr (assq base
              '((japanese-iso-8bit . "euc")
                (iso-2022-jp . "jis")
                (japanese-shift-jis . "sjis")
                (utf-8 . "utf8")

                ;; xemacs だと以下の名前は違うかも…。
                (euc-jis-2004 . "euc")
                (iso-2022-jp-2004 . "jis")
                (japanese-shift-jis-2004 . "sjis")

                (japanese-cp932 . "sjis")
                (eucjp-ms . "euc"))))))

(defun japanese-TeX-get-encoding-string ()
  "Return coding option string for Japanese pTeX family.
For inappropriate encoding, nil instead."
  (or (japanese-TeX-coding-ejsu buffer-file-coding-system)

      ;; 複数ファイルに分割した文書の場合、emacs で開いたファイルが日本
      ;; 語を１字も含まないことがある。このため、そのファイルの
      ;; buffer-file-coding-system は日本語コードが不定に留まって
      ;; しまう可能性がある。そのような場合、master file の
      ;; buffer-file-coding-system を使う。
      (if (stringp TeX-master) ; 自分が子ファイルのとき
         (let ((buf (get-file-buffer (TeX-master-file t))))
           (if buf
               (japanese-TeX-coding-ejsu
                (with-current-buffer buf buffer-file-coding-system)))))

      ;; それでも決められない場合は buffer-file-coding-system の
      ;; default 値を使う。
      (japanese-TeX-coding-ejsu
       (default-value 'buffer-file-coding-system))))

)

;;; Japanese TeX modes

(defvar japanese-TeX-mode nil
  "Non-nil means the current buffer handles Japanese TeX/LaTeX.")
(make-variable-buffer-local 'japanese-TeX-mode)
(put 'japanese-TeX-mode 'permanent-local t)

;;;###autoload
(defun japanese-plain-tex-mode ()
  "Major mode in AUCTeX for editing Japanese plain TeX files.
Set `japanese-TeX-mode' to t, and enter `TeX-plain-tex-mode'."
  (interactive)
  (setq japanese-TeX-mode t)
  (TeX-plain-tex-mode))

(defun japanese-plain-tex-mode-initialization ()
  "Japanese plain-TeX specific initializations."
  (when japanese-TeX-mode
;    (setq TeX-command-default japanese-TeX-command-default)
    (TeX-engine-set japanese-TeX-engine-default)))

(add-hook 'plain-TeX-mode-hook 'japanese-plain-tex-mode-initialization)

;;;###autoload
(defun japanese-latex-mode ()
  "Major mode in AUCTeX for editing Japanese LaTeX files.
Set `japanese-TeX-mode' to t, and enter `TeX-latex-mode'."
  (interactive)
  (setq japanese-TeX-mode t)
  (TeX-latex-mode))

(defun japanese-latex-mode-initialization ()
  "Japanese LaTeX specific initializations."
  (when japanese-TeX-mode
;    (setq TeX-command-default japanese-LaTeX-command-default)
    (TeX-engine-set
     ;; class file 名に頼るのは正しいのか？
     ;; jLaTeX にも jarticle は一応あるし、pLaTeX でも自分で j-article を
     ;; インストールして使っていけない法はない。
     (cond
      ((TeX-match-style "\\`u[jt]\\(article\\|report\\|book\\)\\'\\|\\`uplatex\\'")
       'uptex)
      ((TeX-match-style "\\`[jt]s?\\(article\\|report\\|book\\)\\'")
       'ptex)
      ((TeX-match-style "\\`j-\\(article\\|report\\|book\\)\\'")
       'jtex)
      (t japanese-TeX-engine-default)))
    (setq LaTeX-default-style japanese-LaTeX-default-style)
;    (setq TeX-command-BibTeX
;        (if (and (eq TeX-engine 'ptex) (executable-find "pbibtex"))
;            "pBibTeX" "jBibTeX"))
))

(add-hook 'LaTeX-mode-hook 'japanese-latex-mode-initialization)


;;; Support for various self-insert-command

(fset 'japanese-TeX-self-insert-command
      (cond ((fboundp 'can-n-egg-self-insert-command)
	     'can-n-egg-self-insert-command)
	    ((fboundp 'egg-self-insert-command)
	     'egg-self-insert-command)
	    ((fboundp 'canna-self-insert-command)
	     'canna-self-insert-command)
	    (t
	     'self-insert-command)))

(defun TeX-insert-punctuation ()
  "Insert point or comma, cleaning up preceding space."
  (interactive)
  (expand-abbrev)
  (if (TeX-looking-at-backward "\\\\/\\(}+\\)" 50)
      (replace-match "\\1" t))
  (call-interactively 'japanese-TeX-self-insert-command))

;;; Error Messages

(if japanese-TeX-error-messages
(setq TeX-error-description-list
  '(("Bad \\\\line or \\\\vector argument.*" .
"$B@~$N79$-$r;XDj$9$k!$(B\\line$B$^$?$O(B\\vector$B$N:G=i$N0z?t$,IT@5$G$9!%(B")

    ("Bad math environment delimiter.*" .
"$B?t<0%b!<%ICf$G?t<0%b!<%I3+;O%3%^%s%I(B\\[$B$^$?$O(B\\($B!$$^$?$O!$?t<0%b!<%I30$G(B
$B?t<0%b!<%I=*N;%3%^%s%I(B\\]$B$^$?$O(B\\)$B$r(BTeX$B$,8+$D$1$^$7$?!%$3$NLdBj$O!$?t<0%b!<(B
$B%I$N%G%j%_%?$,%^%C%A$7$F$$$J$+$C$?$j!$3g8L$N%P%i%s%9$,$H$l$F$$$J$+$C$?$j$9(B
$B$k$?$a$K@8$8$^$9!%(B")

    ("Bad use of \\\\\\\\.*" .
"\\\\$B%3%^%s%I$,%Q%i%0%i%UCf$K$"$j$^$7$?!%$3$N;H$$$+$?$OL50UL#$G$9!%(B
$B$3$N%(%i!<%a%C%;!<%8$O(B\\\\$B$,(Bcentering$B4D6-$d(Bflushing$B4D6-$G;H$o$l$?(B
$B;~!$$"$k$$$O(Bcentering/flushing$B@k8@$,M-8z$J$H$3$m$G;H$o$l$?;~$K@8$8$^$9!%(B")

    ("\\\\begin{[^ ]*} ended by \\\\end{[^ ]*}." .
"$BBP1~$9$k(B\\begin$BL?Na$N$J$$(B\\end$BL?Na$r(BLaTeX$B$,8+$D$1$^$7$?!%(B\\end$BL?Na$N4D(B
$B6-L>$r4V0c$($?$+!$M>J,$J(B\\begin$BL?Na$,$"$k$+!$(B\\end$BL?Na$r$o$9$l$?$+$N$$$:(B
$B$l$+$G$7$g$&!%(B")

    ("Can be used only in preamble." .
"$B%W%j%"%s%V%k$G$7$+;H$($J$$(B\\documentclass$B!&(B\\nofiles$B!&(B\\includeonly
\\makeindex$B!&(B\\makeglossary$B$N$&$A$N$$$:$l$+$,(B\\begin{document}$B$h$j$b(B
$B8e$G;H$o$l$F$$$k$N$r(BLaTeX$B$,8!=P$7$^$7$?!%$3$N%(%i!<$O(B\\begin{document}
$B$,M>J,$K$"$C$?;~$K$b@8$8$^$9!%(B")

    ("Command name [^ ]* already used.*" .
"$B$9$G$KDj5A$5$l$F$$$kL?NaL>$^$?$O4D6-L>$KBP$7$F(B\\newcommand$B!&(B
\\newenvironment$B!&(B\\newlength$B!&(B\\newsavebox$B!&(B\\newtheorem$B$N$&$A$N$$$:(B
$B$l$+$r<B9T$7$h$&$H$7$F$$$^$9(B($B$"$k4D6-$rDj5A$9$k$HF1$8L>A0$NL?Na$,<+F0(B
$BE*$KDj5A$5$l$k$N$G!$4{$KB8:_$9$k4D6-$HF1L>$NL?Na$ODj5A$G$-$^$;$s(B)$B!%?7(B
$B$7$$L>A0$r9M$($k$+!$(B\\newcommand$B$+(B\\newenvironment$B$N>l9g$J$iBP1~$9$k(B
\\renew...$BL?Na$r;H$o$J$1$l$P$J$j$^$;$s!%(B")

    ("Counter too large." .
"1. $BJ8;z$G=g=xIU$1$5$l$?$b$N!$$?$V$sHV9fIU$1$5$l$?%j%9%H4D6-$N%i%Y%k$,!$(B
26$B$h$j$bBg$-$$HV9f$r<u$1<h$j$^$7$?!%Hs>o$KD9$$%j%9%H$r;H$C$F$$$k$+!$(B
$B%+%&%s%?$r:F@_Dj$7$F$7$^$C$?$+$N$$$:$l$+$G$7$g$&!%(B

2. $B5SCm$,J8;z$^$?$O5SCm5-9f$G=g=x$E$1$5$l$F$$$^$9$,!$J8;z$^$?$O5-9f$r(B
$B;H$$@Z$C$F$7$^$$$^$7$?!%$*$=$i$/(B\\thanks$BL?Na$N;H$$$9$.$G$9!%(B")


    ("Environment [^ ]* undefined." .
"$BDj5A$5$l$F$$$J$$4D6-$KBP$9$k(B\\begin$BL?Na$r(BLaTeX$B$,8+$D$1$^$7$?!%$*$=$i$/(B
$B4D6-L>$r4V0c$($?$N$G$7$g$&!%(B")

    ("Float(s) lost." .
"parbox$B$N$J$+$K(Bfigure$B4D6-!&(Btable$B4D6-$^$?$O(B\\marginpar$BL?Na$,$"$j$^$7$?(B
\($B$J$*!$(Bparbox$B$O(Bminipage$B4D6-$+(B\\parbox$BL?Na$K$h$C$F:n$i$l$k$+!$5SCm$d?^(B
$B$J$I$KBP$7$F(BLaTeX$B$,@8@.$9$k$b$N$G$9(B\)$B!%$3$l$O=PNO;~$N%(%i!<$J$N$G!$860x(B
$B$H$J$C$F$$$k4D6-$"$k$$$OL?Na$O!$(BLaTeX$B$,LdBj$rH/8+$7$?>l=j$h$j$b$@$$$V(B
$B$sA0$K$"$k2DG=@-$,$"$j$^$9!%=PNO$5$l$F$$$J$$?^!&I=!&K5Cm$J$I$,$$$/$D$+(B
$B$"$k$+$b$7$l$^$;$s$,!$$=$l$i$,860x$G$"$k$H$O8B$j$^$;$s!%(B")

    ("Illegal character in array arg." .
"array$B4D6-$^$?$O(Btabular$B4D6-$N0z?t!$$^$?$O(B\\multicolumn$BL?Na$NBh(B2$B0z?t(B
$B$NCf$KIT@5$JJ8;z$,$"$j$^$7$?!%(B")

    ("Missing \\\\begin{document}." .
"\\begin{document}$BL?Na$h$jA0$K(BLaTeX$B$,=PNO$r9T$J$C$F$7$^$$$^$7$?!%(B
\\begin{document}$BL?Na$rK:$l$?$+!$%W%j%"%s%V%k$K2?$+4V0c$$$,$"$k$N$G$7$g$&!%(B
$BBG$A4V0c$$$K$h$kJ8;z$d!$@k8@$N8m$j$K$h$k2DG=@-$b$"$j$^$9!%Nc$($P!$0z?t$r(B
$B0O$`3g8L$rH4$+$7$?$H$+!$L?NaL>$N(B\\$B$rK:$l$?>l9g$J$I$G$9!%(B")

    ("Missing p-arg in array arg.*" .
"array$B4D6-!&(Btabular$B4D6-$N0z?t!$$"$k$$$O(B\\multicolumn$BL?Na$NBh(B2$B0z?t$NCf$K!$(B
$B3g8L$K0O$^$l$?I=8=$N$D$$$F$$$J$$(Bp$B$,$"$j$^$7$?!%(B")

    ("Missing @-exp in array arg." .
"array$B4D6-!&(Btabular$B4D6-$N0z?t!$$"$k$$$O(B\\multicolumn$BL?Na$NBh(B2$B0z?t$NCf$K!$(B
@$BI=8=$N$D$$$F$$$J$$(B@$B$,$"$j$^$7$?!%(B")

    ("No such counter." .
"\\setcounter$BL?Na$^$?$O(B\\addtocounter$BL?Na$G!$B8:_$7$J$$%+%&%s%?$,;XDj$5$l(B
$B$^$7$?!%$*$=$i$/$?$@$N%?%$%W%_%9$G$7$g$&!%$?$@$7!$%(%i!<$,(Baux$B%U%!%$%k$NCf(B
$B$G@8$8$?>l9g$O!$(B\\newcounter$BL?Na$r%W%j%"%s%V%k$N30$G;H$C$?$N$@$H;W$o$l$^$9!%(B")

    ("Not in outer par mode." .
"figure$B4D6-!&(Btable$B4D6-$"$k$$$O(B\\marginpar$BL?Na$,?t<0%b!<%I$^$?$O(Bparbox$B$NCf(B
$B$G;H$o$l$^$7$?!%(B")

    ("\\\\pushtabs and \\\\poptabs don't match." .
"\\pushtabs$B$HBP1~$7$J$$(B\\poptabs$B$,$_$D$+$C$?$+!$$^$?$O!$BP1~$9$k(B\\poptabs
$B$r$b$?$J$$(B\\pushtabs$B$,$"$k$N$K(B\\end{tabbing}$B$,8=$l$F$7$^$$$^$7$?!%(B")

    ("Something's wrong--perhaps a missing \\\\item." .
"$B%j%9%H4D6-$NCf$K(B\\item$BL?Na$,$J$$$N$,:G$b$"$j$=$&$J%1!<%9$G$9!%(B
thebibliography$B4D6-$G0z?t$rK:$l$?>l9g$K$b@8$8$^$9!%(B")

    ("Tab overflow." .
"\\=$B$,!$(BLaTeX$B$G5v$5$l$k%?%V%9%H%C%W$N:GBg?t$rD6$($F$$$^$9!%(B")

    ("There's no line here to end." .
"\\newline$BL?Na$^$?$O(B\\\\$BL?Na$,%Q%i%0%i%U4V$K$"$j$^$9!%$3$N;H$$$+$?$O(B
$BL50UL#$G$9!%$b$76u9T$r$"$1$?$$$N$G$7$?$i!$(B\\vspace$B$r;H$C$F$/$@$5$$!%(B")

    ("This may be a LaTeX bug." .
"$B$^$C$?$/$o$1$,$o$+$i$J$/$J$C$F$7$^$$$^$7$?!%$?$V$s$3$l0JA0$K8!=P$5$l$?(B
$B%(%i!<$N$;$$$@$H;W$o$l$^$9!%$7$+$7!$(BLaTeX$B<+BN$N%P%0$G$"$k2DG=@-$b$"$j$^$9!%(B
$B$b$7$3$N%(%i!<$,F~NO%U%!%$%k$KBP$9$k:G=i$N%(%i!<$G$"$j!$2?$b4V0c$$$,8+$D(B
$B$+$i$J$$>l9g$O!$$=$N%U%!%$%k$rJ]B8$7$F!$%m!<%+%k%,%$%I$K=q$+$l$F$$$k@UG$(B
$B<T$KO"Mm$7$F$/$@$5$$!%(B")

    ("Too deeply nested." .
"$B%j%9%H4D6-$NF~$l;R$,?<$9$.$^$9!%2?CJ3,$NF~$l;R$,5v$5$l$k$+$O;H$C$F$$$k(B
$B%3%s%T%e!<%?$K0MB8$7$^$9$,!$>/$J$/$H$b(B4$BCJ3,$^$G$O5v$5$l$F$$$^$9(B($BIaDL$O(B
$B$=$l$G==J,$G$7$g$&(B)$B!%(B")

    ("Too many unprocessed floats." .
"$B$3$N%(%i!<$O(B1$B%Z!<%8Cf$N(B\\marginpar$BL?Na$,B?$9$.$k$?$a$K@8$8$k>l9g$b$"(B
$B$j$^$9$,!$$b$C$H$"$j$=$&$J$N$O!$8B3&$rD6$($F?^$dI=$rJ]B8$7$h$&$H$7$?>l(B
$B9g$G$9!%D9$$J8=q$rAHHG$7$F$$$/$H$-!$(BLaTeX$B$O?^$dI=$r8D!9$KJ]B8$7!$%Z!<(B
$B%8$NJ,3d$r9T$J$&;~$K$=$l$i$rA^F~$7$^$9!%$3$N%(%i!<$O!$%Z!<%8$X$NJ,3d$,(B
$B9T$J$o$l$kA0$K!$$"$^$j$K$bB?$/$N(Bfigure$B4D6-$d(Btable$B4D6-$,8+$D$+$C$?>l9g(B
$B$K@8$8$^$9!%$3$NLdBj$O4D6-$N$&$A$N$$$/$D$+$rJ8=q$N=*$o$j$NJ}$K0\F0$9$l(B
$B$P2r7h$G$-$^$9!%$^$?!$$3$N%(%i!<$O(B``logjam''$B$K$h$C$F@8$8$k$3$H$b$"$j$^(B
$B$9!%(B``logjam''$B$H$O!$(BLaTeX$B$,=P8==g=xDL$j$K$7$+?^I=$r=PNO$G$-$J$$$;$$$G!$(B
$B?^I=$N=PNO$,(B1$B%v=j$G$b$D$^$k$H$=$N8e$m$N?^I=$,8.JB$_$9$Y$F$D$C$+$($F$7$^(B
$B$&$3$H$r$$$$$^$9!%$3$N%8%c%`$N860x$O!$Bg$-$9$.$F(B1$B%Z!<%8$J$$$7$O%*%W%7%g(B
$B%s0z?t$G;XDj$5$l$?0LCV$K<}$^$i$J$$$h$&$J?^$dI=$G$"$k2DG=@-$,$"$j$^$9!%$3(B
$B$l$O!$0z?t$K(Bp$B%*%W%7%g%s$,;XDj$5$l$F$$$J$$$H5/$-$d$9$/$J$j$^$9!%(B")

    ("Undefined tab position." .
"\\>$B!&(B\\+$B!&(B\\-$B$^$?$O(B\\<$BL?Na$G!$B8:_$7$J$$%?%V0LCV!$$9$J$o$A(B\\=$BL?Na$GDj(B
$B5A$5$l$F$$$J$$%?%V0LCV$r;XDj$7$h$&$H$7$F$$$^$9!%(B")

    ("\\\\< in mid line." .
"\\<$BL?Na$,(Btabbing$B4D6-$N9T$NESCf$K8=$l$^$7$?!%$3$NL?Na$O9T$N@hF,$K$J$1$l$P(B
$B$J$j$^$;$s!%(B")

    ("Double subscript." .
"$B?t<0Cf$N(B1$B$D$NNs$K(B2$B$D$N2<IU$-J8;z$,$D$$$F$$$^$9!%Nc$($P(Bx_{2}_{3}$B$N$h$&$K!%(B
$B$3$N$h$&$JI=8=$OL50UL#$G$9!%(B")

    ("Double superscript." .
"$B?t<0Cf$N(B1$B$D$NNs$K(B2$B$D$N>eIU$-J8;z$,$D$$$F$$$^$9!%Nc$($P(Bx^{2}^{3}$B$N$h$&$K!%(B
$B$3$N$h$&$JI=8=$OL50UL#$G$9!%(B")

    ("Extra alignment tab has been changed to \\\\cr." .
"array$B4D6-$^$?$O(Btabular$B4D6-$N(B1$BNsCf$K$"$k9`L\$,B?$9$.$^$9!%8@$$49$($k$H!$(B
$BNs$N=*$o$j$^$G$K$"$k(B&$B$N?t$,B?$9$.$^$9!%$*$=$i$/A0$NNs$N:G8e$K(B\\\\$B$r$D$1(B
$B$k$N$rK:$l$?$N$G$7$g$&!%(B")

    ("Extra \\}, or forgotten \\$." .
"$B3g8L$^$?$O?t<0%b!<%I$N%G%j%_%?$,@5$7$/BP1~$7$F$$$^$;$s!%$*$=$i$/(B{$B!&(B\\[$B!&(B
\\($B$"$k$$$O(B$$B$N$&$A$N$$$:$l$+$r=q$-K:$l$?$N$G$7$g$&!%(B")

    ("Font [^ ]* not loaded: Not enough room left." .
"$B$3$NJ8=q$O8B3&$h$j$bB?$/$N%U%)%s%H$r;H$C$F$$$^$9!%$b$7J8=q$NItJ,$4$H$K(B
$BJL!9$N%U%)%s%H$,;H$o$l$F$$$k$N$J$i!$J,3d$7$F=hM}$9$l$PLdBj$O2r7h$5$l$^$9!%(B")

    ("I can't find file `.*'." .
"$BI,MW$J%U%!%$%k$,8+$D$+$j$^$;$s$G$7$?!%$b$78+$D$+$i$J$$%U%!%$%k$N3HD%;R(B
$B$,(Btex$B$N>l9g!$$"$J$?$,;XDj$7$?%U%!%$%k!$$9$J$o$A%a%$%s%U%!%$%k$^$?$O(B
\\input$BL?Na!&(B\\include$BL?Na$GA^F~$5$l$k%U%!%$%k$,8+$D$+$i$J$$$N$G$9!%(B
$B3HD%;R$,(Bsty$B$G$"$l$P!$B8:_$7$J$$J8=q%9%?%$%k$^$?$O%9%?%$%k%*%W%7%g%s$r(B
$B;XDj$7$h$&$H$7$F$$$^$9!%(B")

    ("Illegal parameter number in definition of .*" .
"$B$3$l$O$*$=$i$/!$(B\\newcommand$B!&(B\\renewcommand$B!&(B\\newenvironment$B$^$?$O(B
\\renewenvironment$BL?Na$N$J$+$G(B#$B$,@5$7$/;H$o$l$J$+$C$?$?$a$K@8$8$?%(%i!<(B
$B$G$9!%(B\\#$BL?Na$H$7$F;H$o$l$k>l9g$r=|$1$P!$(B#$B$H$$$&J8;z$O!$Nc$($P(B2$BHVL\$N(B
$B0z?t$r;XDj$9$k(B#2$B$N$h$&$K!$0z?t%Q%i%a!<%?$H$7$F$7$+;H$($^$;$s!%$^$?!$(B
$B$3$N%(%i!<$O!$>e$K$"$2$?(B4$B$D$N%3%^%s%I$,$*8_$$$KF~$l;R$K$J$C$F$$$k>l9g(B
$B$d!$(B\\newenvironment$BL?Na!&(B\\renewenvironment$BL?Na$G(B#2$B$N$h$&$J%Q%i%a!<%?(B
$B$,:G8e$N0z?t$NCf$G;H$o$l$F$$$k>l9g$K$b@8$8$^$9!%(B")

    ("Illegal unit of measure ([^ ]* inserted)." .
"$B$b$7(B
      ! Missing number, treated as zero.
$B$H$$$&%(%i!<$,5/$-$?D>8e$G$"$l$P!$$3$N%(%i!<$N860x$b$=$l$HF1$8$G$9!%(B
$B$=$&$G$J$$>l9g$O!$(BLaTeX$B$,0z?t$H$7$F(Blength$B$r4|BT$7$F$$$k$N$K(Bnumber$B$,(B
$B8=$l$?$3$H$r0UL#$7$F$$$^$9!%$3$N%(%i!<$N:G$b$"$j$,$A$J860x$OD9$5(B0$B$r(B
$BI=$o$9(B0in$B$N$h$&$JI=8=$NBe$o$j$K(B0$B$H$+$$$F$7$^$&$3$H$K$"$j$^$9!%$?$@$7!$(B
$BL?Na$N0z?t$r=q$-K:$l$?>l9g$K$b$3$N%(%i!<$,@8$8$k$3$H$,$"$j$^$9!%(B")

    ("Misplaced alignment tab character \\&." .
"array$B$^$?$O(Btabular$B4D6-$G$N9`L\6h@Z$j$K$N$_;H$o$l$k$Y$-J8;z(B&$B$,IaDL$NJ8(B
$B$NCf$K$"$j$^$7$?!%$?$V$s(B\\&$B$HF~NO$7$?$+$C$?$N$G$7$g$&!%(B")

    ("Missing control sequence inserted." .
"$B$3$N%(%i!<$O!$$*$=$i$/L?NaL>$G$J$$$b$N$r(B\\newcommand$B!&(B\\renewcommand$B!&(B
\\newlength$B$^$?$O(B\\newsavebox$B$NBh(B1$B0z?t$H$7$F;H$C$?$?$a$K@8$8$?$N$G$7$g$&!%(B")

    ("Missing number, treated as zero." .
"$B$3$N%(%i!<$O$?$$$F$$!$0z?t$H$7$F(Bnumber$B$^$?$O(Blength$B$rI,MW$H$7$F$$$kL?Na$K(B
$BBP$7$F0z?t$,M?$($i$l$J$+$C$?$?$a$K@8$8$^$9!%0z?t$r=q$-K:$l$?$N$+!$%F%-%9%H(B
$B$NCf$NBg3g8L(B([])$B$,%*%W%7%g%s0z?t$N;XDj$H4V0c$($i$l$F$7$^$C$?$+$N$I$A$i$+$G(B
$B$7$g$&!%$^$?!$?t$r@8@.$9$k(B\\value$B$N$h$&$JL?Na$d(Blength$BL?Na$NA0$K(B\\protect$B$r(B
$BCV$$$?>l9g$K$b$3$N%(%i!<$O@8$8$^$9!%(B")

    ("Missing [{}] inserted." .
"TeX$B$O4{$K$o$1$,$o$+$i$J$/$J$C$F$$$^$9!%%(%i!<%a%C%;!<%8$K$h$C$F<($5$l$F(B
$B$$$k>l=j$O$?$V$sF~NO$K4V0c$$$,$"$C$?$H$3$m$h$j$b8e$m$K$J$C$F$7$^$C$F$$$k(B
$B$G$7$g$&!%(B")

    ("Missing \\$ inserted." .
"$B$*$=$i$/!$?t<0%b!<%ICf$G$7$+;H$($J$$L?Na$r(BTeX$B$,?t<0%b!<%I30$G8!=P$7$?(B
$B$N$@$H;W$o$l$^$9!%FC$K5-=R$5$l$F$$$J$$8B$j!$(BLaTeX Book(Lamport$BCx(B,$BLu=q(B
$B$O%"%9%-!<=PHG(B)$B$N(B3.3$B@a$K$"$kE:;z!&J,?t!&?t3X5-9f$J$I$N%3%^%s%I$O$9$Y$F(B
$B?t<0%b!<%I$G$7$+;H$($J$$$N$@$H$$$&$3$H$KCm0U$7$F$/$@$5$$!%$?$H$(L?Na$,(B
$B?t<04D6-$NCf$K$"$C$?$H$7$F$b!$(Bbox$B$r@8@.$9$kL?Na$N0z?t$r=hM}$7$O$8$a$?(B
$B;~E@$G$O!$(BTeX$B$O$^$@?t<0%b!<%I$KF~$C$F$$$J$$$N$G$9!%$^$?!$$3$N%(%i!<$O!$(B
$B?t<0%b!<%ICf$G(BTeX$B$,6u9T$r8!=P$7$?>l9g$K$b@8$8$^$9!%(B")

    ("Not a letter." .
"\\hyphenation$BL?Na$N0z?t$NCf$K$J$K$+@5$7$/$J$$$b$N$,$"$j$^$9!%(B")

    ("Paragraph ended before [^ ]* was complete." .
"$BL?Na$N0z?t$NCf$KIT@5$J6u9T$,F~$C$F$7$^$C$F$$$^$9!%$*$=$i$/0z?t$N=*$o$j(B
$B$KJD$83g8L$r$D$1$k$N$rK:$l$?$N$G$7$g$&!%(B")

    ("\\\\[^ ]*font [^ ]* is undefined .*" .
"$B$3$N%(%i!<$O$"$^$j0lHLE*$G$J$$%U%)%s%H$,?t<0%b!<%I$G;H$o$l$?;~$K@8$8(B
$B$^$9!%Nc$($P!$5SCm$NCf$N?t<0$G(B\\sc$BL?Na$,;H$o$l$k$H!$(Bfootnotesize$B$N(B
small caps$B%U%)%s%H$,8F$S$@$5$l$k$3$H$K$J$j$^$9!%$3$NLdBj$O(B\\load$BL?Na$r(B
$B;H$($P2r7h$G$-$^$9!%(B")

    ("Font .* not found." .
"$BL$CN$N(Bfamily/series/shape/size$B$NAH$_9g$o$;$N%U%)%s%H$,;XDj$5$l$^$7$?!%(B
$B$3$N%(%i!<$,5/$-$k%1!<%9$O(B2$B$D9M$($i$l$^$9!%(B
   1) \\size$B%^%/%m$G;H$($J$$%5%$%:$rA*Br$7$h$&$H$7$?!%(B
   2) $B$=$&$G$J$1$l$P!$4IM}<T$N$H$3$m$K9T$C$F!$%U%)%s%HA*Br%F!<%V%k$,(B
      $BIe$C$F$$$k$HJ86g$r$D$1$F$d$j$^$7$g$&(B!")

    ("TeX capacity exceeded, sorry .*" .
"TeX$B$,%a%b%j$r;H$$$-$C$F$7$^$$!$<B9T$rCfCG$7$^$7$?!%$7$+$7!$92$F$J$$$G(B
$B$/$@$5$$!%$3$N%(%i!<$,@8$8$?860x$O!$$?$V$s!$(BTeX$B$K$"$J$?$NJ8=q$r=hM}$G(B
$B$-$k$@$1$NG=NO$,$J$$$+$i$G$O$"$j$^$;$s!%(BTeX$B$K%a%b%j$r;H$$$-$i$;$?860x(B
$B$O!$$*$=$i$/F~NO$7$?%U%!%$%k$NA0$NJ}$G@8$8$?%(%i!<$G$9!%$"$J$?$,K\Ev$K(B
TeX$B$NMFNL$rD6$($?$3$H$r$7$h$&$H$7$?$N$+$I$&$+!$$=$7$F$=$N>l9g$I$&$9$l(B
$B$P$$$$$N$+$rH=CG$9$kJ}K!$r0J2<$K@bL@$7$^$9!%$b$7LdBj$,F~NO%U%!%$%kCf$N(B
$B%(%i!<$K$"$k>l9g$O!$8D!9$N%(%i!<$r2r7h$7$F$$$/J}K!$r$H$k$N$,$h$$$G$7$g(B
$B$&!%(BLaTeX$B$,C;$$%U%!%$%k$G%a%b%j$r;H$$$-$k$3$H$O$a$C$?$K$"$j$^$;$s$+$i!$(B
$B%(%i!<$N5/$-$?0LCV$h$jA0$K=hM}$7$?%Z!<%8$,?t%Z!<%8$7$+$J$1$l$P!$$^$:4V(B
$B0c$$$J$/F~NO%U%!%$%k$KLdBj$,$"$k$O$:$G$9!%(B

$B%(%i!<%a%C%;!<%8$N:G8e$K!$(BTeX$B$,;H$$$-$C$F$7$^$C$?%a%b%j$N<oN`$,<($5$l(B
$B$F$$$^$9!%$=$l$i$N$&$A0lHLE*$J$b$N$K$D$$$F!$9M$($i$l$k860x$r0J2<$K5s$2(B
$B$^$9!%(B

buffer size
===========
$B>O@a!&(B\\caption$B!&(B\\addcontentsline$B$"$k$$$O(B\\addtocontents$BL?Na$N0z?t$H(B
$B$7$FM?$($?%F%-%9%H$,D9$9$.$k>l9g$K@8$8$k$3$H$,$"$j$^$9!%$3$N%(%i!<$O(B
$B$?$$$F$$(B\\end{document}$B$r=hM}$7$F$$$k;~$K@8$8$^$9$,!$(B\\tableofcontents$B!&(B
\\listoffigures$B$"$k$$$O(B\\listoftables$BL?Na$r<B9T$7$F$$$k>l9g$K$b5/$-$k(B
$B$3$H$,$"$j$^$9!%$3$NLdBj$r2r7h$9$k$K$O!$$b$C$HC;$$%F%-%9%H$r%*%W%7%g%s(B
$B0z?t$H$7$FM?$($F$/$@$5$$!%L\<!$d?^I=0lMw$r:n@.$7$F$b!$8+=P$7$,D9$9$.$k(B
$B$HFI$_$K$/$/$J$k$O$:$G$9!%(B

exception dictionary
====================
TeX$B$,;}$C$F$$$kNN0h0J>e$K%O%$%U%M!<%7%g%s>pJs$rM?$($h$&$H$7$F$$$^$9!%(B
$B$"$^$j;H$o$J$$C18l$N(B\\hyphenation$BL?Na$r<h$j=|$$$F!$Be$o$j$K(B\\-$BL?Na$r;H$C(B
$B$F$/$@$5$$!%(B

hash size
=========
$BL?NaL>$NDj5A$^$?$OAj8_;2>H%i%Y%k$NDj5A$,B?$9$.$^$9!%(B

input stack size
================
$B$3$N%(%i!<$O$*$=$i$/L?NaDj5ACf$N8m$j$K$h$k$b$N$G$9!%Nc$($P!$<!$NL?Na$O(B
$B:F5"E*Dj5A$H$J$C$F$*$j!$<+J,<+?H$r;H$C$F(B\\gnu$B$rDj5A$7$F$$$^$9!%(B

	  \\newcommand{\\gnu}{a \\gnu} % $B$3$l$O$@$a(B

$B$3$N(B\\gnu$BL?Na$r8+$D$1$k$H(BTeX$B$O(B\\gnu$B$,2?$r$&$_$@$9$N$+$r7hDj$7$h$&$H$7(B
$B$F$=$NKvHx$r$$$D$^$G$bDI$$$D$E$1!$$d$,$F(B``input stack''$B$r;H$$$-$C$F$7(B
$B$^$$$^$9!%(B

main memory size
================
$B$3$l$O!$(BTeX$B$,C;$$%U%!%$%k$r=hM}$7$F$$$k;~$K;H$$$-$k2DG=@-$N$"$k%a%b%j(B
$B$N$R$H$D$G$9!%(Bmain memory$B$r;H$$$-$k$N$O<!$N(B3$B$D$N>l9g$N$$$:$l$+$G$9!%(B
\(1\)$BHs>o$KD9$/J#;($JL?Na$r?tB?$/Dj5A$7$?!%(B(2)index$B$^$?$O(Bglossary$B$r:n$C(B
$B$F$$$k$H$-!$(B1$B%Z!<%8Cf$K$"$^$j$K$bB?$/$N(B\\index$B$^$?$O(B\\glossary$BL?Na$,$"(B
$B$k!%(B(3)$B@8@.$N$?$a$N>pJs$r(BTeX$B$,J];}$7$-$l$J$$$h$&$J!$$"$^$j$K$bJ#;($J%Z!<(B
$B%8$r@8@.$7$h$&$H$7$?!%:G=i$N(B2$B$D$NLdBj$N2r7hJ}K!$OL@$i$+$G$9!%L?NaDj5A(B
$B$N?t$"$k$$$O(B\\index$B!&(B\\glossary$BL?Na$N?t$r8:$i$9$3$H$G$9!%(B3$BHVL\$NLdBj$O(B
$B$A$g$C$HLq2p$G$9!%$3$l$O!$Bg$-$J(Btabbing$B!&(Btabular$B!&(Barray$B!&(Bpicture$B4D6-$N(B
$B$;$$$G@8$8$k$3$H$,$"$j$^$9!%=PNO0LCV$,7hDj$5$l$k$N$rBT$C$F$$$k?^$dI=$G(B
TeX$B$N%a%b%j$,$$$C$Q$$$K$J$C$F$$$k$N$+$b$7$l$^$;$s!%K\Ev$K(BTeX$B$NMFNL$rD6(B
$B$($F$7$^$C$?$N$+$I$&$+D4$Y$k$?$a$K$O!$%(%i!<$N5/$3$C$?>l=j$ND>A0$K(B
\\clearpage$BL?Na$rF~$l$F$b$&0lEY%3%s%Q%$%k$r<B9T$7$F$_$F$/$@$5$$!%$b$7(B
$B$=$l$G$b%a%b%j$,B-$j$J$/$J$k$h$&$J$i!$$J$s$i$+$N<jCJ$r9V$8$kI,MW$,$"$j(B
$B$^$9!%(BTeX$B$,%Z!<%8$r@ZCG$9$k$+$I$&$+7hDj$9$k$?$a$K$OCJMnA4BN$r=hM}$7$J(B
$B$1$l$P$J$i$J$$$H$$$&$3$H$r;W$$$@$7$F$/$@$5$$!%CJMn$NESCf$K(B\\newpage$BL?(B
$BNa$rF~$l$l$P!$CJMn$N;D$j$r=hM}$9$kA0$K:#$N%Z!<%8$r(BTeX$B$K=PNO$5$;$k$3$H(B
$B$GM>M5$,$G$-$k$+$b$7$l$^$;$s(B(\\pagebreak$BL?Na$G$O$@$a$G$9(B)$B!%$b$7?^$dI=(B
$B$,N/$^$C$F$$$k$3$H$,LdBj$J$N$J$i$P!$?^I=$r$b$C$H8e$m$NJ}$K0\F0$9$k$H$+!$(B
$B$"$k$$$O$b$C$HA0$N;~E@$G=PNO$5$l$k$h$&$K$9$l$P2sHr$G$-$^$9!%$b$7$^$@J8(B
$B=q$r:n@.$7$F$$$kESCf$J$i!$$H$j$"$($:(B\\clearpage$BL?Na$rF~$l$F$*$$$F!$:G(B
$B=*HG$r:n$k;~$^$G$3$NLdBj$OC*>e$2$7$F$*$-$^$7$g$&!%F~NO%U%!%$%k$,JQ$o$k(B
$B$HLdBj$,2r>C$5$l$k>l9g$b$"$k$N$G$9!%(B

pool size
=========
$BAj8_;2>H$N(B\\label$B$,B?$9$.$k$+!$L?Na$NDj5A$,B?$9$.$k$+$N$I$A$i$+$G$9!%(B
$B@53N$K$$$($P!$Dj5A$7$?%i%Y%kL>$*$h$SL?NaL>$K;H$C$?J8;z?t$,B?$9$.$k$H$$(B
$B$&$3$H$G$9!%$G$9$+$i!$$b$C$HC;$$L>A0$r;H$($P$3$NLdBj$O2r7h$7$^$9!%$?$@(B
$B$7!$$3$N%(%i!<$O!$(B\\setcounter$B$J$I$N%+%&%s%?L?Na$d(B\\newenvironment$B!&(B
\\newtheorem$BL?Na$N0z?t$N=*$o$j$r<($91&3g8L$rK:$l$?>l9g$K$b@8$8$^$9!%(B

save size
=========
$B$3$N%(%i!<$O!$@k8@$NM-8zHO0O$dL?Na!&4D6-$,$"$^$j$K$b?<$/F~$l;R$K$J$C$F(B
$B$$$k>l9g$K@8$8$^$9!%$?$H$($P!$(B\\multiput$BL?Na$N0z?t$K(Bpicture$B4D6-$,$"$j!$(B
$B$=$N$J$+$K(B\\footnotesize$B@k8@$,$"$j!$$=$N@k8@$NM-8zHO0O$K(B\\multiput$BL?Na(B
$B$,$"$C$F!$$=$N0z?t$K(B... $B$H$$$&$h$&$J>l9g$G$9!%(B")

    ("Text line contains an invalid character." .
"$BF~NOCf$KIT@5$JJ8;z$,4^$^$l$F$$$^$9!%%U%!%$%k:n@.$N8m$j$K$h$C$F%F%-%9%H(B
$B%(%G%#%?$,$3$NJ8;z$rA^F~$7$F$7$^$C$?$N$G$7$g$&!%<B:]$K2?$,5/$-$?$N$+$O(B
$B%(%G%#%?$K$h$j$^$9!%F~NO%U%!%$%k$rD4$Y$F$_$F!$;XE&$5$l$?J8;z$,8+$D$+$i(B
$B$J$$>l9g$K$O%m!<%+%k%,%$%I$r8+$F$/$@$5$$!%(B")

    ("Undefined control sequence."   .
"TeX$B$,L$Dj5A$NL?NaL>$rH/8+$7$^$7$?!%$*$=$i$/F~NO$N8m$j$G$7$g$&!%$b$7$3(B
$B$N%(%i!<$,(BLaTeX$BL?Na$N=hM}Cf$K@8$8$?>l9g$O!$$=$NL?Na$O4V0c$C$?0LCV$KCV$+(B
$B$l$F$$$^$9!%Nc$($P!$%j%9%H4D6-$NCf$G$J$$$N$K(B\\item$BL?Na$,;H$o$l$?>l9g$J$I(B
$B$G$9!%$^$?!$(B\\documentclass$BL?Na$,$J$$>l9g$K$b$3$N%(%i!<$,@8$8$^$9!%(B")

    ("Use of [^ ]* doesn't match its definition." .
"$B$*$=$i$/IA2h$N$?$a$NL?Na$@$H;W$o$l$^$9$,!$0z?t$N;H$$$+$?$,4V0c$C$F$$(B
$B$^$9!%4V0c$C$F$$$k$N$,(B\\@array$BL?Na$N>l9g$O!$(Barray$B4D6-$+(Btabular$B4D6-$G$N(B
@$BI=8=$N0z?t$K$J$K$+8m$j$,$"$k$N$G$7$g$&!%(Bfragile$B$JL?Na$,(B\\protect$B$5$l$F(B
$B$$$J$$$N$+$b$7$l$^$;$s!%(B")

    ("You can't use `macro parameter character \\#' in [^ ]* mode." .
"$BFC<lJ8;z(B#$B$,IaDL$N%F%-%9%H$NCf$K8=$l$^$7$?!%$*$=$i$/(B\\#$B$H=q$-$?$+$C$?(B
$B$N$G$7$g$&!%(B")

    ("Overfull \\\\hbox .*" .
"$B9TJ,3d$N$?$a$NE,@Z$J>l=j$,8+$D$+$i$J$+$C$?$N$G!$(B1$B9T$K<}$^$k$Y$-J,NL0J>e(B
$B$N=PNO$,9T$J$o$l$F$7$^$$$^$7$?!%(B")

    ("Overfull \\\\vbox .*" .
"$B%Z!<%8J,3d$N$?$a$NE,@Z$J>l=j$,8+$D$+$i$J$+$C$?$N$G!$(B1$B%Z!<%8$K<}$^$k$Y$-(B
$BJ,NL0J>e$N=PNO$,9T$J$o$l$F$7$^$$$^$7$?!%(B")

    ("Underfull \\\\hbox .*" .
"$BM>J,$J?bD>%9%Z!<%9$,$J$$$+$I$&$+=PNO$r3N$+$a$F$/$@$5$$!%$b$7$"$l$P!$$=(B
$B$l$O(B\\\\$BL?Na$^$?$O(B\\newline$BL?Na$K4X78$9$kLdBj$N$?$a$K@8$8$?$b$N$G$9!%Nc(B
$B$($P(B2$B$D$N(B\\\\$BL?Na$,B3$$$F$$$k>l9g$J$I$G$9!%$3$N7Y9p$O(Bsloppypar$B4D6-$d(B
\\sloppy$B@k8@$N;HMQ!$$"$k$$$O(B\\linebreak$BL?Na$NA^F~$J$I$K$h$k>l9g$b$"$j$^$9!%(B")

    ("Underfull \\\\vbox .*" .
"$B%Z!<%8$rJ,3d$9$k$?$a$NE,@Z$J>l=j$,8+$D$1$i$l$:!$==J,$J%F%-%9%H$N$J$$(B
$B%Z!<%8$,$G$-$F$7$^$$$^$7$?!%(B")

;; New list items should be placed here
;;
;; ("err-regexp" . "context")
;;
;; the err-regexp item should match anything

    (".*" . "$B$4$a$s$J$5$$!%3:Ev$9$k%X%k%W%a%C%;!<%8$,$"$j$^$;$s!%(B"))))

(provide 'tex-jp)

;;; tex-jp.el ends here
