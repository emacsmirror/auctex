;;; minted2.el --- AUCTeX style for `minted2.sty' (v2.9)  -*- lexical-binding: t; -*-

;; Copyright (C) 2014--2025 Free Software Foundation, Inc.

;; Author: Tassilo Horn <tsdh@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2014-12-19
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any
;; later version.

;; AUCTeX is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file adds support for `minted2.sty' (v2.9) from 2024/05/09.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(declare-function font-latex-set-syntactic-keywords
                  "font-latex")

(declare-function LaTeX-color-definecolor-list "color" ())
(declare-function LaTeX-xcolor-definecolor-list "xcolor" ())
(declare-function LaTeX-add-newfloat-DeclareFloatingEnvironments
                  "newfloat" (&rest newfloat-declarefloatingenvironments))

(defvar font-latex-syntactic-keywords-extra)
(defvar LaTeX-fontenc-package-options)

(defvar LaTeX-minted2-key-val-options
  '(("autogobble" ("true" "false"))
    ("baselinestretch" ("auto"))
    ("beameroverlays" ("true" "false"))
    ("breakafter")
    ("breakafterinrun" ("true" "false"))
    ("breakaftersymbolpre")
    ("breakaftersymbolpost")
    ("breakanywhere" ("true" "false"))
    ("breakanywheresymbolpre")
    ("breakanywheresymbolpost")
    ("breakautoindent" ("true" "false"))
    ("breakbefore")
    ("breakbeforeinrun" ("true" "false"))
    ("breakbeforesymbolpre")
    ("breakbeforesymbolpost")
    ("breakbytoken" ("true" "false"))
    ("breakbytokenanywhere" ("true" "false"))
    ("breakindent")
    ("breakindentnchars")
    ("breaklines" ("true" "false"))
    ("breaksymbol")
    ("breaksymbolleft")
    ("breaksymbolright")
    ("breaksymbolindent")
    ("breaksymbolindentnchars")
    ("breaksymbolindentleft")
    ("breaksymbolindentleftnchars")
    ("breaksymbolindentright")
    ("breaksymbolseprightnchars")
    ("breaksymbolsep")
    ("breaksymbolsepnchars")
    ("breaksymbolsepleft")
    ("breaksymbolsepleftnchars")
    ("breaksymbolsepright")
    ("breaksymbolseprightnchars")
    ("bgcolor")
    ("codetagify")
    ("curlyquotes" ("true" "false"))
    ("encoding")
    ("escapeinside")
    ("firstline")
    ("firstnumber" ("auto" "last" "integer"))
    ("fontfamily" ("tt" "courier" "helvetica"))
    ("fontseries" ("auto"))
    ("fontsize" ("auto" "\\tiny" "\\large" "\\scriptsize" "\\Large"
                 "\\footnotesize" "\\LARGE" "\\small" "\\huge"
                 "\\normalsize" "\\Huge"))
    ("fontshape" ("auto"))
    ("formatcom")
    ("frame" ("none" "leftline" "topline" "bottomline" "lines" "single"))
    ("framerule")
    ("framesep")
    ("funcnamehighlighting" ("true" "false"))
    ("gobble")
    ("highlightcolor")
    ("highlightlines")
    ("ignorelexererrors" ("true" "false"))
    ("keywordcase" ("lower" "upper" "capitalize"))
    ("label")
    ("labelposition" ("none" "topline" "bottomline" "all"))
    ("lastline")
    ("linenos" ("true" "false"))
    ("numberfirstline" ("true" "false"))
    ("numbers" ("left" "right" "both" "none"))
    ("mathescape" ("true" "false"))
    ("numberblanklines" ("true" "false"))
    ("numbersep")
    ("obeytabs" ("true" "false"))
    ("outencoding")
    ("python3" ("true" "false"))
    ("resetmargins" ("true" "false"))
    ("rulecolor")
    ("samepage" ("true" "false"))
    ("showspaces" ("true" "false"))
    ("showtabs" ("true" "false"))
    ("space")
    ("spacecolor")
    ("startinline" ("true" "false"))
    ;; FIXME: It would be nice to use the function
    ;; `LaTeX-minted2-style-list' here, but with a file local var like:
    ;;     %%% TeX-command-extra-options: "-shell-escape"
    ;; in a .tex file, Emacs asks to apply a variable which is not
    ;; safe and does not restore the window; the splitted frame
    ;; remains.  I couldn't figure out why, so for now, I add the
    ;; styles from Pygments version 2.18.0 here.
    ("style" ("abap" "algol" "algol_nu" "arduino" "autumn"
              "bw" "borland" "coffee" "colorful" "default" "dracula"
              "emacs" "friendly_grayscale" "friendly" "fruity"
              "github-dark" "gruvbox-dark" "gruvbox-light"
              "igor" "inkpot" "lightbulb" "lilypond"
              "lovelace" "manni" "material" "monokai" "murphy"
              "native" "nord-darker" "nord"
              "one-dark" "paraiso-dark" "paraiso-light" "pastie"
              "perldoc" "rainbow_dash" "rrt" "sas"
              "solarized-dark" "solarized-light" "staroffice"
              "stata-dark" "stata-light" "tango" "trac"
              "vim" "vs" "xcode" "zenburn"))
    ("stepnumber")
    ("stepnumberfromfirst")
    ("stepnumberoffsetvalues" ("true" "false"))
    ("stripall" ("true" "false"))
    ("stripnl" ("true" "false"))
    ("tab")
    ("tabcolor")
    ("tabsize")
    ("texcl" ("true" "false"))
    ("texcomments" ("true" "false"))
    ("xleftmargin")
    ("xrightmargin"))
  "Key=value options for minted2 macros and environments.")

(defun LaTeX-minted2-key-val-options ()
  "Return an updated list of key=vals from minted2 package.
This function retrieves values of (user) defined colors and
prepends them to variable `LaTeX-minted2-key-val-options'."
  (append
   (when (or (member "xcolor" (TeX-style-list))
             (member "color" TeX-active-styles))
     (let* ((colorcmd (if (member "xcolor" TeX-active-styles)
                          #'LaTeX-xcolor-definecolor-list
                        #'LaTeX-color-definecolor-list))
            (colors (mapcar #'car (funcall colorcmd)))
            (keys '("bgcolor" "highlightcolor"
                    "rulecolor" "spacecolor" "tabcolor"))
            result)
       (dolist (key keys result)
         (push (list key colors) result))))
   LaTeX-minted2-key-val-options))

(defvar LaTeX-minted2-pygmentize-program (executable-find "pygmentize")
  "Absolute file name to pygmentize program.")

(defvar LaTeX-minted2-language-list nil
  "List containing languages provided by pymentize program.
Initialized once at the first prompt for a Pygments language.
May be reset with `\\[universal-argument] \\[TeX-normal-mode]'.")

;; Add the variable to `TeX-normal-mode-reset-list':
(add-to-list 'TeX-normal-mode-reset-list 'LaTeX-minted2-language-list)

(defun LaTeX-minted2-language-list (&rest _ignored)
  "Return a list of languages provided by pymentize program.
Update the variable `LaTeX-minted2-language-list' if still nil."
  (or LaTeX-minted2-language-list
      (when LaTeX-minted2-pygmentize-program
        (with-temp-buffer
          (shell-command (concat LaTeX-minted2-pygmentize-program " -L lexers")
                         (current-buffer))
          (goto-char (point-min))
          (let (languages)
            (while (re-search-forward "^\\*[[:space:]]\\([^:]+\\):" nil t)
              (dolist (lang (split-string (match-string 1) "[[:space:],]" t))
                (push lang languages)))
            (setq LaTeX-minted2-language-list languages))
          LaTeX-minted2-language-list))))

(defvar LaTeX-minted2-style-list nil
  "List containing styles provided by pymentize program.
Initialized once at the first prompt for a Pygments style.
May be reset with `\\[universal-argument] \\[TeX-normal-mode]'.")

;; Add the variable to `TeX-normal-mode-reset-list':
(add-to-list 'TeX-normal-mode-reset-list 'LaTeX-minted2-style-list)

(defun LaTeX-minted2-style-list (&rest _ignored)
  "Return a list of styles provided by pymentize program.
Update the variable `LaTeX-minted2-style-list' if still nil."
  (or LaTeX-minted2-style-list
      (when LaTeX-minted2-pygmentize-program
        (with-temp-buffer
          (shell-command (concat LaTeX-minted2-pygmentize-program " -L styles")
                         (current-buffer))
          (goto-char (point-min))
          (let (styles)
            (while (re-search-forward "^\\*[[:space:]]\\([^:]+\\):" nil t)
              (dolist (style (split-string (match-string 1) "[[:space:],]" t))
                (push style styles)))
            (setq LaTeX-minted2-style-list styles))
          LaTeX-minted2-style-list))))

(TeX-auto-add-type "minted2-newmint" "LaTeX")

(defvar LaTeX-minted2-newmint-regexp
  '("\\\\newmint\\(edfile\\|inline\\|ed\\)?\\(?:\\[\\([^]]+\\)\\]\\)?{\\([^}]+\\)}"
    (0 2 3 1) LaTeX-auto-minted2-newmint)
  "Match the arguments of \\newmint* macros from minted2 package.")

(defun LaTeX-minted2-auto-prepare ()
  "Reset `LaTeX-auto-minted2-newmint' before parsing."
  (setq LaTeX-auto-minted2-newmint nil))

(defun LaTeX-minted2-auto-cleanup ()
  "Process the parsed results from minted2 package."
  (dolist (mint (LaTeX-minted2-newmint-list))
    (pcase (nth 3 mint)
      ;; \newminted{lang}{opts} => new langcode and langcode* envs.
      ;; \newminted[envname]{lang}{opts} => new envname/envname* envs.
      ("ed" (let* ((env (if (string-empty-p (nth 1 mint))
                            (concat (nth 2 mint) "code")
                          (nth 1 mint)))
                   (env* (concat env "*")))
              (LaTeX-add-environments (list env))
              (LaTeX-add-environments (list env* #'LaTeX-env-args
                                            '(TeX-arg-key-val
                                              (LaTeX-minted2-key-val-options))))
              (add-to-list 'LaTeX-indent-environment-list
                           `(,env current-indentation) t)
              (add-to-list 'LaTeX-indent-environment-list
                           `(,env* current-indentation) t)
              (add-to-list 'LaTeX-verbatim-environments-local env)
              (add-to-list 'LaTeX-verbatim-environments-local env*)))
      ;; \newmintinline{foo}{opts} => \fooinline[key=vals]|code| or
      ;;                              \fooinline[key=vals]{code}
      ;; \newmintinline[macname]{foo}{opts} => \macname[key=vals]|code| or
      ;;                                       \macname[key=vals]{code}
      ("inline" (let ((lang (if (string-empty-p (nth 1 mint))
                                (concat (nth 2 mint) "inline")
                              (nth 1 mint))))
                  (TeX-add-symbols
                   `(,lang [TeX-arg-key-val (LaTeX-minted2-key-val-options)]
                           TeX-arg-verb-delim-or-brace))
                  (add-to-list 'LaTeX-verbatim-macros-with-delims-local lang)
                  (add-to-list 'LaTeX-verbatim-macros-with-braces-local lang)
                  (when (and (fboundp 'font-latex-add-keywords)
                             (eq TeX-install-font-lock 'font-latex-setup))
                    (font-latex-add-keywords `((,lang "[")) 'textual))))
      ;; \newmintedfile{foo}{opts} => \foofile[key=vals]{file-name}
      ;; \newmintedfile[macname]{foo}{opts} => \macname[key=vals]{file-name}
      ("edfile" (let ((lang (if (string-empty-p (nth 1 mint))
                                (concat (nth 2 mint) "file")
                              (nth 1 mint))))
                  (TeX-add-symbols
                   `(,lang [TeX-arg-key-val (LaTeX-minted2-key-val-options)]
                           TeX-arg-file))))
      ;; \newmint{foo}{opts} => \foo[key=vals]|code| or
      ;;                        \foo[key=vals]{code}
      ;; \newmint[macname]{foo}{opts} => \macname[key=vals]|code| or
      ;;                                 \macname[key=vals]{code}
      (_ (let ((lang (if (string-empty-p (nth 1 mint))
                         (nth 2 mint)
                       (nth 1 mint))))
           (TeX-add-symbols
            `(,lang [TeX-arg-key-val (LaTeX-minted2-key-val-options)]
                    TeX-arg-verb-delim-or-brace))
           (add-to-list 'LaTeX-verbatim-macros-with-delims-local lang)
           (add-to-list 'LaTeX-verbatim-macros-with-braces-local lang)
           (when (and (fboundp 'font-latex-add-keywords)
                      (eq TeX-install-font-lock 'font-latex-setup))
             (font-latex-add-keywords `((,lang "[")) 'textual))))))
  ;; Refresh font-locking so that the verbatim envs take effect only
  ;; when there are defined shortcuts:
  (when (and (LaTeX-minted2-newmint-list)
             (fboundp 'font-latex-set-syntactic-keywords)
             (eq TeX-install-font-lock 'font-latex-setup))
    (font-latex-set-syntactic-keywords)))

(add-hook 'TeX-auto-prepare-hook #'LaTeX-minted2-auto-prepare t)
(add-hook 'TeX-auto-cleanup-hook #'LaTeX-minted2-auto-cleanup t)
(add-hook 'TeX-update-style-hook #'TeX-auto-parse t)

(defun LaTeX-minted2-add-syntactic-keywords-extra (type macro)
  "Add TYPE of MACRO to `font-latex-syntactic-keywords-extra'.
TYPE is one of the symbols `brace' or `delim' indicating how
verbatim text is enclosed after the macro.  MACRO is a string or
a list of strings."
  (let ((syntax (if (eq type 'brace)
                    '((1 "|") (2 "|"))
                  '((1 "\"") (2 ".") (3 "\""))))
        regexp)
    (when (listp macro)
      (setq macro (regexp-opt macro "\\(?:")))
    (setq regexp `(,(concat
                     ;; The backslash
                     (regexp-quote TeX-esc)
                     ;; Name of the macro(s)
                     macro
                     ;; The optional argument
                     "\\(?:\\[[^][]*\\(?:\\[[^][]*\\][^][]*\\)*\\]\\)?"
                     ;; The first mandatory argument
                     "\\(?:{[^}]+}\\)"
                     ;; With 'brace, allow braced sub-groups otherwise
                     ;; we stop matching too early.  With 'delim, copy
                     ;; font-latex.el:
                     (if (eq type 'brace)
                         (concat "\\({\\)"
                                   "\\(?:[^}{]*"
                                     "\\(?:{[^}{]*"
                                       "\\(?:{[^}{]*"
                                         "\\(?:{[^}{]*}[^}{]*\\)*"
                                       "}[^}{]*\\)*"
                                     "}[^}{]*\\)*"
                                   "\\)"
                                 "\\(}\\)")
                       (concat
                        ;; Opening delimiter
                        "\\([^a-z@*\n\f{]\\).*?"
                        ;; Closing delimiter
                        "\\(" (regexp-quote TeX-esc) "*\\)\\(\\1\\)")))))
    (add-to-list 'font-latex-syntactic-keywords-extra (append regexp syntax))))

(TeX-add-style-hook
 "minted2"
 (lambda ()

   ;; Run the relevant style hooks and follow what minted2.sty does:
   (unless (member "color" (TeX-style-list))
     (TeX-run-style-hooks "xcolor"))
   (TeX-run-style-hooks "fvextra")

   ;; New symbols
   (TeX-add-symbols
    '("mint"
      [TeX-arg-key-val (LaTeX-minted2-key-val-options)]
      (TeX-arg-completing-read (LaTeX-minted2-language-list) "Language")
      TeX-arg-verb-delim-or-brace)
    '("mintinline"
      [TeX-arg-key-val (LaTeX-minted2-key-val-options)]
      (TeX-arg-completing-read (LaTeX-minted2-language-list) "Language")
      TeX-arg-verb-delim-or-brace)
    '("newminted" ["Environment Name"]
      (TeX-arg-completing-read (LaTeX-minted2-language-list) "Language")
      (TeX-arg-key-val (LaTeX-minted2-key-val-options)))
    '("newmint" ["Macro Name"]
      (TeX-arg-completing-read (LaTeX-minted2-language-list) "Language")
      (TeX-arg-key-val (LaTeX-minted2-key-val-options)))
    '("newmintinline" ["Macro Name"]
      (TeX-arg-completing-read (LaTeX-minted2-language-list) "Language")
      (TeX-arg-key-val (LaTeX-minted2-key-val-options)))
    '("newmintedfile" ["Macro Name"]
      (TeX-arg-completing-read (LaTeX-minted2-language-list) "Language")
      (TeX-arg-key-val (LaTeX-minted2-key-val-options)))
    ;; 3.3 Formatting source code
    '("inputminted"
      [TeX-arg-key-val (LaTeX-minted2-key-val-options)]
      (TeX-arg-completing-read (LaTeX-minted2-language-list) "Language")
      TeX-arg-file)
    ;; 3.4 Using different styles
    '("usemintedstyle"
      [TeX-arg-completing-read (LaTeX-minted2-language-list) "Language"]
      (TeX-arg-completing-read (LaTeX-minted2-style-list) "Style"))
    ;; 5.2 Macro option usage
    '("setminted"
      [TeX-arg-completing-read (LaTeX-minted2-language-list) "Language"]
      (TeX-arg-key-val (LaTeX-minted2-key-val-options)))
    '("setmintedinline"
      [TeX-arg-completing-read (LaTeX-minted2-language-list) "Language"]
      (TeX-arg-key-val (LaTeX-minted2-key-val-options))))

   ;; New environments
   (LaTeX-add-environments
    '("minted" LaTeX-env-args
      [TeX-arg-key-val (LaTeX-minted2-key-val-options)]
      (TeX-arg-completing-read (LaTeX-minted2-language-list) "Language")))

   ;; 4 Floating listings: If option "newfloat" is given, run the
   ;; style hook and use the interface provided by the style,
   ;; otherwise add "listing" manually
   (if (or (LaTeX-provided-package-options-member "minted2" "newfloat")
           (LaTeX-provided-package-options-member "minted2" "newfloat=true"))
       (progn
         (TeX-run-style-hooks "newfloat")
         (LaTeX-add-newfloat-DeclareFloatingEnvironments
          '("listing" "verbatim")))
     (LaTeX-add-environments '("listing" ["Float Position"]))
     (TeX-add-symbols '("listoflistings")
                      '("listingscaption")
                      '("listoflistingscaption"))
     (add-to-list (make-local-variable 'LaTeX-indent-environment-list)
                  '("listing" current-indentation) t)
     (add-to-list 'LaTeX-label-alist '("listing" . LaTeX-listing-label) t)
     (when (fboundp 'reftex-add-label-environments)
       (reftex-add-label-environments
        '(("listing" ?l "lst:" "~\\ref{%s}" caption nil nil)))))

   ;; Add to the auto parser
   (TeX-auto-add-regexp LaTeX-minted2-newmint-regexp)

   ;; Filling
   (add-to-list (make-local-variable 'LaTeX-indent-environment-list)
                '("minted" current-indentation) t)
   (add-to-list 'LaTeX-verbatim-environments-local "minted")

   ;; Fontification
   (when (and (fboundp 'font-latex-add-keywords)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("usemintedstyle"  "[{")
                                ("setminted"       "[{")
                                ("setmintedinline" "[{")
                                ("newminted"       "[{{")
                                ("newmint"         "[{{")
                                ("newmintinline"   "[{{")
                                ("newmintedfile"   "[{{"))
                              'function)
     (font-latex-add-keywords '(("inputminted" "[{{")
                                ("mint"        "[{")
                                ("mintinline"  "[{"))
                              'textual)
     ;; Add \mint & \mintinline to
     ;; `font-latex-syntactic-keywords-extra' and cater for their
     ;; special syntax: \mint[optional]{lang}{verbatim} or
     ;;                 \mint[optional]{lang}|verbatim|
     (LaTeX-minted2-add-syntactic-keywords-extra 'brace
                                                 '("mint" "mintinline"))
     (LaTeX-minted2-add-syntactic-keywords-extra 'delim
                                                 '("mint" "mintinline"))
     ;; Tell font-lock about the update.
     (font-latex-set-syntactic-keywords)))
 TeX-dialect)

(defun LaTeX-minted2-package-options-list ()
  "Return an alist of package options for minted2 package."
  (append
   `(("fontencoding"
      ,(when (member "fontenc" (TeX-style-list))
         LaTeX-fontenc-package-options)))
   '(("chapter")
     ("cache"            ("true" "false"))
     ("cachedir")
     ("finalizecache"    ("true" "false"))
     ("frozencache"      ("true" "false"))
     ("draft"            ("true" "false"))
     ("final"            ("true" "false"))
     ("inputlanglinenos" ("true" "false"))
     ("kpsewhich"        ("true" "false"))
     ("langlinenos"      ("true" "false"))
     ("newfloat"         ("true" "false"))
     ("outputdir")
     ("section"))))

(defun LaTeX-minted2-package-options ()
  "Prompt for package options for the minted2 package."
  (TeX-read-key-val t (LaTeX-minted2-package-options-list)))

;;; minted2.el ends here
