;;; auctb.el --- toolbar icons on AUC-TeX in GNU emacs and XEmacs

;; Copyright (C) 2004 Miguel V. S. Frasson

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;; Author: Miguel V. S. Frasson <frasson@math.leidenuniv.nl>
;; Created: 31 Dec 2003
;; Keywords: tool-bar

;;; Use of this preliminary version

;; Requirements for the images: 1) you should have a folder called "symb-pics"
;; with the pics of the symbols (xpm format is a good one), and the *parent*
;; of this folder should be in `load-path'; 2) each image file is named after
;; the command that it represents in the following rules: the base name is the
;; name of the command without the escape character "\", like \delta ->
;; "delta.xpm"; however, since in some OS filenames are case insensitive, all
;; occurences of capital letter should be replaced by the letter plus a dash:
;; \Rightarrow -> "R-ightarrow.xpm".

;; This package also needs `toolbarx.el' and `latex.el'

;;; Code

(eval-when-compile
  (require 'toolbarx)
  (require 'latex))


(defun auctb-img-filename (str)
  (let ((str-list (append str nil))
	(str-result))
    (dolist (i str-list)
      (cond 
       ;; capital letter -> letter + "-"
       ((and (>= i ?A) (<= i ?Z))
	(setq str-result (cons ?- (cons i str-result))))
       ;; lowercase letter -> letter
       ((and (>= i ?a) (<= i ?z))
        (setq str-result (cons i str-result)))
       ;; open curly brackets `{' -> "ocb--"
       ((eq i ?{)
	(setq str-result (cons ?o str-result))
	(setq str-result (cons ?c str-result))
	(setq str-result (cons ?b str-result))
	(setq str-result (cons ?- str-result))
	(setq str-result (cons ?- str-result)))
       ;; close curly brackets `}' -> "ccb--"
       ((eq i ?})
	(setq str-result (cons ?c str-result))
	(setq str-result (cons ?c str-result))
	(setq str-result (cons ?b str-result))
	(setq str-result (cons ?- str-result))
	(setq str-result (cons ?- str-result)))
       ;; vertical bar `|' -> "v--"
       ((eq i ?|)
	(setq str-result (cons ?v str-result))
	(setq str-result (cons ?- str-result))
	(setq str-result (cons ?- str-result)))
       ;; slash `/' -> "s--"
       ((eq i ?/)
	(setq str-result (cons ?s str-result))
	(setq str-result (cons ?- str-result))
	(setq str-result (cons ?- str-result)))))
    (concat "symb-pics/" (nreverse str-result))))

(let* ((menu-strings-buttons-alist
	;; make a alist os strings with the symbol classes and store it in
	;; `menu-strings-alist'
	(let* ((menu-strings-alist-temp))
	  (dolist (item-external (cdr LaTeX-math-menu)
				 (nreverse menu-strings-alist-temp))
	    (when (listp item-external)
	      ;; if first element is vector, I am supposing that all are
	      ;; vectors as well
	      (if (vectorp (cadr item-external))
		  (let* ((menu-str (car item-external))
			 (menu-buttons))
		    (dolist (button (cdr item-external))
		      (setq menu-buttons
			    (cons (list (intern (aref button 0))
					:image
					(auctb-img-filename (aref button 0))
					:help (aref button 0)
					:command (aref button 1))
				  menu-buttons)))
		    (setq menu-buttons (nreverse menu-buttons))
		    (setq menu-strings-alist-temp
			  (cons (cons menu-str
				      (list
				       (append menu-buttons
					       (list :toolbar
						     '(bottom . top)))))
				menu-strings-alist-temp)))
		;; if another list (therefore, up to second level menu)
		(let ((parent-str (concat (car item-external) " ")))
		  (dolist (item-internal (cdr item-external))
		    (let* ((menu-str (concat parent-str
					     (car item-internal)))
			   (menu-buttons))
		      (dolist (button (cdr item-internal))
			(setq menu-buttons
			      (cons (list (intern (aref button 0))
					  :image
					  (auctb-img-filename (aref button 0))
					  :help (aref button 0)
					  :command (aref button 1))
				    menu-buttons)))
		      (setq menu-buttons (nreverse menu-buttons))
		      (setq menu-strings-alist-temp
			    (cons (cons menu-str
					(list
					 (append menu-buttons
						 (list :toolbar
						       '(bottom . top)))))
				  menu-strings-alist-temp))))))))))
       (list-strings (let* ((list-str-temp))
		       (dolist (i menu-strings-buttons-alist
				  (nreverse list-str-temp))
			 (setq list-str-temp (cons (car i)
						   list-str-temp))))))
  (defvar LaTeX-symbols-toolbar-visible-flag nil
    "Non-nil means that the LaTeX symbols on toolbar are visible.")
  (defconst latex-symbols-toolbar-switch-contents
    `(;; the on-off switch button
      (latex-symbols-switch 
       :image (lambda nil (if LaTeX-symbols-toolbar-visible-flag
			      "ltx-symb-turn-off"
			    "ltx-symb-turn-on"))
       :command (progn
		  (setq LaTeX-symbols-toolbar-visible-flag
			(not LaTeX-symbols-toolbar-visible-flag))
		  (toolbarx-refresh))
					; help message depends on if
					; symb-toolbar is on or off, and
					; in the name of the current class
					; of symbols
       :help (lambda nil
	       (concat "Turn "
		       (if LaTeX-symbols-toolbar-visible-flag
			   "off "
			 "on ")
		       "the toolbar of LaTeX symbols (current class: "
		       (nth (1- LaTeX-symbols-active-menuitem)
			    (quote ,list-strings))
		       ")")))
      ;; the dropdown button, that also switch on the symbols
      ,(append '(:dropdown-group)
	       list-strings
	       '(:variable LaTeX-symbols-active-menuitem)
	       '(:save offer)
	       '(:dropdown-prepend-command
		 (setq LaTeX-symbols-toolbar-visible-flag t))
	       '(:dropdown-help "Select a class of symbols to be displayed"))))
  (defconst latex-symbols-toolbar-contents
    (let* ((ltx-symb)
	   (count 0))
      (dolist (i menu-strings-buttons-alist 
		 (append (nreverse ltx-symb)
			 '(:insert LaTeX-symbols-toolbar-visible-flag)))
	(setq count (1+ count))
	(setq ltx-symb
	      (cons (append (cdr i)
			    `(:insert (eq LaTeX-symbols-active-menuitem
					  ,count)))
		    ltx-symb))))))

(defun install-auctex-toolbar ()
  (interactive)
  (toolbarx-install-toolbar '(:eval-group latex-symbols-toolbar-switch-contents
					  latex-symbols-toolbar-contents)))

(provide 'auctb)

;;; auctb.el ends here.

;; (setq LaTeX-math-menu '("Math" ["Cal-whatever" LaTeX-math-cal t] ("Greek" ["Delta" LaTeX-math-Delta t] ["Phi" LaTeX-math-Phi t] ["Gamma" LaTeX-math-Gamma t] ["Theta" LaTeX-math-Theta t] ["Lambda" LaTeX-math-Lambda t] ["Psi" LaTeX-math-Psi t] ["Pi" LaTeX-math-Pi t] ["Sigma" LaTeX-math-Sigma t] ["Upsilon" LaTeX-math-Upsilon t] ["Omega" LaTeX-math-Omega t]) ("greek" ["alpha" LaTeX-math-alpha t] ["beta" LaTeX-math-beta t] ["delta" LaTeX-math-delta t] ["epsilon" LaTeX-math-epsilon t] ["phi" LaTeX-math-phi t] ["gamma" LaTeX-math-gamma t] ["eta" LaTeX-math-eta t] ["kappa" LaTeX-math-kappa t] ["lambda" LaTeX-math-lambda t] ["mu" LaTeX-math-mu t] ["nabla" LaTeX-math-nabla t] ["nu" LaTeX-math-nu t] ["omega" LaTeX-math-omega t] ["pi" LaTeX-math-pi t] ["theta" LaTeX-math-theta t] ["rho" LaTeX-math-rho t] ["sigma" LaTeX-math-sigma t] ["tau" LaTeX-math-tau t] ["upsilon" LaTeX-math-upsilon t] ["chi" LaTeX-math-chi t] ["psi" LaTeX-math-psi t] ["zeta" LaTeX-math-zeta t]) ("Binary Op" ["pm" LaTeX-math-pm t] ["mp" LaTeX-math-mp t] ["times" LaTeX-math-times t] ["div" LaTeX-math-div t] ["ast" LaTeX-math-ast t] ["star" LaTeX-math-star t] ["circ" LaTeX-math-circ t] ["bullet" LaTeX-math-bullet t] ["cdot" LaTeX-math-cdot t] ["cap" LaTeX-math-cap t] ["cup" LaTeX-math-cup t] ["uplus" LaTeX-math-uplus t] ["sqcap" LaTeX-math-sqcap t] ["vee" LaTeX-math-vee t] ["wedge" LaTeX-math-wedge t] ["setminus" LaTeX-math-setminus t] ["wr" LaTeX-math-wr t] ["diamond" LaTeX-math-diamond t] ["bigtriangleup" LaTeX-math-bigtriangleup t] ["bigtriangledown" LaTeX-math-bigtriangledown t] ["triangleleft" LaTeX-math-triangleleft t] ["triangleright" LaTeX-math-triangleright t] ["lhd" LaTeX-math-lhd t] ["rhd" LaTeX-math-rhd t] ["unlhd" LaTeX-math-unlhd t] ["unrhd" LaTeX-math-unrhd t] ["oplus" LaTeX-math-oplus t] ["ominus" LaTeX-math-ominus t] ["otimes" LaTeX-math-otimes t] ["oslash" LaTeX-math-oslash t] ["odot" LaTeX-math-odot t] ["bigcirc" LaTeX-math-bigcirc t] ["dagger" LaTeX-math-dagger t] ["ddagger" LaTeX-math-ddagger t] ["amalg" LaTeX-math-amalg t]) ("Relational" ["leq" LaTeX-math-leq t] ["geq" LaTeX-math-geq t] ["qed" LaTeX-math-qed t] ["equiv" LaTeX-math-equiv t] ["models" LaTeX-math-models t] ["prec" LaTeX-math-prec t] ["succ" LaTeX-math-succ t] ["sim" LaTeX-math-sim t] ["perp" LaTeX-math-perp t] ["preceq" LaTeX-math-preceq t] ["succeq" LaTeX-math-succeq t] ["simeq" LaTeX-math-simeq t] ["mid" LaTeX-math-mid t] ["ll" LaTeX-math-ll t] ["gg" LaTeX-math-gg t] ["asymp" LaTeX-math-asymp t] ["parallel" LaTeX-math-parallel t] ["subset" LaTeX-math-subset t] ["supset" LaTeX-math-supset t] ["approx" LaTeX-math-approx t] ["bowtie" LaTeX-math-bowtie t] ["subseteq" LaTeX-math-subseteq t] ["supseteq" LaTeX-math-supseteq t] ["cong" LaTeX-math-cong t] ["Join" LaTeX-math-Join t] ["sqsubset" LaTeX-math-sqsubset t] ["sqsupset" LaTeX-math-sqsupset t] ["neq" LaTeX-math-neq t] ["smile" LaTeX-math-smile t] ["sqsubseteq" LaTeX-math-sqsubseteq t] ["sqsupseteq" LaTeX-math-sqsupseteq t] ["doteq" LaTeX-math-doteq t] ["frown" LaTeX-math-frown t] ["in" LaTeX-math-in t] ["ni" LaTeX-math-ni t] ["propto" LaTeX-math-propto t] ["vdash" LaTeX-math-vdash t] ["dashv" LaTeX-math-dashv t]) ("Arrows" ["leftarrow" LaTeX-math-leftarrow t] ["Leftarrow" LaTeX-math-Leftarrow t] ["rightarrow" LaTeX-math-rightarrow t] ["Rightarrow" LaTeX-math-Rightarrow t] ["leftrightarrow" LaTeX-math-leftrightarrow t] ["Leftrightarrow" LaTeX-math-Leftrightarrow t] ["mapsto" LaTeX-math-mapsto t] ["hookleftarrow" LaTeX-math-hookleftarrow t] ["leftharpoonup" LaTeX-math-leftharpoonup t] ["leftharpoondown" LaTeX-math-leftharpoondown t] ["longleftarrow" LaTeX-math-longleftarrow t] ["Longleftarrow" LaTeX-math-Longleftarrow t] ["longrightarrow" LaTeX-math-longrightarrow t] ["Longrightarrow" LaTeX-math-Longrightarrow t] ["longleftrightarrow" LaTeX-math-longleftrightarrow t] ["Longleftrightarrow" LaTeX-math-Longleftrightarrow t] ["longmapsto" LaTeX-math-longmapsto t] ["hookrightarrow" LaTeX-math-hookrightarrow t] ["rightharpoonup" LaTeX-math-rightharpoonup t] ["rightharpoondown" LaTeX-math-rightharpoondown t] ["uparrow" LaTeX-math-uparrow t] ["Uparrow" LaTeX-math-Uparrow t] ["downarrow" LaTeX-math-downarrow t] ["Downarrow" LaTeX-math-Downarrow t] ["updownarrow" LaTeX-math-updownarrow t] ["Updownarrow" LaTeX-math-Updownarrow t] ["nearrow" LaTeX-math-nearrow t] ["searrow" LaTeX-math-searrow t] ["swarrow" LaTeX-math-swarrow t] ["nwarrow" LaTeX-math-nwarrow t]) ("Misc Symbol" ["ldots" LaTeX-math-ldots t] ["cdots" LaTeX-math-cdots t] ["vdots" LaTeX-math-vdots t] ["ddots" LaTeX-math-ddots t] ["aleph" LaTeX-math-aleph t] ["prime" LaTeX-math-prime t] ["forall" LaTeX-math-forall t] ["infty" LaTeX-math-infty t] ["hbar" LaTeX-math-hbar t] ["emptyset" LaTeX-math-emptyset t] ["exists" LaTeX-math-exists t] ["nabla" LaTeX-math-nabla t] ["surd" LaTeX-math-surd t] ["Box" LaTeX-math-Box t] ["triangle" LaTeX-math-triangle t] ["Diamond" LaTeX-math-Diamond t] ["imath" LaTeX-math-imath t] ["jmath" LaTeX-math-jmath t] ["ell" LaTeX-math-ell t] ["neg" LaTeX-math-neg t] ["not" LaTeX-math-not t] ["top" LaTeX-math-top t] ["flat" LaTeX-math-flat t] ["natural" LaTeX-math-natural t] ["sharp" LaTeX-math-sharp t] ["wp" LaTeX-math-wp t] ["bot" LaTeX-math-bot t] ["clubsuit" LaTeX-math-clubsuit t] ["diamondsuit" LaTeX-math-diamondsuit t] ["heartsuit" LaTeX-math-heartsuit t] ["spadesuit" LaTeX-math-spadesuit t] ["mho" LaTeX-math-mho t] ["Re" LaTeX-math-Re t] ["Im" LaTeX-math-Im t] ["angle" LaTeX-math-angle t] ["partial" LaTeX-math-partial t]) ("Var Symbol" ["sum" LaTeX-math-sum t] ["prod" LaTeX-math-prod t] ["coprod" LaTeX-math-coprod t] ["int" LaTeX-math-int t] ["oint" LaTeX-math-oint t] ["bigcap" LaTeX-math-bigcap t] ["bigcup" LaTeX-math-bigcup t] ["bigsqcup" LaTeX-math-bigsqcup t] ["bigvee" LaTeX-math-bigvee t] ["bigwedge" LaTeX-math-bigwedge t] ["bigodot" LaTeX-math-bigodot t] ["bigotimes" LaTeX-math-bigotimes t] ["bigoplus" LaTeX-math-bigoplus t] ["biguplus" LaTeX-math-biguplus t]) ("Log-like" ["arccos" LaTeX-math-arccos t] ["arcsin" LaTeX-math-arcsin t] ["arctan" LaTeX-math-arctan t] ["arg" LaTeX-math-arg t] ["cos" LaTeX-math-cos t] ["cosh" LaTeX-math-cosh t] ["cot" LaTeX-math-cot t] ["coth" LaTeX-math-coth t] ["csc" LaTeX-math-csc t] ["deg" LaTeX-math-deg t] ["det" LaTeX-math-det t] ["dim" LaTeX-math-dim t] ["exp" LaTeX-math-exp t] ["gcd" LaTeX-math-gcd t] ["hom" LaTeX-math-hom t] ["inf" LaTeX-math-inf t] ["ker" LaTeX-math-ker t] ["lg" LaTeX-math-lg t] ["lim" LaTeX-math-lim t] ["liminf" LaTeX-math-liminf t] ["limsup" LaTeX-math-limsup t] ["ln" LaTeX-math-ln t] ["log" LaTeX-math-log t] ["max" LaTeX-math-max t] ["min" LaTeX-math-min t] ["Pr" LaTeX-math-Pr t] ["sec" LaTeX-math-sec t] ["sin" LaTeX-math-sin t] ["sinh" LaTeX-math-sinh t] ["sup" LaTeX-math-sup t] ["tan" LaTeX-math-tan t] ["tanh" LaTeX-math-tanh t]) ("delimiters" ["uparrow" LaTeX-math-uparrow t] ["Uparrow" LaTeX-math-Uparrow t] ["downarrow" LaTeX-math-downarrow t] ["Downarrow" LaTeX-math-Downarrow t] ["{" LaTeX-math-{ t] ["}" LaTeX-math-} t] ["updownarrow" LaTeX-math-updownarrow t] ["Updownarrow" LaTeX-math-Updownarrow t] ["lfloor" LaTeX-math-lfloor t] ["rfloor" LaTeX-math-rfloor t] ["lceil" LaTeX-math-lceil t] ["rceil" LaTeX-math-rceil t] ["langle" LaTeX-math-langle t] ["rangle" LaTeX-math-rangle t] ["backslash" LaTeX-math-backslash t] ["|" LaTeX-math-| t]) ("Delimiters" ["rmoustache" LaTeX-math-rmoustache t] ["lmoustache" LaTeX-math-lmoustache t] ["rgroup" LaTeX-math-rgroup t] ["lgroup" LaTeX-math-lgroup t] ["arrowvert" LaTeX-math-arrowvert t] ["Arrowvert" LaTeX-math-Arrowvert t] ["bracevert" LaTeX-math-bracevert t]) ("Constructs" ["widetilde" LaTeX-math-widetilde t] ["widehat" LaTeX-math-widehat t] ["overleftarrow" LaTeX-math-overleftarrow t] ["overrightarrow" LaTeX-math-overrightarrow t] ["overline" LaTeX-math-overline t] ["underline" LaTeX-math-underline t] ["overbrace" LaTeX-math-overbrace t] ["underbrace" LaTeX-math-underbrace t] ["sqrt" LaTeX-math-sqrt t] ["frac" LaTeX-math-frac t]) ("Accents" ["hat" LaTeX-math-hat t] ["acute" LaTeX-math-acute t] ["bar" LaTeX-math-bar t] ["dot" LaTeX-math-dot t] ["breve" LaTeX-math-breve t] ["check" LaTeX-math-check t] ["grave" LaTeX-math-grave t] ["vec" LaTeX-math-vec t] ["ddot" LaTeX-math-ddot t] ["tilde" LaTeX-math-tilde t]) ("AMS" ("Hebrew" ["digamma" LaTeX-math-digamma t] ["varkappa" LaTeX-math-varkappa t] ["beth" LaTeX-math-beth t] ["daleth" LaTeX-math-daleth t] ["gimel" LaTeX-math-gimel t]) ("Arrows" ["dashrightarrow" LaTeX-math-dashrightarrow t] ["dashleftarrow" LaTeX-math-dashleftarrow t] ["leftleftarrows" LaTeX-math-leftleftarrows t] ["leftrightarrows" LaTeX-math-leftrightarrows t] ["Lleftarrow" LaTeX-math-Lleftarrow t] ["twoheadleftarrow" LaTeX-math-twoheadleftarrow t] ["leftarrowtail" LaTeX-math-leftarrowtail t] ["looparrowleft" LaTeX-math-looparrowleft t] ["leftrightharpoons" LaTeX-math-leftrightharpoons t] ["curvearrowleft" LaTeX-math-curvearrowleft t] ["circlearrowleft" LaTeX-math-circlearrowleft t] ["Lsh" LaTeX-math-Lsh t] ["upuparrows" LaTeX-math-upuparrows t] ["upharpoonleft" LaTeX-math-upharpoonleft t] ["downharpoonleft" LaTeX-math-downharpoonleft t] ["multimap" LaTeX-math-multimap t] ["leftrightsquigarrow" LaTeX-math-leftrightsquigarrow t] ["looparrowright" LaTeX-math-looparrowright t] ["rightleftharpoons" LaTeX-math-rightleftharpoons t] ["curvearrowright" LaTeX-math-curvearrowright t] ["circlearrowright" LaTeX-math-circlearrowright t] ["Rsh" LaTeX-math-Rsh t] ["downdownarrows" LaTeX-math-downdownarrows t] ["upharpoonright" LaTeX-math-upharpoonright t] ["downharpoonright" LaTeX-math-downharpoonright t] ["rightsquigarrow" LaTeX-math-rightsquigarrow t]) ("Neg Arrows" ["nleftarrow" LaTeX-math-nleftarrow t] ["nrightarrow" LaTeX-math-nrightarrow t] ["nLeftarrow" LaTeX-math-nLeftarrow t] ["nRightarrow" LaTeX-math-nRightarrow t] ["nleftrightarrow" LaTeX-math-nleftrightarrow t] ["nLeftrightarrow" LaTeX-math-nLeftrightarrow t]) ("Relational I" ["leqq" LaTeX-math-leqq t] ["leqslant" LaTeX-math-leqslant t] ["eqslantless" LaTeX-math-eqslantless t] ["lesssim" LaTeX-math-lesssim t] ["lessapprox" LaTeX-math-lessapprox t] ["approxeq" LaTeX-math-approxeq t] ["lessdot" LaTeX-math-lessdot t] ["lll" LaTeX-math-lll t] ["lessgtr" LaTeX-math-lessgtr t] ["lesseqgtr" LaTeX-math-lesseqgtr t] ["lesseqqgtr" LaTeX-math-lesseqqgtr t] ["doteqdot" LaTeX-math-doteqdot t] ["risingdotseq" LaTeX-math-risingdotseq t] ["fallingdotseq" LaTeX-math-fallingdotseq t] ["backsim" LaTeX-math-backsim t] ["backsimeq" LaTeX-math-backsimeq t] ["subseteqq" LaTeX-math-subseteqq t] ["Subset" LaTeX-math-Subset t] ["sqsubset" LaTeX-math-sqsubset t] ["preccurlyeq" LaTeX-math-preccurlyeq t] ["curlyeqprec" LaTeX-math-curlyeqprec t] ["precsim" LaTeX-math-precsim t] ["precapprox" LaTeX-math-precapprox t] ["vartriangleleft" LaTeX-math-vartriangleleft t] ["trianglelefteq" LaTeX-math-trianglelefteq t] ["vDash" LaTeX-math-vDash t] ["Vvdash" LaTeX-math-Vvdash t] ["smallsmile" LaTeX-math-smallsmile t] ["smallfrown" LaTeX-math-smallfrown t] ["bumpeq" LaTeX-math-bumpeq t] ["Bumpeq" LaTeX-math-Bumpeq t]) ("Relational II" ["geqq" LaTeX-math-geqq t] ["geqslant" LaTeX-math-geqslant t] ["eqslantgtr" LaTeX-math-eqslantgtr t] ["gtrsim" LaTeX-math-gtrsim t] ["gtrapprox" LaTeX-math-gtrapprox t] ["gtrdot" LaTeX-math-gtrdot t] ["ggg" LaTeX-math-ggg t] ["gtrless" LaTeX-math-gtrless t] ["gtreqless" LaTeX-math-gtreqless t] ["gtreqqless" LaTeX-math-gtreqqless t] ["eqcirc" LaTeX-math-eqcirc t] ["circeq" LaTeX-math-circeq t] ["triangleq" LaTeX-math-triangleq t] ["thicksim" LaTeX-math-thicksim t] ["thickapprox" LaTeX-math-thickapprox t] ["supseteqq" LaTeX-math-supseteqq t] ["Supset" LaTeX-math-Supset t] ["sqsupset" LaTeX-math-sqsupset t] ["succcurlyeq" LaTeX-math-succcurlyeq t] ["curlyeqsucc" LaTeX-math-curlyeqsucc t] ["succsim" LaTeX-math-succsim t] ["succapprox" LaTeX-math-succapprox t] ["vartriangleright" LaTeX-math-vartriangleright t] ["trianglerighteq" LaTeX-math-trianglerighteq t] ["Vdash" LaTeX-math-Vdash t] ["shortmid" LaTeX-math-shortmid t] ["shortparallel" LaTeX-math-shortparallel t] ["between" LaTeX-math-between t] ["pitchfork" LaTeX-math-pitchfork t] ["varpropto" LaTeX-math-varpropto t] ["blacktriangleleft" LaTeX-math-blacktriangleleft t] ["therefore" LaTeX-math-therefore t] ["backepsilon" LaTeX-math-backepsilon t] ["blacktriangleright" LaTeX-math-blacktriangleright t] ["because" LaTeX-math-because t]) ("Neg Rel I" ["nless" LaTeX-math-nless t] ["nleq" LaTeX-math-nleq t] ["nleqslant" LaTeX-math-nleqslant t] ["nleqq" LaTeX-math-nleqq t] ["lneq" LaTeX-math-lneq t] ["lneqq" LaTeX-math-lneqq t] ["lvertneqq" LaTeX-math-lvertneqq t] ["lnsim" LaTeX-math-lnsim t] ["lnapprox" LaTeX-math-lnapprox t] ["nprec" LaTeX-math-nprec t] ["npreceq" LaTeX-math-npreceq t] ["precnsim" LaTeX-math-precnsim t] ["precnapprox" LaTeX-math-precnapprox t] ["nsim" LaTeX-math-nsim t] ["nshortmid" LaTeX-math-nshortmid t] ["nmid" LaTeX-math-nmid t] ["nvdash" LaTeX-math-nvdash t] ["nvDash" LaTeX-math-nvDash t] ["ntriangleleft" LaTeX-math-ntriangleleft t] ["ntrianglelefteq" LaTeX-math-ntrianglelefteq t] ["nsubseteq" LaTeX-math-nsubseteq t] ["subsetneq" LaTeX-math-subsetneq t] ["varsubsetneq" LaTeX-math-varsubsetneq t] ["subsetneqq" LaTeX-math-subsetneqq t] ["varsubsetneqq" LaTeX-math-varsubsetneqq t]) ("Neg Rel II" ["ngtr" LaTeX-math-ngtr t] ["ngeq" LaTeX-math-ngeq t] ["ngeqslant" LaTeX-math-ngeqslant t] ["ngeqq" LaTeX-math-ngeqq t] ["gneq" LaTeX-math-gneq t] ["gneqq" LaTeX-math-gneqq t] ["gvertneqq" LaTeX-math-gvertneqq t] ["gnsim" LaTeX-math-gnsim t] ["gnapprox" LaTeX-math-gnapprox t] ["nsucc" LaTeX-math-nsucc t] ["nsucceq" LaTeX-math-nsucceq t] ["succnsim" LaTeX-math-succnsim t] ["succnapprox" LaTeX-math-succnapprox t] ["ncong" LaTeX-math-ncong t] ["nshortparallel" LaTeX-math-nshortparallel t] ["nparallel" LaTeX-math-nparallel t] ["nvDash" LaTeX-math-nvDash t] ["nVDash" LaTeX-math-nVDash t] ["ntriangleright" LaTeX-math-ntriangleright t] ["ntrianglerighteq" LaTeX-math-ntrianglerighteq t] ["nsupseteq" LaTeX-math-nsupseteq t] ["nsupseteqq" LaTeX-math-nsupseteqq t] ["supsetneq" LaTeX-math-supsetneq t] ["varsupsetneq" LaTeX-math-varsupsetneq t] ["supsetneqq" LaTeX-math-supsetneqq t] ["varsupsetneqq" LaTeX-math-varsupsetneqq t]) ("Binary Op" ["dotplus" LaTeX-math-dotplus t] ["smallsetminus" LaTeX-math-smallsetminus t] ["Cap" LaTeX-math-Cap t] ["Cup" LaTeX-math-Cup t] ["barwedge" LaTeX-math-barwedge t] ["veebar" LaTeX-math-veebar t] ["doublebarwedge" LaTeX-math-doublebarwedge t] ["boxminus" LaTeX-math-boxminus t] ["boxtimes" LaTeX-math-boxtimes t] ["boxdot" LaTeX-math-boxdot t] ["boxplus" LaTeX-math-boxplus t] ["divideontimes" LaTeX-math-divideontimes t] ["ltimes" LaTeX-math-ltimes t] ["rtimes" LaTeX-math-rtimes t] ["leftthreetimes" LaTeX-math-leftthreetimes t] ["rightthreetimes" LaTeX-math-rightthreetimes t] ["curlywedge" LaTeX-math-curlywedge t] ["curlyvee" LaTeX-math-curlyvee t] ["circleddash" LaTeX-math-circleddash t] ["circledast" LaTeX-math-circledast t] ["circledcirc" LaTeX-math-circledcirc t] ["centerdot" LaTeX-math-centerdot t] ["intercal" LaTeX-math-intercal t]) ("Misc" ["hbar" LaTeX-math-hbar t] ["hslash" LaTeX-math-hslash t] ["vartriangle" LaTeX-math-vartriangle t] ["triangledown" LaTeX-math-triangledown t] ["square" LaTeX-math-square t] ["lozenge" LaTeX-math-lozenge t] ["circledS" LaTeX-math-circledS t] ["angle" LaTeX-math-angle t] ["measuredangle" LaTeX-math-measuredangle t] ["nexists" LaTeX-math-nexists t] ["mho" LaTeX-math-mho t] ["Finv" LaTeX-math-Finv t] ["Game" LaTeX-math-Game t] ["Bbbk" LaTeX-math-Bbbk t] ["backprime" LaTeX-math-backprime t] ["varnothing" LaTeX-math-varnothing t] ["blacktriangle" LaTeX-math-blacktriangle t] ["blacktriangledown" LaTeX-math-blacktriangledown t] ["blacksquare" LaTeX-math-blacksquare t] ["blacklozenge" LaTeX-math-blacklozenge t] ["bigstar" LaTeX-math-bigstar t] ["sphericalangle" LaTeX-math-sphericalangle t] ["complement" LaTeX-math-complement t] ["eth" LaTeX-math-eth t] ["diagup" LaTeX-math-diagup t] ["diagdown" LaTeX-math-diagdown t]) ("Accents" ["Hat" LaTeX-math-Hat t] ["Check" LaTeX-math-Check t] ["Tilde" LaTeX-math-Tilde t] ["Acute" LaTeX-math-Acute t] ["Grave" LaTeX-math-Grave t] ["Dot" LaTeX-math-Dot t] ["Ddot" LaTeX-math-Ddot t] ["Breve" LaTeX-math-Breve t] ["Bar" LaTeX-math-Bar t] ["Vec" LaTeX-math-Vec t] ["dddot" LaTeX-math-dddot t] ["ddddot" LaTeX-math-ddddot t]) ("Delimiters" ["bigl" LaTeX-math-bigl t] ["bigr" LaTeX-math-bigr t] ["Bigl" LaTeX-math-Bigl t] ["Bigr" LaTeX-math-Bigr t] ["biggl" LaTeX-math-biggl t] ["biggr" LaTeX-math-biggr t] ["Biggl" LaTeX-math-Biggl t] ["Biggr" LaTeX-math-Biggr t] ["lvert" LaTeX-math-lvert t] ["rvert" LaTeX-math-rvert t] ["lVert" LaTeX-math-lVert t] ["rVert" LaTeX-math-rVert t] ["ulcorner" LaTeX-math-ulcorner t] ["urcorner" LaTeX-math-urcorner t] ["llcorner" LaTeX-math-llcorner t] ["lrcorner" LaTeX-math-lrcorner t]) ("Special" ["nobreakdash" LaTeX-math-nobreakdash t] ["leftroot" LaTeX-math-leftroot t] ["uproot" LaTeX-math-uproot t] ["accentedsymbol" LaTeX-math-accentedsymbol t] ["xleftarrow" LaTeX-math-xleftarrow t] ["xrightarrow" LaTeX-math-xrightarrow t] ["overset" LaTeX-math-overset t] ["underset" LaTeX-math-underset t] ["dfrac" LaTeX-math-dfrac t] ["genfrac" LaTeX-math-genfrac t] ["tfrac" LaTeX-math-tfrac t] ["binom" LaTeX-math-binom t] ["dbinom" LaTeX-math-dbinom t] ["tbinom" LaTeX-math-tbinom t] ["smash" LaTeX-math-smash t] ["eucal" LaTeX-math-eucal t] ["boldsymbol" LaTeX-math-boldsymbol t] ["text" LaTeX-math-text t] ["intertext" LaTeX-math-intertext t] ["substack" LaTeX-math-substack t] ["subarray" LaTeX-math-subarray t] ["sideset" LaTeX-math-sideset t]))))
