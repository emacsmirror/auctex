;;; auc-html.el --- Major mode for editing HTML documents.

;; Copyright (C) 1994 Per Abrahamsen 

;; Author: Per Abrahamsen <abraham@iesd.auc.dk>
;; Keywords: wp
;; Version: $Id: auc-html.el,v 5.5 1994-02-09 00:01:07 amanda Exp $

;; LCD Archive Entry:
;; auc-html|Per Abrahamsen|abraham@iesd.auc.dk|
;; |Major mode for editing HTML documents|
;; $Date: 1994-02-09 00:01:07 $|$Revision: 5.5 $|~/modes/auc-html.el.Z|

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Major mode for editing HTML documents.
;;
;; Place this file in the `site-lisp' directory and add 
;; 
;;    (autoload 'html-mode "auc-html" "HTML editing mode" t)
;;    (setq auto-mode-alist (cons '("\\.html\\'" . html-mode) auto-mode-alist))
;;
;; to the `site-start.el' file or your own `.emacs' file to activate it.

;;; Todo:

;; - Choice of html, mosaic, and html+ tag lists.  
;; - Open close tag sexp movement.
;; - Deleting one of a paired tag should also delete matching tag.
;; - `hilit19.el' regexps.
;; - `font-lock-mode' regexps.
;; - External (Mosaic) preview.
;; - Make `html-insert-tag' context sensitive.
;; - Extra functions to insert section, environment, markup (font), or entity.
;; - Automatically generate TAG list from SGML DTD.
;; - Rewrite as a general SGML mode.

;;; ChangeLog:

;; Not yet released.

;;; Code:

(require 'easymenu)

;;; Attributes

(defvar html-attribute-alist
  '(("id" "Link destination" id)
    ("index" "Entries for index compilation" text)
    ("role" "Purpose of tag" text)
    ("at" "Anchor point" ref)
    ("b" "Bold font")
    ("i" "Italic font")
    ("u" "Underline")
    ("tt" "Typewriter font")
    ("tr" "Serif font")
    ("hv" "Sans serif font")
    ("sup" "Superscript")
    ("sub" "Subscript")
    ("align" "Alignment" align)
    ("indent" "Indented margins")
    ("style" "Formatting style" style)
    ("compact" "Reduced space between items")
    ("narrow" "Keep table narrow")
    ("width" "Width in columns" number)
    ("src" "Included data" html-read-url)
    ("border" "Draw border around object")
    ("colspan" "Columns spanned" number)
    ("rowspan" "Rows spanned" number)
    ("action" "Where to submit" html-read-url)
    ("name" "Name of field" text)
    ("type" "Field type" input-type)
    ("size" "Field size" input-size)
    ("value" "Field value" text)
    ("checked" "Checkbox checked")
    ("disabled" "Field is inactive")
    ("error" "Field is in an error state")
    ("hidden" "Hide object")
    ("cap" "Caption alignment" align)
    ("noflow" "Disable text flow")
    ("ismap" "Can handle mouse clicks")
    ("area" "Clickable area" area)
    ("framed" "Framed with bordered background")
    ("seethru" "Transparency")
    ("inset" "Indented margins")
    ("idref" "Internal reference" id)
    ("effect" nil anchor-effect)
    ("print" nil print)
    ("title" "Destrination title" text)
    ("n" nil text)
    ("href" "Reference to network resource" html-read-url))
  "AList of HTML+ attributes.  
Each entry has the following elements:
* Attribute name.
* Attribute description.
* Attribute argument.  See below.

`nil' or missing means no argument.  Function names mean use that
function to read arguments.  Anything else indicates an unformatted
text argument, but the symbol can be used to give an indication of
what sort of argument it is.

The following are used: id, ref, style, number document-role,
emphasis-role, link-role, group-role, input-type, input-size, mime,
area, and text.

Functions are called with two arguments, an attribute entry and a
default value.  The following are defined: html-read-text, and
html-read-url.")

(defun html-attribute-name (entry)
  ;; Extract name field from ENTRY in html-attribute-alist.
  ;; This must always be the fierst field.
  (if (stringp entry)
      entry
    (nth 0 entry)))

(defun html-attribute-description (entry)
  ;; Extract description field from ENTRY in html-attribute-alist.
  (or (nth 1 (html-attribute-entry entry))
      "Purpose unknown"))

(defun html-attribute-argument (entry)
  ;; Extract type field from ENTRY in html-attribute-alist.
  ;; This must always be the fierst field.
  (nth 2 (html-attribute-entry entry)))

(defun html-attribute-entry (att)
  ;; Return attribute entry for ATT.  ATT may already be an entry, or
  ;; it may be be a string.  Strings are looked up in
  ;; html-attribute-list.
  (cond ((listp att)
	 att)
	((assoc (downcase att) html-attribute-alist))
	(t
	 (list (downcase att)))))
  
(defvar html-max-attribute-length
  ;; Length of longest attribute in html-attribute-alist.
  (apply 'max (mapcar (function (lambda (entry) (length (car entry))))
		      (cons '("attribute") html-attribute-alist))))

(defvar html-read-text-history nil)

(defun html-read-text (att default)
  ;; Read unstructured text attribute.
  (let ((name (html-attribute-name att))
	(text (html-attribute-description att))
	(type (symbol-name (html-attribute-argument att))))
    (read-from-minibuffer (concat text " (" att " "
				  (if (string-match ".*-\\([^---]\\)" type)
				      (substring type
						 (match-beginning 1)
						 (match-end 1))
				    type)
				  "): ")
			  (if default (cons default 1) nil)
			  nil nil
			  'html-read-text-history)))

(defun html-read-url (att default)
  (if (fboundp 'w3-build-url)
      (call-interactively 'w3-build-url)
    (html-read-text att default)))

;;; Tags

(defvar html-tag-alist
  ;; Alist of HTML+ tags.  Each entry has the following elements:
  ;; * Tag name.
  ;; * Tag description
  ;; * Tag type.  One of the following
  ;;   - environment
  ;;     Tag and end tag occupies their own lines. Text between them is
  ;;     indented.
  ;;   - item
  ;;     Should be at the start of line.  Negative indentation.  No
  ;;     end tag.
  ;;   - header
  ;;     Should be on a line for itself.  No end tag.
  ;;   - section
  ;;     Tag at start of line, end tag at end of line,
  ;;   - body
  ;;     Tag and end tag occupies their own lines.  Text between them
  ;;     is not indented.
  ;;   - markup
  ;;     Tag and end tag are part of the text.  Text between them is
  ;;     indented.
  ;;   - break
  ;;     Tag should be at the end of line.  No end tag.
  ;;   - entity
  ;;     Part of the text.  No end tag.
  ;; * List of required attributes.
  ;; * List of recommended attributes.
  ;; * List of optional attributes.
  ;;
  ;; The attributes in the three last fields can either be lists in
  ;; `html-attribute-alist' format or strings refering to the
  ;; descriptions in `html-attribute-alist'.
  '(("htmlplus" nil header
     nil nil ("id" "index"))
    ("head" "Document header" body
     nil nil ("id" "index"))
    ("body" "Document body" body
     nil nil ("id" "index"))
    ("title" "Document title" environment
     nil nil ("id" "index"))
    ("html" "Document role" header
     nil nil (("role" document-role)))
    ("panel" "Floating panel" environment
     nil nil ("at" "id" "index"))
    ("h1" "Section header" section
     nil nil ("id" "index"))
    ("h2" "Section header" section
     nil nil ("id" "index"))
    ("h3" "Section header" section
     nil nil ("id" "index"))
    ("h4" "Section header" section
     nil nil ("id" "index"))
    ("h5" "Section header" section
     nil nil ("id" "index"))
    ("h6" "Section header" section
     nil nil ("id" "index"))
    ("em" "Emphasis" markup
     nil nil (("role" "Emphasis type" emphasis-role)
	      "b" "i" "u" "tt" "tr" "hv" "sup" "sub" "index")) 
    ("p" "Paragraph ending" break
     nil nil ("role" "align" "indent" "id" "index"))
    ("br" "Line break" break
     nil nil ("id" "index"))
    ("sp" "Unbreakable space" entity
     nil nil ("id" "index"))
    ("pre" "Preformatted text" body
     nil nil ("style" "tr" "hv" "width" "id" "index"))
    ("ol" "Ordered list" environment
     nil nil ("compact" "id" "index"))
    ("ul" "Unordered list" environment
     nil nil ("compact" "narrow" "id" "index"))
    ("li" "List item start" item
     nil nil (("src" "Alternative bullet" html-read-url) "id" "index"))
    ("dl" "Definition list" environment
     nil nil ("compact" "id" "index"))
    ("dt" "Term to be definined" item
     nil nil ("id" "index"))
    ("dd" "Definition of term" item
     nil nil ("id" "index"))
    ("tbl" "Table" environment
     nil nil ("compact" "border" "id" "index"))
    ("tt" "Table title" item
     nil nil ("top"))
    ("th" "Table header cell" entity
     nil nil ("colspan" "rowspan" "align"))
    ("td" "Table data cell" entity
     nil nil ("colspan" "rowspan" "align"))
    ("tr" "Table row separator" item
     nil nil nil)
    ("tb" "Table vertical break" item
     nil nil nil)
    ("form" "Form for specifying input" environment
     nil nil ("action" "id" "index"))
    ("mh" "Form mail header" environment
     nil nil ("hidden"))
    ("input" "Form input field" header
     nil nil ("name" "type" "size" "value" "checked" "disabled" "error"))
    ("embed" "Embedded data in foreign format" environment
     nil (("type" "Type of embedded data" mime)) ("id" "index"))
    ("fig" "Figure" environment
     nil nil ("align" "cap" "noflow" "ismap" "src" "id" "index"))
    ("figd" "Figure description" environment
     nil nil ("id" "index"))
    ("figa" "Figure anchor" header
     ("href") nil ("area"))
    ("figt" "Figure text" environment
     nil nil ("at"
	      ("width" "Width as fraction of picture" number)
	      "framed" "href"))
    ("img" "Inline graphics" text
     ("src") nil ("align" "seethru" "ismap"))
    ("group" "Document grouping" body
     nil nil (("role" "Grouping type" group-role) "inset" "id" "index"))
    ("changed" "Change bars" header
     nil nil (("idref" "End of changes" id) ("id" "Start of changes" id)))
    ("a" "Anchor" markup
     nil ("href")
     (("role" "Link type" link-role)
      "effect" "print" "title"
      ("type" nil text)
      ("size" nil text)
      "id"))
    ("link" "Link relationship" header
     ("href" ("role" "Link type" link-role)) nil)
    ("base" "Base document" header
     nil nil ("href"))
    ("isindex" "Accept queries" header
     nil nil ("href"))
    ("nextid" "Allocate new id" header
     (("n") nil nil))))

(defun html-tag-name (entry)
  ;; Extract name field from ENTRY in html-tag-alist.
  ;; This must always be the fierst field.
  (if (stringp entry)
      entry
    (nth 0 entry)))

(defun html-tag-description (entry)
  ;; Extract description field from ENTRY in html-tag-alist.
  (or (nth 1 (html-tag-entry entry))
      "Purpose unkown"))

(defun html-tag-type (entry)
  ;; Extract type field from ENTRY in html-tag-alist.
  (nth 2 (html-tag-entry entry)))

(defun html-tag-entry (tag)
  ;; Return tag entry for TAG.  TAG may already be an entry, or
  ;; it may be be a string.  Strings are looked up in
  ;; html-tag-list.
  (cond ((listp tag)
	 tag)
	((assoc (downcase tag) html-tag-alist))
	(t
	 (list (downcase tag)))))
  
(defun html-tag-recommended-attributes (entry)
  ;; Return list of recommeneded attributes for ENTRY in
  ;; html-tag-alist.
  (setq entry (html-tag-entry tag))
  (mapcar 'html-attribute-entry
	  (append (nth 3 entry) (nth 4 entry))))


(defun html-tag-attributes (entry)
  ;; Return list of attributes for ENTRY in html-tag-alist.
  (setq entry (html-tag-entry entry))
  (mapcar 'html-attribute-entry
	  (append (nth 3 entry) (nth 4 entry) (nth 5 entry))))

(defvar html-max-tag-length
  ;; Length of longest tag in html-tag-alist.
  (apply 'max (mapcar (function (lambda (entry) (length (car entry))))
		      html-tag-alist)))

;; Alist of html tags of type item.
(defvar html-item-alist
  (let ((items nil)
	(tags html-tag-alist))
    (while tags
      (if (eq (html-tag-type (car tags)) 'item)
	  (setq items (cons (car tags) items)))
      (setq tags (cdr tags)))
    items))

;; Regexp matching item tags.
(defvar html-item-regexp
  (concat "<\\(" (mapconcat 'html-tag-name html-item-alist "\\|") "\\)\\b"))

;; Alist of html tags of type break.
(defvar html-break-alist
  (let ((breaks nil)
	(tags html-tag-alist))
    (while tags
      (if (eq (html-tag-type (car tags)) 'break)
	  (setq breaks (cons (car tags) breaks)))
      (setq tags (cdr tags)))
    breaks))

;; Regexp matching break tags.
(defvar html-break-regexp
  (concat "<\\(" (mapconcat 'html-tag-name html-break-alist "\\|") "\\)\\b"))

;; Alist of html tags of type header.
(defvar html-header-alist
  (let ((headers nil)
	(tags html-tag-alist))
    (while tags
      (if (eq (html-tag-type (car tags)) 'header)
	  (setq headers (cons (car tags) headers)))
      (setq tags (cdr tags)))
    headers))

;; Regexp matching header tags.
(defvar html-header-regexp
  (concat "<\\(" (mapconcat 'html-tag-name html-header-alist "\\|") "\\)\\b"))

;; Alist of html tags that opens a new scope.
(defvar html-scope-alist
  (let ((items nil)
	(tags html-tag-alist))
    (while tags
      (if (memq (html-tag-type (car tags)) '(environment markup section))
	  (setq items (cons (car tags) items)))
      (setq tags (cdr tags)))
    items))

;; Regexp matching open scope tags.
(defvar html-open-scope-regexp
  (concat "<\\(" (mapconcat 'html-tag-name html-scope-alist "\\|") "\\)\\b"))

;; Regexp matching close scope tags.
(defvar html-close-scope-regexp
  (concat "<\\(/" (mapconcat 'html-tag-name html-scope-alist "\\|/") "\\)\\b"))

;; Alist of html tags that opens a new environment.
(defvar html-environment-alist
  (let ((items nil)
	(tags html-tag-alist))
    (while tags
      (if (memq (html-tag-type (car tags)) '(environment body))
	  (setq items (cons (car tags) items)))
      (setq tags (cdr tags)))
    items))

;; Regexp matching open environment tags.
(defvar html-open-environment-regexp
  (concat "<\\("
	  (mapconcat 'html-tag-name html-environment-alist "\\|")
	  "\\)\\b"))

;; Regexp matching close environment tags.
(defvar html-close-environment-regexp
  (concat "<\\(/"
	  (mapconcat 'html-tag-name html-environment-alist "\\|/")
	  "\\)\\b"))

;;; Attribute Commands

(defvar html-insert-attribute-history nil)

(defun html-attribute-completion-entry (entry)
  ;; Create completion from ENTRY in `html-attribute-alist'.
  (let ((att (html-attribute-name entry))
	(text (html-attribute-description entry)))
    (if (string-match regexp att)
	(insert (format (concat "%-" (int-to-string html-max-attribute-length)
				"s %s\n")
			att text)))))

(defun html-attribute-help (&optional prefix)
  ;; Display help for all attributess starting with PREFIX in the
  ;; current buffer.  Used free variable TABLE to get entries.
  (or prefix (setq prefix ""))
  (let ((regexp (concat "\\`" prefix)))
    (insert (format (concat "%-" (int-to-string html-max-attribute-length)
			    "s %s\n\n")
		    "Attribute" "Description"))
    (mapcar 'html-attribute-completion-entry table)))

(defun html-attribute-minibuffer-help ()
  ;; Display help for all possible attribute completions of the text
  ;; in the minibuffer.
  (interactive)
  (let ((prefix (buffer-substring (point-min) (point-max))))
    (with-output-to-temp-buffer "*Help*"
      (set-buffer standard-output)
      (html-attribute-help prefix))))

(defvar html-attribute-completion-map
  ;; Used when prompting for a attribute.
  (let ((map (copy-keymap minibuffer-local-completion-map)))
    (define-key map (char-to-string help-char) 'html-attribute-minibuffer-help)
    (define-key map [help] 'html-attribute-minibuffer-help)
    map))

(defvar html-attribute-must-match-map
  ;; Used when prompting for a attribute.
  (let ((map (copy-keymap minibuffer-local-must-match-map)))
    (define-key map (char-to-string help-char) 'html-attribute-minibuffer-help)
    (define-key map [help] 'html-attribute-minibuffer-help)
    map))

(defun html-insert-attribute (att)
  "Insert html attribute ATT."
  (interactive (let* ((minibuffer-local-completion-map
		       html-attribute-completion-map)
		      (entry (html-tag-entry (html-current-tag)))
		      (prompt (concat "Attribute: (press "
				      (key-description (vector help-char))
				      " for help) "))
		      (table (html-tag-attributes entry))
		      (att (completing-read prompt table nil nil nil
					    html-insert-attribute-history)))
		 (list att)))
  (let ((arg (html-attribute-argument att))
	(old (html-attribute-value att))
	(case-fold-search t))
    (if old
	()
      (re-search-forward ">")
      (goto-char (match-beginning 0)))
    (let ((new (funcall (if (fboundp arg) arg 'html-read-text) att old)))
      (just-one-space)
      (insert (html-attribute-name att))
      (if arg
	  (insert "=\"" new "\""))
      (or (eq (following-char) ?>)
	  (just-one-space)))))

(defun html-identify-attribute (att)
  ;; Return entry for attribute ATT, using free variable TAG for a
  ;; suitable list.
  (or (assoc (downcase att) (html-tag-attributes tag))
      (html-attribute-entry att)))

(defun html-current-attributes ()
  ;; List of attributes in current tag.
  (save-excursion
    (if (re-search-backward "[<>]" nil t)
	(cond ((looking-at ">")
	       (error "Not in a tag."))
	      ((looking-at "</")
	       (error "End tag."))
	      ((looking-at "<\\([a-zA-Z]+\\)")
	       (let ((tag (buffer-substring (match-beginning 0)
					     (match-end 0)))
		     (atts nil)
		     (regexp ">\\|\\b\\([a-zA-Z]+\\)\\(=\"\\([^\"]*\\)\"\\)?"))
		 (goto-char (match-end 0))
		 (while (and (re-search-forward regexp nil t)
			     (not (eq (preceding-char) ?>)))
		   (setq atts
			 (cons (downcase (buffer-substring (match-beginning 1)
							   (match-end 1)))
			       atts)))
		 (mapcar 'html-identify-attribute atts)))
	      (t
	       (error "Nameless tag.")))
      (error "No tag."))))

(defun html-current-attribute ()
  ;; The first attribute at or after point.
  (let ((pos (point))
	(att nil)
	(regexp ">\\|\\b\\([a-zA-Z]+\\)\\(=\"\\([^\"]*\\)\"\\)?"))
    (save-excursion
      (if (re-search-backward "[<>]" nil t)
	  (cond ((looking-at ">") nil)
		((looking-at "</") nil)
		((looking-at "<\\([a-zA-Z]+\\)")
		 (goto-char (match-end 0))
		 (while (and (re-search-forward regexp nil t)
			     (not (eq (preceding-char) ?>))
			     (not att))
		   (or (< (point) pos)
		       (setq att (buffer-substring (match-beginning 1)
						   (match-end 1)))))
		 att)
		(t nil))
	nil))))

(defun html-select-attribute ()
  ;; Query the user for an attribute suitable for the current tag.
  (let* ((table (or (html-current-attributes)
		    (error "No tags.")))
	 (case-fold-search t)
	 (default (html-current-attribute))
	 (minibuffer-local-must-match-map html-attribute-completion-map)
	 (prompt (concat "Attribute: "
			  (if default
			      (concat "(" default ") "))))
	 (answer (completing-read prompt table nil t nil
				  html-insert-attribute-history)))
    (if (or (null answer) (string-equal answer ""))
	default
      answer)))

(defun html-attribute-value (att)
  ;; Return the value of attribute ATT in current tag.
  ;; `match-data' will contain the portion of the buffer used by ATT.
  (let ((case-fold-search t))
    (save-excursion
      (if (re-search-backward "[<>]" nil t)
	  (cond ((looking-at ">")
		 (error "Not in a tag."))
		((looking-at "</")
		 (error "End tag."))
		((looking-at "<\\([a-zA-Z]+\\)")
		 (and (re-search-forward (concat ">\\|\\b"
						 (regexp-quote att)
						 "\\(=\"\\([^\"]*\\)\"\\)?")
					 nil t)
		      (not (eq (preceding-char) ?>))
		      (match-beginning 2)
		      (buffer-substring (match-beginning 2)
					(match-end 2))))
		(t
		 (error "Nameless tag.")))
	(error "No tag.")))))

(defun html-delete-attribute (att)
  "Delete attribute ATT in current tag."
  (interactive (list (html-select-attribute)))
  (if (html-attribute-value att)
      (progn 
	(replace-match "")
	(just-one-space)
	(if (eq (following-char) ?>)
	    (delete-backward-char 1)))))

;;; Tag Commands

(defun html-current-tag (&optional close-ok)
  ;; If point is inside a standa alone or open tag, return its name.
  ;; If optional argument CLOSE-OK is non-nil, accept close tags too.
  (save-excursion
    (if (re-search-backward "[<>]" nil t)
	(cond ((looking-at ">") nil)
	      ((looking-at "<\\(/[a-zA-Z]+\\)")
	       (and close-ok
		    (buffer-substring (match-beginning 1) (match-end 1))))
	      ((looking-at "<\\([a-zA-Z]+\\)")
	       (buffer-substring (match-beginning 1) (match-end 1)))
	      (t nil)))))

(defun html-insert-tag-with-attributes (entry)
  ;; Insert tag described in ENTRY and prompt for required and
  ;; recommended attributes.
  (insert "<" (html-tag-name entry) ">")
  (save-excursion
    (backward-char)
    (mapcar 'html-insert-attribute
	    (mapcar 'html-attribute-name
		    (html-tag-recommended-attributes entry)))))

(defun html-tag-completion-entry (entry)
  ;; Create completion from ENTRY in `html-tag-alist'.
  (let ((tag (html-tag-name entry))
	(text (html-tag-description entry)))
    (if (string-match regexp tag)
	(insert (format (concat "%-" (int-to-string html-max-tag-length)
				"s %s\n")
			tag text)))))

(defun html-tag-help (&optional prefix)
  ;; Display help for all tags starting with PREFIX in the current buffer.
  (or prefix (setq prefix ""))
  (let ((regexp (concat "\\`" (regexp-quote prefix))))
    (insert (format (concat "%-" (int-to-string html-max-tag-length)
			    "s %s\n\n")
		    "Tag" "Description"))
    (mapcar 'html-tag-completion-entry html-tag-alist)))

(defun html-tag-minibuffer-help ()
  ;; Display help for all possible tag completions of the text in the
  ;; minibuffer. 
  (interactive)
  (let ((prefix (buffer-substring (point-min) (point-max))))
    (with-output-to-temp-buffer "*Help*"
      (set-buffer standard-output)
      (html-tag-help prefix))))

(defvar html-tag-completion-map
  ;; Used when prompting for a tag.
  (let ((map (copy-keymap minibuffer-local-completion-map)))
    (define-key map (char-to-string help-char) 'html-tag-minibuffer-help)
    (define-key map [help] 'html-tag-minibuffer-help)
    map))

(defvar html-insert-tag-history nil)

(defvar html-default-tag "a"
  "Default tag used by `html-insert-tag.")

(defun html-insert-tag (tag)
  "Insert html tag TAG."
  (interactive (let* ((minibuffer-local-completion-map html-tag-completion-map)
		      (prompt (concat "Tag: (default " html-default-tag
				      ", press "
				      (key-description (vector help-char))
				      " for help) "))
		      (tag (completing-read prompt html-tag-alist nil nil nil
					    html-insert-tag-history)))
		 (list tag)))

  (if (or (null tag) (string-equal tag ""))
      (setq tag html-default-tag)
    (setq html-default-tag tag))
	    
  (let ((entry (html-tag-entry tag))
	(case-fold-search t))
    (if entry
	(let ((type (html-tag-type entry)))
	  (cond ((memq type '(environment body))
		 (if (html-looking-at-backward "^[ \t]*")
		     (html-indent-line)
		   (newline-and-indent))
		 (html-insert-tag-with-attributes entry)
		 (newline-and-indent)
		 (save-excursion
		   (newline)
		   (insert "</" tag ">")
		   (html-indent-line)
		   (if (looking-at "[ \t]*$")
		       ()
		     (newline-and-indent))))
		((eq type 'item)
		 (if (html-looking-at-backward "^[ \t]*")
		     ()
		   (newline))
		 (html-insert-tag-with-attributes entry)
		 (html-indent-line))
		((eq type 'header)
		 (if (html-looking-at-backward "^[ \t]*")
		     (html-indent-line)
		   (newline-and-indent))
		 (html-insert-tag-with-attributes entry)
		 (newline-and-indent))
		((eq type 'break)
		 (html-insert-tag-with-attributes entry)
		 (if (looking-at "[ \t]*$")
		       ()
		     (newline-and-indent)))
		((eq type 'section)
		 (if (html-looking-at-backward "^[ \t]*")
                     ()
                   (newline))
		 (html-insert-tag-with-attributes entry)
		 (save-excursion
                   (insert "</" tag ">")
		   (if (looking-at "[ \t]*$")
                       ()
                     (newline-and-indent))))
		((eq type 'markup)
		 (html-insert-tag-with-attributes entry)
		 (save-excursion
		   (insert "</" tag ">")))
		((eq type 'entity)
		 (html-insert-tag-with-attributes entry))
		(t
		 (error "Unknow tag type %s" type))))
      (insert "<" tag ">"))))

(defun html-delete-tag ()
  "Delete tag containing point."
  (interactive "*")
  (save-excursion
    (if (and (re-search-backward "[<>]" nil t)
	     (looking-at "<"))
	(let ((pos (point)))
	  (forward-sexp)
	  (kill-region pos (point)))
      (error "No tag."))))

;;; Simle Commands

(defun html-help ()
  "Describe all known tags and attributes."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (set-buffer standard-output)
    (with-output-to-temp-buffer "*Help*"
      (set-buffer standard-output)
      (html-tag-help)
      (insert "\n\n")
      (html-attribute-help))))

(defun html-insert-left-angle (arg)
  "Insert left angle for HTML.
If the cursor is just after a `<', change it to `&lt;', otherwise
insert a `<'. With prefix argument, insert that many `<' characters."
  (interactive "*P")
  (if (or arg (bobp) (not (= (preceding-char) ?<)))
      (self-insert-command (prefix-numeric-value arg))
    (delete-backward-char 1)
    (insert "&lt;")))

(defun html-insert-right-angle (arg)
  "Insert right angle for HTML.
If the cursor is just after a `>', change it to `&gt;', otherwise
insert a `>'. With prefix argument, insert that many `>' characters."
  (interactive "*P")
  (if (or arg (bobp) (not (= (preceding-char) ?>)))
      (self-insert-command (prefix-numeric-value arg))
    (delete-backward-char 1)
    (insert "&lt;")))

(defun html-insert-ampersand (arg)
  "Insert ampersand for HTML.
If the cursor is just after a `&', change it to `&lt;', otherwise
insert a `&'. With prefix argument, insert that many `&' characters."
  (interactive "*P")
  (if (or arg (bobp) (not (= (preceding-char) ?&)))
      (self-insert-command (prefix-numeric-value arg))
    (delete-backward-char 1)
    (insert "&lt;")))

(defun html-complete-word ()
  "Complete HTML tag or word before point."
  (interactive "*")
  (if (html-looking-at-backward "</?\([a-z][A-Z]*\)")
      (let* ((from (match-beginning 1))
	     (to (match-end 1))
	     (old (buffer-substring from to))
	     (new (try-completion old html-tag-alist)))
	(cond ((eq new t))
	      ((null new)
	       (error "Can't find completion for tag `%s'." old))
	      ((string-equal old new)
	       (with-output-to-temp-buffer "*Completions*"
		 (display-completion-list (all-completions old
							   html-tag-list))))
	      (t
	       (delete-region from to)
	       (insert new))))
    (ispell-complete-word)))

(defun html-delete (arg)
  "Delete attribute at or after point.  
If there is no attribute, delete tag instead.
With ARG always delete tag."
  (interactive "*P")
  (if (and (null arg) (html-current-attribute))
      (call-interactively 'html-delete-attribute)
    (html-delete-tag)))

;;; Indentation

(defvar html-scope-indent 2
  "*Extra indentation for lines between a matching pair of HTML tags.")

(defvar html-item-indent -2
  "*Extra indentation for lines starting a list item.")

(defvar html-angle-indent 5
  "*Extra indentation for each unmatched left angle.")

(defun html-calculate-indent ()
  ;; Calculate indentation for current line.
  (save-excursion
    (back-to-indentation)
    (let* ((case-fold-search t)
	   ;; Speciel effects at the for text at the beginning of this line.
	   (offset (cond ((looking-at ">")
			  (- html-angle-indent))
			 ((looking-at html-item-regexp)
			  html-item-indent)
			 ((looking-at html-close-scope-regexp)
			  (- html-scope-indent))
			 (t
			  0))))

      ;; Analyze previous line.
      (skip-chars-backward " \t\n")
      (let ((eoln (point)))
	(back-to-indentation)
	(setq offset (+ (current-column) offset))

	;; Compensate for speciel effects on previous line.
	(cond ((looking-at html-item-regexp)
	       (setq offset (- offset html-item-indent)))
	      ((looking-at ">")
	       (setq offset (+ offset html-angle-indent)))
	      ((looking-at html-close-scope-regexp)
	       (setq offset (+ offset html-scope-indent))))

	;; Analyze text.
	(while (< (point) eoln)
	  (cond ((looking-at html-open-scope-regexp)
		 (setq offset (+ offset html-scope-indent html-angle-indent)))
		((looking-at html-close-scope-regexp)
		 (setq offset (+ (- offset html-scope-indent)
				 html-angle-indent)))
		((looking-at "<")
		 (setq offset (+ offset html-angle-indent)))
		((looking-at ">")
		 (setq offset (- offset html-angle-indent)))
		((looking-at "[^<>\n]+"))
		(t
		 (error "Passed end of line before end of line.")))
	  (goto-char (match-end 0)))
		 
	(max 0 offset)))))

(defun html-indent-line ()
  "Indent current line as html text."
  (interactive)
  (save-excursion
    (back-to-indentation)
    (let ((indent (html-calculate-indent)))
      (if (< indent (current-column))
	  (backward-delete-char-untabify (- (current-column) indent))
	(indent-to indent))))
  (if (html-looking-at-backward "^[ \t]*")
      (back-to-indentation)))

(defun html-comment-indent ()
  ;; Calculate column for HTML comment.
  (save-excursion
    (end-of-line)
    (skip-chars-backward " \t")
    (max (1+ (current-column)) comment-column)))

;;; Paragraphs

(defvar html-paragraph-separate
  ;; What separates paragraphs in html text.
  (concat html-close-environment-regexp "\\|" html-break-regexp))
	  
(defvar html-paragraph-start
  ;; What separates of starts paragraphs in html text.
  (concat html-paragraph-separate
	  "\\|" html-open-environment-regexp
	  "\\|" html-item-regexp
	  "\\|" html-header-regexp))

(defun html-fill-paragraph ()
  "Fill paragraph at or after point."
  (interactive "*")
  (let ((case-fold-search t))
    (save-excursion
      (cond ((or (looking-at html-paragraph-separate)
		 (not (looking-at html-paragraph-start)))
	     ;; Move to the start of the paragraph unless we are already there.
	     (re-search-backward html-paragraph-start nil t))
	    ((html-looking-at-backward "^[ \t]*"))
	    (t
	     ;; Make sure paragraphs start at a new line.
	     (newline)))
      (let ((end (save-excursion
		   (if (not (eobp)) (forward-char 1))
		   (if (re-search-forward html-paragraph-start nil t)
		       (match-beginning 0)
		     (point-max)))))
	(html-indent-line)
	(while (< (point) end)
	  (if (or (looking-at html-header-regexp)
		  (looking-at html-open-environment-regexp)
		  (looking-at html-close-environment-regexp)
		  (looking-at html-break-regexp))
	      ;; Certain tags start forces a line break.
	      (let ((par (looking-at "<[Pp]\\b")))
		(forward-sexp 1)
		(if (looking-at "[ \t]*$")
		    (skip-chars-forward " \t\n")
		  (newline (if par 2 1)))
		(html-indent-line))
	    (skip-chars-forward "^ \t\n")
	    (and (< (point) end)
		 (re-search-forward "[ \t\n]*" end t)
		 (replace-match " " t t)))
	  (if (> (current-column) fill-column)
	      (progn			;Fill if needed.
		(do-auto-fill)
		(html-indent-line)))
	  (skip-chars-forward " \t\n")
	  (if (or (looking-at html-header-regexp)
		  (looking-at html-open-environment-regexp)
		  (looking-at html-close-environment-regexp)
		  (looking-at html-item-regexp))
	      (progn 
		(newline)
		(html-indent-line))))))))

;;; Keymap and Menubar

(defvar html-mode-map
  ;; Keymap used in html mode.
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'html-indent-line)
    (define-key map "\n" 'reindent-then-newline-and-indent)
    (define-key map "\eq" 'html-fill-paragraph)
    (define-key map "\e\t" 'html-complete-word)
    (define-key map "\C-c\C-a" 'html-insert-attribute)
    (define-key map "\C-c\C-d" 'html-delete-attribute)
    (define-key map "\C-c\C-t" 'html-insert-tag)
    (define-key map "\C-c\C-p" 'w3-preview-this-buffer)
    (define-key map "\C-c?" 'html-help)
    (define-key map "<" 'html-insert-left-angle)
    (define-key map ">" 'html-insert-right-angle)
    (define-key map "&" 'html-insert-ampersand)
    map)
  "Major mode keymap for html mode.")

(defun html-attribute-enable-symbol (entry)
  ;; Symbol used for enabling manu entry for attribute ENTRY.
  (intern (concat "html-menu-enable-" (html-attribute-name entry))))

(defun html-tag-menu-entry (entry)
  ;; Create menu entry from ENTRY in `html-tag-alist'.
  (let* ((tag (html-tag-name entry))
	 (text (html-tag-description entry)))
    (vector (format (concat "%-"
			    (int-to-string html-max-tag-length)
			    "s %s")
		    tag text)
	    (list 'html-insert-tag tag)
	    t)))

(defun html-attribute-menu-entry (entry)
  ;; Create menu entry from ENTRY in `html-attribute-alist'.
  (let* ((att (html-attribute-name entry))
	 (text (html-attribute-description entry))
	 (symbol (html-attribute-enable-symbol entry)))
    (make-local-variable symbol)
    (set symbol nil)
    (vector (format (concat "%-"
			    (int-to-string html-max-attribute-length)
			    "s %s")
		    att text)
	    (list 'html-insert-attribute att)
	    symbol)))

;; Variables controling enabling of menu items.
(defvar html-has-w3 nil)
(defvar html-can-delete-tag nil)
(defvar html-can-delete-attribute nil)

(defun html-enable-attribute-entry (entry)
  ;; Return non-nil iff ENTRY is defined in free variable ACTIVE.
  (set (html-attribute-enable-symbol entry)
       (assoc (html-attribute-name entry) active)))

(defun html-enable-html-menu ()
  ;; Enable menu bar entries for html mode.
  (if (eq major-mode 'html-mode)
      (let* ((tag (assoc (html-current-tag) html-tag-alist))
	     (active (html-tag-attributes tag)))
	(setq html-can-delete-tag (html-current-tag t))
	(setq html-can-delete-attribute (html-current-attribute))
	(setq html-has-w3 (fboundp 'w3-preview-this-buffer))
	(mapcar 'html-enable-attribute-entry html-attribute-alist)
	nil)
    t))

(add-hook 'activate-menubar-hook 'html-enable-html-menu)

(easy-menu-define html-mode-menu
    html-mode-map
    "Menu used in html mode."
  (list "HTML"
	(cons "Insert Tag       (C-c C-t) "
	      (mapcar 'html-tag-menu-entry html-tag-alist))
	[     "Delete Tag       (C-u C-c C-d)"
	      html-delete-tag html-can-delete-tag ]
	(cons "Insert Attribute (C-c C-a) "
	      (mapcar 'html-attribute-menu-entry html-attribute-alist))
	[     "Delete Attribute (C-c C-d)"
	      (html-delete-attribute html-can-delete-attribute)
	      html-can-delete-attribute ]
	[ "Preview" w3-preview-this-buffer html-has-w3 ]))

;;; Mode

(defvar html-mode-abbrev-table nil
  "Abbrev table used while in html mode.")

(defvar html-mode-syntax-table
  ;; Syntax table used in html mode.
  (let ((table (copy-syntax-table text-mode-syntax-table))) 
    (modify-syntax-entry ?- "_ 3" table)
    (modify-syntax-entry ?! ". 2" table)
    (modify-syntax-entry ?< "(>1" table)
    (modify-syntax-entry ?> ")<4" table)
    table))

(defvar html-paragraph-separate
  ;; What separates paragraphs in html text.
  (concat html-close-environment-regexp
	  "\\|" html-break-regexp))
	  
(defvar html-paragraph-start
  ;; What separates of starts paragraphs in html text.
  (concat html-paragraph-separate
	  "\\|" html-open-environment-regexp
	  "\\|" html-item-regexp
	  "\\|" html-header-regexp))

;;;###autoload
(defun html-mode ()
  "Major mode for editing HTML text.

\\{html-mode-map}
Turning on html mode runs `text-mode-hook', then `html-mode-hook'."
  (interactive)
  (kill-all-local-variables)

  ;; Comments
  (make-local-variable 'comment-start)
  (setq comment-start "<!-- ")
  (make-local-variable 'comment-end)
  (setq comment-end " -->")
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "<!--+ *")
  (make-local-variable 'comment-multi-line)
  (setq comment-multi-line t)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'html-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)

  ;; Indentation
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'html-indent-line)

  ;; Paragraphs 
  (make-local-variable 'paragraph-start)
  (setq paragraph-start html-paragraph-start)
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate html-paragraph-separate)

  ;; Miscellanies
  (use-local-map html-mode-map)
  (easy-menu-add html-mode-menu)
  (setq mode-name "HTML")
  (setq major-mode 'html-mode)
  (set-syntax-table html-mode-syntax-table)
  (setq local-abbrev-table html-mode-abbrev-table)
  (run-hooks 'text-mode-hook 'html-mode-hook))

;;; Utilities

(defun html-looking-at-backward (regexp)
  ;; Return non-nil if the text before point matches REGEXP.
  (let ((pos (point)))
    (save-excursion
      (re-search-backward regexp)
      (eq (match-end 0) pos))))

(provide 'html-mode)
(provide 'auc-html)

;;; auc-html.el ends here
