;;; @ tex-buf.el - External commands for AUC TeX.
;;;
;;; $Id: tex-buf.el,v 1.21 1993-02-16 04:08:55 amanda Exp $

(provide 'tex-buf)
(require 'tex-site)

;;; @@ Copyright
;;;
;;; Copyright (C) 1991 Kresten Krab Thorup
;;; Copyright (C) 1993 Per Abrahamsen 
;;; 
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; @@ Interactive Commands
;;;
;;; The general idea is, that there is one process and process buffer
;;; associated with each master file, and one process and process buffer
;;; for running TeX on a region.   Thus, if you have N master files, you
;;; can run N + 1 processes simultaneously.  
;;;
;;; Some user commands operates on ``the'' process.  The following
;;; algorithm determine what ``the'' process is.
;;;
;;; IF   last process started was on a region 
;;; THEN ``the'' process is the region process
;;; ELSE ``the'' process is the master file (of the current buffer) process

(defun TeX-command-master ()
  "Run command on the current document."
  (interactive)
  (setq TeX-current-process-region-p nil)
  (TeX-command (TeX-command-query (TeX-master-file)) 'TeX-master-file))

(defun TeX-command-region (old)
  "Run TeX on the current region.

Query the user for a command to run on the temporary file specified by
the variable TeX-region.  If the chosen command is so marked in
TeX-command-list, and no argument (or nil) is given to the command,
the region file file be recreated with the current region.

If the master file for the document has a header, it is written to the
temporary file before the region itself.  The documents header is all
text until TeX-header-end.

If the master file for the document has a trailer, it is written to
the temporary file before the region itself.  The documents trailer is
all text after TeX-trailer-start."
  (interactive "P")
  (setq TeX-current-process-region-p t)
  (let ((command (TeX-command-query (TeX-region-file))))
    (if (and (nth 4 (assoc command TeX-command-list))
	     (null old))
	(if (null (mark))
	    (error "Mark not set.")
	  (let ((begin (min (point) (mark)))
		(end (max (point) (mark))))
	    (TeX-region-create (TeX-region-file "tex")
			       (buffer-substring begin end)
			       (file-name-nondirectory (buffer-file-name))
			       (count-lines (point-min) begin)))))
    (TeX-command command 'TeX-region-file)))

(defun TeX-recenter-output-buffer (line)
  "Redisplay buffer of TeX job output so that most recent output can be seen.
The last line of the buffer is displayed on line LINE of the window, or
at bottom if LINE is nil." 
  (interactive "P")
  (let ((process (TeX-active-process)))
    (if process
	(let ((TeX-buffer (process-buffer process))
	      (old-buffer (current-buffer)))
	  (pop-to-buffer TeX-buffer t)
	  (bury-buffer TeX-buffer)
	  (goto-char (point-max))
	  (recenter (if line
			(prefix-numeric-value line)
		      (/ (window-height) 2)))
	  (pop-to-buffer old-buffer))
      (message "No process currently running for this document."))))

(defun TeX-kill-job ()
  "Kill the currently running TeX job."
  (interactive)
  (quit-process (process-name (TeX-active-process)) t))

(defun TeX-home-buffer (arg)
  "Go to the buffer where you last issued a TeX command.  
If there is no such buffer, or you already are in that buffer, find
the master file."
  (interactive "P")

  (if (or (null TeX-command-buffer)
	  (eq TeX-command-buffer (current-buffer)))
      (find-file (TeX-master-file "tex"))
    (switch-to-buffer TeX-command-buffer)))

;;; @@ Command Query

(defun TeX-command (name file)
  "Run command NAME on the file you get by calling FILE.

FILE is a function return a file name.  It has one optional argument,
the extension to use on the file.

Use the information in TeX-command-list to determine how to run the
command."
  (let ((command (TeX-command-expand (nth 1 (assoc name TeX-command-list))
				     file))
	(hook (nth 2 (assoc name TeX-command-list)))
	(confirm (nth 3 (assoc name TeX-command-list))))

    ;; Verify the expanded command
    (if confirm
	(setq command
	      (read-from-minibuffer (concat name " command: ") command)))
    
    ;; Now start the process
    (let ((buffer (current-buffer)))
      (apply hook name command (apply file nil) nil)
      (pop-to-buffer buffer))))

(defun TeX-command-expand (command file)
  "Expand COMMAND for FILE as described in TeX-expand-list."
  (let ((list TeX-expand-list))
    (while list
      (let ((string (car (car list)))	;First element
	    (expansion (car (cdr (car list)))) ;Second element
	    (arguments (cdr (cdr (car list))))) ;Remaining elements
	(while (string-match string command)
	  (let ((prefix (substring command 0 (match-beginning 0)))
		(postfix (substring command (match-end 0))))
	    (setq command (concat prefix
				  (cond ((fboundp expansion)
					 (apply expansion arguments))
					((boundp expansion)
					 (apply (eval expansion) arguments))
					(t
					 (error "Nonexpansion %s." expansion)))
				  postfix)))))
      (setq list (cdr list)))
    command))

(defun TeX-command-query (name)
  "Query the user for a what TeX command to use."
  (let* ((default (if (buffer-modified-p)
		      TeX-command-default
		    (TeX-process-get-variable name
					      'TeX-command-next
					      TeX-command-default)))
	 (completion-ignore-case t)
	 (answer (completing-read (concat "Command: (default " default  ") ")
				  TeX-command-list nil t)))
    (if (and answer
	     (not (string-equal answer "")))
	answer
      default)))

(defvar TeX-command-next nil
  "The default command next time TeX-command is invoked.")

 (make-variable-buffer-local 'TeX-command-next)

(defvar TeX-command-default nil
  "The default command for TeX-command in the current major mode.")

(defun TeX-printer-query ()
  "Query the user for a printer name."

  (if TeX-printer-list
      (let ((printer (completing-read (concat "Printer: (default "
					      TeX-printer-default ") ")
				      TeX-printer-list)))
	(if (not (string-equal "" printer))
	    (setq TeX-printer-default printer))))
  TeX-printer-default)

;;; @@ Command Hooks

(defun TeX-command-hook (name command file)
  "Create a process for NAME using COMMAND to process FILE.
Return the new process."
  (let ((buffer (TeX-process-buffer-name file)))
    (TeX-process-check file)		; Check that no process is running
    (setq TeX-command-buffer (current-buffer))
    (pop-to-buffer (get-buffer-create buffer) t)
    (erase-buffer)
    (insert "Running `" name "' on `" file "' with ``" command "''\n")
    (let ((process (start-process name buffer "sh" "-c" command)))
      (setq mode-name name)
      (setq TeX-parse-hook 'TeX-parse-command)
      (TeX-command-mode-line process)
      (set-process-filter process 'TeX-command-filter)
      (set-process-sentinel process 'TeX-command-sentinel)
      process)))

(defun TeX-format-hook (name command file)
  "Create a process for NAME using COMMAND to format FILE with TeX."
  (let ((process (TeX-command-hook name command file)))
    ;; Hook to TeX debuger.
    (require 'tex-dbg)
    (setq TeX-parse-hook 'TeX-parse-TeX)
    (TeX-parse-reset)
    ;; Updating the mode line.
    (setq TeX-current-page "[0]")
    (TeX-format-mode-line process)
    (set-process-filter process 'TeX-format-filter)
    process))

(defun TeX-TeX-hook (name command file)
  "Create a process for NAME using COMMAND to format FILE with TeX."
  (let ((process (TeX-format-hook name command file)))
    (setq TeX-sentinel-hook 'TeX-TeX-sentinel)
    process))

(defun TeX-LaTeX-hook (name command file)
  "Create a process for NAME using COMMAND to format FILE with TeX."
  (let ((process (TeX-format-hook name command file)))
    (setq TeX-sentinel-hook 'TeX-LaTeX-sentinel)
    process))

(defun TeX-BibTeX-hook (name command file)
  "Create a process for NAME using COMMAND to format FILE with BibTeX."
  (let ((process (TeX-command-hook name command file)))
    (setq TeX-sentinel-hook 'TeX-BibTeX-sentinel)))

(defun TeX-compile-hook (name command file)
  "Ignore first and third argument, start compile with second argument."
  (TeX-process-set-variable name 'TeX-command-next TeX-command-default)
  (compile command))

(defun TeX-shell-hook (name command file)
  "Ignore first and third argument, start shell-command with second argument."
  (TeX-process-set-variable name 'TeX-command-next TeX-command-default)
  (shell-command command))

;;; @@ Command Sentinels

(defun TeX-command-sentinel (process msg)
  "Process TeX command output buffer after the process dies."
  (let* ((buffer (process-buffer process))
	 (name (process-name process)))
    (cond ((null (buffer-name buffer))	; buffer killed
	   (set-process-buffer process nil)
	   (set-process-sentinel process nil))
	  ((memq (process-status process) '(signal exit))
	   (save-excursion
	     (set-buffer buffer)
	     
	     ;; Append post-mortem information to the buffer
	     (goto-char (point-max))
	     (insert "\n" mode-name " " msg)
	     (forward-char -1)
	     (insert " at "
		     (substring (current-time-string) 0 -5))
	     (forward-char 1)
	     
	     ;; Do command specific actions.
	     (TeX-command-mode-line process)
	     (setq TeX-command-next TeX-command-default)
	     (goto-char (point-min))
	     (apply TeX-sentinel-hook process name nil)
	     
	     
	     ;; If buffer and mode line will show that the process
	     ;; is dead, we can delete it now.  Otherwise it
	     ;; will stay around until M-x list-processes.
	     (delete-process process)
	     
	     ;; Force mode line redisplay soon
	     (set-buffer-modified-p (buffer-modified-p)))))))

(defvar TeX-sentinel-hook (function (lambda (process name)))
  "Hook to cleanup TeX command buffer after temination of PROCESS.
NAME is the name of the process.  Point is set to   ")

 (make-variable-buffer-local 'TeX-sentinel-hook)

(defun TeX-TeX-sentinel (process name)
  "Cleanup TeX output buffer after running TeX.
Return nil ifs no errors were found."
  (TeX-format-mode-line process)
  (if (re-search-forward "^! " nil t)
      (progn
	(message (concat name " errors in `" (buffer-name)
			 "'. Use C-c ` to display."))
	(setq TeX-command-next TeX-command-TeX)
	t)
    (setq TeX-command-next TeX-command-Show)
    nil))

(defun TeX-LaTeX-sentinel (process name)
  "Cleanup TeX output buffer after running LaTeX."
  (cond ((TeX-TeX-sentinel process name)
	 (setq TeX-command-next TeX-command-LaTeX))
	((re-search-forward "^LaTeX Warning: Citation" nil t)
	 (message "You should run BibTeX to get citations right.")
	 (setq TeX-command-next TeX-command-BibTeX))
	((re-search-forward "^LaTeX Warning: \\(Reference\\|Label(s)\\)" nil t)
	 (message "You should run LaTeX again to get references right.")
	 (setq TeX-command-next TeX-command-LaTeX))
	((re-search-forward "^LaTeX Version" nil t)
	 (message (concat name ": successfully ended."))
	 (setq TeX-command-next TeX-command-Show))
	(t
	 (message (concat name ": problems."))
	 (setq TeX-command-next TeX-command-LaTeX))))

(defun TeX-BibTeX-sentinel (process name)
  "Cleanup TeX output buffer after running BibTeX."
  (message "You should perhaps run LaTeX again to get citations right.")
  (setq TeX-command-next TeX-command-LaTeX))

;;; @@ Process Control

(defun TeX-process-get-variable (name symbol &optional default)
  "Return the value in the process buffer for NAME of SYMBOL.

Return DEFAULT if the process buffer does not exist or SYMBOL is not
defined."
  (let ((buffer (TeX-process-buffer name)))
    (if buffer
	(save-excursion
	  (set-buffer buffer)
	  (if (boundp symbol)
	      (eval symbol)
	    default))
      default)))

(defun TeX-process-set-variable (name symbol value)
  "Set the variable SYMBOL in the process buffer to VALUE.
Return nil iff no process buffer exist."
  (let ((buffer (TeX-process-buffer name)))
    (if buffer
	(save-excursion
	  (set-buffer buffer)
	  (set symbol value)
	  t)
      nil)))

(defun TeX-process-check (name)
  "Check if a process for the TeX document NAME already exist.
If so, give the user the choice of aborting the process or the current
command."
  (save-some-buffers)
  (let ((process (TeX-process name)))
    (cond ((null process))
	  ((not (eq (process-status process) 'run)))
	  ((yes-or-no-p (concat "Process `"
				(process-name process)
				"' for document `"
				name
				"' running, kill it? "))
	   (delete-process process))
	  (t
	   (error "Cannot have two processes for the same document.")))))

(defun TeX-process-buffer-name (name)
  "Return name of AUC TeX buffer associated with the document NAME."
  (concat "*" name " output*"))

(defun TeX-process-buffer (name)
  "Return the AUC TeX buffer associated with the document NAME."
  (get-buffer (TeX-process-buffer-name name)))

(defun TeX-process (name)
  "Return AUC TeX process associated with the document NAME."
  (get-buffer-process (TeX-process-buffer name)))

;;; @@ Process Filters

(defun TeX-command-mode-line (process)
  "Format the mode line for a buffer containing output from PROCESS."
    (setq mode-line-process (concat ": "
				    (symbol-name (process-status process))))
    (set-buffer-modified-p (buffer-modified-p)))

(defun TeX-command-filter (process string)
  "Filter to process normal output."
  (save-excursion
    (set-buffer (process-buffer process))
    (goto-char (point-max))
    (insert string)
    (save-excursion)
    ))

(defvar TeX-current-page nil
  "The page number currently being formatted, enclosed in brackets.")

 (make-variable-buffer-local 'TeX-current-page)

(defun TeX-format-mode-line (process)
  "Format the mode line for a buffer containing TeX output from PROCESS."
    (setq mode-line-process (concat " " TeX-current-page ": "
				    (symbol-name (process-status process))))
    (set-buffer-modified-p (buffer-modified-p)))

(defun TeX-format-filter (process string)
  "Filter to process TeX output."
  (save-excursion
    (set-buffer (process-buffer process))
    (goto-char (point-max))
    (insert string)
    (save-excursion 
      (if (re-search-backward "\\[[0-9]+\\(\\.[0-9\\.]+\\)?\\]" nil t)
	  (let ((new (buffer-substring (match-beginning 0)
				       (match-end 0))))
	    (if (not (string-equal new TeX-current-page))
		(setq TeX-current-page new)))))
    (TeX-format-mode-line process)))

(defvar TeX-parse-hook nil
  "Function to call to parse content of TeX output buffer.")

 (make-variable-buffer-local 'TeX-parse-hook)

;;; @@ Active Process

(defvar TeX-current-process-region-p nil
  "This variable is set to t iff the last TeX command is on a region.")

(defun TeX-active-process ()
  "Return the active process for the current buffer."
  (if TeX-current-process-region-p
      (TeX-process (TeX-region-file))
    (TeX-process (TeX-master-file))))

(defun TeX-active-buffer ()
  "Return the buffer of the active process for this buffer."
  (if TeX-current-process-region-p
      (TeX-process-buffer (TeX-region-file))
    (TeX-process-buffer (TeX-master-file))))

(defun TeX-active-master (&optional extension)
  "The master file currently being compiled."
  (if TeX-current-process-region-p
      (TeX-region-file extension)
    (TeX-master-file extension)))

(defvar TeX-command-buffer nil
  "The buffer from where the last TeX command was issued.")

;;; @@ Region File

(defun TeX-region-create (file region original offset)
  "Create a new file named FILE with the string REGION
The region is taken from ORIGINAL starting at line OFFSET.

The current buffer and master file is searched, in order to ensure
that the TeX header and trailer information is also included.

The OFFSET is used to provide the debugger with information about the
original file."
  (let* (;; We shift buffer a lot, so we must keep track of the buffer
	 ;; local variables.  
	 (header-end TeX-header-end)
	 (trailer-start TeX-trailer-start)
	 
	 ;; We seach for header and trailer in the master file.
	 (master-name (TeX-master-file "tex"))
	 (master-buffer (find-file-noselect master-name))
	 
	 ;; And insert them into the FILE buffer.
	 (file-buffer (find-file-noselect file))
	 
	 ;; We search for the header from the master file, if it is
	 ;; not present in the region.
	 (header (if (string-match header-end region)
		     ""
		   (save-excursion
		     (save-restriction
		       (set-buffer master-buffer)
		       (goto-char (point-min))
		       ;; NOTE: We use the local value of
		       ;; TeX-header-end from the master file.
		       (if (not (re-search-forward TeX-header-end nil t))
			   ""
			 (beginning-of-line 2)
			 (buffer-substring (point-min) (point)))))))
	 
	 ;; We search for the trailer from the master file, if it is
	 ;; not present in the region.
	 (trailer-offset 0)
	 (trailer (if (string-match trailer-start region)
		      ""
		    (save-excursion
		      (save-restriction
			(set-buffer master-buffer)
			(goto-char (point-max))
			;; NOTE: We use the local value of
			;; TeX-trailer-start from the master file.
			(if (not (re-search-backward TeX-trailer-start nil t))
			    ""
			  (beginning-of-line 1)
			  (setq trailer-offset
				(count-lines (point-min) (point)))
			  (buffer-substring (point) (point-max))))))))
    (save-excursion
      (set-buffer file-buffer)
      (erase-buffer)
      (insert "\\message{ @name<" master-name ">}"
	      header
	      "\n\\message{ @name<" original "> @offset<")
      (insert (int-to-string (- offset
				(count-lines (point-min) (point))))
	      "> }\n"
	      region
	      "\n\\message{ @name<"  master-name "> @offset<")
      (insert (int-to-string (- trailer-offset
				(count-lines (point-min) (point))))
	      "> }\n"
	      trailer)
      (save-buffer 0))))

(defun TeX-region-file (&optional extension)
  "Return TeX-region file name with EXTENSION."
  (if extension
      (concat TeX-region "." extension)
    TeX-region))

(defvar TeX-region "_region_"
  "*Base name for temporary file for use with TeX-region.")

(defvar TeX-trailer-start nil
  "Regular expression delimiting start of trailer in a TeX file.")

 (make-variable-buffer-local 'TeX-trailer-start)

(defvar TeX-header-end nil
  "Regular expression delimiting end of header in a TeX file.")

 (make-variable-buffer-local 'TeX-header-end)

;;; @@ Emacs

(run-hooks 'TeX-after-tex-buf-hook)

;;; Local Variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: ";;; @+\\|(......"
;;; End:
