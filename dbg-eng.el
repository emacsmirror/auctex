;;; @ dbg-eng.el - English LaTeX error messages
;;;
;;; $Id: dbg-eng.el,v 5.2 1993-05-28 01:53:08 amanda Exp $

(provide 'dbg-eng)
(require 'tex-buf)

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

;;; @@ Error Descriptions

(defconst TeX-error-description-list
  '(("Bad \\\\line or \\\\vector argument.*" .
"The first argument of a \\line or \\vector command, which specifies the
slope, is illegal\.")

    ("Bad math environment delimiter.*" .
"TeX has found either a math-mode-starting command such as \\[ or \\(
when it is already in math mode, or else a math-mode-ending command
such as \\) or \\] while in LR or paragraph mode.  The problem is caused
by either unmatched math mode delimiters or unbalanced braces\.")

    ("Bad use of \\\\\\\\.*" .
"A \\\\ command appears between paragraphs, where it makes no sense. This
error message occurs when the \\\\ is used in a centering or flushing
environment or else in the scope of a centering or flushing
declaration.")

    ("\\\\begin{[^ ]*} ended by \\\\end{[^ ]*}." .
"LaTeX has found an \\end command that doesn't match the corresponding
\\begin command. You probably misspelled the environment name in the
\\end command, have an extra \\begin, or else forgot an \\end.")

    ("Can be used only in preamble." .
"LaTeX has encountered, after the \\begin{document}, one of the
following commands that should appear only in the preamble:
\\documentstyle, \\nofiles, \\includeonly, \\makeindex, or
\\makeglossary.  The error is also caused by an extra \\begin{document}
command.")

    ("Command name [^ ]* already used.*" .
"You are using \\newcommand, \\newenvironment, \\newlength, \\newsavebox,
or \\newtheorem to define a command or environment name that is
already defined, or \\newcounter to define a counter that already
exists. (Defining an environment named gnu automatically defines the
command \\gnu.) You'll have to choose a new name or, in the case of
\\newcommand or \\newenvironment, switch to the \\renew ...  command.")

    ("Counter too large." .
"Some object that is numbered with letters, probably an item in a
enumerated list, has received a number greater than 26. Either you're
making a very long list or you've been resetting counter values.")

    ("Environment [^ ]* undefined." .
"LaTeX has encountered a \\begin command for a nonexistent environment.
You probably misspelled the environment name. ")

    ("Float(s) lost." .
"You put a figure or table environment or a \\marginpar command inside a
parbox---either one made with a minipage environment or \\parbox
command, or one constructed by LaTeX in making a footnote, figure,
etc. This is an outputting error, and the offending environment or
command may be quite a way back from the point where LaTeX discovered
the problem. One or more figures, tables, and/or marginal notes have
been lost, but not necessarily the one that caused the error.")

    ("Illegal character in array arg." .
"There is an illegal character in the argument of an array or tabular
environment, or in the second argument of a \\multicolumn command.")

    ("Missing \\\\begin{document}." .
"LaTeX produced printed output before encountering a \\begin{document}
command. Either you forgot the \\begin{document} command or there is
something wrong in the preamble. The problem may be a stray character
or an error in a declaration---for example, omitting the braces around
an argument or forgetting the \\ in a command name.")

    ("Missing p-arg in array arg.*" .
"There is a p that is not followed by an expression in braces in the
argument of an array or tabular environment, or in the second argument
of a \\multicolumn command.")

    ("Missing @-exp in array arg." .
"There is an @ character not followed by an @-expression in the
argument of an array or tabular environment, or in the second argument
of a \\multicolumn command.")

    ("No such counter." .
"You have specified a nonexistent counter in a \\setcounter or
\\addtocounter command. This is probably caused by a simple typing
error.  However, if the error occurred while a file with the extension
aux is being read, then you probably used a \\newcounter command
outside the preamble.")

    ("Not in outer par mode." .
"You had a figure or table environment or a \\marginpar command in math
mode or inside a parbox.")

    ("\\\\pushtabs and \\\\poptabs don't match." .
"LaTeX found a \\poptabs with no matching \\pushtabs, or has come to the
\\end{tabbing} command with one or more unmatched \\pushtabs commands.")

    ("Something's wrong--perhaps a missing \\\\item." .
"The most probable cause is an omitted \\item command in a list-making
environment. It is also caused by forgetting the argument of a
thebibliography environment.")

    ("Tab overflow." .
"A \\= command has exceeded the maximum number of tab stops that LaTeX
permits.")

    ("There's no line here to end." .
"A \\newline or \\\\ command appears between paragraphs, where it makes no
sense. If you're trying to ``leave a blank line'', use a \\vspace
command.")

    ("This may be a LaTeX bug." .
"LaTeX has become thoroughly confused. This is probably due to a
previously detected error, but it is possible that you have found an
error in LaTeX itself. If this is the first error message produced by
the input file and you can't find anything wrong, save the file and
contact the person listed in your Local Guide.")

    ("Too deeply nested." .
"There are too many list-making environments nested within one another.
How many levels of nesting are permitted may depend upon what computer
you are using, but at least four levels are provided, which should be
enough.")

    ("Too many unprocessed floats." .
"While this error can result from having too many \\marginpar commands
on a page, a more likely cause is forcing LaTeX to save more figures
and tables than it has room for.  When typesetting its continuous
scroll, LaTeX saves figures and tables separately and inserts them as
it cuts off pages. This error occurs when LaTeX finds too many figure
and/or table environments before it is time to cut off a page, a
problem that is solved by moving some of the environments farther
towards the end of the input file. The error can also be caused by a
``logjam''---a figure or table that cannot be printed causing others
to pile up behind it, since LaTeX will not print figures or tables out
of order. The jam can be started by a figure or table that either is
too large to fit on a page or won't fit where its optional placement
argument says it must go. This is likely to happen if the argument
does not contain a p option.")

    ("Undefined tab position." .
"A \\>, \\+, \\-, or \\< command is trying to go to a nonexistent tab
position---one not defined by a \\= command.")

    ("\\\\< in mid line." .
"A \\< command appears in the middle of a line in a tabbing environment.
This command should come only at the beginning of a line.")

    ("Counter too large." .
"Footnotes are being ``numbered'' with letters or footnote symbols and
LaTeX has run out of letters or symbols. This is probably caused by
too many \\thanks commands.")

    ("Double subscript." .
"There are two subscripts in a row in a mathematical
formula---something like x_{2}_{3}, which makes no sense.")

    ("Double superscript." .
"There are two superscripts in a row in a mathematical
formula---something like x^{2}^{3}, which makes no sense.")

    ("Extra alignment tab has been changed to \\\\cr." .
"There are too many separate items (column entries) in a single row of
an array or tabular environment. In other words, there were too many &
's before the end of the row. You probably forgot the \\\\ at the end of
the preceding row.")

    ("Extra \\}, or forgotten \\$." .
"The braces or math mode delimiters don't match properly. You probably
forgot a {, \\[, \\(, or $.")

    ("Font [^ ]* not loaded: Not enough room left." .
"The document uses more fonts than TeX has room for. If different parts
of the document use different fonts, then you can get around the
problem by processing it in parts.")

    ("I can't find file `.*'." .
"TeX can't find a file that it needs. If the name of the missing file
has the extension tex, then it is looking for an input file that you
specified---either your main file or another file inserted with an
\\input or \\include command. If the missing file has the extension sty
, then you have specified a nonexistent document style or style
option.")

    ("Illegal parameter number in definition of .*" .
"This is probably caused by a \\newcommand, \\renewcommand,
\\newenvironment, or \\renewenvironment command in which a # is used
incorrectly.  A # character, except as part of the command name \\#,
can be used only to indicate an argument parameter, as in #2, which
denotes the second argument. This error is also caused by nesting one
of the above four commands inside another, or by putting a parameter
like #2 in the last argument of a \\newenvironment or \\renewenvironment
command.")

    ("Illegal unit of measure ([^ ]* inserted)." .
"If you just got a

      ! Missing number, treated as zero.

error, then this is part of the same problem.  If not, it means that
LaTeX was expecting a length as an argument and found a number
instead.  The most common cause of this error is writing 0 instead of
something like 0in for a length of zero, in which case typing return
should result in correct output. However, the error can also be caused
by omitting a command argument.")

    ("Misplaced alignment tab character \\&." .
"The special character &, which should be used only to separate items
in an array or tabular environment, appeared in ordinary text. You
probably meant to type \\&.")

    ("Missing control sequence inserted." .
"This is probably caused by a \\newcommand, \\renewcommand, \\newlength,
or \\newsavebox command whose first argument is not a command name.")

    ("Missing number, treated as zero." .
"This is usually caused by a LaTeX command expecting but not finding
either a number or a length as an argument. You may have omitted an
argument, or a square bracket in the text may have been mistaken for
the beginning of an optional argument. This error is also caused by
putting \\protect in front of either a length command or a command such
as \\value that produces a number.")

    ("Missing [{}] inserted." .
"TeX has become confused. The position indicated by the error locator
is probably beyond the point where the incorrect input is.")

    ("Missing \\$ inserted." .
"TeX probably found a command that can be used only in math mode when
it wasn't in math mode.  Remember that unless stated otherwise, all
the commands of Section can be used only in math mode. TeX is not in
math mode when it begins processing the argument of a box-making
command, even if that command is inside a math environment. This error
also occurs if TeX encounters a blank line when it is in math mode.")

    ("Not a letter." .
"Something appears in the argument of a \\hyphenation command that
doesn't belong there.")

    ("Paragraph ended before [^ ]* was complete." .
"A blank line occurred in a command argument that shouldn't contain
one. You probably forgot the right brace at the end of an argument.")

    ("\\\\[^ ]*font [^ ]* is undefined .*" .
"These errors occur when an uncommon font is used in math mode---for
example, if you use a \\sc command in a formula inside a footnote,
calling for a footnote-sized small caps font.  This problem is solved
by using a \\load command.")

    ("Font .* not found." .                                    
"You requested a family/series/shape/size combination that is totally
unknown.  There are two cases in which this error can occur:
  1) You used the \\size macro to select a size that is not available.
  2) If you did not do that, go to your local `wizard' and
     complain fiercely that the font selection tables are corrupted!")
 
    ("TeX capacity exceeded, sorry .*" .
"TeX has just run out of space and aborted its execution. Before you
panic, remember that the least likely cause of this error is TeX not
having the capacity to process your document.  It was probably an
error in your input file that caused TeX to run out of room. The
following discussion explains how to decide whether you've really
exceeded TeX's capacity and, if so, what to do. If the problem is an
error in the input, you may have to use the divide and conquer method
described previously to locate it. LaTeX seldom runs out of space on a
short input file, so if running it on the last few pages before the
error indicator's position still produces the error, then there's
almost certainly something wrong in the input file.

The end of the error indicator tells what kind of space TeX ran out
of. The more common ones are listed below, with an explanation of
their probable causes.

buffer size 
===========
Can be caused by too long a piece of text as the argument
of a sectioning, \\caption, \\addcontentsline, or \\addtocontents
command. This error will probably occur when the \\end{document} is
being processed, but it could happen when a \\tableofcontents,
\\listoffigures, or \\listoftables command is executed. To solve this
problem, use a shorter optional argument. Even if you're producing a
table of contents or a list of figures or tables, such a long entry
won't help the reader.

exception dictionary 
====================
You have used \\hyphenation commands to give TeX
more hyphenation information than it has room for. Remove some of the
less frequently used words from the \\hyphenation commands and insert
\\- commands instead.

hash size 
=========
Your input file defines too many command names and/or uses
too many cross-ref- erencing labels.

input stack size 
================
This is probably caused by an error in a command
definition. For example, the following command makes a circular
definition, defining \\gnu in terms of itself:

          \\newcommand{\\gnu}{a \\gnu} % This is wrong!

When TeX encounters this \\gnu command, it will keep chasing its tail
trying to figure out what \\gnu should produce, and eventually run out
of ``input stack''.

main memory size 
================
This is one kind of space that TeX can run out of when processing a
short file. There are three ways you can run TeX out of main memory
space: (1) defining a lot of very long, complicated commands, (2)
making an index or glossary and having too many \\index or \\glossary
commands on a single page, and (3) creating so complicated a page of
output that TeX can't hold all the information needed to generate it.
The solution to the first two problems is obvious: define fewer
commands or use fewer \\index and \\glossary commands. The third problem
is nastier. It can be caused by large tabbing, tabular, array, and
picture environments. TeX's space may also be filled up with figures
and tables waiting for a place to go.  To find out if you've really
exceeded TeX's capacity in this way, put a \\clearpage command in your
input file right before the place where TeX ran out of room and try
running it again. If it doesn't run out of room with the \\clearpage
command there, then you did exceed TeX's capacity.  If it still runs
out of room, then there's probably an error in your file.  If TeX is
really out of room, you must give it some help. Remember that TeX
processes a complete paragraph before deciding whether to cut the
page. Inserting a \\newpage command in the middle of the paragraph,
where TeX should break the page, may save the day by letting TeX write
the current page before processing the rest of the paragraph. (A
\\pagebreak command won't help.) If the problem is caused by
accumulated figures and tables, you can try to prevent them from
accumulating---either by moving them further towards the end of the
document or by trying to get them to come out sooner.  If you are
still writing the document, simply add a \\clearpage command and forget
about the problem until you're ready to produce the final version.
Changes to the input file are likely to make the problem go away.

pool size 
=========
You probably used too many cross-ref-erencing \\labels and/or defined
too many new command names. More precisely, the labels and command
names that you define have too many characters, so this problem can be
solved by using shorter names. However, the error can also be caused
by omitting the right brace that ends the argument of either a counter
command such as \\setcounter, or a \\newenvironment or \\newtheorem
command.

save size 
=========
This occurs when commands, environments, and the scopes of
declarations are nested too deeply---for example, by having the
argument of a \\multiput command contain a picture environment that in
turn has a \\footnotesize declaration whose scope contains a \\multiput
command containing a ....")

    ("Text line contains an invalid character." .
"The input contains some strange character that it shouldn't. A mistake
when creating the file probably caused your text editor to insert this
character. Exactly what could have happened depends upon what text
editor you used. If examining the input file doesn't reveal the
offending character, consult the Local Guide for suggestions.")

    ("Undefined control sequence."   .  
"TeX encountered an unknown command name. You probably misspelled the
name. If this message occurs when a LaTeX command is being processed,
the command is probably in the wrong place---for example, the error
can be produced by an \\item command that's not inside a list-making
environment. The error can also be caused by a missing \\documentstyle
command.")

    ("Use of [^ ]* doesn't match its definition." .
"It's probably one of the picture-drawing commands, and you have used
the wrong syntax for specifying an argument. If it's \\@array that
doesn't match its definition, then there is something wrong in an
@-expression in the argument of an array or tabular
environment---perhaps a fragile command that is not \\protect'ed.")

    ("You can't use `macro parameter character \\#' in [^ ]* mode." .
"The special character # has appeared in ordinary text. You probably
meant to type \\#.")

    ("Overfull \\\\hbox .*" .
"Because it couldn't find a good place for a line break, TeX put more
on this line than it should.")

    ("Overfull \\\\vbox .*" .
"Because it couldn't find a good place for a page break, TeX put more
on the page than it should. ")

    ("Underfull \\\\hbox .*" .
"Check your output for extra vertical space.  If you find some, it was
probably caused by a problem with a \\\\ or \\newline command---for
example, two \\\\ commands in succession. This warning can also be
caused by using the sloppypar environment or \\sloppy declaration, or
by inserting a \\linebreak command.")

    ("Underfull \\\\vbox .*" .
"TeX could not find a good place to break the page, so it produced a
page without enough text on it. ")

;;; New list items should be placed here 
;;; 
;;; ("err-regexp" . "context") 
;;; 
;;; the err-regexp item should match anything

    (".*" . "No help available"))	; end definition 
"A list of the form (\"err-regexp\" . \"context\") used by function
\\{TeX-help-error} to display help-text on an error message or warning.
err-regexp should be a regular expression matching the error message
given from TeX/LaTeX, and context should be some lines describing that
error")

;;; @@ Emacs

(run-hooks 'TeX-after-dbg-eng-hook)

;;; Local Variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: ";;; @+\\|(......"
;;; End:
