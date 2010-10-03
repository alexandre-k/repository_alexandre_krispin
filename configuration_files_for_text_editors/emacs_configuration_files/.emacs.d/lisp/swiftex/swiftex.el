;;; swiftex.el --- major modes for LaTeX and LaTeX doc.sty documents

;;;; COPYRIGHT NOTICE
;;;
;;; Copyright (C) 1995-1999 Matt Swift <swift@alum.mit.edu>
;;;
;; Author:  Matt Swift <swift@alum.mit.edu>
;;	Johanes Braams
;;	Frank Mittelbach
;; Maintainer:  Matt Swift <swift@alum.mit.edu>
;; Version:  $Revision: 1.35 $
;; Keywords: tex, latex, doc, doc.sty, ltxdoc, dtx
;; LCD Archive Entry:
;; swifTeX|Matt Swift|swift@alum.mit.edu|
;; Major modes for LaTeX and LaTeX doc.sty documents|
;; $Date: 2001/09/08 04:09:20 $|$Revision: 1.35 $|~/modes/swiftex.el.gz|

;;; FIX last field above will be wrong when I have a tarball

;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with your Emacs program (without which this file is not useful); if not,
;;; write to the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;;

;;; Commentary:
;;;
;;; This file is distributed by its author with
;;; documentation in Info format that gives an overview of
;;; this package and describes how to install and use and
;;; use it.  It should always be available on CTAN FIX.

;;;; IMPLEMENTATION
;;; CODE:

;;;;; CUSTOMIZATION GROUPS

;;; :prefix "swiftex-"
;;; :prefix "doctex-"
;;; :prefix "rx-"
;;; :prefix "stx-"
;;; :prefix "dtx-"
(defgroup swiftex nil
  "Major mode for LaTeX files."
  :tag "SwifTeX"
  :group 'tex)
(defgroup doctex nil
  "Major mode for documented LaTeX source (.dtx) files."
  :tag "docTeX"
  :group 'tex
  :group 'swiftex)

;;; (defcustom
;;; :type
;;; :options
;;; :version
;;; :set
;;; :get
;;; :initialize
;;; ;; customize common options
;;; :tag
;;; :group
;;; :link
;;; :load
;;; :require

;;;;; VERSION INFO
(defconst stx-version "$Revision: 1.35 $"
  "RCS version of Matt's master swiftex.el.")

;;;;; GENERAL ACTIONS
(provide 'swiftex)

;;; Patch below from Karl Eichwalder <ke@gnu.franken.de>.  I think I want to do
;;; it differently eventually.
;;; FIX Don't customize for now, since it may disappear.
(defvar stx-tex-derivation t
  "Non-nil means use Emacs standard `tex-mode' for derivation.
Nil means use `LaTeX-mode' from the AUC TeX package.")
(if stx-tex-derivation
    (require 'tex-mode)
  (require 'tex-site))

;;; `concat-chars' was renamed to `string' in 20.3
;;; thanks to Peter Møller Neergaard for noticing this
(if (not (fboundp 'string))
    (if (fboundp 'concat-chars)
	(defalias 'string 'concat-chars)
      (error
       "I'm confused: neither `string' nor `concat-chars' are defined as functions")))

;;; FIX perhaps a variable to let the user choose between old and new outlines?
;;; do i depend on the old one?  yes probably that is all
;;; the weird boln stuff
(require 'outline)

;;;;; REGEXP and STRING CONSTANTS

;;; Using the following constants makes regular expressions more readable in
;;; Lisp code.

(defconst rx-whitespace "\\s-*"
  "Matches optional characters with whitespace syntax.")
;;; Notice that this expression has parentheses.

;;; FIX: As defined, it will match two newlines in a row, which is a
;;; construction that would break TeX in the places where I use it.  I would
;;; like to improve the matching to avoid matching this case, to be consistent
;;; with TeX, but I don't see that it will cause a problem under normal
;;; circumstances.
(defconst rx-whitespace-newlines "\\(\\s-\\|\n\\)*"
  "Matches optional newlines or characters with whitespace syntax.")
(defconst rx-normal-boln "^"
  "Matches unhidden beginning of line.")
(defconst rx-hidden-boln "[\^M]"
  "Matches beginning of line when hidden in outline mode.")
(defconst rx-eoln "$")
(defconst rx-normal-whiteln (concat rx-normal-boln rx-whitespace rx-eoln)
  "Matches empty line.")
(defconst rx-dollar "\\$"
  "Matches a dollar sign.")
(defconst rx-backslash "\\\\"
  "Matches a backslash.")
(defconst rx-white-back (concat rx-whitespace rx-backslash)
  "Matches optional whitespace plus backslash." )
(defconst rx-or "\\|"
  "Regexp string for logical OR." )
;;; FIX: why do I need to avoid the \n?  It's only working for top-level!
;;; don't I mean innermost level?
(defconst rx-brace-pair "{[^}\n]*}"
  "Matches one balanced set of braces on a single line.")
(defconst rx-tex-grop "{"
  "Matches TeX group open character -- a left brace.
Also (ab)used as a regular string for insertions.")
(defconst rx-tex-grcl "}"
  "Matches TeX group close character -- a right brace.
Also (ab)used as a regular string for insertions.")
(defconst stx-str-backslash "\\")

;;;;; FONT LOCK / HILIT STUFF [FIX]

;;; FIX: what do I mean 'loads'?
;;; FIX: :require?
(defvar stx-hilit nil
  "Non-nil means highlight swifTeX buffers.
Loads the library \"hilit-swiftex\".")

;;; FIX: what do I mean 'loads'?
;;; FIX: :require?
;;; FIX: link to font-latex customization?
(defvar stx-fontlock t
  "Non-nil means font lock swifTeX buffers.
Loads the library \"font-latex\" until I write something
better.")

;;; FIX what does load mean?  :require?
(defvar dtx-hilit t
  "Non-nil means highlight docTeX buffers.
Loads the library \"hilit-doctex\".")

;;; FIX what does load mean?
;;; FIX :require?
;;; FIX :link to font-latex customization?
(defvar dtx-fontlock nil
  "Non-nil means font-lock docTeX buffers.
Loads library \"font-latex\" for now until I write something
better.")
  

;;;;; SWIFTEX MODE

;;; Much of what is defined below takes docTeX mode into account.  Only the
;;; code forms unique to docTeX mode appear in its section below.

;;;;;;        USER VARIABLES

;;; FIX: use :set?
;;; FIX: link?
(defcustom stx-sectioning-offset 1
  "Outline level offset for all LaTeX sectioning commands in a swifTeX buffer.
See the documentation for the variable
`stx-sectioning-commands'."
  :tag "Sectioning Commands Outline Offset"
  :type 'integer
  :group 'swiftex)

;;; FIX; :set?
(defcustom stx-sectioning-commands '(
				  ("documentclass" . -2)
				  ;; leave off final
				  ;; closebrace, since the
				  ;; \> in the regexp needs
				  ;; to match the end of the
				  ;; strings here.
				  ("begin{document" . -2)
				  ("end{document" . -2)
				  ("endinput" . -2)
				  ("part" . -2)
				  ("frontmatter" . -1)
				  ("mainmatter" . -1)
				  ("backmatter" . -1)
				  ("chapter" . -1)
				  ("appendix" . -1)
				  ("section".  0)
				  ("subsection" . 1)
				  ("subsubsection" . 2)
				  ("paragraph" . 3)
				  ("subparagraph" . 4)
				  )
  "An alist of LaTeX sectioning commands with level offsets for swifTeX mode.
List elements have the form (NAME . NUMBER) where NAME is a
string containing a LaTeX sectioning command with no
backslash, and NUMBER is an integer which will be added to
`stx-sectioning-offset' to get the actual outline level.

The normal top level outline heading of `%*' is level 1.
The default offset for \"\\section\" is 0, so its default
level is 1.  For documents with chapters, you might want to
set `stx-sectioning-offset' to 2, so that \"\\chapter\" and
\"%*\" are the same level.

An \"endinput\" entry, if it exists, is treated specially in
docTeX mode.  See `stx-set-outlinevars'.

Changes to `stx-sectioning-offset' and
`stx-sectioning-commands' will take effect the next time
`stx-set-vars' is called.  This is normally called only by
`swiftex-mode' and `doctex-mode'."
  :tag "Sectioning Commands and Levels"
  :type '(repeat (cons :format "%v"
		       (string :tag "Command")
		       (integer :tag "Level")))
  :group 'swiftex)
;;; FIX I don't understand :format, I copied this from
;;; somewhere.

;;; FIX: no nil
;;; FIX: use :set?
(defcustom stx-command-parseps '(
			;;; below ought to match first section of
			;;; `stx-command-parstarts' exactly:
				"begin" "end"
				"par"
				"opening" "closing"
				"\\[" "caption" "label"	"usepackage"
				"documentclass" "date" "author" "title"
				"tableofcontents" "maketitle"
				"listoffigures" "listoftables"
				"bibliographystyle" "bibliography"
				"include" "input" "includeonly"
				"frontmatter" "mainmatter" "backmatter"
				)
  "A list of LaTeX commands which separate Emacs paragraphs.

When these regexp strings occur after optional whitespace
and a backslash, they separate Emacs paragraphs, and resist
being filled.

More specifically, `stx-set-vars' combines these strings
with logical OR (`rx-or') and tacks them on to
`paragraph-separate' with a logical OR.

Sectioning commands included in `stx-sectioning-commands'
don't need to be given here because they will get into
`paragraph-separate' via `outline-regexp'.

Do not make this list nil.

Do not add strings to this variable without also adding them
to `stx-command-parstarts', or otherwise ensuring they make
it into `paragraph-start'."
  :tag "Paragraph Separating Commands"
  :type '(repeat regexp)
  :group 'swiftex
  :group 'doctex)
(make-variable-buffer-local 'stx-command-parseps)

;;; FIX: no nil
;;; FIX: use :set?
(defcustom stx-command-parstarts '(
			;;; below do not receive fill text:
				  "begin" "end"
				  "par"
				  "opening" "closing"
				  "\\[" "caption" "label" "usepackage"
				  "documentclass" "date" "author" "title"
				  "lastchange" "copystyle" "usepackage"
				  "tableofcontents" "maketitle"
				  "listoffigures" "listoftables"
				  "bibliographystyle" "bibliography"
				  "include" "input" "includeonly"
				  "frontmatter" "mainmatter" "backmatter"
			;;; below receive fill text:
				  "indent" "noindent"
				  "bibitem" "item"
				  "footnote"
				  )

  "A list of LaTeX commands which start or separate Emacs paragraphs.

When these regexps occur after a backslash at the beginning
of a line, those that are not in `paragraph-separate' start
Emacs paragraphs, and receive text on their line during
fills.

Sectioning commands included in `stx-sectioning-commands'
don't need to be given here because they will get into
`paragraph-separate' via `outline-regexp'.

Do not make this list nil.

See also `stx-command-parseps'."
  :tag "Paragraph Starting or Separating Commands"
  :type '(repeat regexp)
  :group 'swiftex
  :group 'doctex)
(make-variable-buffer-local 'stx-command-parstarts)

;;; FIX: need key-specification :type
;;; FIX :set?
(defcustom swiftex-dtx-prefix-key '"\C-c\C-d"
  "Prefix key for certain docTeX mode commands available in swifTeX mode.
A change in this variable takes effect the next time
`swiftex-mode' is called."
  :tag "Prefix for Certain docTeX Commands"
  :type 'string
  :group 'swiftex)

;;;;;;        FILE VARIABLES

(defvar stx-local-command-parstarts nil
  "Partial list of LaTeX commands that start or separate Emacs paragraphs.
This variable exists so that it can be set as a file
variable.  After the line that sets it, include a line with
`eval: stx-set-vars'

When these regexp strings occur after optional whitespace
and a backslash, those not in `paragraph-separate' start
Emacs paragraphs, and receive text on their line during
fills.

More specifically, `stx-set-vars' combines these strings
with logical OR (`rx-or'), precedes them by optional
whitespace and a backslash, and tacks them on to
`paragraph-separate' with a logical OR.

Sectioning commands included in `stx-sectioning-commands'
don't need to be given here because they will get into
`paragraph-separate' via `outline-regexp'.

See `stx-local-parstarts' for a way to add starters that are
not LaTeX commands.

See also `stx-local-parseps'.")
(make-variable-buffer-local 'stx-local-command-parstarts)

(defvar stx-local-command-parseps nil
  "Partial list of LaTeX commands that separate Emacs paragraphs.
This variable exists so that it can be set as a file
variable.  After the line that sets it, include a line with
`eval: stx-set-vars'.

When these regexp strings occur after optional whitespace
and a backslash, they separate Emacs paragraphs, and resist
being filled.

More specifically, `stx-set-vars' combines these strings
with logical OR (`rx-or'), precedes them by optional
whitespace and a backslash, and tacks them on to
`paragraph-separate' with a logical OR.

Sectioning commands included in `stx-sectioning-commands'
don't need to be given here because they will get into
`paragraph-separate' via `outline-regexp'.

Do not add strings to this list without also adding them to
`stx-local-command-parstarts', or otherwise ensuring they
make it into `paragraph-start'.

See `stx-local-parseps' for a way to add separators that are
not LaTeX commands.")
(make-variable-buffer-local 'stx-local-command-parseps)

(defvar stx-local-parstarts nil
  "A part of the variable `paragraph-start' in swifTeX and docTeX modes.
This variable exists so that it can be set as a file
variable.  After the line that sets it, include a line with
`eval: stx-set-vars'.

The regexp strings in this list that are not in
`paragraph-separate' start Emacs paragraphs, and receive
text on their line during a fill.

More specifically, `stx-set-vars' combines these strings
with logical OR (`rx-or') and tacks them on to
`paragraph-start' with a logical OR.

Sectioning commands included in `stx-sectioning-commands'
don't need to be given here because they get into
`paragraph-separate' via `outline-regexp'.

See `stx-local-command-parstarts' for an easy way to add
starters that are LaTeX commands.

See also `stx-local-parseps'.")
(make-variable-buffer-local 'stx-local-parstarts)

(defvar stx-local-parseps nil
  "A part of the variable `parargraph-separate' in swifTeX and docTeX modes.
This variable exists so that it can be set as a file
variable.  After the line that sets it, include a line with
`eval: stx-set-vars'.

The regexp strings in this list separate Emacs paragraphs,
and resist being filled.

More specifically, `stx-set-vars' combines these strings
with logical OR (`rx-or') and tacks them on to
`paragraph-separate' with a logical OR.

Sectioning commands included in `stx-sectioning-commands'
don't need to be given here because they will get into
`paragraph-separate' via `outline-regexp'.

Do not add strings to this list without also adding them to
`stx-local-parstarts', or otherwise ensuring they make it
into `paragraph-start'.

See `stx-local-command-parseps' for an easy way to add
separators that are LaTeX commands.")
(make-variable-buffer-local 'stx-local-parseps)
;;;;;;        HOOK VARIABLES
;;; FIX see what define-derived will do as docstring for this
;;; (defvar swiftex-mode-hook nil
;;;   "Hook run as the last thing after entering swifTeX mode.
;;; When entering docTeX mode, the last thing done is to run this hook then
;;; `doctex-mode-hook.")
;;;;;;        INTERNAL VARIABLES

(defvar stx-comment-start "%"
  "A regexp that matches the start of a comment.
It is \"%\" in swifTeX mode and \"\\\\^\\\\^A\" in docTeX mode.")
(make-variable-buffer-local 'stx-comment-start)

(defvar stx-line-start ""
  "A regexp for the beginning of all significant lines.
It is the empty string in swifTeX mode and \"%*\" in docTeX mode.")
(make-variable-buffer-local 'stx-line-start)

;;;;;;        INLINE FUNCTION-VARIABLES

;;; Without these abbreviations, other code would be very hard to read.

(defsubst stx-boln ()
"Return regexp for the beginning of a line in swifTeX or docTeX modes.
The regexp returned matches lines hidden in an outline, and
depends on the value of `stx-line-start'."
  (concat "\\(" rx-normal-boln rx-or rx-hidden-boln "\\)"
	  stx-line-start))

(defsubst stx-bwb ()
"Return a regexp boln + w-space + b-slash that is possibily hidden in an outline."
  (concat (stx-boln) rx-whitespace rx-backslash))

(defsubst stx-whiteln ()
"Return a regexp for a blank line that is possibly hidden in an outline."
  (concat (stx-boln) rx-whitespace rx-eoln))
;;;;;;        MODE INITIALIZATION FUNCTIONS

(defun stx-set-outlinevars (sectioning-commands sectioning-offset doctex)
  "Set `outline-regexp' and define `outline-function' for LaTeX buffers.

Headings of the form `stx-comment-start' followed by at
least one star (\"*\"), and elements of SECTIONING-COMMANDS
are included.  The variable SECTIONING-OFFSET controls the
relative depths of these two kinds of headings.  DOCTEX
non-nil means the buffer is a docTeX buffer.

In default use, the first two arguments will always be the
variables of the same name with either `stx-' or `dtx-'
prefixed, e.g., `dtx-sectioning-offset'.

If an entry for \"endinput\" exists in SECTIONING-COMMANDS
it is treated specially.  In swifTeX mode (i.e., DOCTEX
non-nil), it is treated just like the other sectioning
commands.  In docTeX mode, \"\\endinput\" will be an
outline heading only if it begins in the first column of the
buffer.  This is because 1) there is no reason for it to
occur as an executed macro behind a comment in the
documentation text, and 2) when it occurs with preceding
whitespace in a macrocode environment, doc.sty considers it
to be part of a macro being defined, and does not obey it,
so neither will docTeX."
  (let (;; modifications to `sectioning-commands' below are local
	(sectioning-commands (copy-sequence sectioning-commands))
	;; `endinput-entry' will be nil if \endinput is not there, i.e., not
	;; supposed to be an outline heading; or will be the cons cell if there
	(endinput-entry (assoc "endinput" sectioning-commands)))
    ;; STEP ONE -- set `outline-regexp'
    (make-local-variable 'outline-regexp)
    (setq outline-regexp
	  ;;`outline-regexp' is prefixed by "^" when it is used by the outline
	  ;;commands, and enclosed in regexp parentheses, so we don't need to
	  ;;use "^" or parentheses here.
	  (rx-catenate-or
	   nil
	   (concat stx-line-start stx-comment-start "[*]+")
	   (if (and doctex endinput-entry)
	       (progn
		 ;; Remove the endinput entry from `stx-sectioning-commands' so
		 ;; it doesn't get included below in the wrong way.
		 (delete endinput-entry sectioning-commands)
		 ;; include it in the right way here
		 (concat rx-backslash "endinput")))
	   (concat stx-line-start
		   rx-white-back
		   "\\("
		   ;; These are the actual sectioning commands.
		   (rx-catenate-or (mapcar 'car sectioning-commands))
		   "\\)"
		   "\\>")))
    ;; STEP TWO -- define `stx-outline-level' or `dtx-outline-level'and
    ;; `byte-compile' it.  We want to make `(s|d)tx-outline-level' as fast as
    ;; possible, so we avoid evaluating conditionals and variable references
    ;; which are going to be the same throughout a buffer's life.  The tradeoff
    ;; is requiring the byte compiling code to be in memory.
    (byte-compile
     (eval
      (`
       (defun (, (if doctex
		     'dtx-outline-level
		   'stx-outline-level)) ()
	 (, (concat "Return the outline level for heading at point.

For a sectioning command, return the value of
`" (if doctex
       "d"
     "s") "tx-sectioning-offset' plus the number associated with the
particular heading in `" (if doctex
     "d"
     "s") "tx-sectioning-commands'.

For a normal outline heading `stx-comment-start' followed by
at least one star (\"*\"), return the number of stars.

Like the standard function `outline-level', this function
must be called at the beginning of a header line."))
	 ;; First check if we're on a normal outline heading.
	 (if (looking-at (concat stx-line-start ;    ""  or "%*"
				 stx-comment-start ; "%" or "\\^\\^A"
				 "[*]+"))
	     ;; Return the number of stars.  The formula is different in docTeX
	     ;; mode and swifTeX mode.
	     (, (if doctex
		    ;; length of match minus length of "%^^A"
		    '(- (- (match-end 0) (match-beginning 0)) 4)
		  ;; length of match minus length of "%"
		  '(1- (- (match-end 0) (match-beginning 0)))))
	   ;; Still don't know where we are.
	   ;; Check if we're on a sectioning command.
	   (let ((count 0)
		 (max (, (length sectioning-commands))))
	     (while (and (< count max)
			 (not (looking-at
			       (concat stx-line-start
				       rx-white-back
				       (car (elt '(, sectioning-commands)
						 count))
				       "\\>"))))
	       (setq count (1+ count)))
	     (if (equal count max)
		 ;; Still haven't figured where we are.
		 ;; If docTeX mode and \endinput is a heading, check for it.
		 (, (if (and doctex endinput-entry)
			(` (if (looking-at (concat rx-backslash
						   "endinput"))
			       (, (cdr endinput-entry))
			     ;; We really don't know where we are.  (In docTeX
			     ;; mode and looking for \endinput.)
			     (error
			      ;; FIX: s-or-d tx-outline-level
			      "`stx-outline-level' falling through! (a)")))
		      ;; We really don't know where we are. (In swifTeX mode,
		      ;; or docTeX mode and not looking for \endinput.)
		      ;; FIX: s-or-d tx-outline-level
		      '(error "`stx-outline-level' falling through! (b)")))
	       (+ (, sectioning-offset)
		  (cdr (elt '(, sectioning-commands) count))))))))))))

(defun stx-set-parvars ()
  "Set `paragraph-' variables for swifTeX or docTeX mode.

Set `paragraph-start' to a string containing each of the
following strings separated by logical OR (`rx-or'):

    BOLN + whitespace + EOLN
    BOLN + whitespace + `$$'
    `$$' + whitespace + EOLN
    `\\\\' + whitespace + EOLN
    BOLN + `outline-regexp'
    BOLN + significant text + single comment
    BOLN + whitespace + double comment
    `stx-local-parstarts'
    BOLN + whitespace + `\\' + `command' + `\\>'
        where `command' is an element of either
          `stx-command-parstarts'   or
          `stx-local-command-parstarts'.

Set `paragaph-separate' to the same string, with the
substitution of `parseps' for `parstarts' in the obvious
places.

The variable `rx-whitespace' determines what whitespace is\;
the variable `rx-eoln' determines EOLN\; the function
`stx-boln' determines BOLN.  Significant text is one or more
non-newlines followed by an even number of backslashes
\(including zero backslashes).

The regexps are actually defined in a more efficient but
logically equivalent way.

The function `stx-merge-list' provides a convenient way to
add items to list variables."
  (let ((common
	 (rx-catenate-or nil
			 (stx-whiteln)
			 (concat (stx-boln) rx-whitespace rx-dollar rx-dollar)
			 (concat "\\("
				 rx-backslash rx-backslash      rx-or
				 rx-dollar rx-dollar
				 "\\)"
				 rx-whitespace
				 rx-eoln)
			 (concat (stx-boln) outline-regexp)
			 (concat (stx-boln)
				 ".*[^\n\\]"
				 "\\(" rx-backslash rx-backslash "\\)*"
				 comment-start)
			 (concat (stx-boln)
				 rx-whitespace
				 stx-comment-start
				 "\\(" stx-comment-start "\\)+"))))
    (setq paragraph-start
	  (rx-catenate-or nil
			  common
			  (concat (stx-boln)
				  rx-white-back
				  "\\("
				  (rx-catenate-or stx-command-parstarts)
				  (rx-catenate-or stx-local-command-parstarts)
				  "\\)"
				  "\\>"
				  (rx-catenate-or stx-local-parstarts))))
    (setq paragraph-separate
	  (rx-catenate-or nil
			  common
			  (concat (stx-boln)
				  rx-white-back
				  "\\("
				  (rx-catenate-or stx-command-parseps)
				  (rx-catenate-or stx-local-command-parseps)
				  "\\)"
				  "\\>"
				  (rx-catenate-or stx-local-parseps))))))

;;;;;;        DEFINE MODE
(define-derived-mode swiftex-mode latex-mode "swifTeX"
  "Major mode for LaTeX documents derived from the standard LaTeX mode.

The variables `paragraph-separate' and `paragraph-start' are
carefully chosen to make filling and movement convenient.
See `stx-set-parvars'.

One consequence of their values is that single-commented
paragraphs fill normally, and double-or-more commented lines
resist filling (they separate Emacs paragraphs).  Comments
with non-whitespace before a \"%\" also resist filling.

Outline headings are either a single \"%\" in the first
column plus at least one star (\"*\"), or the standard LaTeX
sectioning commands.  See `stx-set-outlinevars'.

The TAB key seems to behave as usual but inserts spaces only
\(that is, no `\\t' characters).  It is confusing when `\\t'
characters occur inside verbatim environments (including
macrocode environments) because the typeset appearance will
not match the appearance of the source file, and
code-indenting conventions will often get mangled.

A number of commands facilitate movement and save typing.

\\[stx-up-block]	moves point past the next \"\\end{name}\".
\\[stx-next-braces]	moves point out of enclosing braces and just into the next set.
\\[stx-begin-block]	inserts \"\\begin{}\" and prompts for the argument.
\\[stx-insert-block]	inserts a \"\\begin{} \\end{}\" pair and prompts for the argument.
\\[stx-close-block]	creates an \"\\end{...}\" to match the last unclosed \"\\begin{...}\".
\\[stx-close-block-from-inside]	closes a LaTeX block after the user has typed the argument to \"\\begin\".
\\[stx-emphasize]	inserts \"\\emph{}\" and leaves point between braces.
\\[stx-emphasize-word]	surrounds word at point with \"\\emph{\" and \"}\".

Certain docTeX mode commands are available with the prefix
`swiftex-dtx-prefix-key':

The following commands maintain the version information in
the LaTeX buffer.  They act on lines that define
\\fileversion, \\filedate, or \\docdate, or the equivalent
components of the \\ProvidesPackage or \\ProvidesClass
declarations if the first are absent.

\\[dtx-get-fileinfo]	displays the buffer's version in the minibuffer.
\\[dtx-update-minor-version]	updates the minor version.
\\[dtx-update-major-version]	updates the major version.

The last two also update the file date to the current date.

\\[dtx-update-documentation-date]	makes the documentation date current.
\\[dtx-insert-change]	inserts a LaTeX \\changes entry.
\\[add-change-log-entry]	adds a ChangeLog entry.

Some convenient ways to customize behavior in individual
files are provided by the `stx-local-' variables.

\\{swiftex-mode-map}"
  (make-local-variable 'adaptive-fill-regexp)
  (setq adaptive-fill-regexp
	;; whitespace + optional stx-comment-start + whitespace
	;; Notice that a line with more than one "%" is a paragraph separator
	;; and therefore never part of a paragraph anyway.
	(concat rx-whitespace
		stx-comment-start "?"
		rx-whitespace))
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (outline-minor-mode 1)
  (make-local-variable 'outline-level)
  (setq outline-level (function stx-outline-level))
  (stx-set-vars)
  (if stx-fontlock
      (progn
       (require 'font-latex)
       (font-latex-setup))
    (if stx-hilit
	(require 'hilit-swiftex))))

;;;;;;        DEFINE KEYMAP
(defvar swiftex-dtx-mode-map (make-sparse-keymap)
  "Keymap for docTeX mode commands in swifTeX mode.")

;; avoid message while byte-compiling
(defvar swiftex-mode-map)
(define-key swiftex-mode-map swiftex-dtx-prefix-key swiftex-dtx-mode-map)
(define-key swiftex-dtx-mode-map "g" 'dtx-insert-change)
(define-key swiftex-dtx-mode-map "l" 'add-change-log-entry)
(define-key swiftex-dtx-mode-map "v" 'dtx-get-fileinfo)
(define-key swiftex-dtx-mode-map "u" 'dtx-update-minor-version)
(define-key swiftex-dtx-mode-map "U" 'dtx-update-major-version)
(define-key swiftex-dtx-mode-map "D" 'dtx-update-documentation-date)
(define-key swiftex-mode-map     "\t"       'stx-tab)
(define-key swiftex-mode-map     "\C-c;"    'comment-region)
(define-key swiftex-mode-map     "\M-\t"    'stx-next-braces)
(define-key swiftex-mode-map     "\C-c\t"   'stx-next-braces)
(define-key swiftex-mode-map     "\C-c\C-]" 'stx-up-block)
(define-key swiftex-mode-map     "\C-]\C-]" 'up-list)
(define-key swiftex-mode-map     "\C-cl"    'tex-bibtex-file)
(define-key swiftex-mode-map     "\C-cp"    'stx-pdflatex-file)
(define-key swiftex-mode-map     "\C-cg"    'stx-pdfview-file)
(define-key swiftex-mode-map     "\""       'stx-insert-quote)
(define-key swiftex-mode-map     "\C-cv"    'stx-verify-blocks)
(define-key swiftex-mode-map     "\C-c\M-u" 'stx-goto-first-unended-begin)
(define-key swiftex-mode-map     "\C-c\M-n" 'stx-goto-last-unbegun-end)
(define-key swiftex-mode-map     "\C-c\C-u" 'stx-goto-prev-unended-begin)
(define-key swiftex-mode-map     "\C-c\C-n" 'stx-goto-next-unbegun-end)
(define-key swiftex-mode-map     "\C-c\C-e" 'stx-close-block)
(define-key swiftex-mode-map     "\C-c\C-m" 'stx-emphasize-word)

;;;;;;        FUNCTIONS AND VARIABLES TAKEN FROM AUC TEX
;;; The following are taken from AUC TeX.
;;;
;;; I have mirrored these variables with my own variables.  This way, if a
;;; user's .emacs sets things up for AUC TeX, this mode will do the right
;;; thing.  And a properly-named version exists, for someone who is looking for
;;; things by completion.  I have changed documentation strings conform to
;;; proper convention.

(defvar TeX-open-quote "``"
  "String inserted by typing \\[stx-insert-quote] to open a quotation.")
(defvar rx-tex-open-quote TeX-open-quote
  "String inserted by typing \\[stx-insert-quote] to open a quotation.")
(defvar TeX-close-quote "''"
  "String inserted by typing \\[stx-insert-quote] to close a quotation.")
(defvar rx-tex-close-quote TeX-close-quote
  "String inserted by typing \\[stx-insert-quote] to close a quotation.")
(defvar TeX-quote-after-quote nil
  "See documentation for `stx-insert-quote'.")
(defvar stx-quote-after-quote TeX-quote-after-quote
  "See documentation for `stx-insert-quote'.")

(defun stx-insert-quote (arg)
  "Insert the appropriate quote marks for TeX.
Inserts the value of `rx-tex-open-quote' (normally ``) or
`rx-tex-close-quote' (normally '') depending on the
context.

`stx-quote-after-quote' non-nil means this insertion works
only after \".  A prefix argument ARG means always insert \"
characters."
  (interactive "*P")
  (if arg
      (self-insert-command (prefix-numeric-value arg))
    (if stx-quote-after-quote
	(insert (cond ((bobp)
		       ?\")
		      ((not (= (preceding-char) ?\"))
		       ?\")
		      ((save-excursion
			 (forward-char -1)
			 (bobp))
		       (delete-backward-char 1)
		       rx-tex-open-quote)
		      ((save-excursion
			 (forward-char -2) ;;; at -1 there is double quote
			 (looking-at "[ \t\n]\\|\\s("))
		       (delete-backward-char 1)
		       rx-tex-open-quote)
		      (t
		       (delete-backward-char 1)
		       rx-tex-close-quote)))
      (insert (cond ((bobp)
		     rx-tex-open-quote)
		    ((= (preceding-char) (string-to-char stx-str-backslash))
		     ?\")
		    ((= (preceding-char) ?\")
		     ?\")
		    ((save-excursion
		       (forward-char (- (length rx-tex-open-quote)))
		       (looking-at (regexp-quote rx-tex-open-quote)))
		     (delete-backward-char (length rx-tex-open-quote))
		     ?\")
		    ((save-excursion
		       (forward-char (- (length rx-tex-close-quote)))
		       (looking-at (regexp-quote rx-tex-close-quote)))
		     (delete-backward-char (length rx-tex-close-quote))
		     ?\")
		    ((save-excursion
		       (forward-char -1)
		       (looking-at "[ \t\n]\\|\\s("))
		     rx-tex-open-quote)
		    (t
		     rx-tex-close-quote))))))

;;;;;;        INTERACTIVE FUNCTIONS

;;; If you have tabs in a LaTeX buffer, you will be in for some surprises when
;;; you use a verbatim environment.  We define a version of `tab-to-tab-stop'
;;; that only will insert spaces.  I can't imagine that the savings of
;;; file-size is at all significant using tabs instead of spaces, but you can
;;; always debind this from tab if you want usual Emacs tabbing.  I strongly
;;; recommend you keep the new space-tabbing for `doctex-mode' because every
;;; macrocode environment is a verbatim environment.

;; This function's code is based on the standard `tab-to-tab-stop' in
;; the standard "indent.el".
(defun stx-tab ()
  "Insert spaces (no tabs) to next defined tab-stop column.
The variable `tab-stop-list' is a list of columns at which
there are tab stops.  Use \\[edit-tab-stops] to edit them
interactively."
  (interactive "*")
  (if abbrev-mode (expand-abbrev))
  (let ((tabs tab-stop-list))
    (while (and tabs (>= (current-column) (car tabs)))
      (setq tabs (cdr tabs)))
    (if tabs
	(insert (make-string (- (car tabs) (current-column)) ?\ ))
      (insert ?\ ))))
(defun stx-up-block ()
  "Move point past the next \"\\end{name}\".
Let * = point before calling this function,
** = point afterward, and \\n = a new line:

* [possible text here]
  [more possible text]
\\end{name}
\\n
**"
  (interactive)
  (goto-char (re-search-forward (concat rx-backslash "end" rx-brace-pair)))
  (forward-line 1)
  (newline))
(defun stx-next-braces ()
  "Move point out of enclosing braces and just into the next set.
That is, call `up-list' and then go to position of next
\"{\" plus one."
  (interactive)
  (up-list 1)
  (goto-char (search-forward rx-tex-grop)))

;;; Ugh. FIX.  Horribly inefficient conditional on 'doctex-mode.
(defun stx-next-beginorend ()
  "Leave point at next \\begin or \\end in swifTeX or docTeX mode buffer.
Signal error if not found."
  (re-search-forward
   (concat "\\(" rx-normal-boln rx-or rx-hidden-boln "\\)"
	   (if (equal major-mode 'doctex-mode) "%+" "")
	   rx-whitespace rx-backslash "\\(begin" rx-or "end\\)"
	   rx-whitespace rx-tex-grop)))

;;; FIX where is point left?  what does match-data have?
;;; FIX: why is this a defsubst and not the above?
;;; Ugh. FIX.  Horribly inefficient conditional on 'doctex-mode.
(defsubst stx-prev-beginorend ()
  "Leave point at previous \\begin or \\end in swifTeX or docTeX mode buffer.
Signal error if not found."
  (re-search-backward
   (concat "\\(" rx-normal-boln rx-or rx-hidden-boln "\\)"
	   (if (equal major-mode 'doctex-mode) "%+" "")
	   rx-whitespace rx-backslash "\\(begin" rx-or "end\\)"
	   rx-whitespace rx-tex-grop)))

;;; Ugh. FIX.  Horribly inefficient conditional on 'doctex-mode.
(defun stx-next-is-end ()
  "Return beginning if true, nil if false, or error if no prev."
  (and (stx-next-beginorend)
       ;; FIX can't i goto-char match-beginning or something?
       (stx-prev-beginorend)
       (let ((p (match-beginning 0)))
	 (if (looking-at
	      (concat "\\(" rx-normal-boln rx-or rx-hidden-boln "\\)"
		      (if (equal major-mode 'doctex-mode)
			  "%+" "")
		      rx-whitespace rx-backslash "end"
		      rx-whitespace rx-tex-grop))
	     p))))

;;; Ugh. FIX.  Horribly inefficient conditional on 'doctex-mode.
(defun stx-prev-is-begin ()
  "Return beginning if true, nil if false, or error if no prev."
  (if (stx-prev-beginorend)
      (let ((p (match-beginning 0)))
	(if (looking-at
	     (concat "\\(" rx-normal-boln rx-or rx-hidden-boln "\\)"
		     (if (equal major-mode 'doctex-mode)
			 "%+" "")
		     rx-whitespace rx-backslash
		     "begin"
		     rx-whitespace rx-tex-grop))
	    p))))
(defun stx-goto-next-unbegun-end ()
  "Move point to the next unbegun \\end after point.
Signal error if there isn't one.
Leave mark at original point if there is."
  (interactive)
  (stx-unbegun-end-internal)
  (message "Point moved to next unbegun \\end.  Mark is at previous point."))
(defun stx-goto-last-unbegun-end ()
  "Move point to the last unbegun \\end in the buffer.
Signal error if there isn't one.
Leave mark at original point if there is."
  (interactive)
  (stx-unbegun-end-internal 'last)
  (message "Point moved to first unbegun end.  Mark is at previous point."))
(defun stx-unbegun-end-internal (&optional last)
  "Internal function to find an unbegun end.
LAST non-nil means start at `(point-min)'."
  (interactive)
  (let (case-fold-search
	(b 0)
	(e 0)
	result)
    (save-excursion
      (if last (goto-char (point-min)))
      (condition-case nil
	  (setq result (stx-fue-recursor))
	(error (if last
		   (error "There is no unbegun \\end")
		 (error "No unbegun \\end after point")))))
    (if (car result)
	(error "Something's wrong with stx-goto-first-unbegun end")
      (push-mark)
      (if (interactive-p)
	  (message "Point moved to first unbegun end.  Mark is at previous point."))
      (goto-char (cadr result)))))

;; avoid message while byte-compiling
(defvar b)
(defvar e)
(defun stx-fue-recursor ()
  "Internal recursive function to find unbegun end."
  (let ((next-is-end (stx-next-is-end)))
    (if next-is-end
	(setq e (1+ e))
      (setq b (1+ b)))
    (if (> e b)
	(list nil next-is-end)
      (forward-line)
      (stx-fue-recursor))))
(defun stx-goto-first-unended-begin ()
  "Move point to the first unended \\begin in the buffer.
Signal error if there isn't one.
Leave mark at original point if there is."
  (interactive)
  (stx-unended-begin-internal 'first)
  (message "Point moved to first unended \\begin.  Mark is at previous point."))
(defun stx-goto-prev-unended-begin (&optional silent)
  "Move point to the first unended \\begin before point.
Signal error if there isn't one.
Leave mark at original point if there is.

Optional argument SILENT non-nil means don't print a message."
  (interactive)
  (stx-unended-begin-internal)
  (if (not silent)
      (message "Point moved to previous unended \\begin.  Mark is at previous point.")))
(defun stx-unended-begin-internal (&optional first)
  "Internal function to find unended begin.
FIRST non-nil means start at \(point-max\)."
  (let (case-fold-search
	(b 0)
	(e 0)
	result)
    (save-excursion
      (if first (goto-char (point-max)))
      (condition-case nil
	  (setq result (stx-fub-recursor))
	(error (if first
		   (error "There is no unended \\begin")
		 (error "No unended \\begin before point")))))
    (if (car result)
	(error "Something's wrong with stx-goto-last-unended begin")
      (push-mark)
      (goto-char (cadr result)))))
(defun stx-fub-recursor ()
  "Internal recursive function to find unended begin."
  (let ((prev-is-begin (stx-prev-is-begin)))
    (if prev-is-begin
	(setq b (1+ b))
      (setq e (1+ e)))
    (if (> b e)
	(list nil prev-is-begin)
      (stx-fub-recursor))))

;; This is weird.  Two errors mean everything's OK.
(defun stx-verify-blocks ()
  "Move point to a mismatched \\begin .. \\end block.
Leave point alone if there are none.  Leave mark at old
point if we move point."
  (interactive)
  (condition-case nil
      (stx-goto-first-unended-begin)
    (error (condition-case nil
	       (stx-goto-last-unbegun-end)
	     (error (message "Your \\begin .. \\end blocks are OK."))))))
(defun stx-close-block ()
  "Create an \"\\end{...}\" to match the last unclosed \"\\begin{...}\".
The \"\\begin{...}\" must follow `stx-bwb', which is
duplicated in front of the \"\\end{...}\".

Return indentation string."
  (interactive "*")
  (let ((bolp (bolp))
	case-fold-search
	text
	indentation)
    (save-excursion
      (condition-case nil
          (stx-goto-prev-unended-begin 'silent)
        (error (error "Couldn't find unended \\begin above!  (stx-close-block)")))
      (setq indentation (buffer-substring
			 (save-excursion
			   (re-search-forward (concat (stx-boln) rx-whitespace)))
			 (point)))
      (re-search-forward (concat rx-backslash "begin"
				 "\\(" rx-whitespace rx-brace-pair
				 "\\)"))
      (setq text (buffer-substring (match-beginning 1) (match-end 1))))
    (if (not bolp)
      (let ((bol (save-excursion (beginning-of-line) (point))))
	(if (save-excursion
		   (re-search-backward
		    (concat (stx-boln) rx-whitespace
			    rx-backslash "end"
			    rx-whitespace rx-brace-pair) bol t))
	    ;; if there is an \\end before point on the same line
	    (insert ?\n)
	  ;; if not
	  (beginning-of-line)
	  (setq bolp t))))
    (insert indentation "\\end" text)
    (if bolp
	(insert ?\n)
      (forward-char))
    ;; return value
    indentation))
(defun stx-close-block-from-inside ()
  "Close LaTeX block after typing the argument to \"\\begin\".
Let * be point.
Initial buffer contents:

 \\begin{foo*}

Final buffer contents:

    \\begin{foo}
    *
    \\end{foo}"
  (interactive "*")
  (forward-line 1)
  (let ((indent (stx-close-block)))
    (forward-line  -1)
    (open-line 1)
    (insert indent)))
(defun stx-command (command &optional arg)
  "Insert a one-argument LaTeX command \"\\COMMAND{}\".
ARG non-nil means place ARG between braces following COMMAND
and leave point (*) after both:

\\COMMAND{ARG} *

If ARG is nil, leave point between the braces:
  \\COMMAND{*}"
  (interactive "*")
  (insert "\\" command "{")
  (if arg
      (insert arg "} ")
    (insert "}")
    (backward-char)))

;; This code is based on `current-word' in the standard 19.28 simple.el.
;;
;; It seemed wisest to imitate its "strict" behavior.
;;
;; We explicitly add the original point to undo list, because we have to
;; explicitly move point during the normal execution of the function to keep it
;; on the same text on which it was called.
;;
;; FIX: consider `insert-before-markers'
(defun stx-enclose-word (before after)
  "Insert string BEFORE before word at point and string AFTER after it.
Keep point over the same text as when the function is called."
  (let* ((oldpoint (point))
	 (start oldpoint)
	 (end oldpoint))
    ;; Since the following movement commands do not alter
    ;; `buffer-undo-list', we add point manually.  We could do this
    ;; any time before the first command that modified it, which is
    ;; the `insert' below.
    (setq buffer-undo-list (cons oldpoint buffer-undo-list))
    (skip-syntax-backward "w")
    (setq start (point))
    (goto-char oldpoint)
    (skip-syntax-forward "w")
    (setq end (point))
    (if (and (eq start oldpoint)
	     (eq end oldpoint))
	(error "Point is neither in nor adjacent to a word!")
      ;; because the head of `buffer-undo-list' is non-nil, `insert'
      ;; is going to add a boundary to it before adding its element.
      (insert after)
      ;; We remove the boundary now.
      (setcdr buffer-undo-list (cdr (cdr buffer-undo-list)))
      (goto-char start)
      (insert before)
      ;; We remove the boundary as before.
      (setcdr buffer-undo-list (cdr (cdr buffer-undo-list)))
      (goto-char (+ oldpoint (length before))))))
(defun stx-emphasize-word ()
  "Surround word at point with \"\\emph{\" and \"}\"."
  (interactive "*")
  (stx-enclose-word "\\emph{" "}"))
(defun stx-emphasize ()
  "Call `(stx-command \"emph\")'."
  (interactive "*")
  (stx-command "emph"))
(defun stx-begin-block ()
  "Call `(stx-command \"begin\")'."
  (interactive "*")
  (stx-command "begin"))
(defalias 'stx-insert-block 'tex-latex-block)
(defun stx-block-comment ()
  "Call `(stx-insert-block \"comment\")'."
  (interactive "*")
  (stx-insert-block "comment"))
(defun stx-block-quotation ()
  "Call `(stx-insert-block \"quotation\")'."
  (interactive "*")
  (stx-insert-block "quotation"))
;; avoid message while byte-compiling
(eval-when-compile (require 'tex-mode))
(defun stx-pdflatex-file ()
  "Prompt to save all buffers and run PDFLaTeX on current buffer's file.
This function is a wrapper for `tex-file'."
  (interactive)
  (let ((tex-command "pdflatex"))
    (tex-file)))

;; avoid message while byte-compiling
(defvar tex-print-file)
(defvar tex-last-buffer-texed)
(defun stx-pdfview-file ()
  "Preview with gv the last `.pdf' file made by running PDFLaTeX under Emacs.
This means, made using \\[tex-region], \\[tex-buffer] or
\\[tex-file]."
  (interactive)
  (let ((print-file-name-pdf (tex-append tex-print-file ".pdf"))
	test-name)
    (if (and (not (equal (current-buffer) tex-last-buffer-texed))
	     (buffer-file-name)
	     ;; Check that this buffer's printed file is up to date.
	     (file-newer-than-file-p
	      (setq test-name (tex-append (buffer-file-name) ".pdf"))
	      (buffer-file-name)))
	(setq print-file-name-pdf test-name))
    (if (not (file-exists-p print-file-name-pdf))
        (error "No appropriate `.pdf' file could be found")
      (if (tex-shell-running)
          (tex-kill-job)
        (tex-start-shell))
      (tex-send-command "gv" print-file-name-pdf t))))


;;;;; DOCTEX MODE

;;; The code forms below are unique to docTeX mode, but rely on many of the
;;; forms in the swifTeX mode section above.

;;;;;;        USER VARIABLES

;;; FIX :link?
(defcustom dtx-sectioning-offset 1
  "Outline level offset for all LaTeX sectioning commands in a docTeX buffer.
See the documentation for the variable
`stx-sectioning-commands'."
  :tag "Sectioning Commands Outline Offset"
  :type 'integer
  :group 'doctex)

;;; FIX :link?
(defcustom dtx-sectioning-commands '(
				  ("documentclass" . -2)
				  ;; leave off final
				  ;; closebrace, since the
				  ;; \> in the regexp needs
				  ;; to match the end of the
				  ;; strings here.
				  ("begin{document" . -2)
				  ("end{document" . -2)
				  ("endinput" . -2)
				  ("StopEventually" . -2)
				  ("Finale" . -2)
				  ("part" . -2)
				  ("appendix" . -1)
				  ("chapter" . -1)
				  ("section".  0)
				  ("subsection" . 1)
				  ("subsubsection" . 2)
				  ("paragraph" . 3)
				  ("subparagraph" . 4)
				  )
  "Equivalent of `stx-sectioning-commands' for docTeX buffers.
See documentation for that variable."
  :tag "Sectioning Commands and Levels"
  :type '(repeat (cons :format "%v"
		       (string :tag "Command")
		       (integer :tag "Level")))
  :group 'doctex)

;;; FIX prefix key type
;;; FIX :set?
(defcustom doctex-dtx-prefix-key '"\C-c\C-d"
  "Prefix key for all commands unique to docTeX mode.
A change in this variable takes effect the next time `doctex-mode' is
called."
  :tag "Prefix key for all docTeX Commands"
  :type 'string
  :group 'doctex)
  
(defcustom rx-tex-def-cmd
  (concat "\\("
;;; FIX Frankenstein-specific stuff here; move to
;;; FIX should have file-variable for new defining commands
	  (regexp-opt '("def" "long\\\\def"
			 "gdef" "global\\\\def"
			 "edef" "long\\\\edef"
			 "xdef" "global\\\\edef"
			 "ReserveCS" "ReserveCS\\*"
			 "InitCS" "InitCS\\*"
			 "newcommand" "newcommand\\*"
			 "renewcommand" "renewcommand\\*"
			 "defcommand" "defcommand\\*"
			 "requirecommand" "requirecommand\\*"
			 "CheckCommand" "CheckCommand\\*"
			 "CheckName" "CheckName\\*"
			 "NewUserInfo" "NewUserInfo\\*"
			 "CheckName" "CheckName\\*"
			 "RequireNewName" "RequireNewName\\*"
			 "DeclareRobustCommand"
			 "DeclareRobustCommand\\*"
			 "NewNameDef" "NewNameDef\\*"
			 "let" "global\\\\let"
			 "EElet" "global\\\\EElet"
			 "newlet" "global\\\\newlet"))
	  "\\)")
  "Matches a (La)TeX defining command without its argument."
  :tag "Defining Command Regexp"
  :type 'regexp
  :group 'doctex)
(defcustom rx-dtx-version "\\(v?\\)\\([0-9.]*\\)\\([a-z]?\\)"
  "Matches a LaTeX version string."
  :tag "LaTeX Version Regexp"
  :type 'regexp
  :group 'doctex)
(defcustom rx-dtx-date "[0-9]+/[0-9]+/[0-9]+"
  "Matches a LaTeX date string."
  :tag "LaTeX Date Regexp"
  :type 'regexp
  :group 'doctex)

;;;;;;        FILE VARIABLES
(defvar dtx-bibstyle-mode nil
  "Non-nil means assume the buffer contains a bibstyle source.

FIX: this should be detected automatically.  But for now, set this
in the file variables.  Right now this only has an effect on the functions
which update the file version and date and documentation
date.")

;;;;;;        HOOK VARIABLES
;;; FIX see what define-derived-mode gives for its dox
;;; (defvar doctex-mode-hook nil
;;;   "Hook run as the last thing after entering docTeX mode.
;;; When entering docTeX mode, the last thing done is to run `swiftex-mode-hook'
;;; then this hook.")

;;;;;;        INTERNAL VARIABLES

(defvar dtx-file-version nil
  "The version string of the file in the current buffer.")
(make-variable-buffer-local 'dtx-file-version)

(defvar dtx-prefix-version nil
  "The leading part of the version string up until the major version.

This is the optional \"v\" plus any following elements that
will still leave two elements to the right, which are the
major and minor versions.")
(make-variable-buffer-local 'dtx-prefix-version)

(defvar dtx-major-version nil
  "The major version part of the version string.")
(make-variable-buffer-local 'dtx-major-version)

(defvar dtx-minor-version nil
  "The minor version part of the version string.")
(make-variable-buffer-local 'dtx-minor-version)

;;;;;;        INLINE FUNCTION-VARIABLES

;;; Without these abbreviations, other code would be very hard to read.

;;; The version control commands are going to be behind double %%'s, this is
;;; just a fact of how bibstyle sources have to get generated.  You can't have
;;; the regular \TeX{} commands in the bibstyle.
(defsubst dtx-bwcb ()
  "Return a regexp boln + w-space + \"%%\" + w-space + b-slash.
Do so even if hidden in an outline."
  (concat (stx-boln) rx-whitespace "%%" rx-whitespace rx-backslash))
(defsubst dtx-tex-defn (name)
  "Return regexp string matching a line that defines NAME.
The match begins at the beginning of the line, and goes
through the \"{\" that begins the definition.

The definitions searched for are expected to be in a doctex
file, but not in the documentation, in a macrocode
environment.

The string returned will depend on the value of
`dtx-bibstyle-mode'."
  (concat (if dtx-bibstyle-mode (dtx-bwcb) (stx-bwb))
	  rx-tex-def-cmd
	  rx-tex-grop "?"
	  rx-backslash
	  name
	  rx-tex-grcl "?"
	  rx-whitespace
	  rx-tex-grop))

;;;;;;        DEFINE MODE
;; avoid message while byte-compiling
(defvar doctex-mode-syntax-table)
(define-derived-mode doctex-mode swiftex-mode "docTeX"
  "Major mode for LaTeX doc documents derived from swifTeX mode.

DocTeX mode preserves the filling and outlining behavior of
swifTeX mode behind the wall of of \"%\" characters in the
first column of a LaTeX doc.sty buffer.

You won't want to fill inside a \"macrocode\" environment.

The TAB key seems to behave as usual but inserts spaces only
\(that is, no `\\t' characters).  It is confusing when `\\t'
characters occur inside verbatim environments \(including
macrocode environments) because the typeset appearance will
not match the appearance of the source file, and
code-indenting conventions will often get mangled.

Outline headings all begin with a \"%\" in the first column.
LaTeX sectioning commands are outline headings, and so is
\"^^A\" followed by at least one star \(\"*\").  (The ltxdoc
document class defines the \"^^A\" character as a LaTeX
comment.)  See `stx-set-outlinevars'.

The following commands maintain the version information in
the LaTeX buffer.  They act on lines that define
\\fileversion, \\filedate, or \\docdate, or the equivalent
components of the \\ProvidesPackage or \\ProvidesClass
declarations if the first are absent.

\\[dtx-get-fileinfo]	displays the buffer's version in the minibuffer.
\\[dtx-update-minor-version]	updates the minor version.
\\[dtx-update-major-version]	updates the major version.

The last two also update the file date to the current date.

\\[dtx-update-documentation-date]	makes the documentation date current.
\\[dtx-insert-change]	inserts a LaTeX \\changes entry.
\\[add-change-log-entry]	adds a ChangeLog entry.

A number of commands save typing.  `insert' commands (on
uppercase keys) begin and end the relevant environment and
leave point in between\; `begin' commands \(on lowercase
keys) just begin the environment.  All prompt for the name
of the thing in question.

\\[dtx-interrupt-macrocode]	interrupts a \"macrocode\" environment for more documentation.

Some convenient ways to customize behavior in individual
files are provided by the `stx-local-' variables.

Some function and variable names begin with `stx-' and some
with `dtx-'.  The latter are unique to docTeX mode\; the
former are in common with swifTeX mode.  The difference is
unimportant if you just want to use docTeX mode.

\\{doctex-mode-map}"
  ;; Change the syntax of % to punctuation.  It's not a
  ;; comment in docTeX mode, and leaving it as a comment
  ;; char causes problems.  I'm not sure what syntax class
  ;; it should have, however.
  (modify-syntax-entry ?% "." doctex-mode-syntax-table)
  ;; Bug in Emacs:  you have to declare this a buffer-local
  ;; hook. FIX: version?
  (make-local-hook 'write-contents-hooks)
  (add-hook 'write-contents-hooks 'dtx-update-checksum nil t)
  (setq stx-comment-start "\\^\\^A")
  (setq stx-line-start "%*")
  ;; FIX: merge?
  ;; FIX: should have customizable dtx-extra-command-parstarts
  (setq stx-command-parstarts
	(append stx-command-parstarts '("DescribeMacro" "DescribeEnv")))
  (setq stx-command-parseps
	(append stx-command-parseps '("DescribeMacro" "DescribeEnv")))
  (setq outline-level (function dtx-outline-level))
  (stx-set-vars)
  ;; FIX I think this is not so great, these variables aren't independent from
  ;; stx-fontlock
  (if dtx-fontlock
      (progn
       (require 'font-latex)
       (font-latex-setup))
    (if dtx-hilit
	(require 'hilit-swiftex))))

;;;;;;        DEFINE KEYMAP

(defvar doctex-dtx-mode-map (make-sparse-keymap)
  "Keymap for all new commands in docTeX mode.")
;; avoid message while byte-compiling
(defvar doctex-mode-map)
(define-key doctex-mode-map doctex-dtx-prefix-key doctex-dtx-mode-map)
(define-key doctex-dtx-mode-map "g" 'dtx-insert-change)
(define-key doctex-dtx-mode-map "l" 'add-change-log-entry)
(define-key doctex-dtx-mode-map "e" 'dtx-begin-environment)
(define-key doctex-dtx-mode-map "E" 'dtx-insert-environment)
(define-key doctex-dtx-mode-map "f" 'dtx-begin-bibfunction)
(define-key doctex-dtx-mode-map "F" 'dtx-insert-bibfunction)
(define-key doctex-dtx-mode-map "m" 'dtx-begin-macro)
(define-key doctex-dtx-mode-map "M" 'dtx-insert-macro)
(define-key doctex-dtx-mode-map "i" 'dtx-interrupt-macrocode)
(define-key doctex-dtx-mode-map "v" 'dtx-get-fileinfo)
(define-key doctex-dtx-mode-map "s" 'dtx-update-checksum)
(define-key doctex-dtx-mode-map "c" 'dtx-begin-macrocode)
(define-key doctex-dtx-mode-map "C" 'dtx-insert-macrocode)
(define-key doctex-dtx-mode-map "u" 'dtx-update-minor-version)
(define-key doctex-dtx-mode-map "U" 'dtx-update-major-version)
(define-key doctex-dtx-mode-map "d" 'dtx-update-documentation-date)

;;;;;;        FUNCTIONS TO SAVE TYPING

(defun dtx-begin-macrocode ()
  "Begin a \"macrocode\" environment."
  (interactive "*")
  (beginning-of-line)
  (insert "%    \\begin{macrocode}"))
(defun dtx-insert-macro ()
  "Insert a \"macro\" environment and matching \"macrocode\" environment."
  (interactive "*")
  (end-of-line)
  (insert-string "
% \\begin{macro}
%
%    \\begin{macrocode}
%    \\end{macrocode}
% \\end{macro}
%")
  (forward-line -5)
  (end-of-line)
  (insert rx-tex-grop (read-string "Macro: " stx-str-backslash) rx-tex-grcl)
  (forward-line 1)
  (end-of-line))
(defun dtx-begin-macro ()
  "Begin a \"macro\" environment."
  (interactive "*")
  (beginning-of-line)
  (insert-string "% \\begin{macro}
")
  (forward-line -1)
  (end-of-line)
  (insert rx-tex-grop (read-string "Macro: " stx-str-backslash) rx-tex-grcl)
  (forward-line 1)
  (end-of-line))
(defun dtx-insert-environment ()
  "Insert an \"environment\" environment."
  (interactive "*")
  (end-of-line)
  (insert-string "
% \\begin{environment}
%
%    \\begin{macrocode}
%    \\end{macrocode}
% \\end{environment}
%")
  (forward-line -5)
  (end-of-line)
  (insert rx-tex-grop (read-string "Environment: ") rx-tex-grcl)
  (forward-line 1)
  (end-of-line))
(defun dtx-begin-environment ()
  "Begin an \"environment\" environment."
  (interactive "*")
  (beginning-of-line)
  (insert-string "% \\begin{environment}
")
  (forward-line -1)
  (end-of-line)
  (insert rx-tex-grop (read-string "Environment: ") rx-tex-grcl)
  (forward-line 1)
  (end-of-line))
(defun dtx-insert-bibfunction ()
  "Insert a \"bibfunction\" environment."
  (interactive "*")
  (end-of-line)
  (insert-string "
% \\begin{bibfunction}
%
%    \\begin{macrocode}
%    \\end{macrocode}
% \\end{bibfunction}
%")
  (forward-line -5)
  (end-of-line)
  (insert rx-tex-grop (read-string "FUNCTION: ") rx-tex-grcl)
  (forward-line 1)
  (end-of-line))
(defun dtx-begin-bibfunction ()
  "Begin a \"bibfunction\" environment."
  (interactive "*")
  (beginning-of-line)
  (insert-string "%  \\begin{bibfunction}
")
  (forward-line -1)
  (end-of-line)
  (insert rx-tex-grop (read-string "FUNCTION: ") rx-tex-grcl)
  (forward-line 1)
  (end-of-line))
(defun dtx-insert-change ()
  "Insert a LaTeX change log entry with automatic date."
  (interactive "*")
  (if (null dtx-file-version)
      (dtx-get-fileinfo))
  (end-of-line)
  (insert-string "
% \\changes")
  (insert rx-tex-grop dtx-file-version rx-tex-grcl)
  (insert rx-tex-grop (dtx-current-date) rx-tex-grcl)
  (insert rx-tex-grop (read-string "Change: ") rx-tex-grcl))
(defun dtx-insert-macrocode ()
  "Insert a \"macrocode\" environment."
  (interactive "*")
  (insert-string "%    \\begin{macrocode}

%    \\end{macrocode}
")
  (forward-line -2))
(defun dtx-interrupt-macrocode ()
  "Interrupt a \"macrocode\" environment for more documentation."
  (interactive "*")
  (backward-char 1)
  (end-of-line)
  (insert-string "
%    \\end{macrocode}
%
%    \\begin{macrocode}")
  (forward-line -1)
  (end-of-line))

;;;;;;        FUNCTIONS TO HANDLE VERSIONS, DATES, CHECKSUMS
(defun dtx-get-fileinfo (&optional noecho)
  "Set `dtx-' v.c. var.s according to lines in the buffer, and echo them.

That is, set `dtx-file-version', `dtx-prefix-version',
`dtx-major-version', and `dtx-minor-version', and echo them
to the minibuffer.

Optional NOECHO non-nil means don't echo information.

To set them, look for a line that defines
\"\\fileversion{<version>}\" (see `dtx-tex-defn').
<version> is any number of integers separated by dots with
an optional prefix \"v\" and an optional suffix of a
lowercase letter.  If no such line exists, then use the line
with \"\\ProvidesPackage\" or \"\\ProvidesClass\".

The major and minor version strings will be the last two
elements of the <version>, counting the possible suffix as a
separate element if it exists."
  (interactive)
  (let (case-fold-search
	fileversion filedate docdate)
    (save-excursion
      (goto-char (point-min))
      (if (or
	   ;; Look for definition of \fileversion.
	   (and (re-search-forward
		 (dtx-tex-defn "fileversion")
		 nil t)
		;; we search twice because we want to remain independent
		;; of how many \\(...\\) pairs are in `dtx-tex-defn'.
		;; FIX: we can not acceptably skip anything here -- what can we
		;; skip?  What if we skip more?
		(re-search-forward
		 (concat rx-whitespace rx-dtx-version
			 rx-whitespace rx-tex-grcl)
		 nil t))
	   ;; If the first search failed, point is still at point-min.
	   ;; Look for an optional argument to \ProvidesFoo.
	   (and (re-search-forward
		 (concat (stx-bwb) "Provides"
			 "\\(Package" rx-or "Class\\)"
			 ;; This used to be: rx-tex-grop "\\w+" rx-tex-grcl
			 ;; which is more restrictive than the rx-brace-pair I
			 ;; used elsewhere.  I see no reason to use the more
			 ;; restrictive regexp.
			 ;; thanks to Peter Møller Neergaard for noticing this
			 rx-whitespace rx-brace-pair)
		 nil t)
		;; FIX: the only thing we can acceptably skip here is
		;; rx-whitespace-newlines -- what if we skip more?
		(re-search-forward
		 (concat "\\["
			 rx-whitespace rx-dtx-date
			 rx-whitespace rx-dtx-version ".*\\]")
		 nil t)))
	  ;; We've found what we need.
	  (let* ((ver-match-data (match-data))
		 (the-list (dtx-massage-version
			    ;; NOTE: byte-compiling turns the arithmetic into constants
			    (buffer-substring (elt ver-match-data (* 2 2))
					      (elt ver-match-data (1+ (* 2 3))))))
		 (prefix (buffer-substring (elt ver-match-data (* 2 1))
					   (elt ver-match-data (1+ (* 2 1)))))
		 (n (length the-list))
		 (k 0))
	    (cond
	     ((= n 0)
	      (setq dtx-major-version ""
		    dtx-minor-version ""))
	     ((= n 1)
	      (setq dtx-major-version (elt the-list 0)
		    dtx-minor-version ""))
	     (t
	      (setq dtx-major-version (elt the-list (- n 2))
		    dtx-minor-version (elt the-list (- n 1)))))
	    (setq dtx-prefix-version prefix)
	    ;; now add onto the prefix whatever is right
	    (while (< k (- n 2))
	      (setq dtx-prefix-version
		    (concat dtx-prefix-version
			    (if (or (string-equal dtx-prefix-version "")
				    (string-equal dtx-prefix-version "v"))
				"" ".")
			    (elt the-list k))
		    k (1+ k)))
	    (setq fileversion (dtx-set-version 'noecho)))
	;; We have not found what we need.
	(error "Can't find file info! (dtx-get-fileinfo)"))
      (if (null noecho)
	  (progn
	    (goto-char (point-min))
	    (if (and (re-search-forward (dtx-tex-defn "docdate") nil t)
		     (re-search-forward rx-dtx-date nil t))
		(setq docdate (match-string 0))
	      (error
	       "Can't find documentation date!  (dtx-get-fileinfo)"))
	    (goto-char (point-min))
	    (if (and (re-search-forward (dtx-tex-defn "filedate") nil t)
		     (re-search-forward rx-dtx-date nil t))
		(setq filedate (match-string 0))
	      (if (and (re-search-forward
			(concat (stx-bwb)
				"Provides\\(Package" rx-or
				"Class\\)"
				rx-whitespace rx-brace-pair
				rx-whitespace-newlines "\\[")
			nil t)
		       (re-search-forward rx-dtx-date nil t))
		  (setq filedate (match-string 0))
		(error "Can't find file date!  (dtx-get-fileinfo)")))
	    (message "Version: %s  Date: %s  Docdate: %s"
		     fileversion filedate docdate))))))
(defun dtx-update-documentation-date ()
  "Update the \"\\docdate\" to the current date and update the checksum."
  (interactive "*")
  ;; FIX why update the checksum?
;;;  (dtx-update-checksum 'noecho)
  (save-excursion
    (goto-char (point-min))
    (let (case-fold-search)
      (if (and (re-search-forward (dtx-tex-defn "docdate") nil t)
	       (re-search-forward rx-dtx-date nil t))
	  (replace-match (dtx-current-date))
	(error
	 "Can't find documentation date!  (dtx-update-documentation-date)")))))
(defun dtx-update-fileinfo ()
  "Update the \fileversion and \filedate.

First assemble `dtx-file-version' by calling
`dtx-set-version'.  Then find and update the version and
date in the line that defines them (see `dtx-tex-defn')
using the current values of `dtx-file-version' and
`(dtx-current-date)', respectively.  If no such defining
line exists, use a \"\\ProvidesPackage\" or
\"\\ProvidesClass\" line."
  (dtx-set-version)
  (let (case-fold-search)
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward (dtx-tex-defn "fileversion") nil t)
	  (progn
	    ;; The argument can contain matching braces.
	    (backward-char)
	    (delete-region (point) (forward-list))
	    (insert rx-tex-grop dtx-file-version rx-tex-grcl))
	;; If first search failed, point is still point-min.
	(if (and
	     (re-search-forward
	      (concat (stx-bwb) "Provides\\(Package" rx-or "Class\\)"
		      rx-whitespace rx-brace-pair
		      rx-whitespace-newlines "\\["
		      rx-whitespace rx-dtx-date rx-whitespace)
	      nil t)
	     (re-search-forward rx-dtx-version nil t))
	    (replace-match dtx-file-version)
	  (error "Can't find file version!  (dtx-update-fileinfo)")))
      ;; The date is only changed if a change above succeeds.
      (goto-char (point-min))
      (if (re-search-forward (dtx-tex-defn "filedate") nil t)
	  (progn
	    (backward-char)
	    (delete-region (point) (forward-list))
	    (insert rx-tex-grop (dtx-current-date) rx-tex-grcl))
	(if (and (re-search-forward
		  (concat (stx-bwb)
			  "Provides\\(Package" rx-or
			  "Class\\)"
			  rx-whitespace rx-brace-pair
			  rx-whitespace-newlines "\\[")
		  nil t)
		 (re-search-forward rx-dtx-date nil t))
	    (replace-match (dtx-current-date))
	  (error "Can't find file date!  (dtx-update-fileinfo)"))))))
(defun dtx-update-minor-version ()
  "Increment the minor version and update the checksum."
  (interactive "*")
  (if (null dtx-file-version)
      (dtx-get-fileinfo))
  (setq dtx-minor-version
	(cond ((string-equal "" dtx-minor-version)
	       (read-string "Minor version: "))
	      ((string-match "[a-z]" dtx-minor-version)
	       (string (1+ (string-to-char dtx-minor-version))))
	      (t
	       (number-to-string (1+ (string-to-number dtx-minor-version))))))
  ;; FIX: why bother to update the checksum?
;;;  (dtx-update-checksum 'noecho)
  (dtx-update-fileinfo))

;;; FIX: check behavior
;;;  1.3b        major            1.4
;;;  1.5         major            2
;;;  2           major            3
(defun dtx-update-major-version ()
  "Increment the major version and update the checksum."
  (interactive "*")
  (if (null dtx-file-version)
      (dtx-get-fileinfo))
  (setq dtx-minor-version ""
	dtx-major-version (number-to-string 
			   (1+ (string-to-number dtx-major-version))))
  ;; FIX: why bother to update the checksum?
;;;  (dtx-update-checksum 'noecho)
  (dtx-update-fileinfo))
(defun dtx-set-version (&optional noecho)
  "Assemble `dtx-file-version' from its constituent parts.

NOECHO non-nil means don't echo the version to the
minibuffer.  Returns `dtx-file-version'."
  (let (case-fold-search)
    (setq dtx-file-version
	  (concat dtx-prefix-version
		  (if (or (string-equal dtx-prefix-version "")
			  (string-equal dtx-prefix-version "v"))
		      "" ".")
		  dtx-major-version
		  (if (not (string-equal dtx-minor-version ""))
		      (concat (if (or (string-equal dtx-major-version "")
				      (string-match "[a-z]" dtx-minor-version))
				  "" ".")
			      dtx-minor-version))))
    (or noecho
	(message "The version of this file is now <%s>"
		 dtx-file-version))
    dtx-file-version))
(defun dtx-massage-version (s)
  "Massage a version string into a list of strings.
Given a string S matching a series of unsigned integers
separated by dots, return a list of those integers as
strings.

If a lowercase letter ends S, it will be tacked on to the
returned list."
  (let (case-fold-search
	(k 0)
	(n (length s))
	element
	number-list
	kth-char-as-string)
    (while (< k n)
      (setq kth-char-as-string (string (elt s k)))
      (cond
       ((string-match "[0-9]" kth-char-as-string)
        (setq element (concat element kth-char-as-string)))
       ((string-match "\\." kth-char-as-string)
        (setq number-list (append number-list (list element))
	      element nil))
       ((string-match "[a-zA-Z]" kth-char-as-string)
	(if (or (not (equal k (1- n)))
		(string-match "[A-Z]" kth-char-as-string))
	    (error (concat "Bad letter \"%s\" in version string \"%s\"!  "
			   "The last character is permitted to be a LOWERCASE letter.  "
			   "(dtx-massage-version)") kth-char-as-string s)
	  (setq number-list (append number-list (list element))
		element kth-char-as-string)))
       (t
        (error "Can't parse version string \"%s\"!  (dtx-massage-version)" s)))
      (setq k (1+ k)))
    (setq number-list (append number-list (list element)))
    (if (equal number-list (list nil))
        (setq number-list nil))
    number-list))

;;; Thanks to Karl Eichwalder <ke@gnu.franken.de> for his improvement of this
;;; function.
(defun dtx-current-date ()
  "Return the current date as a string in the form \"1991/01/23\"."
  (format-time-string "%Y/%m/%d"))
(defun dtx-report-checksum-error (function checksum)
"Report a checksum error to the user.
Use FUNCTION, which takes a string argument, to report the
error CHECKSUM.  Returns nil so it won't halt
`write-contents-hooks'."
  (cond
   ((eq checksum 'checksum)
    (funcall function "Can't find CheckSum declaration!"))
   ((eq checksum 'macrocode)
    (funcall function "Mismatched macrocode environments!"))
   ((eq checksum 'finale)
    (funcall function "Couldn't find Finale!"))
   ((eq checksum 'stop)
    (funcall function "Couldn't find StopEventually or AddToCheckSum!")))
  nil)
(defun dtx-update-checksum (&optional noecho)
  "Update the argument of the \\CheckSum or \\AddToCheckSum command.
If a error occurs while computing the checksum, signal an
error in the interactive case or issue a warning message in
the non-interactive case.

NOECHO non-nil means don't echo the CheckSum to the minibuffer."
  (interactive "*")
  (let ((opoint (point))
	(checksum (dtx-compute-checksum))
	case-fold-search)
    (if (stringp checksum)
	;; The checksum was successfully computed.
	(save-excursion
	  (goto-char (point-min))
	  ;; \CheckSum and \AddToCheckSum should be behind at least one comment.
	  ;; If there is a \CheckSum in the file, there will not be a
	  ;; \AddToCheckSum and vice versa.
	  (if (or
	       ;; We find \CheckSum
	       ;; FIX: what if more than one comment?
	       (and (re-search-forward (concat (stx-boln) "%" rx-whitespace
					       rx-backslash "CheckSum{")
				       ;; Leave point at beginning if we don't find
				       ;; it.
				       nil t)
		    (progn
		      (backward-char)
		      (delete-region (point) (forward-list))
		      t))
	       ;; We find \AddToCheckSum.
	       (and (re-search-forward (concat (stx-boln) "%" rx-whitespace
					       rx-backslash "AddToCheckSum{")
				       nil 'move)
		    (progn
		      (backward-char)
		      (delete-region (point) (forward-list))
		      t))
	       ;; We query the user if we're interactive.
	       (and (if (interactive-p)
			t
		      (dtx-report-checksum-error 'message 'checksum)
		      ;; don't proceed
		      nil)
		    (goto-char opoint)
		    (y-or-n-p
		     "I can't find a `\\CheckSum{}' declaration.  Should I insert one at point? ")
		    (progn
		      (insert "\\CheckSum")
		      t)))
	      (progn
		(insert rx-tex-grop checksum rx-tex-grcl)
		(if (null noecho) (message "Checksum updated to %s." checksum))
		;; Return nil so that we can put this function in
		;; `write-contents-hooks'.
		nil)))
      ;; Checksum was not successfully computed.
      (if (interactive-p)
	  (dtx-report-checksum-error 'error checksum)
	(dtx-report-checksum-error 'message checksum)))))
(defun dtx-compute-checksum ()
"Return the `doctex-mode' checksum or an error code.
The checksum is the number of backslashes in the macrocode
environments between \\StopEventually and \\Finale if those
declarations are found, and all macrocode environments if an
\\AddToCheckSum declaration is found.  The error codes are
`macrocode' for unbalanced macrocode environments, `finale'
for a missing \\Finale, and `stop' for a missing
\\StopEventually.  When a `macrocode' error occurs, the
function leaves mark at point and skips to the unclosed
\\begin.

I think `'move' is a more efficient final argument to `re-search-*'
than t; when it doesn't matter which, I've used `'move'."
  (let* (checksum-error
	 (opoint (point))
	 ;; WARNING: see above notes on comment syntax.
	 (parse-sexp-ignore-comments nil)
	 (leave-me-here opoint)
	 case-fold-search
	 ;; The number of `%' characters must be exact, so we don't use `stx-boln'.
	 (begin-macrocode (concat "\\(" rx-normal-boln rx-or rx-hidden-boln "\\)%    "
				  rx-backslash "begin{macrocode\*?}"))
	 (end-macrocode (concat "\\(" rx-normal-boln rx-or rx-hidden-boln "\\)%    "
				rx-backslash "end{macrocode\*?}"))
	 (stop-eventually (save-excursion
			    (goto-char (point-min))
			    ;; The argument to \StopEventually might contain
			    ;; some backslashes that we don't want to count, so
			    ;; we skip over it with `forward-list'.
			    ;; WARNING: see above notes on comment syntax.
			    (and
			     (re-search-forward (concat (stx-boln) "%"
							rx-whitespace rx-backslash
							"StopEventually")
						nil 'move)
			     (forward-list))))
	 (addtochecksum (save-excursion
			  (goto-char (point-min))
			  (re-search-forward (concat (stx-boln) "%"
						     rx-whitespace rx-backslash
						     "AddToCheckSum{")
					     nil t))))
    (unwind-protect
	(if (or stop-eventually addtochecksum)
	    (save-excursion
	      ;; Goto `stop-eventually' if it's there, else beginning.
	      (goto-char (or stop-eventually (point-min)))
	      (let ((backslashes 0)
		    (codeblocks 0)
		    ;; We stop at either \Finale or the end.
		    (finale (if (not stop-eventually)
				(point-max)
			      (save-excursion
				;; \Finale must be on a line by itself
				(re-search-forward (concat (stx-boln) "%"
							   rx-whitespace rx-backslash
							   "Finale") nil 'move)))))
		(if finale
		    (while (not (or checksum-error
				    (equal (point) finale)))
		      (let ((begin-code (re-search-forward begin-macrocode finale 'move))
			    (end-code (save-excursion
					(re-search-forward end-macrocode finale 'move))))
			(if begin-code
			    (if end-code
				;; we have a well-defined macrocode region
				(progn
				  (setq codeblocks (1+ codeblocks))
				  (while (and (not (equal (point) end-code))
					      (search-forward "\\" end-code 'move))
				    (setq backslashes (1+ backslashes))))
			      ;; if `end-code' is nil but `begin-code' is non-nil we have a
			      ;; problem
			      (setq leave-me-here begin-code)
			      (setq checksum-error 'macrocode))
			  ;; if both `begin-code' and `end-code' are nil, the
			  ;; checksum is zero, but that's not an error.
			  )))
		  ;; if `finale' is nil we have a problem
		  (setq checksum-error 'finale))
		;; The count is too high because it counted the \ in each "\end{macrocode}".
		;; This is the checksum as a string.
		(int-to-string (- backslashes codeblocks))))
	  ;; If `stop-eventually' and `addtochecksum' are nil we have a problem.
	  (setq checksum-error 'stop))
      (if checksum-error
	  (progn
	    (push-mark opoint)
	    (goto-char leave-me-here)
	    checksum-error)
	(goto-char leave-me-here)))))

;;;;;;        API FUNCTIONS

;;; FIX: are set-outlinevars and set-parvars API?
;;; if not, then should give abstract description of what
;;; this func does.
(defun stx-set-vars ()
  "Call `stx-set-outlinevars' with appropriate args, then `stx-set-parvars'."
  (interactive)
  (if (equal major-mode 'doctex-mode)
      (stx-set-outlinevars dtx-sectioning-commands dtx-sectioning-offset t)
    (stx-set-outlinevars stx-sectioning-commands stx-sectioning-offset nil))
  (stx-set-parvars))
(defmacro stx-merge-list (old new &optional warn)
  "Modify a list OLD to include all NEW's elements not in OLD.
Compare elements with `equal'.  New elements are added at the end of OLD.  NEW
and OLD can be both lists or both alists.  NEW or OLD or both can be null.
Duplicate elements in NEW will not be duplicated in OLD.  Returns OLD.  NEW is
not altered.

For alists, add NEW's keys to end of OLD if OLD does not have them.
If OLD does have a matching key, change its value to NEW's value for
that key.  The key is the car of each element, the value is the cdr.

Optional WARN non-nil means print a message whenever the value of one
of OLD's keys is being replaced.

In the case that OLD is nil, this macro evaluates OLD twice, so do not call
`merge-list' with a computed argument that has a side effect that shouldn't be
done twice.  The only alternative I see is to work like `add-to-list' and
require a constant symbol name as an argument, effectively requiring you to
compute things once before you call the function.  NEW and WARN are evaluated
exactly once."
  `(let ((n ,new)
	 (o ,old)
	 (w ,warn))
     (unless o
       ;; Move the car of `n' to `o'.  Then we proceed as if we started in
       ;; this state.  This filters duplicates from `new' as advertised,
       ;; whereas setting `old' to 'new' would not.  If `old' is the *constant*
       ;; nil then we get an error from `set', which is what we want.
       (setq o (set ',old (list (pop n)))))
     (when n
       ;; Assume that if the car of `o' is a list, then `o' is an alist,
       ;; otherwise it is a list.  Same for `n'.
       (let ((old-is-alist (listp (car o)))
	     n-car)
	 (when (not (eq old-is-alist (listp (car n))))
	   (error "Arguments to `merge-list' must be both lists or both alists"))
	 (if old-is-alist
	     ;; `o' and `n' are alists and non-null
	     (while (setq n-car (pop n))
	       (let ((o-cons (assoc (car n-car) o)))
		 (if (null o-cons)
		     (nconc o (list n-car))
		   (when w
		     (message "merge-list: changing value of key %s from %s to %s"
			      (car o-cons) (cdr o-cons) (cdr n-car)))
		   (setcdr o-cons (cdr n-car)))))
	   ;; `old' and `new' are regular lists and non-null
	   (while (setq n-car (pop n))
	     (unless (member n-car o)
	       (nconc o (list n-car)))))))
     o))

;;; FIX: emacs-19 only, right?
;; This function can cause screwups if byte-compiled, and if calling it is part
;; of a hook that gets called by `get-buffer-create', for example if it's part
;; of the hook belonging to your `default-major-mode'.
(defun rx-catenate-or (strlist &rest strings)
  "Catenate strings with `rx-or' between them.

Return catenation with `rx-or' of the elements of STRLIST
and optional STRINGS which follow.  Ignore STRINGS that are
nil and elements of STRLIST that are nil.  Return nil if
passed no strings at all.

Empty strings will get catenated, and should probably not be
passed to this function.

STRINGS and the elements of STRLIST are expected to be
strings or nil, but can be other types.  See the source for
full details.

CAUTION: if you are experiencing strange bugs with this
function, see the warning in \"swiftex.el\"."
  (if (not (listp strlist))
      (error "First arg %S to `rx-catenate-or' must be a list!" strlist))
  (let* ((strlist (mapconcat (function identity) (delq nil strlist) rx-or))
	 (strings (mapconcat (function identity) (delq nil strings) rx-or))
         (islist (not (string-equal "" strlist)))
	 (arestrings (not (string-equal "" strings))))
    (if (and arestrings islist)
	(concat strlist rx-or strings)
      (if arestrings
	  strings
	(if islist
	    strlist)))))

;;;; EMACS FILE VARIABLES
;;;
;;; Local Variables:
;;; mode: checkdoc-minor
;;; fill-column: 60
;;; End:
;;;
;;; swiftex.el ends here
