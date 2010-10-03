;;;; tdtd.el --- Tony's DTD mode
;; $Id: tdtd.el,v 1.41 1999/03/26 22:08:00 tkg Exp $
;; $Name: tdtd071 $

;; Copyright (C) 1996, 1997, 1998, 1999 Tony Graham

;; Author: Tony Graham <tgraham@mulberrytech.com>
;; Contributors: Juanma Barranquero, Adam di Carlo

;;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;;; Commentary:

;; Macros for editing DTDs

;; Requires tdtd-font.el
;; Requires 'etags for `find-tag-default'
;; Requires 'imenu for "Goto" menu
;; Requires 'make-regexp for tdtd-font.el
;; Requires 'reporter for `dtd-submit-bug-report'
;; Send bugs to tdtd-bug@menteith.com


;;;; Code:
(eval-and-compile
  (require 'font-lock))
(eval-and-compile
  (autoload 'sgml-validate "psgml"))
(eval-and-compile
  (autoload 'reporter-submit-bug-report "reporter"))
;; XEmacs users don't always have imenu.el installed, so use
;; condition-case to cope if we cause an error by requiring imenu.
(eval-and-compile
  (condition-case nil
	(require 'imenu)
    (error nil)))

;; We need etags for `find-tag-default'
(require 'etags)
(require 'tdtd-font "tdtd-font")

(provide 'tdtd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Version information

(defconst tdtd-version "0.7.1"
  "Version number of tdtd.")

(defun tdtd-version ()
  "Returns the value of the variable tdtd-version."
  tdtd-version)

(defconst tdtd-maintainer-address "tdtd-bug@menteith.com")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables

(defvar dtd-autodetect-type t
  "*Non-nil enables autodetection of XML or SGML when entering dtd-mode.")

(defvar dtd-xml-flag nil
  "*Non-nil enables XML-specific behaviour, where implemented.")

(defvar dtd-decl-flag nil
  "*Non-nil enables SGML Declaration-specific behaviour, where implemented.")

(defvar dtd-sys-decl-flag nil
  "*Non-nil enables System Declaration-specific behaviour, where implemented."
  )

(defun dtd-xml-p ()
  "Returns t when XML-specific behaviour is enabled, otherwise returns nil."
  (if dtd-xml-flag
      t nil))

(defun dtd-decl-p ()
  "Returns t when SGML Declaration-specific behaviour is enabled, otherwise nil."
  (if dtd-decl-flag
      t nil))

(defun dtd-sys-decl-p ()
  "Returns t when System Declaration-specific behaviour is enabled, otherwise nil."
  (if dtd-sys-decl-flag
      t nil))

(defvar dtd-indent-tabs-mode nil
  "*Initial value of indent-tabs-mode on entering dtd-mode")

(defvar dtd-default-filespec "*.dtd *.ent"
  "*Inital prompt value for `dtd-etags''s FILESPEC argument.")

(defvar dtd-filespec-history (list dtd-default-filespec)
  "Minibuffer history list for `dtd-etags' and `dtd-grep''s FILESPEC argument.")

(defvar dtd-grep-pattern-history nil
  "Minibuffer history list for `dtd-grep''s PATTERN argument.")

(defvar dtd-grep-case-sensitive-flag nil
  "*Non-nil disables case insensitive searches by `dtd-grep'.")

(defvar dtd-grep-command-format
  "grep -n %s -- '%s' %s"
  "*Format string for the grep command called by `dtd-grep'.")

;; Aren't shell escapes fun!
;; "\" -> "\\\\\\"
;; any other significant character -> "\\" + character
(defvar dtd-etags-regex-option
  "--regex=/\\<\\!\\\\\\(ELEMENT\\\\\\|ENTITY\\[\\ \\\\\\t]\\+%\\\\\\|NOTATION\\\\\\|ATTLIST\\\\\\)\\[\\ \\\\\\t]\\+\\\\\\(\\[^\\ \\\\\\t]\\+\\\\\\)/\\\\\\2/"
  "*Complete, including \"--regex=\", etags regular expression option string
for the etags command line for extracting tags (in the Emacs sense)
from DTDs.")
;; Use this regex with 4NT:
;; "--regex=\"/<!\\(ELEMENT\\|ENTITY[ \\t]+%%\\|NOTATION\\|ATTLIST\\)[\\t]+\\([^ \\t]+\\)/\\2/\""

(defvar dtd-etags-program "etags"
  "*Name (and possibly path) of the etags program")

(defvar dtd-etags-output-file "TAGS"
  "*Name of the etags output file")

(defvar dtd-attribute-type-history
  (list "CDATA" "ID" "IDREF" "IDREFS"
	"ENTITY" "ENTITIES" "NMTOKEN" "NMTOKENS" "NOTATION ")
  "Minibuffer history list for attribute types.")

(defvar dtd-attribute-tag-history nil
  "Minibuffer history list for attribute tags.")

(defvar dtd-attribute-default-history
  (list "#IMPLIED" "#REQUIRED")
  "Minibuffer history list for attribute types.")

(defvar dtd-default-element-type-name nil
  "Default for element type names.")

(defvar dtd-declared-element-type-names nil
  "List of element type names recently declared.")

(defvar dtd-referenced-element-type-names nil
  "List of element type names recently entered in content models.")

(defvar dtd-declared-parameter-entity-names nil
  "List of parameter entity names recently declared.")

(defvar dtd-referenced-parameter-entity-names nil
  "List of parameter entity names recently entered in content models.")

(defvar dtd-element-type-name-history nil
  "Minibuffer history list for element type names.")

(defvar dtd-parameter-entity-value-history nil
  "Minibuffer history list for parameter entity values.")

(defvar dtd-external-entity-public-history nil
  "Minibuffer history list for external entity public identifiers.")

(defvar dtd-external-entity-system-history nil
  "Minibuffer history list for external entity system identifiers.")

(defvar dtd-element-comment-history nil
  "Minibuffer history list for element comments.")

(defvar dtd-element-content-spec-history
  (list "(#PCDATA)" "EMPTY" "ANY")
  "Minibuffer history list for element content specifications.")

(defvar dtd-upcase-name-comment-flag nil
  "*Non-nil enables converting descriptive name comments to uppercase.")

(defvar dtd-prompt-descriptive-name t
  "*Non-nil enables prompting for descriptive names of elements, etc.")

(defvar dtd-prompt-descriptive-comment t
  "*Non-nil enables prompting for descriptive comments for elements, etc.")

(defvar dtd-outdent-attribute-pe nil
  "*Non-nil enables outdenting parameter entities used for attributes.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Character sequences

(defvar dtd-sgml-mdo "<!"
  "*Markup Delimiter Open (MDO) character sequence")

(defvar dtd-sgml-mdc ">"
  "*Markup Delimiter Close (MDC) character sequence")

(defvar dtd-comment-start "<!--"
  "*Comment start character sequence")

(defvar dtd-comment-end "-->"
  "*Comment end character sequence")

;; SGML Syntactic Literals
(defvar dtd-empty-literal "EMPTY"
  "EMPTY element syntactic literal")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables controlling indentation

(defvar dtd-dtd-max-column 70
  "*Rightmost column for text in the DTD")

(defvar dtd-mdc-indent-column (1- dtd-dtd-max-column)
  "*Column at which to insert a MDC")

(defvar dtd-comment-start-column 25
  "*Column for starting text in a comment")

(defvar dtd-comment-max-column (- dtd-dtd-max-column 3)
  "*Maximum column number for text in a comment")

;; (defvar dtd-sgml-comment-max-column (- dtd-dtd-max-column 2))

(defvar dtd-element-name-column 12
  "*Column for element name in an element declaration")

(defvar dtd-element-tag-omission-column 25
  "*Column for inserting the omissibility indicators, if used")

(defvar dtd-element-content-spec-start-column 29
  "*Column at which to start content model")

(defvar dtd-element-content-spec-continuation-column 30
  "*Column at which to start second and subsequent lines of content model")

(defvar dtd-xml-element-content-spec-start-column 25
  "*Column at which to start XML content model")

(defvar dtd-xml-element-content-spec-continuation-column
  (1+ dtd-xml-element-content-spec-start-column)
  "*Column at which to start second and subsequent lines of XML content model")

(defvar dtd-entity-entity-value-start-column 24
  "*Column at which to start parameter entity's entity value")

(defvar dtd-entity-entity-value-continuation-column
  (1+ dtd-entity-entity-value-start-column)
  "*Column at which to start second and subsequent lines of parameter entity value")

(defvar dtd-attribute-name-column (+ dtd-element-name-column 2)
  "*Indent for inserting attribute names in attribute definitions")

(defvar dtd-attribute-default-column (- dtd-dtd-max-column 10)
  "*Indent for inserting attribute defaults in attribute definitions")

(defvar dtd-line-comment
  (concat
   dtd-comment-start
   " "
   (make-string
    (- dtd-dtd-max-column
       (length (concat dtd-comment-start "  " dtd-comment-end)))
    ?=)
   " "
   dtd-comment-end
   "\n")
  "*Separator comment line: \"<!-- ==== -->\".")

(defvar dtd-init-comment-column 16)

(defvar dtd-init-comment-fill-prefix
  (concat
   dtd-comment-start
   (make-string (- dtd-init-comment-column (length dtd-comment-start))
		?\ ))
  "*Prefix for comments making up the initial comment in a module.")

(defvar dtd-design-comment-start-column (+ (length dtd-comment-start) 2)
  "*Column for starting text in a \"Design Considerations\" comment")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions

(defun dtd-autodetect-type ()
  "Check for initial declaration and set flags accordingly."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (skip-chars-forward "\\s-")
    (cond
     ((looking-at "<\\?xml")
      (setq dtd-xml-flag t)
      (setq dtd-decl-flag nil)
      (setq dtd-sys-decl-flag nil))
     ((looking-at "<!SGML")
      (setq dtd-xml-flag nil)
      (setq dtd-decl-flag t)
      (setq dtd-sys-decl-flag nil))
     ((looking-at "<!SYSTEM")
      (setq dtd-xml-flag nil)
      (setq dtd-decl-flag nil)
      (setq dtd-sys-decl-flag t))
     (t
      (setq dtd-xml-flag nil)
      (setq dtd-decl-flag nil)
      (setq dtd-sys-decl-flag nil)))))

;; If you want to untabify everything every time, add this to functions
;;  (untabify (point-min) (point-max))
;; Its easier to just not insert tabs using:
;;  (setq indent-tabs-mode nil)
(defun dtd-untabify-buffer ()
  "Untabify the entire buffer"
  (interactive)
  (untabify (point-min) (point-max)))

(defun dtd-fix-entities ()
  "Quick and dirty addition of \";\" to entity references lacking it"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while
	(re-search-forward "\\([%&][-A-Za-z0-9.:_]+\\)\\([^-A-Za-z0-9.:_;]\\)" nil t)
      (replace-match "\\1;\\2" nil nil)
      ;; match 2 may be the start of another entity reference
      (goto-char (match-beginning 2)))))

(defun dtd-indent-or-newline-to (target-column)
  "Indent to TARGET COLUMN or, if at or past target, insert newline and indent."
  (if (> (current-column)
	 target-column)
      (insert "\n"))
  (indent-to target-column))

(defun dtd-center-comment (comment)
  "Center a comment on line"
  (interactive "sComment: ")
  (insert dtd-comment-start)
  (indent-to (/ (- dtd-dtd-max-column (length comment)) 2))
  (insert comment)
  (indent-to dtd-comment-max-column)
  (insert dtd-comment-start)
  (insert "\n"))

(defun dtd-recenter-comment ()
  "Recenter text in a presumably modified comment line"
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      (if (looking-at "^<!--\\s-*\\(\\w+\\(\\W+\\w+\\)*\\)\\s-*-->$")
	  (let ((contents (match-string 1)))
	    (delete-region (match-beginning 0) (match-end 0))
	    (dtd-center-comment contents))))))

(defun dtd-filled-comment (comment)
  "Insert a comment with \"=\" in most of the whitespace"
  (interactive "sComment: ")
  (dtd-comment comment "="))

;; TDTD house style puts all comments starting on a favourite column
(defun dtd-comment (comment &optional fill-character)
  "Insert COMMENT starting at the usual column.

With a prefix argument, e.g. \\[universal-argument] \\[dtd-comment], insert separator comment
lines above and below COMMENT in the manner of `dtd-big-comment'."
  (interactive "sComment: ")
  (if current-prefix-arg
      (insert dtd-line-comment))
  (insert "\n")
  (backward-char)
  (let ((fill-column (1- dtd-comment-max-column))
	(fill-prefix (make-string (1- dtd-comment-start-column) ?\ ))
	(comment-start dtd-init-comment-fill-prefix)
	(saved-auto-fill-function auto-fill-function))
    (auto-fill-mode 1)
    (insert dtd-comment-start)
    (if (and
	 (stringp fill-character)
	 (not (string-equal fill-character "")))
	(progn
	  (insert " ")
	  (insert (make-string (- dtd-comment-start-column
				  (current-column)
				  2)
			       (string-to-char fill-character)))))
    (indent-to (1- dtd-comment-start-column))
    (fill-region (point) (save-excursion
			   (insert comment)
			   (point))
		 nil
		 1
		 1)
    ;; The fill does the right thing, but in Emacs 19.34 it always ends with
    ;; an extra newline, so we delete the newline.
    (if (bolp)
	(delete-backward-char 1))
    (if (not saved-auto-fill-function)
	(auto-fill-mode 0))
    (if (and
	 (stringp fill-character)
	 (not (string-equal fill-character "")))
	(progn
	  (insert " ")
	  (insert (make-string (- dtd-comment-max-column
				  (current-column)
				  1)
			       (string-to-char fill-character)))))
    (indent-to dtd-comment-max-column)
    (insert dtd-comment-end)
    (insert "\n")
    (if current-prefix-arg
	(insert dtd-line-comment))
    (if font-lock-mode
	(save-excursion
	  (font-lock-fontify-region
	   (dtd-font-lock-region-point-min)
	   (dtd-font-lock-region-point-max))))))

(defun dtd-recomment ()
  "Fix text position in a presumably modified comment line"
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      (if (looking-at "^\\(<!--*\\)?\\s-*\\([^ \t\n>]+\\([- \t]+[^- \t\n>]+\\)*\\)\\s-*\\(-->\\)?\n")
	  (let ((contents (match-string 2)))
	    (delete-region (match-beginning 0) (match-end 0))
	    (dtd-comment contents))))))

(defun dtd-join-comments (mark point)
  "Join comments by removing the \"interior\" comment delimiters."
  (interactive "r")
  (save-excursion
    (save-match-data
      (goto-char (min mark point))
      ;; Insert a comment start if there isn't one at the beginning.
      (if (not (looking-at dtd-comment-start))
	  (progn
	    (if (looking-at (make-string (length dtd-comment-start) ?\ ))
		(delete-region (match-beginning 0) (match-end 0)))
	    (insert dtd-comment-start)))
      ;; Delete the "interior" comment delimiters
      (goto-char (min mark point))
      (while (re-search-forward
	      (concat "[ \t\n]*" dtd-comment-end "\n" dtd-comment-start)
	      (max mark point) t)
	(replace-match (concat "\n" (make-string
				     (length dtd-comment-start)
				     ?\ ))
		       nil nil))))
  ;; Insert a comment end if there isn't one at the end
  ;;      (goto-char (- (max mark point)
  ;;		    (length dtd-comment-end)))
  ;;      (if (not (looking-at dtd-comment-end))
  ;;	  
  (if font-lock-mode
      (save-excursion
	(font-lock-fontify-region
	 (dtd-font-lock-region-point-min)
	 (dtd-font-lock-region-point-max)))))

(defun dtd-declare-element (element-tag element-name element-comment content-spec)
  "Insert an element type declaration at the current point.

ELEMENT-TAG is the element type name as it appears in start- and
end-tags.  ELEMENT-NAME is a descriptive name for the element.  It is
output as a comment.  ELEMENT-COMMENT is a further comment about the
element.  This comment is not output if ELEMENT-COMMENT is an empty
string.  CONTENT-SPEC is the element type's content specification.

When dtd-xml-flag in nil, the omitted tag minimization parameter is
output as part of the element type declaration.  When CONTENT-SPEC is
\"EMPTY\", the minimization parameter is \"- o\", otherwise it is \"- -\"

When dtd-upcase-name-comment-flag is non-nil, the text of the
descriptive element name is converted to uppercase before output.

ELEMENT-NAME is not prompted for if dtd-prompt-descriptive-name is
nil, and ELEMENT-COMMENT is not prompted for if
dtd-prompt-descriptive-comment is nil.

An example inserted element type declaration is as follows:

<!--                    Element name                               -->
<!--                    Comment about the element: what it's for,
                        or something about its behaviour           -->
<!ELEMENT  element-tag  - - (insert, your, content, specification,
                             here)                                   >
"
  (interactive
   ;; Hackery and fakery
   (let ((element-tag nil))
     (list (progn
	     ;; set element-tag to what's read from the minibuffer
	     (setq element-tag
		   (dtd-read-from-minibuffer
		    "Element tag: "
		    (car dtd-referenced-element-type-names)
		    'dtd-referenced-element-type-names))
	     ;; complain if element-tag is an empty string and there's no
	     ;; default
	     (if (string-equal element-tag "")
		 (if default
		     (setq element-tag default)
		   (error "You must supply an element tag name"))
	       ;; return element-tag is we had one already
	       element-tag))
	   (if dtd-prompt-descriptive-name
	       (read-from-minibuffer (format "<%s> descriptive name: "
					     element-tag)
				     ;; use element-tag as the default for
				     ;; the descriptive name since we often
				     ;; base the descriptive name on it
				     element-tag nil nil nil)
	     "")
	   (if dtd-prompt-descriptive-comment
	       (read-from-minibuffer (format "<%s> comment: "
					     element-tag)
				     nil nil nil
				     'dtd-element-comment-history)
	     "")
	   (read-from-minibuffer (format "<%s> content spec: "
					 element-tag)
				 nil nil nil
				 'dtd-element-content-spec-history))))
  (if (not
       (string-equal element-name ""))
      (progn
	(if dtd-upcase-name-comment-flag
	    (setq element-name (upcase element-name)))
	(dtd-comment element-name)))
  (if (not
       (string-equal element-comment ""))
      (dtd-comment element-comment))
  (insert "\n")
  (backward-char)
  (insert "<!ELEMENT  ")
  (insert element-tag)
  (if dtd-autodetect-type
      (dtd-autodetect-type))
  (if (not dtd-xml-flag)
      (progn
	(dtd-indent-or-newline-to (1- dtd-element-tag-omission-column))
	(if (string-equal content-spec dtd-empty-literal)
	    (insert "- o")
	  (insert "- -"))))
  (dtd-indent-or-newline-to
   (if dtd-xml-flag
       (1- dtd-xml-element-content-spec-start-column)
       (1- dtd-element-content-spec-start-column)))
  (let ((fill-column dtd-mdc-indent-column)
	(fill-prefix (make-string
		      (if dtd-xml-flag
			  (1-
			   dtd-xml-element-content-spec-continuation-column)
			  (1- dtd-element-content-spec-continuation-column))
			?\ ))
	(saved-auto-fill-function auto-fill-function))
    (auto-fill-mode 1)
    (fill-region (point) (save-excursion
			   (insert content-spec)
			   (point))
		 nil
		 1
		 1)
    ;; The fill does the right thing, but in Emacs 19.34 it always ends with
    ;; an extra newline, so we delete the newline.
    (if (bolp)
	(delete-backward-char 1))
;;    (setq content-spec-end (point))
    (if (not saved-auto-fill-function)
	(auto-fill-mode 0))
    (dtd-indent-or-newline-to dtd-mdc-indent-column)
    (insert (concat dtd-sgml-mdc "\n"))
    (auto-fill-mode nil)
    (if font-lock-mode
	(save-excursion
	  (font-lock-fontify-region
	   (dtd-font-lock-region-point-min)
	   (dtd-font-lock-region-point-max))))
    ;; Add to the list of element type names that we've already seen
    (add-to-list 'dtd-declared-element-type-names element-tag)
    ;; Work out whether we can remove element-tag from the list of
    ;; element type names that we've seen referenced in content models
    ;; and parameter entity declarations
    ;;
    ;; Before we work anything out, we need to remove element-type-name
    ;; from the start of dtd-referenced-element-type-names (since that's
    ;; the minibuffer history list variable)
    (setq dtd-referenced-element-type-names
	  (cdr dtd-referenced-element-type-names))
    (if (member element-tag dtd-referenced-element-type-names)
	;; There has to be an easier way to do this.
	(setq dtd-referenced-element-type-names
	      (catch 'roll-referenced-element-type-names
		(let ((bypassed-element-type-names nil))
		  (while (car dtd-referenced-element-type-names)
		    (let ((element-type-name
			   (car dtd-referenced-element-type-names)))
		      (setq dtd-referenced-element-type-names
			    (cdr dtd-referenced-element-type-names))
		      (if (equal element-tag element-type-name)
			  (throw
			   'roll-referenced-element-type-names
			   (append dtd-referenced-element-type-names
				   bypassed-element-type-names))
			(setq bypassed-element-type-names
			      (reverse
			       (add-to-list 'bypassed-element-type-names
					    element-type-name))))))))))
    (dtd-analyse-content-spec content-spec)))

(defun dtd-analyse-content-spec (content-spec)
  "Eventually, decide if need declarations for anything referenced in CONTENT-SPEC.

CONTENT-SPEC is an element type's content specification as provided to
`dtd-declare-element' or `dtd-declare-parameter-entity'."
;;  (message "%s" content-spec)
  (if (not (or (string-equal content-spec "ANY")
	       (string-equal content-spec "EMPTY")))
      (let ((match-index nil)
	    (token-list nil))
	(save-match-data
	  (while (string-match "\\(%?[A-Za-z#]+;?\\)"
			       content-spec match-index)
	    (add-to-list 'token-list (substring content-spec
					       (match-beginning 1)
					       (match-end 1)))
	    (setq match-index (match-end 0))))
	(while token-list
	  (let* ((token (car token-list))
		 (pe-flag (string-equal "%" (substring token 0 1))))
	    (if (not (string-equal token "#PCDATA"))
		(progn
;;		  (message "%s" token)
		  (if pe-flag
		      (progn
			(setq token (substring token
					       1
					       (1- (length token))))
			(if (not (member token
					 dtd-declared-parameter-entity-names))
			    (add-to-list
			     'dtd-referenced-parameter-entity-names
			     token)))
		    (if (not (member token
				     dtd-declared-element-type-names))
			(add-to-list 'dtd-referenced-element-type-names
				     token))))))
	    (setq token-list (cdr token-list))))))

(defun dtd-read-from-minibuffer (prompt default history)
  "Read from minibuffer with default and command history."
(let ((value nil))
  (if (string-equal
       ""
       (setq value
	     (read-from-minibuffer (if default
				       (format
					"%s(default `%s') "
					prompt default)
				     (format "%s" prompt))
				   nil nil nil
				   history)))
	     default
	     value)))

(defun dtd-declare-attribute
  (attribute-tag attribute-comment attribute-type attribute-default)
  "Declare an attribute.

ATTRIBUTE-TAG is the attribute name.  ATTRIBUTE-NAME is a descriptive
name for the attribute.  It is output as a comment.  ATTRIBUTE-COMMENT
is a further comment about the attribute.  This comment is not output
if ATTRIBUTE-COMMENT is an empty string.  ATTRIBUTE-TYPE is the
attribute's type or enumeration declaration.  ATTRIBUTE-DEFAULT is the
attribute's default.

ATTRIBUTE-COMMENT is not prompted for if
dtd-prompt-descriptive-comment is nil.

If called interactively and not after \"<!ATTLIST\", also calls
`dtd-declare-attribute-list' before prompting for ATTRIBUTE-TAG.

`dtd-declare-attribute' takes care of inserting or moving the \">\"
that closes the attribute list declaration."
  (interactive
   (let ((attribute-tag nil))
     ;; Hackery and fakery
     ;; If we're not after "<!ATTLIST", call dtd-declare-attribute-list.
     ;; This is an abomination, but don't know a better way to do it.
     ;;
     ;; Do `save-excursion', etc. while we check.
     (if (not (save-excursion
		(save-match-data
		  (re-search-backward "^<!" nil t)
		  (looking-at "<!ATTLIST"))))
	 (dtd-declare-attribute-list
	  ;; This is largely a repeat of the (interactive) statement
	  ;; in dtd-declare-attribute-list
	  (dtd-read-from-minibuffer
	   "Element type: "
	   (save-excursion
	     (save-match-data
	       (re-search-backward "^<!ELEMENT[ \t]+\\([^ \t\n]+\\)" nil t)
	       (match-string 1)))
	   'dtd-declared-element-type-names)))
     (list (setq attribute-tag
		 (dtd-read-from-minibuffer "Attribute tag: "
					   nil
					   'dtd-attribute-tag-history))
	   (if dtd-prompt-descriptive-comment
	       (dtd-read-from-minibuffer (format "\"%s\" comment: "
						 attribute-tag)
					 nil nil)
	     "")
	   (dtd-read-from-minibuffer
	    (format "\"%s\" type or enumeration: " attribute-tag)
	    (car dtd-attribute-type-history)
	    'dtd-attribute-type-history)
	   (dtd-read-from-minibuffer
	    (format "\"%s\" default: " attribute-tag)
	    (car dtd-attribute-default-history)
	    'dtd-attribute-default-history))))
  (if (and (stringp attribute-comment)
	   (not (string-equal attribute-comment "")))
      (save-excursion
	(re-search-backward "^<!ATTLIST" nil t)
	(insert dtd-comment-start)
	(dtd-indent-or-newline-to (1- dtd-attribute-name-column))
	(if (and
	     dtd-outdent-attribute-pe
	     (string-equal (substring attribute-tag 0 1) "%"))
	    (delete-backward-char 1))
	(insert attribute-tag)
	(dtd-indent-or-newline-to (1- dtd-comment-start-column))
	(let ((fill-column dtd-mdc-indent-column)
	      (fill-prefix (make-string
			    (1- dtd-comment-start-column) ?\ ))
	      (saved-auto-fill-function auto-fill-function))
	  ;;    (auto-fill-mode 1)
	  ;;    (setq content-spec-start (point))
	  (fill-region (point) (save-excursion
				 (insert attribute-comment)
				 (insert "\n")
				 (point))
		       nil
		       1
		       1)
	  ;; The fill does the right thing, but in Emacs 19.34 it
	  ;; always ends with an extra newline, so we delete the newline.
	  (if (bolp)
	      (delete-backward-char 1))
	  (dtd-indent-or-newline-to dtd-comment-max-column)
	  (insert dtd-comment-end)
	  (insert "\n"))
	;; Now see if we need to merge with a previous comment
	(let ((previous-comment-beginning
	       (save-excursion
		 (re-search-backward "<!" nil t 2)
		 (if (looking-at dtd-comment-start)
		     (point)
		   nil))))
	  (if previous-comment-beginning
	      (dtd-join-comments previous-comment-beginning (point))))))
  ;; Do it all again for the actual declaration
  (let ((fill-column dtd-mdc-indent-column)
	(fill-prefix (make-string
		      (1- dtd-comment-start-column) ?\ ))
	(saved-auto-fill-function auto-fill-function))
    ;; If we're after a declaration, delete the preceding mdc and any
    ;; whitespace around it.
    (if (not (save-excursion
	       (re-search-backward "^<!\\|>" nil t)
	       (looking-at "<!ATTLIST")))
	(save-excursion
	  (delete-region (point)
			 (save-excursion
			   (re-search-backward "[^ \t\n\r>]" nil t)
			   (1+ (point))))))
    (dtd-indent-or-newline-to (1- dtd-attribute-name-column))
    (if (string-equal (substring attribute-tag 0 1) "%")
	(delete-backward-char 1))
    (insert attribute-tag)
    (dtd-indent-or-newline-to (1- dtd-comment-start-column))
    (fill-region (point) (save-excursion
			   (insert attribute-type)
			   (insert "\n")
			   (point))
		 nil
		 1
		 1)
    ;; The fill does the right thing, but in Emacs 19.34 it always ends with
    ;; an extra newline, so we delete the newline.
    (if (bolp)
	(delete-backward-char 1))
    ;; We want the default to line up on dtd-attribute-default-column
    ;; unless it's too long, in which case we want one space between
    ;; the end of the default and the mdc (>).
    (if (< (length attribute-default)
	   (- dtd-dtd-max-column dtd-attribute-default-column 1))
	(dtd-indent-or-newline-to (1- dtd-attribute-default-column))
      (dtd-indent-or-newline-to (- dtd-dtd-max-column
				   (length attribute-default)
				   2)))
    (insert attribute-default)
    ;; Insert an mdc only if there isn't one between here and the
    ;; next mdo.
    (if (or (looking-at "<")
	    (not (save-excursion
		   (re-search-forward "^<!\\|>" nil t)
		   (goto-char (match-beginning 0))
		   (looking-at ">"))))
	(progn
	  (dtd-indent-or-newline-to dtd-mdc-indent-column)
	  (insert dtd-sgml-mdc)))
    (insert "\n")
    (if font-lock-mode
	(save-excursion
	  (font-lock-fontify-region
	   (dtd-font-lock-region-point-min)
	   (dtd-font-lock-region-point-max))))))

(defun dtd-declare-attribute-list (element-type-name)
  "Declare an attribute list."
  (interactive
   (list (dtd-read-from-minibuffer
	  "Element type: "
	  (save-excursion
	    (save-match-data
	      (re-search-backward "^<!ELEMENT[ \t]+\\([^ \t\n]+\\)" nil t)
	      (match-string 1)))
	  'dtd-declared-element-type-names)))
  (insert (concat "<!ATTLIST  " element-type-name "\n")))

(defun dtd-declare-notation
  (notation-tag notation-name notation-comment public-identifier system-identifier)
  "Insert a notation declaration.

NOTATION-TAG is the notation name as it appears in notation
references.  NOTATION-NAME is a descriptive name for the notation.  It
is output as a comment.  NOTATION-COMMENT is a further comment about
the notation.  The comment is not output if NOTATION-COMMENT is an
empty string.  PUBLIC-IDENTIFIER is the notation\'s public identifier
\(without '\"' characters).  SYSTEM-IDENTIFIER is the notation's system
identifier (without '\"' characters).

NOTATION-NAME is not prompted for if dtd-prompt-descriptive-name is
nil, and NOTATION-COMMENT is not prompted for if
dtd-prompt-descriptive-comment is nil.

An example inserted notation type declaration is as follows:

<!--                    Notation name                                -->
<!--                    Comment about the notation: what it's for,
                        or something about its behaviour           -->
<!NOTATION notation-tag   PUBLIC
\"-//Complete//NOTATION Public Identifier//EN\"
                                                                   -->
%notation-tag;
"
  (interactive
   ;; Hackery and fakery
   (let ((notation-tag nil))
     (list (progn
	     ;; set notation-tag to what's read from the minibuffer
	     (setq notation-tag
		   (dtd-read-from-minibuffer
		    "Notation tag: "
		    (car dtd-referenced-notation-type-names)
		    'dtd-referenced-notation-type-names))
	     ;; complain if notation-tag is an empty string and there's no
	     ;; default
	     (if (string-equal notation-tag "")
		 (if default
		     (setq notation-tag default)
		   (error "You must supply an notation tag name"))
	       ;; return notation-tag is we had one already
	       notation-tag))
	   (if dtd-prompt-descriptive-name
	       (read-from-minibuffer (format "`%s' descriptive name: "
					     notation-tag)
				     ;; use notation-tag as the default for
				     ;; the descriptive name since we often
				     ;; base the descriptive name on it
				     notation-tag nil nil nil)
	     "")
	   (if dtd-prompt-descriptive-comment
	       (read-from-minibuffer (format "`%s' comment: "
					     notation-tag)
				     nil nil nil
				     'dtd-notation-comment-history)
	     "")
	   (read-from-minibuffer (format "`%s' content spec: "
					 notation-tag)
				 nil nil nil
				 'dtd-notation-content-spec-history))))
  (if (not
       (string-equal notation-name ""))
      (progn
	(if dtd-upcase-name-comment-flag
	    (setq notation-name (upcase notation-name)))
	(dtd-comment notation-name)))
  (if (not
       (string-equal notation-comment ""))
	(dtd-comment notation-comment))
;;  (insert "\n")
;;  (backward-char)
  (insert "<!NOTATION ")
  (insert notation-tag)
  (dtd-indent-or-newline-to (1- dtd-element-content-spec-start-column))
  (if (not
       (string-equal public-identifier ""))
      (progn
	(insert "PUBLIC\n")
	(insert (concat "\"" public-identifier "\"\n")))
    (insert "SYSTEM "))
  (if (not
       (string-equal system-identifier ""))
      (insert (concat "\"" system-identifier "\"\n")))
  (indent-to dtd-mdc-indent-column)
  (insert dtd-sgml-mdc)
  (insert "\n")
  (if font-lock-mode
	(save-excursion
	  (font-lock-fontify-region
	   (dtd-font-lock-region-point-min)
	   (dtd-font-lock-region-point-max)))))

(defun dtd-declare-external-entity
  (entity-tag entity-name entity-comment public-identifier system-identifier)
  "Insert an entity declaration and references for an external public entity.

ENTITY-NAME is a descriptive name for the entity.  It is output as a
comment.  ENTITY-COMMENT is a further comment about the entity.  The
comment is not output if ENTITY-COMMENT is an empty string.
ENTITY-TAG is the entity name as it appears in entity references.
PUBLIC-IDENTIFIER is the entity's public identifier (without '\"'
characters).  SYSTEM-IDENTIFIER is the entity's system identifier
\(without '\"' characters).

ENTITY-NAME is not prompted for if dtd-prompt-descriptive-name is nil,
and ENTITY-COMMENT is not prompted for if
dtd-prompt-descriptive-comment is nil.

An example inserted entity type declaration is as follows:

<!--                    Entity name                                -->
<!--                    Comment about the entity: what it's for,
                        or something about its behaviour           -->
<!ENTITY % entity-tag   PUBLIC
\"-//Complete//ENTITY Public Identifier//EN\"
                                                                   -->
%entity-tag;
"
;;  (interactive "sEntity tag: \nsEntity name: \nsEntity comment: \nsPublic Identifier: \nsSystem Identifier: ")
  (interactive
   ;; Hackery and fakery
   (let ((entity-tag nil))
     (list (setq entity-tag
		 (read-from-minibuffer "External entity tag: "
				       nil nil nil
				       'dtd-referenced-parameter-entity-names))
	   (if dtd-prompt-descriptive-name
	       (read-from-minibuffer (format "`%%%s;\' descriptive name: "
					     entity-tag)
				     nil nil nil nil)
	     "")
	   (if dtd-prompt-descriptive-comment
	       (read-from-minibuffer (format "`%%%s;' comment: "
					     entity-tag)
				     nil nil nil nil)
	     "")
	   (read-from-minibuffer (format "`%%%s;' public identifier: "
					 entity-tag)
				 nil nil nil
				 'dtd-external-entity-public-history)
	   (read-from-minibuffer (format "`%%%s;' system identifier: "
					 entity-tag)
				 nil nil nil
				 'dtd-external-entity-system-history))))
  (if (not
       (string-equal entity-name ""))
      (progn
	(if dtd-upcase-name-comment-flag
	    (setq entity-name (upcase entity-name)))
	(dtd-comment entity-name)))
;;  (insert "\n")
  (if (not
       (string-equal entity-comment ""))
      (progn
	(dtd-comment entity-comment)
	(insert "\n")))
;;  (insert "\n")
;;  (backward-char)
  (insert "<!ENTITY % ")
  (insert entity-tag)
  (if dtd-autodetect-type
      (dtd-autodetect-type))
  (dtd-indent-or-newline-to
   (if dtd-xml-flag
       (1- dtd-xml-element-content-spec-start-column)
       (1- dtd-element-content-spec-start-column)))
  (if (not
       (string-equal public-identifier ""))
      (progn
	(insert "PUBLIC\n")
	(insert (concat "\"" public-identifier "\"\n")))
    (insert "SYSTEM "))
  (if (not
       (string-equal system-identifier ""))
      (insert (concat "\"" system-identifier "\"\n")))
  (indent-to dtd-mdc-indent-column)
  (insert (concat dtd-sgml-mdc "\n"))
  (insert (concat "%" entity-tag ";\n"))
  (if font-lock-mode
	(save-excursion
	  (font-lock-fontify-region
	   (dtd-font-lock-region-point-min)
	   (dtd-font-lock-region-point-max)))))

(defun dtd-declare-parameter-entity
  (entity-tag entity-name entity-comment entity-value)
  "Insert a parameter entity declaration at the current point.

ENTITY-NAME is a descriptive name for the entity.  It is output as a
comment.  ENTITY-COMMENT is a further comment about the entity.  The
comment is not output if ENTITY-COMMENT is an empty string.
ENTITY-TAG is the entity type name as it appears in start- and
end-tags.  ENTITY-VALUE is the entity's value specification, without
the '\"' characters.

ENTITY-NAME is not prompted for if dtd-prompt-descriptive-name is nil,
and ENTITY-COMMENT is not prompted for if
dtd-prompt-descriptive-comment is nil.

An example inserted parameter entity declaration is as follows:

<!--                    Entity name                               -->
<!--                    Comment about the entity: what it's for,
                        or something about its behaviour           -->
<!ENTITY % entity-tag   \"parameter, entity, contents\"                >
"
  (interactive
   ;; Hackery and fakery
   (let ((entity-tag nil))
     (list (setq entity-tag
		 (read-from-minibuffer "Entity tag: "
				       nil nil nil
				       'dtd-referenced-parameter-entity-names))
	   (if dtd-prompt-descriptive-name
	       (read-from-minibuffer (format "`%%%s;\' descriptive name: "
					     entity-tag)
				     nil nil nil nil)
	     "")
	   (if dtd-prompt-descriptive-comment
	       (read-from-minibuffer (format "`%%%s;' comment: "
					     entity-tag)
				     nil nil nil nil)
	     "")
	   (read-from-minibuffer (format "`%%%s;' value: "
					 entity-tag)
				 nil nil nil
				 'dtd-parameter-entity-value-history))))
  (if (not
       (string-equal entity-name ""))
      (progn
	(if dtd-upcase-name-comment-flag
	    (setq entity-name (upcase entity-name)))
	(dtd-comment entity-name)))
  (if (not
       (string-equal entity-comment ""))
      (dtd-comment entity-comment))
  (insert "<!ENTITY % ")
  (insert entity-tag)
  (dtd-indent-or-newline-to (1- dtd-entity-entity-value-start-column))
  (insert "\"")
  (let ((fill-column dtd-mdc-indent-column)
	(fill-prefix (make-string
		      (1- dtd-entity-entity-value-continuation-column) ?\ ))
	(saved-auto-fill-function auto-fill-function))
    (auto-fill-mode 1)
    (fill-region (point) (save-excursion
			   (insert entity-value)
			   (insert "\"")
			   (point))
		 nil
		 1
		 1)
    ;; The fill does the right thing, but in Emacs 19.34 it always ends with
    ;; an extra newline, so we delete the newline.
    (if (bolp)
	(delete-backward-char 1))
    (if (not saved-auto-fill-function)
	(auto-fill-mode 0))
    (dtd-indent-or-newline-to dtd-mdc-indent-column)
    (insert (concat dtd-sgml-mdc "\n"))
    (auto-fill-mode nil)
    (if font-lock-mode
	(save-excursion
	  (font-lock-fontify-region
	   (dtd-font-lock-region-point-min)
	   (dtd-font-lock-region-point-max))))
    (add-to-list 'dtd-declared-parameter-entity-names entity-tag)
    (if (member entity-tag dtd-referenced-parameter-entity-names)
	(setq dtd-referenced-parameter-entity-names
	      (delete entity-tag dtd-referenced-parameter-entity-names)))
    (dtd-analyse-content-spec entity-value)))

(defun dtd-insert-mdc ()
  "Without moving point, indent to usual column and insert MDC (>).
MDC = Markup Declaration Close"
  (interactive)
  (save-excursion
    (let ((current-point (point))
	  (mdc-column 0)
	  (mdc dtd-sgml-mdc))
      (if (if (looking-at dtd-sgml-mdo)
	      1
	    (re-search-backward (concat "^" dtd-sgml-mdo) nil t))
	  (progn
	    (if (looking-at dtd-comment-start)
	      (progn
		(setq mdc dtd-comment-end)
		(setq mdc-column dtd-comment-max-column))
	      (progn
		(setq mdc dtd-sgml-mdc)
		(setq mdc-column dtd-mdc-indent-column)))
	    (goto-char current-point)
	    (beginning-of-line)
	    (cond
	     ;; The main "looking-at" regular expression doesn't handle
	     ;; line containing just "-->" or ">" very well, so handle
	     ;; as special case.
	     ((looking-at "[ \t]*\\(--\\)?>$")
	      (progn
		;; Delete to the end of line so we can then insert our mdc
	       (delete-region (point) (save-excursion
					(end-of-line)
					(point)))
	       (indent-to mdc-column)
	       (insert mdc)))
	     ;; This is the main "looking-at" regular expression that
	     ;; handles most things
	     ((looking-at "^\\(\\(<!\\|[ \t]*\\)?\\(--\\)?[ \t]*\\([^ \t\n>]+\\([- \t]+[^- \t\n>]+\\)*\\)?\\)[ \t]*\\(\\(--\\)?>?\\)?$")
	      (progn
		;;(message ":%s:%s:%s:%s:" mdc mdc-column (match-string 4) (match-string 1))
		(goto-char (match-end 1))
		(delete-region (point) (save-excursion
					 (end-of-line)
					 (point)))
		(dtd-indent-or-newline-to mdc-column)
		(insert mdc))))))))
  (if font-lock-mode
      (save-excursion
	(font-lock-fontify-region
	 (dtd-font-lock-region-point-min)
	 (dtd-font-lock-region-point-max)))))

;; A work in progress
(defun dtd-fill-paragraph (&optional justify)
  "DTD fill paragraph function.  A work in progress."
  (interactive "P")
  ;;(insert "DTD")
  (save-excursion
    (if dtd-autodetect-type
	(dtd-autodetect-type))
    (let ((current-point (point))
	  (fill-column dtd-mdc-indent-column)
	  (fill-prefix (make-string
			(if dtd-xml-flag
			    (1-
			     dtd-xml-element-content-spec-continuation-column)
			  (1- dtd-element-content-spec-continuation-column))
			?\ ))
	  (saved-auto-fill-function auto-fill-function))
      (if (not (looking-at dtd-sgml-mdo))
	  (re-search-backward (concat "^" dtd-sgml-mdo) nil t))
      (if (looking-at "\\(<!ELEMENT\\)\\(\\s-+\\)")
	  (progn
	    (goto-char (match-end 1))
	    (delete-region (match-beginning 2) (match-end 2))
	    (dtd-indent-or-newline-to (1- dtd-element-name-column))
	    (if (looking-at "\\(\\sw\\|\\s_\\)+\\(\\s-+\\)")
		(progn
		  (message "It's an element")
		  (delete-region (match-beginning 2) (match-end 2))
		  (goto-char (match-end 1))
		  (if (looking-at "\\([-o]\\)\\s-+\\([-o]\\)\\s-+")
		      (let ((omit-start (match-string 1))
			    (omit-end (match-string 2)))
			(message "It has ommissibility indicators")
			(message "%s %s" omit-start omit-end)
			(delete-region
			 (match-beginning 0) (match-end 0))
			(dtd-indent-or-newline-to
			 (1- dtd-element-tag-omission-column))
			(insert omit-start)
			(insert " ")
			(insert omit-end)))
		  (dtd-indent-or-newline-to
		   (1-
		    (if dtd-xml-flag
			dtd-xml-element-content-spec-start-column
		      dtd-element-content-spec-start-column)))
		  (auto-fill-mode 1)
;;		  (let ((content-model-start (point))
;;			(content-model-end (save-excursion
;;					     (forward-sexp)
;;					     (point))))
		  (save-excursion
		    (let ((content-model-end (save-excursion
					       (forward-sexp)
					       (point))))
		    (while (re-search-forward
			    "\n+"
			    content-model-end t)
		      (replace-match " " nil nil))))
		  (fill-region (point) (save-excursion
					 (forward-sexp)
					 (point))
			       nil
			       1
			       1)
		  ;; The fill does the right thing, but it always ends with
		  ;; an extra newline, so we delete the newline.
		  (backward-char 1)
		  ;;    (setq content-spec-end (point))
		  (if (not saved-auto-fill-function)
		      (auto-fill-mode 0))))
	    (dtd-insert-mdc)
;;		  (insert "$")
;;	    (if (looking-at "\\(\\s-*\\)>")
;;		(progn
;;		  (delete-region (match-beginning 1) (match-end 1))
;;		  (dtd-indent-or-newline-to dtd-mdc-indent-column)
;;		  (insert "$")
))
;;		  (dtd-insert-mdc)
;;		  (insert "\n")
;;		  (auto-fill-mode nil)
		  (if font-lock-mode
	(save-excursion
	  (font-lock-fontify-region
	   (dtd-font-lock-region-point-min)
	   (dtd-font-lock-region-point-max)))))))
;;	    (if (looking-at "ANY\\|CDATA\\|EMPTY\\|RCDATA")
;;		(progn
;;		  (dtd-indent-or-newline-to
;;		   (1- dtd-element-content-spec-start-column)))
;;	      (message "It's something else"))))))
	;; If we're not doing anything, return nil so the built-in function
	;; will run
;;  (font-lock-fontify-block))


;; Functions that set fill-prefix, etc. for various regions in a DTD
(defun dtd-comment-setup ()
  "Setup the auto-fill variables for comments in the body of the DTD"
  (interactive)
  (setq fill-prefix (make-string (1- dtd-comment-start-column) ?\ ))
  (auto-fill-mode 'true)
  (setq fill-column dtd-comment-max-column))

(defun dtd-design-comment-setup ()
  "Setup the auto-fill variables for \"Design Considerations\" comments"
  (interactive)
  (setq fill-prefix (make-string
		     (1- dtd-design-comment-start-column)
		     ?\ ))
  (auto-fill-mode 'true)
  (setq fill-column dtd-comment-max-column))

(defun dtd-declaration-setup ()
  "Setup the auto-fill variables for declarations"
  (interactive)
  (setq fill-prefix (make-string
		     (1- dtd-element-content-spec-continuation-column)
		     ?\ ))
  (auto-fill-mode 'true)
  (setq fill-column dtd-mdc-indent-column))

(defun dtd-init-comment-setup ()
  "Setup the auto-fill variables for initial comments"
  (interactive)
  (setq fill-prefix dtd-init-comment-fill-prefix)
  (auto-fill-mode 'true)
  (setq fill-column (- dtd-dtd-max-column (length dtd-comment-end))))

;;(setq fill-paragraph-function 'dtd-fill-paragraph)

;; Not quite sure what this gets me, but these variables seem to apply
;; in auto-fill mode
(make-local-variable 'comment-start)
(setq comment-start "<!-- ")
(make-local-variable 'comment-end)
(setq comment-end " -->")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; imenu stuff

(defun dtd-sort-alist (alist)
  "Sort an alist"
  (sort
   alist
   (lambda (a b) (string< (car a) (car b)))))

(defun dtd-imenu-create-index-function ()
  "Create an alist of elements, etc. suitable for use with imenu."
  (let ((element-alist '())
	(notation-alist '())
	(general-entity-alist '())
	(parameter-entity-alist '()))
    (goto-char (point-min))
    (while
	(re-search-forward
	 "^<!ELEMENT[ \t]+\\([^ \t\n]+\\)+[ \t\n]+\\([-o][ \t]+[-o][ \t]+\\)?(*" nil t)
      (setq element-alist
	    (cons (cons (buffer-substring-no-properties
			 (match-beginning 1)
			 (match-end 1))
			(match-end 0))
		  element-alist)))
    (goto-char (point-min))
    (while
	(re-search-forward
	 "^<!NOTATION[ \t]+\\([^ \t\n]+\\)" nil t)
      (setq notation-alist
	    (cons (cons (buffer-substring-no-properties
			 (match-beginning 1)
			 (match-end 1))
			(match-beginning 1))
		  notation-alist)))
    (goto-char (point-min))
    (while
	(re-search-forward
	 "^<!ENTITY[ \t]+\\([^% \t\n]+\\)" nil t)
      (setq general-entity-alist
	    (cons (cons (buffer-substring-no-properties
			 (match-beginning 1)
			 (match-end 1))
			(match-beginning 1))
		  general-entity-alist)))
    (goto-char (point-min))
    (while
	(re-search-forward
	 "^<!ENTITY[ \t]+%[ \t]+\\([^ \t\n]+\\)[ \t\n]+[\"']?" nil t)
      (setq parameter-entity-alist
	    (cons (cons (buffer-substring-no-properties
			 (match-beginning 1)
			 (match-end 1))
			(match-end 0))
		  parameter-entity-alist)))
    (append
     (if notation-alist
	 (list (cons "<!NOTATION" (dtd-sort-alist notation-alist))))
     (if general-entity-alist
	 (list (cons "<!ENTITY" (dtd-sort-alist general-entity-alist))))
     (if parameter-entity-alist
	 (list (cons "<!ENTITY %" (dtd-sort-alist parameter-entity-alist))))
     (dtd-sort-alist element-alist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grep stuff

;;;###autoload
(defun dtd-grep (pattern filespec)
  "Grep for PATTERN in files matching FILESPEC.

Runs `grep' with PATTERN and FILESPEC as arguments.

PATTERN is the pattern on which `grep' is to match.  PATTERN is quoted
with single quotes in the `grep' command arguments to avoid
interpretation of characters in PATTERN.  `dtd-grep' maintains a
history of PATTERNs so you can easily re-use a previous value.

FILESPEC is the names or regular expression for the files to be
scanned by grep.  Since `dtd-grep' uses `grep', regular expressions
and multiple filenames are supported, and \"*.dtd\" and \"*.dtd
*.ent\" are both valid FILESPEC values.

When called interactively, the initial FILESPEC is taken from
dtd-default-filespec, but `dtd-grep' also maintains a history of
FILESPEC arguments so you can easily re-use a previous value.  The
history is shared with `dtd-etags' so you can re-use the same FILESPEC
with both functions.
"
  (interactive
   (list
    (dtd-read-from-minibuffer "Pattern: "
			      (find-tag-default)
			      'dtd-grep-pattern-history)
    (dtd-read-from-minibuffer "Files: "
			      (car dtd-filespec-history)
			      'dtd-filespec-history)))
  ;; We include "--" in the command in case the pattern starts with "-"
  (grep (format dtd-grep-command-format
		(if (not dtd-grep-case-sensitive-flag)
		    "-i")
		pattern
		filespec)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tags stuff

;;;###autoload
(defun dtd-etags (filespec)
  "Execute etags on FILESPEC and match on DTD-specific regular expressions.

Runs the \"etags\" program on the specified file with a regular
expression for finding element, entity, attribute list, and notation
declarations.  `dtd-tags' then modifies the output tags file to
disambiguate parameter entity names versus other names.

FILESPEC is the names or regular expression for the files to be
scanned by etags.  `dtd-etags' uses `shell-command' to execute etags
specifically so regular expressions and multiple filenames are
supported, and \"*.dtd\" and \"*.dtd *.ent\" are both valid FILESPEC
values.

When called interactively, the initial prompt is taken from
dtd-default-filespec, but `dtd-etags' also maintains a history of
FILESPEC arguments so you can easily re-use a previous value.  The
history is shared with `dtd-grep' so you can re-use the same FILESPEC
with both functions.

The output tags file is added to tags-table-list using
`visit-tags-table', and, depending on the value of tags-add-tables,
you may be prompted whether to add the new tags table to the current
list or to start a new list.

`dtd-etags' also uses the dtd-etags-program, dtd-etags-regex-option,
and dtd-etags-output-file variables to construct the command passed to
`shell-command'."
  (interactive
   (list (dtd-read-from-minibuffer "Files: "
				   (car dtd-filespec-history)
				   'dtd-filespec-history)))
  (shell-command
   (format "%s %s --output=%s %s"
	   dtd-etags-program
	   dtd-etags-regex-option
	   dtd-etags-output-file
	   filespec))
  (save-excursion
    (save-window-excursion
      ;; We could be visiting the TAGS file, which causes an
      ;; unecessary complication when Emacs prompts to see if
      ;; we want to reload the buffer, so we just kill it out
      ;; of hand.  Since more than one buffer can be visiting it,
      ;; we make sure we catch them all.
      (while (get-file-buffer dtd-etags-output-file)
	(kill-buffer (get-file-buffer dtd-etags-output-file)))
      (find-file dtd-etags-output-file)
      (while (re-search-forward
	      "^\\(<!ENTITY[ \t]+%[ \t]+[^\177]+\177\\)\\([^%\1]+\\)"
	      nil t)
	(replace-match "\\1%\\2;" nil nil))
      ;; Go again to find element names to add to
      ;; dtd-declared-element-type-names
      (goto-char (point-min))
      ;; Remove whatever we had as dtd-declared-element-type-names
      (setq dtd-declared-element-type-names nil)
      ;; Add the element names as found by the etags program
      (while (re-search-forward
	      "^<!ELEMENT[ \t]+\\([^\177]+\\)"
	      nil t)
	(add-to-list 'dtd-declared-element-type-names (match-string 1)))
      ;; Don't bother saving a backup
      (setq backup-inhibited t)
      (save-buffer)
      (kill-buffer (current-buffer))
      (visit-tags-table
       (expand-file-name dtd-etags-output-file)))))

(defun dtd-find-tag-hook ()
  "Slight customization of find-tags."
  ;; If we're looking at an element declaration, move point to the first
  ;; syntactic literal, element type name, or parameter entity reference
  ;; in the content model.
  (cond
   ((looking-at
     "<!ELEMENT[ \t]+[^ \t]+[ \t\n]+\\([-o][ \t]+[-o][ \t]+\\)?(*")
    (goto-char (match-end 0)))
   ((looking-at
     "<!ATTLIST[ \t]+[^ \t]+[ \t\n]+")
    (goto-char (match-end 0)))
   ((looking-at
     "<!ENTITY[ \t]+%[ \t]+[^ \t]+[ \t\n]+\"?")
      (goto-char (match-end 0)))))


(defvar dtd-mode-abbrev-table nil
  "Abbrev table used while in DTD mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode map stuff

(defvar dtd-mode-map nil
  "Keymap for DTD mode.")

(if dtd-mode-map
    ()
  (setq dtd-mode-map (make-sparse-keymap))
  (define-key dtd-mode-map ">"                         'dtd-electric-mdc)
  (define-key dtd-mode-map '[(control c) (>)]          'dtd-insert-mdc)
  (define-key dtd-mode-map '[(control c) (control a)]  'dtd-declare-attribute)
  (define-key dtd-mode-map '[(control c) (control n)]  'dtd-declare-notation)
  (define-key dtd-mode-map '[(control c) (meta control %)]
    'dtd-declare-external-entity)
  (define-key dtd-mode-map '[(control c) (control %)]
    'dtd-declare-parameter-entity)
  (define-key dtd-mode-map '[(control c) (control e)]  'dtd-declare-element)
  (define-key dtd-mode-map '[(control c) (control c)]  'dtd-comment)
  (define-key dtd-mode-map '[(control c) (control v)]  'sgml-validate)
  ;;(define-key dtd-mode-map '[(control c) (b)]     'dtd-big-comment)
  (define-key dtd-mode-map '[(meta g) (meta f)]    'font-lock-fontify-buffer)
  (define-key dtd-mode-map '[(meta g) (meta control g)]
    'font-lock-fontify-buffer)
  (define-key dtd-mode-map '[(meta control g)]     'font-lock-fontify-buffer)
  ;; This overrides the sgml-mode mapping and puts it back to its default
  (define-key dtd-mode-map '[(meta tab)]           'complete-tag))

(defun dtd-electric-mdc ()
  (interactive)
  (insert ">")
  (if font-lock-mode
      (save-excursion
	(font-lock-fontify-region
	 (dtd-font-lock-region-point-min)
	 (dtd-font-lock-region-point-max)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax table stuff

(defvar dtd-mode-syntax-table nil
  "Syntax table used while in DTD mode.")

(if dtd-mode-syntax-table
    ()
  (setq dtd-mode-syntax-table (make-syntax-table))
  ;; set the non-alphanumeric characters in XML names to
  ;; 'symbol constituent' class
  (modify-syntax-entry ?: "_" dtd-mode-syntax-table)
  (modify-syntax-entry ?_ "_" dtd-mode-syntax-table)
  (modify-syntax-entry ?- "_ 1234" dtd-mode-syntax-table)
  (modify-syntax-entry ?. "_" dtd-mode-syntax-table)
  ;; "-" is a special case because it is the first and second characters
  ;; of the start- and end-comment sequences.
  (modify-syntax-entry ?- "_ 1234" dtd-mode-syntax-table)
  ;; "%" does double duty in parameter entity declarations and references.
  ;; Not necessary to make "%" and ";" act like parentheses since the
  ;; font lock highlighting tells you when you've put the ";" on the
  ;; end of a parameter entity reference.
  (modify-syntax-entry ?% "_" dtd-mode-syntax-table)
  (modify-syntax-entry ?\; "_" dtd-mode-syntax-table)
  ;; "/" is just punctuation in DTDs, and really only has a role in
  ;; Formal Public Identifiers
  (modify-syntax-entry ?/ "." dtd-mode-syntax-table)
  ;; Sometimes a string is more than just a string, Dr Freud.
  ;; Unfortunately, the syntax stuff isn't fussy about matching
  ;; on paired delimeters, and will happily match a single quote
  ;; with a double quote, and vice versa.  At least the font
  ;; lock stuff is more fussy and won't change colour if the
  ;; delimiters aren't paired.
  (modify-syntax-entry ?\" "$" dtd-mode-syntax-table)
  (modify-syntax-entry ?\' "$" dtd-mode-syntax-table)
  ;; The occurrence indicators and connectors are punctuation to us.
  (modify-syntax-entry ?| "." dtd-mode-syntax-table)
  (modify-syntax-entry ?, "." dtd-mode-syntax-table)
  (modify-syntax-entry ?& "." dtd-mode-syntax-table)
  (modify-syntax-entry ?? "." dtd-mode-syntax-table)
  (modify-syntax-entry ?+ "." dtd-mode-syntax-table)
  (modify-syntax-entry ?* "." dtd-mode-syntax-table)
  ;; `<' and `>' are also punctuation
  (modify-syntax-entry ?< "." dtd-mode-syntax-table)
  (modify-syntax-entry ?> "." dtd-mode-syntax-table)
  ;; "#" is syntax too
  (modify-syntax-entry ?# "_" dtd-mode-syntax-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font-lock utility functions

(defun dtd-font-lock-mark-block-function ()
  "Function to mark the area of text we want to fontify.

Used with font-lock-fontify-block.  Set font-lock-mark-block-function
to this function for this function to take effect.

This function marks the area beginning five \"<!\" before point and five
\">\" at ends of lines after point.  The default without a function like
this is to fontify 16 lines before and after point, but then the region
often starts or ends partway through a comment or declaration, turning
that half white because the keywords didn't match, and it just looks so
ugly."
  (let ((current-point (point)))
    (re-search-forward ">$" (point-max) 'limit 5)
    (set-mark (point))
    (goto-char current-point)
    (re-search-backward "^<!" (point-min) 'limit 5)))

(defun dtd-font-lock-region-point-min ()
  "Return the start point of the region we want to fontify"
  (save-excursion
    (re-search-backward "^<!" (point-min) 'limit 5)
;;    (insert "@")
    (point)))

(defun dtd-font-lock-region-point-max ()
  "Return the start point of the region we want to fontify"
  (save-excursion
    (re-search-forward ">$" (point-max) 'limit 5)
;;    (insert "!")
    (point)))


;;;###autoload
(defun dtd-mode ()
  "Major mode for SGML and XML DTDs.

dtd-mode features include:

 - dtd-etags function for creating Emacs TAGS files for easy lookup of
   any element, parameter entity, or notation's definition using
   Emacs's built-in tag-lookup functions;

 - Font lock highlighting of declarations so that the important
   information stands out;

 - XML-specific behaviour that, at user option, is triggered by
   automatic detection of the XML Declaration; and

 - Functions for writing and editing declarations and comments to ease
   creating and keeping a consistent style.

dtd-mode builds on sgml-mode, and the full sgml-mode functions are
still available.  Use with Lennart Staflin's psgml package is
recommended.

dtd-mode uses many user-definable variables to control the formatting
of declarations, some of which are shown in the following examples:

 		        dtd-comment-start-column    dtd-dtd-max-column
                        |       	     dtd-comment-max-column  |
                        |       				  |  |
<!--                    This is a comment                          -->

           dtd-element-name-column
           |    	dtd-element-tag-ommission-column
           |            |   dtd-element-content-spec-start-column
           |            |   |dtd-element-content-spec-continuation-column
           |            |   ||  		    dtd-dtd-max-column
           |            |   ||  				     |
<!ELEMENT  element-tag  - - (insert, your, content, specification,
                             here)                                   >

\\{dtd-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map dtd-mode-map)
  (setq mode-name "DTD")
  (setq major-mode 'dtd-mode)
  (setq local-abbrev-table dtd-mode-abbrev-table)
  ;; XEmacs users don't all have imenu
  (if (featurep 'imenu)
      (progn
	;; If you don't have imenu, you'll get a "free variable"
	;; warning for imenu-create-index-function when you
	;; byte-compile, but not having imenu won't cause problems
	;; when you use tdtd
	(setq imenu-create-index-function 'dtd-imenu-create-index-function)
	(setq imenu-extract-index-name-function 'dtd-imenu-create-index-function)
	(imenu-add-to-menubar "Goto")))
  (set-syntax-table dtd-mode-syntax-table)
  ;; XML specific behaviour can be specific to a buffer
  (make-local-variable 'dtd-xml-flag)
  ;; Maybe select XML-specific behaviour if we have an XML declaration
  (if dtd-autodetect-type
      (dtd-autodetect-type))
  ;; dtd font-lock highlighting setup
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'font-lock-mark-block-function)
  (cond
   (dtd-xml-flag
    (setq font-lock-defaults '(dtd-xml-font-lock-keywords t)))
   (dtd-decl-flag
    (setq font-lock-defaults '(dtd-decl-font-lock-keywords t)))
   (dtd-sys-decl-flag
    (setq font-lock-defaults '(dtd-sys-decl-font-lock-keywords t)))
   (t
    (setq font-lock-defaults '(dtd-sgml-font-lock-keywords t))))
  (setq font-lock-mark-block-function 'dtd-font-lock-mark-block-function)
  ;; Enable mode-specific behaviour on finding tags (in the Emacs sense)
  (make-local-variable 'find-tag-hook)
  (add-hook 'find-tag-hook
	    'dtd-find-tag-hook)
  ;; Maybe insert space characters when user hits "Tab" key
  (setq indent-tabs-mode dtd-indent-tabs-mode)
  (run-hooks 'dtd-mode-hooks))


;;;; Bug reporting

(defun dtd-submit-bug-report ()
  "Submit via mail a bug report on TDTD."
  (interactive)
  (and (y-or-n-p "Do you really want to submit a report on DTD mode? ")
       (reporter-submit-bug-report
	tdtd-maintainer-address
	(concat "tdtd.el " tdtd-version)
	(list 
	 'dtd-attribute-default-column
	 'dtd-attribute-default-history
	 'dtd-attribute-name-column
	 'dtd-attribute-tag-history
	 'dtd-attribute-type-history
	 'dtd-autodetect-type
	 'dtd-comment-end
	 'dtd-comment-max-column
	 'dtd-comment-start
	 'dtd-comment-start-column
	 'dtd-declared-element-type-names
	 'dtd-declared-parameter-entity-names
	 'dtd-default-element-type-name
	 'dtd-default-filespec
	 'dtd-design-comment-start-column
	 'dtd-dtd-max-column
	 'dtd-element-comment-history
	 'dtd-element-content-spec-continuation-column
	 'dtd-element-content-spec-history
	 'dtd-element-content-spec-start-column
	 'dtd-element-name-column
	 'dtd-element-tag-omission-column
	 'dtd-element-type-name-history
	 'dtd-empty-literal
	 'dtd-entity-entity-value-start-column
	 'dtd-etags-output-file
	 'dtd-etags-program
	 'dtd-etags-regex-option
	 'dtd-filespec-history
	 'dtd-grep-case-sensitive-flag
	 'dtd-grep-pattern-history
	 'dtd-indent-tabs-mode
	 'dtd-init-comment-column
	 'dtd-init-comment-fill-prefix
	 'dtd-line-comment
	 'dtd-mdc-indent-column
	 'dtd-outdent-attribute-pe
	 'dtd-parameter-entity-value-history
	 'dtd-referenced-element-type-names
	 'dtd-referenced-parameter-entity-names
	 'dtd-sgml-mdc
	 'dtd-sgml-mdo
	 'dtd-upcase-name-comment-flag
	 'dtd-xml-element-content-spec-continuation-column
	 'dtd-xml-element-content-spec-start-column
	 'dtd-xml-flag
	 )
	nil
	nil
     "Please change the Subject header to a concise bug description.\nRemember to cover the basics, that is, what you expected to\nhappen and what in fact did happen.  Please remove these\ninstructions from your message.")
    (save-excursion
      (goto-char (point-min))
      (mail-position-on-field "Subject")
      (beginning-of-line)
      (delete-region (point) (progn (forward-line) (point)))
      (insert
       "Subject: tdtd version " tdtd-version " is wonderful but...\n"))))


;;;; Last provisions
;;;(provide 'tdtd)

;;; tdtd.el ends here
