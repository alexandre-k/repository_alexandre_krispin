;;; hilit-swiftex.el -- enhancements for highlighting swiftex and doctex
;;;                      buffers using hilit19.el or hl319.el.
;;;
;;; This file was based on hilit-LaTeX.el v1.05 by Peter Galbraith
;;; (rhogee@bathybius.meteo.mcgill.ca).

;;; to do:  we need to reset the patterns explicitly whenever we
;;;         change modes, since we need to eval variables; or can we use
;;;         functions?

;;; need to distinguish doctex from swiftex.

(require 'hilit19)
(provide 'hilit-swiftex)

(hilit-set-mode-patterns
 '(doctex-mode)
 '(
   ;; outline heading
   ("^\\^\\^A\\*+.*$" nil comment)
   ;; comment
   ("^\\^\\^A.*$" nil comment)
   ;; label
   ("\\\\item\\(\\[[^]]*\\]\\)?" nil label)
   ;; formula
   ("\\\\("  "\\\\)" formula)           ; \( \)
   ("[^\\\\]\\\\\\[" "\\\\\\]" formula) ; \[ \] but not \\[len]
   ("[^$\\]\\($\\($[^$]*\\$\\|[^$]*\\)\\$\\)" 1 formula) ; '$...$' or '$$...$$'
   ;;   ^^ but not \$102.54 is expensive for a car wash.
   ;; crossref
   ("|[^|\n]*|" nil crossref)
   (hilit-bracket-region 
   ;       (\\(no\\)?cite  -> changed to any cite commands that users make up.
    "\\\\\\([^ ]*cite\\|\\(page\\)?ref\\|eqref\\|label\\|index\\|glossary\\){"
    crossref)
   ;; bold
   (hilit-bracket-wysiwyg "{\\\\bfseries" bold)
   (hilit-bracket-region "\\\\textbf{" bold)
   ;; italic
   (hilit-bracket-wysiwyg "{\\\\\\(em\\|itshape\\|slshape\\)" italic)
   (hilit-bracket-region "\\\\emph{" italic)
   (hilit-bracket-region "\\\\text\\(it\\|sl\\){" italic)
   ;; include
   (hilit-bracket-region  "\\\\\\(include\\*?\\|input\\|bibliography\\){" include)
   ;; defun
   ("\\\\ReserveCS{?\\\\[a-zA-Z-@]+" defun)
   ;; FIX, many things don't necessarily have {'s after them.
   ;; FIX: global and long;; see doctex
   (hilit-bracket-region "\\\\new\\(let\\|tokens\\|length\\|theorem\\|counter\\){" defun)
   ;; FIX want to have matching braces...
   (hilit-bracket-region "\\\\NewNameDef\\*?{" defun)
;; causes error!
   (hilit-bracket-region "\\(\\\\long\\|\\\\global\\)+\\\\def[^}]*{" defun)
   ;; keyword
   (hilit-bracket-region "\\\\\\(v\\|h\\)space\\(\*\\)?{" keyword)
   (hilit-bracket-region "\\\\text\\(tt\\|sf\\|rm\\){" keyword)
   (hilit-bracket-region "\\\\footnote\\(mark\\|text\\)?{" keyword)
   (hilit-bracket-region 
    "\\\\\\(sub\\)*\\(paragraph\\|section\\)\\(\*\\|\\[.*\\]\\)?{" keyword)
   (hilit-bracket-region 
    "\\\\\\(chapter\\|part\\)\\(\*\\|\\[.*\\]\\)?{" keyword)
   ("\\\\[a-z]+box" nil keyword)
   ;; define
   ("\\\\\\(setlength\\|settowidth\\|addtolength\\|setcounter\\|addtocounter\\)" nil define)
   (hilit-bracket-region "\\\\\\(title\\|author\\|date\\|thanks\\){" define)
   ;; decl
   ("\\\\\\(scshape\\|bfseries\\|em\\|itshape\\|rmfamily\\|sffamily\\|slshape\\|ttfamily\\)\\b" nil decl)
   ("``" "''" string)
   ("\\\\\\(raggedright\\|makeindex\\|makeglossary\\|maketitle\\)\\b" nil
    decl)
   (hilit-bracket-region 
    "\\\\\\(pagestyle\\|thispagestyle\\|pagenumbering\\|numberwithin\\){" decl)
   (hilit-bracket-region "\\\\documentclass\\(\\[.*\\]\\)?{" decl)
   (hilit-bracket-region "\\\\\\(begin\\|end\\|nofiles\\|includeonly\\){" 
                         decl)
   ("\\\\\\(normalsize\\|small\\|footnotesize\\|scriptsize\\|tiny\\|large\\|Large\\|LARGE\\|huge\\|Huge\\)\\b" nil decl)
   ("\\\\\\(smaller\\|larger\\)\\(\\[[^]]*\\]\\)?" nil decl)
   (hilit-bracket-region "\\\\relsize{"  decl)
   ("\\\\\\(appendix\\|tableofcontents\\|listoffigures\\|listoftables\\)\\b"
    nil decl)
   ;; label
   (hilit-bracket-wysiwyg "{\\\\scshape" label)
   (hilit-bracket-region "\\\\caption\\(\\[[^]]*\\]\\)?{" label)
))

;; avoid message while byte-compiling
(eval-when-compile (defvar hilit-on-the-fly-in-use))
(hilit-set-mode-patterns
 '(swiftex-mode)
 '(
   ;; outline heading
   ("^%\\*+.*$" nil comment)
   ("\\(^\\|[^\\]\\)\\(%.*\\)$" 2 comment) ; comments
   ;; label
   ("\\\\item\\(\\[[^]]*\\]\\)?" nil label)
   ;; formula
   ("\\\\("  "\\\\)" formula)           ; \( \)
   ("[^\\\\]\\\\\\[" "\\\\\\]" formula) ; \[ \] but not \\[len]
   ("[^$\\]\\($\\($[^$]*\\$\\|[^$]*\\)\\$\\)" 1 formula) ; '$...$' or '$$...$$'
   ;;   ^^ but not \$102.54 is expensive for a car wash.
   ;; crossref
   ("|[^|\n]*|" nil crossref)
   (hilit-bracket-region 
   ;       (\\(no\\)?cite  -> changed to any cite commands that users make up.
    "\\\\\\([^ ]*cite\\|\\(page\\)?ref\\|eqref\\|label\\|index\\|glossary\\){"
    crossref)
   ;; bold
   (hilit-bracket-wysiwyg "{\\\\bfseries" bold)
   (hilit-bracket-region "\\\\textbf{" bold)
   ;; italic
   (hilit-bracket-wysiwyg "{\\\\\\(em\\|itshape\\|slshape\\)" italic)
   (hilit-bracket-region "\\\\emph{" italic)
   (hilit-bracket-region "\\\\text\\(it\\|sl\\){" italic)
   ;; include
   (hilit-bracket-region  "\\\\\\(include\\*?\\|input\\|bibliography\\){" include)
   ;; defun
   ("\\\\ReserveCS{?\\\\[a-zA-Z-@]+" defun)
   ;; FIX, many things don't necessarily have {'s after them.
   ;; FIX: global and long ; ak, all messed up.
   (hilit-bracket-region 
    "\\\\\\(re\\)?new\\(environment\\|command\\|let\\)\\\\[.\n]*{" defun)
   (hilit-bracket-region "\\\\new\\(let\\|tokens\\|length\\|theorem\\|counter\\){" defun)
   ;; FIX want to have matching braces...
   (hilit-bracket-region "\\\\NewNameDef\\*?{" defun)
;; changed; see \\long in doctex...
   ;; keyword
   (hilit-bracket-region "\\\\\\(v\\|h\\)space\\(\*\\)?{" keyword)
   (hilit-bracket-region "\\\\text\\(tt\\|sf\\|rm\\){" keyword)
   (hilit-bracket-region "\\\\footnote\\(mark\\|text\\)?{" keyword)
   (hilit-bracket-region 
    "\\\\\\(sub\\)*\\(paragraph\\|section\\)\\(\*\\|\\[.*\\]\\)?{" keyword)
   (hilit-bracket-region 
    "\\\\\\(chapter\\|part\\)\\(\*\\|\\[.*\\]\\)?{" keyword)
   ("\\\\[a-z]+box" nil keyword)
   ;; define
   ("\\\\\\(setlength\\|settowidth\\|addtolength\\|setcounter\\|addtocounter\\)" nil define)
   (hilit-bracket-region "\\\\\\(title\\|author\\|date\\|thanks\\){" define)
   ;; decl
   ("\\\\\\(scshape\\|bfseries\\|em\\|itshape\\|rmfamily\\|sffamily\\|slshape\\|ttfamily\\)\\b" nil decl)
   ("``" "''" string)
   ("\\\\\\(raggedright\\|makeindex\\|makeglossary\\|maketitle\\)\\b" nil
    decl)
   (hilit-bracket-region 
    "\\\\\\(pagestyle\\|thispagestyle\\|pagenumbering\\|numberwithin\\){" decl)
   (hilit-bracket-region "\\\\documentclass\\(\\[.*\\]\\)?{" decl)
   (hilit-bracket-region "\\\\\\(begin\\|end\\|nofiles\\|includeonly\\){" 
                         decl)
   ("\\\\\\(normalsize\\|small\\|footnotesize\\|scriptsize\\|tiny\\|large\\|Large\\|LARGE\\|huge\\|Huge\\)\\b" nil decl)
   ("\\\\\\(smaller\\|larger\\)\\(\\[[^]]*\\]\\)?" nil decl)
   (hilit-bracket-region "\\\\relsize{"  decl)
   ("\\\\\\(appendix\\|tableofcontents\\|listoffigures\\|listoftables\\)\\b"
    nil decl)
   ;; label
   (hilit-bracket-wysiwyg "{\\\\scshape" label)
   (hilit-bracket-region "\\\\caption\\(\\[[^]]*\\]\\)?{" label)
))

;; define decl label keyword bold italic comment string include

;; from the hilit-src
    ;; and anotherone for LaTeX
;;    (crossref	  DarkGoldenrod	    Goldenrod	       underline)
;;    (formula	  Goldenrod	    DarkGoldenrod      underline)
;; 

;   ;; comments
;   ("[^\\]%.*$" nil comment)

;   ;; the following two match \foo[xx]{xx} or \foo*{xx} or \foo{xx}
;   ("\\\\\\(sub\\)*\\(paragraph\\|section\\)\\(\*\\|\\[.*\\]\\)?{" "}"
;    keyword)
;   ("\\\\\\(chapter\\|part\\)\\(\*\\|\\[.*\\]\\)?{" "}" keyword)
;   ("\\\\footnote\\(mark\\|text\\)?{" "}" keyword)
;   ("\\\\[a-z]+box" nil keyword)
;   ("\\\\\\(v\\|h\\)space\\(\*\\)?{" "}" keyword)

;   ;; (re-)define new commands/environments/counters
;   ("\\\\\\(re\\)?new\\(environment\\|command\\){" "}" defun)
;   ("\\\\new\\(length\\|theorem\\|counter\\){" "}" defun)

;   ;; various declarations/definitions
;   ("\\\\\\(setlength\\|settowidth\\|addtolength\\|setcounter\\|addtocounter\\)" nil define)
;   ("\\\\\\(title\\|author\\|date\\|thanks\\){" "}" define)

;   ("\\\\documentstyle\\(\\[.*\\]\\)?{" "}" decl)
;   ("\\\\\\(begin\\|end\\|nofiles\\|includeonly\\){" "}" decl)
;   ("\\\\\\(raggedright\\|makeindex\\|makeglossary\\|maketitle\\)\\b" nil
;    decl)
;   ("\\\\\\(pagestyle\\|thispagestyle\\|pagenumbering\\){" "}" decl)
;   ("\\\\\\(normalsize\\|small\\|footnotesize\\|scriptsize\\|tiny\\|large\\|Large\\|LARGE\\|huge\\|Huge\\)\\b" nil decl)
;   ("\\\\\\(appendix\\|tableofcontents\\|listoffigures\\|listoftables\\)\\b"
;    nil decl)
;   ("\\\\\\(bf\\|em\\|it\\|rm\\|sf\\|sl\\|ss\\|tt\\)\\b" nil decl)

;   ;; label-like things
;   ("\\\\item\\(\\[[^]]*\\]\\)?" nil label)
;   ("\\\\caption\\(\\[[^]]*\\]\\)?{" "}" label)

;   ;; formulas
;   ("[^\\]\\\\("  "\\\\)" formula)                   ; \( \)
;   ("[^\\]\\\\\\[" "\\\\\\]" formula)                ; \[ \]
;   ("[^\\$]\\($\\($[^$]*\\$\\|[^$]*\\)\\$\\)" 1 formula) ; '$...$' or '$$...$$'
   
;   ;; things that bring in external files
;   ("\\\\\\(include\\|input\\|bibliography\\){" "}" include)

;   ;; "wysiwyg" emphasis -- these don't work with nested expressions
;   ;; ("{\\\\\\(em\\|it\\|sl\\)" "}" italic)
;   ;; ("{\\\\bf" "}" bold)

;   ("``" "''" string)

;   ;; things that do some sort of cross-reference
;   ("\\\\\\(\\(no\\)?cite\\|\\(page\\)?ref\\|label\\|index\\|glossary\\){" "}" crossref)
;   ))

 (defun hilit-bracket-region (open)
  "Find region within curly brackets for hilit pattern.
ARG is pattern for beginning of pattern and ends with {.
It cannot be white space.  
Patterns ends simply at the matching closing bracket.

Used for patterns like:
 \\chapter{stuff with matching {} ending with }"
  (if (re-search-forward open nil t)
      (let ((here (point))
            (the-start (match-beginning 0)))
        (backward-char 1)               ; point is on bracket
        (if hilit-on-the-fly-in-use
            ;; if hl319's on-the-fly hilighting is in use then we can't use 
            ;; forward-list because it uses the built-in scan-lists and will 
            ;; return an error if there is no matching bracket yet.
            (re-search-forward "}" 
                               (progn (save-excursion (end-of-line)(point))) 
                               1)
          (forward-list 1))
        (cons the-start (point)))))

(defun hilit-inside-bracket-region (open)
  "Find region within curly brackets for hilit pattern.
ARG is pattern for beginning of pattern and ends with {.
It cannot be white space.  
Patterns ends simply at the matching closing bracket.

Used for patterns like:
 \\textbf{only stuff within bracket is highlited}"
  (if (re-search-forward open nil t)
      (let ((the-start (point)))
        (backward-char 1)               ; point is on bracket
        (if hilit-on-the-fly-in-use
            (re-search-forward "}" 
                               (progn (save-excursion (end-of-line)(point))) 1)
          (forward-list 1))
        (backward-char 1)
        (cons the-start (point)))))

(defun hilit-inside-environment (open)
  "Find region within curly brackets for hilit pattern.
ARG is pattern for \\begin{something}."
  (if (re-search-forward open nil t)
      (let ((the-start (progn (forward-line 1)(point))) ;Start on next line
            (the-end))
        (re-search-backward "\\\\begin{\\([^}]+\\)}" nil t)
        (setq the-end (buffer-substring (match-beginning 1)(match-end 1)))
        (if hilit-on-the-fly-in-use     ;Search no more than 10 lines away
            (if (search-forward (concat "\\end{" the-end "}") 
                                (save-excursion (forward-line 10)(point)) t)
                (cons the-start (progn (forward-line -1)(point)))
              (cons the-start the-start))
          (if (search-forward (concat "\\end{" the-end "}") nil t)
              (cons the-start (match-beginning 0))
            (end-of-line)               ;Move off the \begin{}
            (cons the-start the-start))))))

(defun hilit-bracket-wysiwyg (open)
  "Find region within curly brackets for hilit pattern.
ARG is pattern for beginning of pattern and starts with {.
Patterns ends simply at the matching closing bracket.

Used for patterns like:
 {\em stuff with matching {} ending with }"
  (if (re-search-forward open nil t)
      (let ((the-start (match-beginning 0)))
        (goto-char the-start)
        (if hilit-on-the-fly-in-use
            (re-search-forward "}" 
                               (progn (save-excursion (end-of-line)(point))) 1)
          (forward-list 1))
        (cons the-start (point)))))


(defvar hilit-AmSLaTeX-commands nil
  "*Set to t if you want to hightlight AmSLaTeX commands.")

(defvar hilit-multilingual-strings nil
  "*Set to t if you want to hightlight multilingual quoted strings.
Highlights:  \"`german\"', \"< french \">, << french >> and + 8-bit french ;.")

(defvar hilit-LaTeX-commands t
  "*Set to nil if you don't want to highlight unknown LaTeX commands")

(defvar hilit-on-the-fly-in-use nil
  "Used by hilit-LaTeX to determine if on-the-fly hilighting is in use")

;; I need to modify hl319.el's on-the-fly highlighter so that it
;; tells me on-the-fly highlighting is in use.
(defun hilit-rehighlight-changed-lines (st en len)
  "Quietly rehighlight just this line.
Useful as an after change hook in VM/gnus summary buffers and dired buffers."
  (save-match-data
    (let ((hilit-on-the-fly-in-use t))
      ;; (> (- en st) hilit-max-change-rehighlight)
      (hilit-rehighlight-region st en 'quietly))))

