;;; itemize.el -- 

; changed + to * to allow items w/o whitespace in front - pjh
(defvar it-item-begin-regexp
  "^[ \t]*\\([-*]\\|[a-zA-Z0-9]\\([.)]\\|\\.)\\)\\|([a-zA-Z0-9])\\)[ \t]+"
  "Regexp matching the beginning of an item.
Matches stuff like
  - itemtext...
  * itemtext...
  1. itemtext...
  1) itemtext...
  1.) itemtext...
  (1) itemtext...
  a. itemtext
  a) itemtext
  a.) itemtext
  (a) itemtext")

(defun show-it-match ()
  (interactive)
  (re-search-forward it-item-begin-regexp)
  (message "%s" (buffer-substring (match-beginning 0) (match-end 0))))

(defvar it-itemize-beginnning nil
  "Beginning of an itemize. To be instantiated for LaTeX or troff -ms.")

(defvar it-itemize-beginning-LaTeX "\\begin{itemize}\n"
  "Beginning of an itemize for LaTeX.")

(defvar it-itemize-beginnning-troff "\n"
  "Beginning of an itemize for troff -ms (quite simple).")

(defvar it-itemize-end nil
  "End of an itemize. To be instantiated for LaTeX or troff -ms.")

(defvar it-itemize-end-LaTeX "\\end{itemize}"
  "End of an itemize for LaTeX.")

;; some people would probably prefer .PP
(defvar it-itemize-end-troff ".LP"
  "End of an itemize for troff -ms.")

(defvar it-item-beginning-LaTeX "\\item "
  "Beginning of an item for LaTeX.")

(defvar it-item-beginning-troff ".IP \\(bu\n"
  "Beginning of an item for troff -ms.")

(defvar it-item-beginning nil
  "Beginning of an item. To be instantiated for LaTeX or troff -ms.")

(defun itemize-region (begin end &optional remove-blanks)
  "Perform itemization between BEGIN and END with the current bindings of
it-itemize-beginning, it-item-begin-regexp, and it-itemize-end.
To be called by LaTeX-itemize-region or troff-itemize-region.
If optional argument REMOVE-BLANKS is non-nil, remove leading whitespace
from all lines."
  (save-restriction
    (save-excursion
      (narrow-to-region begin end)
      (goto-char (point-min))
      (insert it-itemize-beginning)
      (while (re-search-forward it-item-begin-regexp (point-max) 'move)
	(replace-match it-item-beginning t t))
      (insert it-itemize-end)
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]+" (point-max) t)
	(replace-match "" t t)))))

(defun LaTeX-itemize-region (begin end)
  "Process region between point and mark for LaTeX's \"itemize\".
Put \"\\begin{itemize}\\n\" at the beginning, \"\\end{itemize}\\n\" at the end
and replace item beginnings matching it-item-begin-regexp with \"\\item \"."
  (interactive "r")
  (let ((it-itemize-beginning it-itemize-beginning-LaTeX)
	(it-itemize-end it-itemize-end-LaTeX)
	(it-item-beginning it-item-beginning-LaTeX))
    (itemize-region begin end)))

(defun troff-itemize-region (begin end)
  "Process region between point and mark as a number of .IP's for troff.
Replaces item beginnings matching it-item-begin-regexp with \".IP\\n\"."
  (interactive "r")
  (let ((it-itemize-beginning it-itemize-beginnning-troff)
	(it-itemize-end it-itemize-end-troff)
	(it-item-beginning it-item-beginning-troff))
    (itemize-region begin end t)))
