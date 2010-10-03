;; We override the use of Unicode characters, and just use plain ascii
;; instead.  Most of these are close to what the hyperspec does with
;; these chars.

;; Append this to the end of dpans2texi.el before running "make".

(setq dp-tr-alist
  '(("Õ" . "==")
    ("Ö" . ">>")
    ("Ø" . "_0") ("Ù" . "_9") ("Ú" . "_8") ("Û" . "_7") ("Ü" . "_6")
    ("Ý" . "_5") ("Þ" . "_4") ("ß" . "_3") ("à" . "_2") ("á" . "_1")
    ("â" . "/=") ("ã" . "<=")
    ("ä" . ":")  ("å" . "oh") ("æ" . "ee") ("ç" . "ay") ("è" . "uh") ("é" . "'")
    ("ê" . "'") ("ë" . "*") ("ì" . "alpha") ("í" . "epsilon") ("î" . "pi") ("ï" . "delta")
    ("ð" . "") ("ñ" . ">") ("ò" . "<") ("ó" . "<=") ("õ" . "U") ("ö" . ">=")
    ("÷" . "<element of>") ("ø" . "]]") ("ù" . "[[") ("ú" . "+") ("û" . "") ("ô" . "<Newline>")
    ("ý" . "=>") ("þ" . "'") ("ÿ" . "`")))

(defun dp-tr ()
  "Map 8bit values in Info files to multibyte chars."
  (interactive)
  (let ((re (concat "[" (mapconcat (lambda (x) (car x)) dp-tr-alist "")
		    "]"))
	case-fold-search)
    (dolist (file (directory-files default-directory nil "ansicl-?[0-9]*$"))
      (with-temp-buffer
	(let ((coding-system-for-read 'latin-1))
	  (insert-file-contents file))
	(dp-pm)
	(while (re-search-forward re nil t)
	  (replace-match (cdr (assoc (match-string 0)
				   dp-tr-alist)) t t))
	(let ((coding-system-for-write 'us-ascii))
	  (write-region (point-min) (point-max) file))))))
