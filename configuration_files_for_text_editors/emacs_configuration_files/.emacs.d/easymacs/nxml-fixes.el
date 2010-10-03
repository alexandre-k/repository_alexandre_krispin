;; A place to put fixes and enhancements contributed to nxml by users.
;; See also nxml-mode-os-additions.el

;; From Lennart

(defun nxml-indent-line ()
   "Indent current line as XML."
   (let ((indent (nxml-compute-indent))
	(from-end (- (point-max) (point))))
     (when indent
       (beginning-of-line)
       (let ((bol (point)))
	(skip-chars-forward " \t")
         (unless (= (current-column) indent)
           (delete-region bol (point))
           (indent-to indent)))
       (when (> (- (point-max) from-end) (point))
	(goto-char (- (point-max) from-end))))))

(defun drkm-nxml:ancestor-axis-path (&optional print-message)
  "Return all the elements in the ancestor axis of the current
    element.  If called interactively, show it in the echo area."
  (interactive "p")
  (nxml-ensure-scan-up-to-date)
  (let ((path ""))
    (save-excursion
      (condition-case ()
          (while t
            (nxml-backward-up-element)
            (setq path (concat
                        "/" (xmltok-start-tag-qname) path)))
        (error nil)))
    (when print-message
      (message "%s" path))
    path))



(provide 'nxml-fixes)