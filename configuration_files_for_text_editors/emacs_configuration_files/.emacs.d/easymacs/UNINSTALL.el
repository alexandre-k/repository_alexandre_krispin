;;; UN-INSTALLING EASYMACS

;; Open this file in Emacs, and in the "Emacs-Lisp" menu above, select
;; the command called "Evaluate Buffer".  
;;
;; This will comment out the line in your Emacs initialization file
;; that loads Easymacs, and then Emacs will quit.  You can then remove
;; the Easymacs folder.  The next time Emacs starts up, it will be
;; without Easymacs.

;; Emacs code below

(let* ((easymacs-dir (file-name-directory (or load-file-name
                                              buffer-file-name)))
       (init-file (or user-init-file "~/_emacs.el"))
       ;; Evade find-file advice
       (init-buffer (find-file-noselect init-file)))
  (with-current-buffer init-buffer
    (goto-char (point-min))
    (while (re-search-forward
           (concat "^(load \".*easymacs\\.el\")$") nil t)
      (beginning-of-line)
      (insert ";")
      (forward-line))
    (save-buffer)))
  
(save-buffers-kill-emacs)
