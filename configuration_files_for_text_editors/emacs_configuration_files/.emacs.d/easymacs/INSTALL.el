;;; INSTALLING EASYMACS

;; In order to install Easymacs, all you need to do are these three
;; steps (if you are looking at this file within Emacs, then you may
;; already have done the first two):
;; 
;; 1) Make sure that the easymacs.zip file is extracted somewhere on
;;    your hard drive.
;;
;; 2) Start emacs, and open up the file you are now looking at (you
;;   may already have done this).  To open a file in Emacs, look in
;;   the "File" menu.
;;
;; 3) In the "Emacs-Lisp" menu above, select the command called
;;    "Evaluate Buffer".
;;
;; That's all there is to it.  From now on, Easymacs will start up
;; automatically whenever you run Emacs.
;;
;; --------------------------------------------------------------
;;
;; Further info:
;;
;; Instead of selecting "Evaluate Buffer" from the Emacs-Lisp menu,
;; you can also type Alt-x, then "eval-buffer", then hit Return.
;;
;; This file simply creates or modifies an emacs configuration file in
;; the folder that Emacs regards as your Home folder.  If a
;; configuration file already exists, then it is modified to append a
;; line to it that looks like this:
;;
;; (load "path/to/easymacs/folder/easymacs.el")


;; Emacs code below

(let* ((easymacs-dir (file-name-directory (or load-file-name
                                              buffer-file-name)))
       (easymacs-file (concat easymacs-dir "easymacs.el"))
       (load-line (concat "(load \"" easymacs-file "\")"))
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
    (goto-char (point-max))
    (insert (concat "\n\n" load-line "\n"))
    (save-buffer))
  (kill-buffer nil)
  
  (unless (and (boundp 'easymacs-loaded) easymacs-loaded)
    (load easymacs-file)))
  
