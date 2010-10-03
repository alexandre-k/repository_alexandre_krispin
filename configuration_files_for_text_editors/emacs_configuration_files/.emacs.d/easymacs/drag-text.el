;; This code is adapted from a file posted to the emacs-devel mailing
;; list by Martin Rudalics, with the following info:

;; ------------------------------------------------------------------

;;; m&d.el --- move'n drag support functions

;; Copyright (C) 2005 Martin Rudalics

;; Time-stamp: "2007-02-26 07:56:15 martin"
;; Author: Martin Rudalics <rudalics@gmx.at>
;; Keywords: sexps
;; Version: 0.1

;; m&d.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; m&d.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; ------------------------------------------------------------------

;; I have just taken bits and pieces since the whole m&d package is
;; too complex for my needs.  Word-oriented functions added at the end

(defun easymacs-ensure-mark ()
  (when mark-active (setq deactivate-mark nil)))

;; was m&d-drag-char-right
(defun easymacs-drag-char-right ()
  "If region is active drag it right by one char else drag char
at point right."
  (interactive)
  (cond
   (mark-active
    (let* ((beg (region-beginning))
	   (end (region-end))
	   (mark-beg (1+ (- (mark) beg)))
	   (point-beg (1+ (- (point) beg))))
      (if (= end (point-max))
	  (progn (message "Can't drag text") (ding))
	(transpose-regions beg end end (1+ end))
	(set-mark (+ mark-beg beg))
	(setq deactivate-mark nil)
        (goto-char (+ point-beg beg)))))
   ((or (eobp) (= (point) (1- (point-max))))
    (easymacs-ensure-mark)
    (message "Can't drag text") (ding))
   (t (let ((to (1+ (point))))
	(transpose-regions (point) to to (1+ to))
	(goto-char to)))))

;; was m&d-drag-char-left
(defun easymacs-drag-char-left ()
  "If region is active drag it left by one char else drag char at point left."
  (interactive)
  (cond
   (mark-active
    (let* ((beg (region-beginning))
	   (end (region-end))
	   (mark-beg (- (mark) beg 1))
	   (point-beg (- (point) beg 1)))
      (if (= beg (point-min))
	  (progn (message "Can't drag text") (ding))
	(transpose-regions (1- beg) beg beg end)
	(set-mark (+ mark-beg beg))
	(setq deactivate-mark nil)
        (goto-char (+ point-beg beg)))))
   ((or (bobp) (eobp))
    (easymacs-ensure-mark)
    (message "Can't drag text") (ding))
   (t (let ((to (1- (point))))
	(transpose-regions to (point) (point) (1+ (point)))
	(goto-char to)))))

;; was m&d-drag-line-down
(defun easymacs-drag-line-down ()
  "Drag region down by one line. Region defaults to current line.
Region is always rounded up to whole lines."
  (interactive)
  (let* ((beg (save-excursion
		(when mark-active
                  (goto-char (region-beginning)))
		(line-beginning-position)))
	 (end (save-excursion
                (when mark-active
                  (goto-char (region-end)))
		(line-beginning-position 2)))
	 (to (save-excursion
	       (goto-char end)
	       (line-beginning-position 2)))
	 (recenter (when (= beg (window-start))
		     (1+ (count-lines beg end)))))
    (unless (and (>= (point) beg) (< (point) end))
      ;; `point' should be within dragged region.
      (goto-char (1- end)))
    (if (> to end)
        (progn
          (if (save-excursion (goto-char to) (not (bolp)))
              ;; Pobably at eob.
              (transpose-regions beg (1- end) end to)
            (transpose-regions beg end end to))
          (easymacs-ensure-mark)
          (when recenter
            (recenter recenter)))
      (easymacs-ensure-mark)
      (message "Can't drag text")
      (ding))))


;; was m&d-drag-line-up
(defun easymacs-drag-line-up ()
  "Drag region up by one line. Region defaults to current line.
Region is always rounded up to whole lines."
  (interactive)
  (let* ((beg (save-excursion
		(when mark-active
                  (goto-char (region-beginning)))
		(line-beginning-position)))
	 (end (save-excursion
                (when mark-active
                  (goto-char (region-end)))
                (line-beginning-position 2)))
	 (from (save-excursion
		 (goto-char beg)
		 (line-beginning-position 0)))
	 (recenter (when (= from (window-start))
		     (count-lines beg end))))
	(if (> beg from)
	    (progn
	      (if (save-excursion (goto-char end) (not (bolp)))
		  ;; Probably at eob.
		  (transpose-regions from (1- beg) beg end)
		(transpose-regions from beg beg end))
              (easymacs-ensure-mark)
	      (when recenter
		(recenter recenter)))
	  (easymacs-ensure-mark)
	  (message "Can't drag text")
	  (ding))))


;; New functions

(defun easymacs-drag-word-right ()
  "Drag region right by one word. Region defaults to current word.
Region is always rounded up to whole words."
  (interactive)
    (let* ((beg (save-excursion
                  (when mark-active
                    (goto-char (region-beginning)))
                  (skip-syntax-backward "^ ")
                  (point)))
           (end (save-excursion
                  (when mark-active
                    (goto-char (region-end)))
                  (skip-syntax-forward "^ ")
                  (point)))
           (word-beg (save-excursion
                       (goto-char end)
                       (skip-syntax-forward " ")
                       (point)))
           (word-end (save-excursion
                       (goto-char word-beg)
                       (skip-syntax-forward "^ ")
                       (point))))
      (if (or (= end (point-max))
              (= word-end (point-max)))
          (progn (message "Can't drag text") (ding))
        (if (= (point) end)
            (progn
              (backward-char)
              (transpose-regions beg end word-beg word-end)
              (forward-char))
          (transpose-regions beg end word-beg word-end)))
      (easymacs-ensure-mark)))
        
(defun easymacs-drag-word-left ()
  "Drag region left by one word. Region defaults to current word.
Region is always rounded up to whole words."
  (interactive)
  (let* ((end (save-excursion
                (when mark-active
                  (goto-char (region-end)))
                (skip-syntax-forward "^ ")
                (point)))
         (beg (save-excursion
                (when mark-active
                  (goto-char (region-beginning)))
                (skip-syntax-backward "^ ")
                (point)))
         (word-end (save-excursion
                     (goto-char beg)
                     (skip-syntax-backward " ")
                     (point)))
         (word-beg (save-excursion
                     (goto-char word-end)
                     (skip-syntax-backward "^ ")
                     (point))))
    (if (= beg (point-min))
        (progn (message "Can't drag text") (ding))
      (if (= (point) end)
          (progn
            (backward-char)
            (transpose-regions beg end word-beg word-end)
            (forward-char))
        (transpose-regions beg end word-beg word-end)))
    (easymacs-ensure-mark)))
        
(global-set-key (kbd "<M-C-up>")        'easymacs-drag-line-up)
(global-set-key (kbd "<M-C-down>")      'easymacs-drag-line-down)

(global-set-key (kbd "<M-C-left>")      'easymacs-drag-word-left)
(global-set-key (kbd "<M-C-right>")     'easymacs-drag-word-right)

(global-set-key (kbd "<M-C-S-left>")      'easymacs-drag-char-left)
(global-set-key (kbd "<M-C-S-right>")     'easymacs-drag-char-right)
