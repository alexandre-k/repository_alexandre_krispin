;;; block-comm.el --- create and enlarge C/Java block comments

;; Copyright (C) 2003, 2004 Michael Schierl

;; Author: Michael Schierl <schierlm-public@gmx.de>
;; Created: 11 August 2003
;; Keywords: java c blockcomment jde
;; Version: 0.3

(defconst blockcomment-version "0.3"
  "Version of blockcomment mode.")

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; designed as a minor mode for Java or C editing. Idea is taken from
;; Borland's JBuilder.

;; when pressing RET after a line starting with "/*",
;; the following line will be started with "* ".  If the next
;; line does not start like this (i. e. there is no end for the comment),
;; the comment is automatically closed.

;; when pressing RET inside such a multi line comment, the new line is
;; automatically prefixed with "* ".

;; when pressing RET at any other place, it acts as if
;; blockcomment-mode is not loaded. By dynamically determining which
;; action to take this even works if multiple modes try to hook RET
;; like this and you enable and disable them in some "irregular"
;; order.

;; to use this file, (auto)load it and add `turn-on-blockcomment-mode'
;; to the hook of your java/c mode.


;;; History:

;; 0.3 fixed a bug triggered when one tries to add a comment at the
;;     very beginning of a file.

;; 0.2 - changed the way RET is hooked
;;     - added autoload cookies
;;     - added jde-blockcomment-mode

;; 0.1 initial version

;;; Code:

(require 'cc-mode)

(defvar blockcomment-mode nil)
(make-variable-buffer-local 'blockcomment-mode)

(defvar  blockcomment-mode-doc-comment-generator nil
  "*Function name to be used for creating new doc comments.
This is executed when pressing RET behind the text `/**'.  Set this to e.g.
`jde-javadoc-autodoc-at-line' for use with jde")

(make-variable-buffer-local 'blockcomment-mode-doc-comment-generator)

(or (assq 'blockcomment-mode minor-mode-alist)
              (setq minor-mode-alist
                    (cons '(blockcomment-mode "/bc")
     minor-mode-alist)))

(defvar blockcomment-mode-map (make-sparse-keymap))
(setq minor-mode-map-alist (cons (cons 'blockcomment-mode
           blockcomment-mode-map)
     minor-mode-map-alist))

(define-key blockcomment-mode-map (kbd "RET") 'blockcomment-RET-hook)

;;;###autoload
(defun blockcomment-mode (arg)
  "Toggle Blockcomment mode.
With a prefix ARG, enable blockcomment mode iff arg is nonzero."
  (interactive "P")
  (setq blockcomment-mode
        (if (null arg)
            (not blockcomment-mode)
          (> (prefix-numeric-value arg) 0)))
  (force-mode-line-update)
  (if (interactive-p)
      (if blockcomment-mode
          (message "Blockcomment mode enabled")
        (message "Blockcomment mode disabled"))))

;;;###autoload
(defun jde-blockcomment-mode (arg)
  "Toggle Blockcomment mode with JDE support.
This will call `jde-javadoc-autodoc-at-line' when a new comment is
created.  With a prefix ARG, enable blockcomment mode iff arg is
nonzero."
  (interactive "P")
  (setq blockcomment-mode-doc-comment-generator 'jde-javadoc-autodoc-at-line)
  (blockcomment-mode arg))


;;;###autoload
(defun turn-on-blockcomment-mode ()
    "Turn on Blockcomment mode.
Useful for adding to a major mode hook variable.
Example:
    (add-hook 'c-mode-hook 'turn-on-blockcomment-mode)
to automatically turn on blockcomment mode when opening a C source
file."
  (blockcomment-mode 1))

;;;###autoload
(defun turn-on-jde-blockcomment-mode ()
    "Turn on Blockcomment mode with JDE support.
For more information, see `jde-blockcomment-mode'.  Useful for adding
to a major mode hook variable.
Example:
    (add-hook 'jde-mode-hook 'turn-on-blockcomment-mode)

to automatically turn on jde blockcomment mode when opening a JDE source
file."
  (jde-blockcomment-mode 1))

(defun blockcomment-RET-hook (arg)
  "Run when user presses return.
Checks if current line is part of a block comment and enlarges/closes
it in that case.  With a prefix ARG, no special actions are done."
  (interactive "*P")
  (cond
   (arg
    (let ((blockcomment-mode nil))
      (call-interactively (key-binding (kbd "RET")))))
   ((> (point) (point-min))
    (let ((newtag nil) (intag nil) pp pps (plainnewtag nil) (pntpos nil))
      (save-excursion
 (setq pp (point))
 (skip-chars-backward "^\n")
 (setq pps (buffer-substring-no-properties (point) pp))
 (unless (string-match "\\*\\/" pps)
   (skip-chars-forward " \t")
   (cond
    ((>= (point) pp) nil)
    ((looking-at "\\/\\*")
     (skip-chars-forward "^\n")
     (skip-chars-forward "\n")
     (skip-chars-forward " \t")
     (if (looking-at "\\*")
  (setq intag t)
       (setq newtag t)))
    ((looking-at "\\*\\/") nil)
    ((looking-at "\\*")
     (setq intag 1)
     (while (number-or-marker-p intag)
       (skip-chars-backward " \t")
       (skip-chars-backward "\n")
       (skip-chars-backward "^\n")
       (skip-chars-forward " \t")
       (cond
        ((looking-at "\\/\\*")
  (setq intag t))
        ((looking-at "[^*]")
  (setq intag nil))))))))
      (let ((blockcomment-mode nil))
 (call-interactively (key-binding (kbd "RET"))))
      (when intag
 (insert "* ")
 (c-indent-command))
      (when newtag
 (save-excursion
   (skip-chars-backward " \t")
   (goto-char (- (point) 4)) ; don't forget the RET
   (setq plainnewtag (looking-at "\\/\\*\\*\n"))
   (skip-chars-backward " \t")
   (when (and (not (bobp))
       (string= "\n"
         (buffer-substring-no-properties (1- (point))
             (point))))
     (goto-char (1- (point))))
   (setq pntpos (point)))
 (cond
  ((and plainnewtag blockcomment-mode-doc-comment-generator)
   (save-excursion
     (delete-region pntpos (point))
     (forward-line 1)
     (call-interactively blockcomment-mode-doc-comment-generator))
   (forward-line 2)
   (end-of-line))
  (t
   (insert "* ")
   (c-indent-command)
   (save-excursion
     (skip-chars-forward "^\n")
     (insert "\n*/")
     (c-indent-command)))))))
   (t
    (let ((blockcomment-mode nil))
      (call-interactively (key-binding (kbd "RET")))))))

(provide 'block-comm)
