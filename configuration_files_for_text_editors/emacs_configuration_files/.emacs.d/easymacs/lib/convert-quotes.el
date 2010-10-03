;;; convert-quotes.el --- Convert straight quotes to ``, '' and '

;; Copyright (C) 1998 by Free Software Foundation, Inc.
  
;; Author: Bill White <billw@mchsi.com>
;; Keywords: tex

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Warning: this is alpha code!  Use at your own risk!  Don your
;; asbestos longjohns!  Might eat your mail for lunch!

;; Converts:
;;   left " -> ``
;;   right " -> ''
;;   left ' -> `

;;; Code:

(defun convert-quotes ()
  "Convert quotes throughout the current buffer for proper processing
  in TeX/LaTeX."
  (interactive)
  (save-excursion
    (save-restriction
      (save-match-data
        (widen)
        ;; convert left double quotes
        (goto-char (point-min))
        (while (re-search-forward "\\(^\\|[ \t\n]+\\|(\\|<\\|{\\)\"" nil t nil)
          (replace-match "\\1``" t nil nil nil))
        ;; convert right double quotes
        (goto-char (point-min))
        (while (re-search-forward "\\([^\\\\]\\)\"" nil t nil)
          (replace-match "\\1''" t nil nil nil))
        ;; convert left single quotes
        (goto-char (point-min))
        (while (re-search-forward "\\(^\\|[ \t\n]+\\|(\\|<\\|{\\)'" nil t nil)
          (replace-match "\\1`" t nil nil nil)))))
  nil)

(provide 'convert-quotes)
