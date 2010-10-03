;; easymacs-slime.el: integrating easymacs with slime

;; Here are some convenience functions for running Slime, the Common
;; Lisp IDE for Emacs.  These used to be included with Easymacs, but
;; then I decided that it would be better to focus Easymacs on the
;; non-technical user.

;; Copyright (C) 2003-5 Peter Heslin
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Massachusettes Ave,
;; Cambridge, MA 02139, USA.


  (defun easymacs-slime-start ()
    (interactive)
    (unless (slime-connected-p)
      (when (yes-or-no-p "Start Slime? ")
        (require 'slime)
        (slime-setup)
        (if slime-lisp-implementations
            (let ((slime-default-lisp
                   (intern-soft
                    (completing-read "Lisp to use: " slime-lisp-implementations))))
              (save-excursion
                (slime)))
            (let* ((choices
                    (loop for lisp in
                         '("sbcl" "clisp" "cmucl" "openmcl" "lispworks" "alisp")
                       when (executable-find lisp)
                       collect lisp))
                   (inferior-lisp-program
                    (if (executable-find inferior-lisp-program)
                        inferior-lisp-program
                        (completing-read "Lisp to use: " choices))))
              (save-excursion
                (slime)))))))

  (add-hook 'lisp-mode-hook 'easymacs-slime-start) 

  ;; http://www.ifa.au.dk/~harder/dpans.html
  (require 'info-look)
  (info-lookup-add-help
   :mode 'lisp-mode
   :regexp "[^][()'\" \t\n]+"
   :ignore-case t
   :doc-spec '(("(ansicl)Symbol Index" nil nil nil)))

  (defun easymacs-lisp-help ()
    (interactive)
    (call-interactively 'info-lookup-symbol)
    (switch-to-buffer "*info*")
    (delete-other-windows))
  
  (defun easymacs-slime-usual-buffers ()
    "Switches from a lisp buffer to the REPL, and from there to
the debugger, if it exists.  If it doesn't exist, or if in the
debugger, switches to the most recent lisp buffer."
    (interactive)
    (switch-to-buffer
     (case major-mode
       ('lisp-mode
        (slime-output-buffer))
       ('slime-repl-mode
        (if (sldb-get-default-buffer)
            (sldb-get-default-buffer)
            (slime-recently-visited-buffer 'lisp-mode)))
       ('sldb-mode
        (slime-recently-visited-buffer 'lisp-mode)))))
  
  (defun easymacs-slime-mode-hook ()
    ;; Slime doesn't like local-set-key
    (define-key slime-mode-map (kbd "<return>") 'newline-and-indent)
    (define-key slime-mode-map (kbd "C-j") 'newline)
    (define-key slime-mode-map (kbd "<S-return>") 'slime-complete-symbol)
    (define-key slime-mode-map (kbd "<C-S-return>") 'slime-complete-form)
    (define-key slime-mode-map (kbd "<S-f1>") 'easymacs-lisp-help)
    (define-key slime-mode-map (kbd "M-q") 'slime-reindent-defun)

    (define-key slime-mode-map (kbd "<f12>") 'slime-eval-defun)
    (define-key slime-mode-map (kbd "<S-f12>") 'slime-compile-defun)
    (define-key slime-mode-map (kbd "<M-f12>") 'slime-eval-buffer)
    (define-key slime-mode-map (kbd "<M-S-f12>") 'slime-compile-and-load-file)
    (define-key slime-mode-map (kbd "<C-f12>") 'slime-macroexpand-1)
    (define-key slime-mode-map (kbd "<C-S-f12>") 'slime-macroexpand-all)

    (define-key slime-mode-map (kbd "<f11>") 'slime-eval-last-expression)
    (define-key slime-mode-map (kbd "<S-f11>") 'slime-interactive-eval)
    (define-key slime-mode-map (kbd "<M-f11>") 'slime-eval-last-expression-display-output)
    (define-key slime-mode-map (kbd "<M-S-f11>") 'slime-pprint-eval-last-expression)
    (define-key slime-mode-map (kbd "<C-f11>") 'slime-inspect)
    (define-key slime-mode-map (kbd "<C-S-f11>") 'slime-describe-symbol)

    (define-key slime-mode-map (kbd "<f10>") 'easymacs-slime-usual-buffers)
    (define-key slime-mode-map (kbd "<S-f10>") 'slime-selector)

                                        ; unsure about these
    (define-key slime-mode-map (kbd "<M-f10>") 'check-parens)
    (define-key slime-mode-map (kbd "<M-S-f10>") 'slime-sync-package-and-default-directory)
    (define-key slime-mode-map (kbd "<C-f10>") 'slime-highlight-edits-mode)
    (define-key slime-mode-map (kbd "<C-S-f10>") 'slime-toggle-trace-fdefinition)

    (setq lisp-indent-function 'common-lisp-indent-function)
    )
;  (add-hook 'slime-mode-hook (lambda () (slime-autodoc-mode t)))
  (add-hook 'slime-mode-hook 'easymacs-slime-mode-hook)

  (defun easymacs-slime-repl-mode-hook () 
    (define-key slime-repl-mode-map (kbd "<S-f1>") 'easymacs-lisp-help)
    (define-key slime-repl-mode-map (kbd "<f10>")  'easymacs-slime-usual-buffers))
;  (add-hook 'slime-repl-mode-hook (lambda () (slime-autodoc-mode t)))
  (add-hook 'slime-repl-mode-hook 'easymacs-slime-repl-mode-hook)

  (defun easymacs-sldb-mode-hook () 
    (define-key sldb-mode-map (kbd "<f10>")  'easymacs-slime-usual-buffers))
  (add-hook 'sldb-mode-hook 'easymacs-sldb-mode-hook)

  ;; From cldoc.el
  (autoload 'turn-on-cldoc-mode "cldoc" nil t)
  (add-hook 'lisp-mode-hook 'turn-on-cldoc-mode)
  
  (add-hook 'slime-repl-mode-hook
            #'(lambda ()
                (turn-on-cldoc-mode)
                (define-key slime-repl-mode-map " " nil)))
  (add-hook 'slime-mode-hook
            #'(lambda () (define-key slime-mode-map " " nil)))
  (setq slime-use-autodoc-mode nil)

