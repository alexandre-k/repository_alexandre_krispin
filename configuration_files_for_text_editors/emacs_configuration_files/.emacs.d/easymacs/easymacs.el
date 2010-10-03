;;; Easymacs
;;
;; easymacs.el is an easy to learn configuration for Emacs.
;; 
;; Author: Peter Heslin <p.j.heslin@dur.ac.uk>
;; Maintainer: Peter Heslin <p.j.heslin@dur.ac.uk>
;; Location: 
;; Keywords: Emacs configuration cua-mode
;; 
;; Copyright (C) 2003--2007 Peter Heslin
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

;(setq my-start-time (current-time))

(defvar easymacs-version "2.0")

(defgroup easymacs nil
  "Some settings for the easymacs configuration"
  :group 'emacs
  :prefix "easymacs-")

(defcustom easymacs-bind-numeric-keypad nil
  "Whether to bind the keys on the numeric keypad to some 
  commonly used functions when the numlock light is off."
  :type 'boolean
  :group 'easymacs)

(defcustom easymacs-favorite-font nil
  "Hack to save the currently selected font and load it again
when Emacs re-starts."
  :type 'boolean
  :group 'easymacs)

(defcustom easymacs-auto-compile nil
  "If non-nil, the absence of easymacs.elc automatically triggers
  a compilation of all lisp files in the easymacs distribution.
  Useful if you are reinstalling frequently."
  :type 'boolean
  :group 'easymacs)

(defcustom easymacs-mac-standard-modifiers t
  "If non-nil, use the command key for Emacs control, and the
  control key for meta, which is somewhat like normal Mac usage.
  If nil, use the control key for Emacs control, and the command
  key for Alt, which is more suitable for people with fingers
  trained on a PC keyboard."
  :type 'boolean
  :group 'easymacs)

(unless (string-match "^22\\|^23\\|24\\|25" emacs-version)
  (error "This version of Emacs is too old to run Easymacs; aborting.
Please upgrade to at least GNU Emacs version 22."))

(defvar easymacs-bug-address "easymacs-user@lists.sourceforge.net")
(setq report-emacs-bug-address easymacs-bug-address)
(setq report-emacs-bug-pretest-address easymacs-bug-address)

(defvar easymacs-loaded nil)
(defvar easymacs-dir nil)

(defun easymacs-init-hook ()
  (when easymacs-favorite-font
    (set-default-font easymacs-favorite-font)))

(add-hook 'after-init-hook 'easymacs-init-hook)

(defun easymacs-save-font ()
  (interactive)
  (customize-set-variable 'easymacs-favorite-font
                          (cdr (assq 'font (frame-parameters))))
  (customize-save-customized))

(defun easymacs-customize ()
  (interactive)
  (customize-group 'easymacs))

(defun easymacs-keypad-enable ()
  (interactive)
  (customize-set-variable 'easymacs-bind-numeric-keypad t)
  (customize-save-customized))

(defun easymacs-keypad-disable ()
  (interactive)
  (customize-set-variable 'easymacs-bind-numeric-keypad nil)
  (customize-save-customized))

(defun easymacs-keypad-toggle ()
  (interactive)
  (if easymacs-bind-numeric-keypad
      (progn
        (easymacs-keypad-disable)
        (message "Numeric keypad set to numbers."))
    (easymacs-keypad-enable)
    (message "Numeric keypad set to convenience commands.")))

(defun easymacs-keypad-show ()
  (interactive)
  (message
   (if easymacs-bind-numeric-keypad
       "Numeric keypad currently set to convenience commands."
     "Numeric keypad currently set to numbers.")))

(defvar easymacs-w32-system
  (memq system-type '(emx win32 w32 mswindows ms-dos windows-nt))
  "*Non-nil means Emacs is running on Windows 9x/NT.")

(defvar easymacs-mac-system
  (equal window-system 'mac)
  "Non-nil means running on a mac")
  
;;;;;;;;;; Setup load-path to include subdirs of this file's dir
(defun easymacs-setup-paths ()


  (setq easymacs-dir (file-name-directory
                   (or load-file-name
                       buffer-file-name)))
  (add-to-list 'load-path easymacs-dir)
  (add-to-list 'load-path (concat easymacs-dir "lib/"))
  (add-to-list 'load-path (concat easymacs-dir "auctex/"))
  (add-to-list 'load-path (concat easymacs-dir "auctex/preview/"))
  (add-to-list 'load-path (concat easymacs-dir "auctex/preview/images/"))
  (add-to-list 'load-path (concat easymacs-dir "dictionary/"))
  (add-to-list 'load-path (concat easymacs-dir "nxml-mode/"))
  (add-to-list 'load-path (concat easymacs-dir "w3/lisp/"))
 
  (setq exec-path (cons (concat easymacs-dir "bin/") exec-path))

  (if easymacs-w32-system
      (setenv "PATH" (concat easymacs-dir "bin;" (getenv "PATH")))
    (setenv "PATH" (concat easymacs-dir "bin/:" (getenv "PATH"))))

  (when easymacs-mac-system
    ;; Fink, DarwinPorts, TeX, etc.

    (setenv "PATH"
            (concat
             (getenv "PATH")
             ":/usr/local/bin:/usr/local/teTeX/bin/powerpc-apple-darwin-current:/sw/bin:/opt/local/bin"))
    (setq exec-path (append exec-path
                            '("/usr/local/bin" 
                              "/usr/local/teTeX/bin/powerpc-apple-darwin-current"
                              "/sw/bin"
                              "/opt/local/bin")))

      ;; Deal with modifiers
      (setq mac-command-key-is-meta t)
      (defun easymacs-mac-standard-modifiers ()
        (setq mac-reverse-ctrl-meta t))
      (defun easymacs-mac-pc-style-modifiers ()
        (setq mac-reverse-ctrl-meta nil))

      (setq mac-allow-anti-aliasing t
            mac-emulate-three-button-mouse t)
      

      ;; custom settings may not be loaded until after this file
      (add-hook 'emacs-startup-hook
                '(lambda ()
                   (if easymacs-mac-standard-modifiers
                       (easymacs-mac-standard-modifiers)
                     (easymacs-mac-pc-style-modifiers))))
      
      (defun easymacs-mac-swap-modifiers ()
        (interactive)
        (if easymacs-mac-standard-modifiers
            (progn
              (easymacs-mac-pc-style-modifiers)
              (customize-set-variable 'easymacs-mac-standard-modifiers nil)
              (customize-save-customized)
              (message "Now using PC-style modifier keys."))
          (easymacs-mac-standard-modifiers)
          (customize-set-variable 'easymacs-mac-standard-modifiers t)
          (customize-save-customized)
          (message "Now using standard Mac modifier keys."))))

  (require 'info)
  (info-initialize)
  (add-to-list 'Info-directory-list
               (concat easymacs-dir "auctex/doc/"))
  (add-to-list 'Info-directory-list
               (concat easymacs-dir "docs/"))
  (add-to-list 'Info-directory-list
               (concat easymacs-dir "w3/texi/"))
  
  (setq TeX-lisp-directory (concat easymacs-dir "auctex/"))
  (setq TeX-data-directory (concat easymacs-dir "auctex/"))
  (setq TeX-auto-global (concat easymacs-dir "auctex/var/"))

  (defun easymacs-byte-compile-directory (dir)
    "Recursively byte-compile all .el files in DIR and its subdirs"
    (let ((files (directory-files dir t)))
      (while files
        (unless (string-match "^\\.\\|^_" (file-name-nondirectory (car files)))
          (if (file-directory-p (car files))
              (easymacs-byte-compile-directory (car files))
            (when (and (string-match "\\.el$" (car files))
                       (not (string-match "prv-xemacs\\.el$" (car files))))
              (byte-compile-file (car files)))))
        (setq files (cdr files)))))

  (defun easymacs-remove-byte-compile-directory (dir)
    "Recursively remove all .elc files in DIR and its subdirs"
    (let ((files (directory-files dir t)))
      (while files
        (unless (string-match "^\\.\\|^_" (file-name-nondirectory (car files)))
          (if (file-directory-p (car files))
              (easymacs-remove-byte-compile-directory (car files))
            (when (string-match "\\.elc$" (car files))
              (delete-file (car files)))))
        (setq files (cdr files)))))

  (defun easymacs-remove-byte-compiled-files ()
    (interactive)
    (easymacs-remove-byte-compile-directory easymacs-dir))

  (defun easymacs-compile ()
    (interactive)
    ;; Just in case the recursive compile bombs out somewhere
    (byte-compile-file (concat easymacs-dir "easymacs.el"))
    (easymacs-byte-compile-directory easymacs-dir)
    (delete-other-windows)
    (bury-buffer "*Compile-Log*"))
  
  ;; Maybe byte-compile all libraries on first loading
  (when (and easymacs-auto-compile
             (not (file-exists-p (concat easymacs-dir "easymacs.elc"))))
    (easymacs-compile))


  (defun easymacs-make-dist ()
    (interactive)
    (save-some-buffers)
    (easymacs-remove-byte-compiled-files)
    (cd easymacs-dir)
    (when (file-exists-p (concat easymacs-dir "easymacs-bin"))
      (delete-file (concat easymacs-dir "easymacs-bin")))
    (cd "..")
    (start-process "zip" "*zip*"
                   "zip" "-r" "easymacs.zip" "easymacs" "-x" "*_darcs*"))

  (when easymacs-w32-system
    (let ((paths '("c:/Cygwin/bin" "d:/Cygwin/bin"
                   "c:/Program Files/Aspell/bin"
                   "d:/Program Files/Aspell/bin" )))
      (dolist (path paths)
        (when (file-directory-p path)
          (add-to-list 'exec-path path)))))

  
  (defun easymacs-make-html-docs ()
    (interactive)
    (cd (concat easymacs-dir "docs"))
    (find-file "./easymacs.texi")
    (makeinfo-compile
     (concat makeinfo-run-command " --html " buffer-file-name)
     t nil)
    (sit-for 3)
    (dolist (file (directory-files (concat easymacs-dir "docs/easymacs") t ".html$"))
      (find-file-literally file)
      (when (re-search-forward "<table summary=\"\">" nil t)
        (replace-match "<table border=1 cellpadding=10px"))
      (goto-char (point-min))
      (while (re-search-forward
              "^<p><br></td></tr><tr align=\"left\"><td valign=\"top\" width=\"20%\"><br></td></tr>"
              nil t)
        (replace-match "</p><p>"))
      (save-buffer)
      (kill-buffer nil))
    (start-process "zip" "*zip*"
                   "zip" "-r" "easymacs-doc.zip" "easymacs" ))

  (defun easymacs-make-pdf-docs ()
    (interactive)
    (cd (concat easymacs-dir "docs"))
    (find-file "./easymacs.texi")
    (shell-command 
     (concat "pdftex " buffer-file-name)))

  ;; Doesn't work on OS X, where you have to dump from an undumped
  ;; Emacs
  (defun easymacs-dump ()
    (interactive)
    (start-process-shell-command "Dump" "*Dump*"
                                 (concat invocation-directory invocation-name)
                                 "--no-site-file -q --batch"
                                 "--load" (concat "'" easymacs-dir "preloads.el" "'")
                                 "--execute '(dump-emacs"
                                 (concat "\"" easymacs-dir "easymacs-bin" "\"")
                                 (concat "\"" invocation-directory invocation-name "\"") ")'"))
  
  )


      
;;;;;;;;;; General Settings
(defun easymacs-general-settings ()

  ;; Makes debugging easier
  (setq message-log-max 1000)

  (setq inhibit-startup-message t)
  (setq inhibit-splash-screen t)
  (setq initial-scratch-message nil)

  ;; set current buffer's filename, and full path in titlebar
  (setq frame-title-format '("Emacs %b" (buffer-file-name ": %f")))

  ;; Show path info in buffers with otherwise identical filenames
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'post-forward)

  ;; Make very frequent autosaves
  (setq auto-save-interval 5)

  ;; Make searches case-insensitive
  (set-default 'case-fold-search t)

  ;; Make all backups in a single directory
  (when (boundp 'backup-directory-alist)
    (let ((dir (expand-file-name "~/.emacs-backups")))
      (or (file-directory-p dir) (make-directory dir))
      (setq backup-directory-alist `(("." . ,dir)))))
  
  ;; Strip CTRL-M from Shell output
  (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)

  ;; Make all "yes or no" prompts show "y or n" instead
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Use dialog boxes, if available
  (setq use-dialog-box t)

  ;; Put current line number and column in the mode line
  (line-number-mode 1)       
  (setq column-number-mode t)

  ;; Use menu-bar
  (menu-bar-mode 1)

  ;; Paste at cursor, rather than pointer
  (setq mouse-yank-at-point t)

  (autoload 'screen-lines-mode "screen-lines"
    "Toggle Screen Lines minor mode for the current buffer." t)
  (autoload 'turn-on-screen-lines-mode "screen-lines"
    "Turn on Screen Lines minor mode for the current buffer." t)
  (autoload 'turn-off-screen-lines-mode "screen-lines"
    "Turn off Screen Lines minor mode for the current buffer." t)
  (setq screen-lines-minor-mode-string " sl")
  (setq-default screen-lines-mode t)
  ;; The above doesn't seem to work, so let's try this:
  (add-hook 'find-file-hooks 'turn-on-screen-lines-mode)
  
  ;; For <M-delete> to work properly
  (setq kill-whole-line t)

  ;; Enable recently-opened files menu
  (setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
  (recentf-mode 1)
  (setq recentf-max-menu-items 30)
  (setq recentf-max-saved-items 500)
  (setq recentf-exclude '("[.]bm-repository$" "[.]tidyrc$"
                          "^temp-tidy-" "[.]bmk$" "[.]abbrev_defs"
                          "[.]elc$" "^/tmp/mutt" "/usr/dict/words"))

  ;; Save list when used, in case of crashes
  (defadvice recentf-open-files (after easymacs-recentf-advice activate)
    (recentf-save-list))
    
  
  ;; Enable font-lock (syntax highlighting) in modes which support it
  (global-font-lock-mode t)
  (setq font-lock-maximum-decoration t)

  ;; show matching and mismatching brackets etc
  (setq show-paren-delay 0)
  (show-paren-mode t)

  ;; Always save eshell history without asking
  (setq eshell-save-history-on-exit 't)
  (setq eshell-ask-to-save-history 'always)
  (setq eshell-prefer-to-shell t)
  
  ;; Ansi color escapes in eshell/shell
  (autoload 'ansi-color-apply "ansi-color")
  (add-hook 'eshell-preoutput-filter-functions
	    'ansi-color-apply)
  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

  ;; Don't auto-complete ambiguities
  (setq eshell-cmpl-cycle-completions nil)
  
  ;; Don't echo passwords when communicating with interactive programs:
  (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

  ;; Show end of file
  (setq default-indicate-empty-lines t)
  (mapcar (lambda (pair) (if (boundp (car pair))
                           (set (car pair) (cdr pair))))
        '((mode-line-in-non-selected-windows  . t)
          (default-indicate-buffer-boundaries . t)
          (overflow-newline-into-fringe       . t)))
  
  (require 'color-file-completion)
  (set-face-foreground 'completion-setup-directory-face "Blue")

  (setq backward-delete-char-untabify-method 'hungry)
  (temp-buffer-resize-mode 1)
  (setq view-read-only t)

  (add-hook 'Man-mode-hook 'goto-address)
  (add-hook 'Info-mode-hook 'goto-address)
  (add-hook 'view-mode-hook 'goto-address)
  (add-hook 'text-mode-hook 'goto-address)
  (setq Man-notify-method 'pushy)

  ;; Settings for text mode (open unidentified files in text mode)
  (setq default-major-mode 'text-mode)
  (add-hook 'text-mode-hook 'turn-on-auto-fill)
  
  ;; Longlines doesn't work so well with indentation in auctex and
  ;; nxml modes
;  (add-hook 'text-mode-hook 'longlines-mode) 

  (add-hook 'text-mode-hook 'outline-minor-mode)
  (add-hook 'text-mode-hook 'easymacs-flyspell-mode)

  ;; No tab chars, please
  (setq-default indent-tabs-mode nil)

  ;; Keep cursor out of the prompt
  (setq minibuffer-prompt-properties
        (plist-put minibuffer-prompt-properties
                   'point-entered 'minibuffer-avoid-prompt))

  (if easymacs-w32-system
      (setq w32-num-mouse-buttons 2)
    (server-start))
  
  (setq grep-command "easymacs-grep -in ")
  (setq reftex-grep-command grep-command)

  (require 'w3)

  
  ) ;; End of easymacs-general-settings

;;;;;;;;;; Advice

(defun easymacs-advice ()
  ;; from http://www-xray.ast.cam.ac.uk/~gmorris/dotemacs.html
  
  ;; kill-emacs-hook does not seem to do what I would want.
  (defvar my-before-kill-emacs-hook nil
    "Hook to run before `save-buffers-kill-emacs'.")

  (defadvice save-buffers-kill-emacs (around before-kill-hook activate)
    "Run `my-before-kill-emacs-hook' before save-buffers-kill-emacs."
    ;; This is so that an abortive exit does not mess up our session.
    ;; Also quite nice since it means we do not get prompted to save
    ;; files etc unless we have really decided to exit emacs.  But
    ;; maybe we will change our mind if we know files have to be
    ;; saved?
    (when (yes-or-no-p "Really exit Emacs? ")
      (run-hooks 'my-before-kill-emacs-hook)
      (setq confirm-kill-emacs nil)       ; no need to ask twice
      ad-do-it))

  (defadvice find-file (around confirm-new-file activate)
    "If file does not exist, prompt."
    (let ((file (ad-get-arg 0)))
      (when (or (not (interactive-p))
               (find-buffer-visiting file)
               (string-match "\\`/\\[" file) ; old-style TRAMP
               (string-match "\\`/[a-zA-Z0-9@]:" file) ; new-style TRAMP
               (file-directory-p file)
               ;; file-exists-p does not handle wildcards.
               (file-expand-wildcards file)
               ;; A nice trick, but not necessary.
;;;         (string-match "0\n\\'" (shell-command-to-string
;;;                                 (format "ls %s; echo $?" file)))
               (yes-or-no-p
                (format "`%s' does not exist, create new file? " file)))
        
        ;; Is this a good idea? If we open a new file by accident,
        ;; despite the confirmation, we probably don't want the directory.
        (unless (file-directory-p (file-name-directory file))
          (make-directory (file-name-directory file) t))
        ad-do-it)))

  (defadvice switch-to-buffer (around confirm-new-buffer activate)
    "If buffer does not exist, prompt."
    (let ((buff (ad-get-arg 0)))
      (if (or (not (interactive-p))
              (get-buffer buff)
              (yes-or-no-p
               (format "`%s' does not exist, create new file? " buff)))
          ad-do-it)))

  ;; Advice above seems to screw up eshell, 
  (defalias 'eshell/open 'easymacs-find-file) 
  (defalias 'eshell/o 'easymacs-find-file) 
  (defun easymacs-find-file (file)
    (interactive)
    (switch-to-buffer (find-file-noselect file)))

  )

;;;;;;;;;; Scrolling
(defun easymacs-scrolling ()

  ;; Scroll bars
  (require 'scroll-bar)
  (set-scroll-bar-mode 'right)

  ;; Selecting with the mouse, and mouse-wheel
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control) . 20)))
  
  ;; The super-excellent package for intelligent scrolling
  (require 'scroll-in-place)

  (defun scroll-down-one-line (&optional sticky)
    "Scroll one line; with STICKY, keep cursor in same place"
    (interactive)
    (let ((scroll-default-lines 1)
          (pos (point)))
      (scroll-down-in-place)
      (when sticky
        (goto-char pos))))
  
  (defun scroll-down-one-line-sticky ()
    (interactive)
    (scroll-down-one-line t))
  
  (defun scroll-up-one-line (&optional sticky)
    "Scroll one line; with STICKY, keep cursor in same place"
    (interactive)
    (let ((scroll-default-lines 1)
          (pos (point)))
      (scroll-up-in-place)
      (when sticky
        (goto-char pos))))

  (defun scroll-up-one-line-sticky ()
    (interactive)
    (scroll-up-one-line t))

  (setq scroll-command-groups
	(list '(scroll-down-one-line
                scroll-up-one-line
                scroll-up-one-line-sticky
                scroll-down-one-line-sticky) '(mwheel-scroll)))

  ;; If you want the cursor to stay on the same line as you scroll
  ;; with the mouse, use this:
;  (defadvice mwheel-scroll (around mwheel-scroll-sticky activate)
;    (let ((pos (point)))
;      ad-do-it
;      (goto-char pos)))

  )

;;;;;;;;;; CUA-mode and cursor
(defun easymacs-cua-cursor ()

  (cua-mode t)
  (put 'backward-sentence           'CUA 'move)
  (put 'forward-sentence            'CUA 'move)	    
  (put 'easymacs-smart-home         'CUA 'move)
  (put 'easymacs-smart-end          'CUA 'move)
  (put 'scroll-down-one-line        'CUA 'move)
  (put 'scroll-down-one-line-sticky 'CUA 'move)
  (put 'scroll-up-one-line          'CUA 'move)
  (put 'scroll-up-one-line-sticky   'CUA 'move)
  (put 'easymacs-bounce-sexp        'CUA 'move)
  (put 'top-of-screen               'CUA 'move)
  (put 'bottom-of-screen            'CUA 'move)
  ;; Your X server may have to be configured for this
  ;; Also, is currently broken in a terminal
  ;;        (keypad-setup 'numeric t)
  ;;        (keypad-setup 'prefix))

  (defun easymacs-fix-cua ()
    "Toggle cua-mode off and on to recover from an error"
    (interactive)
    (cua-mode -1)
    (cua-mode 1))
  
  ;; This may cause unexpected behavior if left on
  (setq mark-even-if-inactive nil)

  (setq cursor-in-non-selected-windows t)
  (setq focus-follows-mouse t)

  ;; Keep selection active after copy
  (setq cua-keep-region-after-copy t)

  ;; Different cursors for different modes
  (require 'cursor-chg)
  (setq curchg-default-cursor-color "Black")
  (setq curchg-input-method-cursor-color "Red")
  (change-cursor-mode)
  
  (define-key cua--cua-keys-keymap [(control z)] nil)
  (require 'aquamacs-redo)
 
  )

;;;;;;;;;; Folding, imenu
(defun easymacs-folding ()

  (require 'hideshow)
  (require 'outline)
  (require 'fold-dwim)

  (autoload 'speedbar-frame-mode "speedbar" "Popup a speedbar frame" t)
  (autoload 'speedbar-get-focus "speedbar" "Jump to speedbar frame" t)
  
  ;; Search in comments as well as "code"
  (setq hs-isearch-open t)
  (setq hs-hide-comments-when-hiding-all t)
  (setq hs-allow-nesting t)

  (defun imenu-or-not ()
    "Try to add an imenu when we visit a file, catch and nil if
the mode doesn't support imenu."
    (condition-case nil
	(imenu-add-menubar-index)
      (error nil)))
  (add-to-list 'find-file-hooks 'imenu-or-not)

  (setq imenu-max-items 50
        imenu-scanning-message nil)
  ;;Get rid of an annoying error
  (defun imenu-progress-message (prevpos &optional relpos reverse)
       ())
  
  )

;;;;;;;;;; Flyspell, dictionary-el and dabbrev
(defvar easymacs-have-spell 'nil)
(defun easymacs-flyspell-dict ()

  (if (executable-find "aspell")
      (progn
        (setq easymacs-have-spell 'aspell
              ispell-really-aspell t)
        (if (boundp 'carbon-emacs-package-version)
            ;; Includes aspell, so does its own tweaking of
            ;; ispell-program-name and extra-args
            (setq ispell-extra-args
                  (append (list "--sug-mode=fast") ispell-extra-args))
          ;; Non-carbon-package aspell
          (setq-default ispell-program-name "aspell")
;          (setq ispell-extra-args '("--sug-mode=fast"))
))
    (when (executable-find "ispell")
      (setq-default ispell-program-name "ispell")
      (setq easymacs-have-spell 'ispell)))
  
  (defun easymacs-flyspell-mode (&optional arg)
    (when (and easymacs-have-spell
             (not buffer-read-only))
      (if arg
          (flyspell-prog-mode)
      (flyspell-mode 1))))
  
  ;; Still buggy with large tex files
  (setq flyspell-large-region 100000000)

  (setq flyspell-persistent-highlight t)

;(defadvice flyspell-auto-correct-previous-word
;    (around easymacs-flyspell-auto-correct)
;    "Correct current word if misspelled, else previous
;    misspelling.  Protect against accidentally changing a word
;    that cannot be seen, because it is somewhere off the screen."
;    (let ((top) (bot))
;      (save-excursion
;        (top-of-screen)
;        (setq top (point))
;        (bottom-of-screen)
;        (end-of-line)
;        (setq bot (point)))
;      (save-restriction
;        (narrow-to-region top bot)
;        (save-excursion
;          (re-search-forward "\\s \\|\\'" nil t)
;          (overlay-recenter (point))
;          ad-do-it))))
;  (ad-activate 'flyspell-auto-correct-previous-word)

  (defun easymacs-toggle-flyspell-mode ()
    (interactive)
    (cond
     ((eq major-mode 'nxml-mode)
      (flyspell-xml-lang-mode 'toggle))
     ((eq major-mode 'latex-mode)
      (flyspell-babel-mode 'toggle))
     (t
      (flyspell-mode))))
        
  ;; dictionary-el (http://www.myrkr.in-berlin.de/dictionary/)

  ;; autoloads for dictionary-el
  (load "dictionary-init")

  (defvar easymacs-external-dict-server "www.dict.org")
  ;; Check to see if a dictionary server is running on the local
  ;; machine; otherwise, use an external one.
  (condition-case nil
      (progn
        (open-network-stream "dict-test" nil "localhost" 2628)
        (delete-process "dict-test")
        (setq dictionary-server "localhost"))
    (error (setq dictionary-server easymacs-external-dict-server)))

  ;; Hippie expand doesn't work as well, eg. for latex:labels
  (setq dabbrev-check-all-buffers t)

  )

;;;;;;;;;; Misc. packages
(defun easymacs-misc-packages ()

  ;; linum (Show line numbers)
  (require 'linum)

  ;; Blank mode (show spaces and returns visually)
  (autoload 'blank-mode-on "blank-mode" "Turn on blank visualization." t)
  (autoload 'blank-mode-off "blank-mode" "Turn off blank visualization." t)
  (autoload 'blank-mode "blank-mode" "Toggle blank visualization." t)
  (autoload 'blank-mode-customize "blank-mode" "Customize blank visualization." t)

  ;; Pretty color themes
  (require 'color-theme)
  (color-theme-initialize)
  (require 'zenburn)

  ;; require 'generic-x sets too many variables on windows
  (when easymacs-w32-system
    (setq max-specpdl-size 1000))
    ;; Modes for a multitude of odd config files
  (require 'generic-x)

  (setq tramp-default-method "ssh")
  (add-to-list 'backup-directory-alist
	       (cons tramp-file-name-regexp nil))
  (setq tramp-auto-save-directory "~/.tramp-autosave")

  ;; Browse recently cut text
  (autoload 'browse-kill-ring "browse-kill-ring" "Browse the Kill Ring" t)

  ;; Vim's ~ function
  (autoload 'joc-toggle-case "joc-toggle-case"
    "Toggle case of current letter" t)
  (autoload 'joc-toggle-case-backwards "joc-toggle-case"
    "Toggle case of current letter" t)

  ;; Align columns on whitespace
  (autoload 'align-cols "align-cols" "Align columns on whitespace" t)

  ;; Add speedbar to the Tools menu
  (define-key-after (lookup-key global-map [menu-bar tools])
    [speedbar] '("Speedbar" . speedbar-frame-mode) [calendar])

  ;; Save our session
  (setq-default save-place t)
  (require 'saveplace)
  (savehist-mode 1)

  (require 'printing)

  ;; For M-x scratch
  (load "scrat")
  (define-key lisp-interaction-mode-map (kbd "<f12>")   'back-from-scratch)
  (defadvice scratch (after easymacs-scratch activate)
    (delete-other-windows))

  (load "drag-text")
  
  )

;;;;;;;;;; Switching buffers
(defun easymacs-switching-buffers ()
  ;; This is the simplest and clearest emulation of MS-Windows buffer
  ;; switching that I have tried -- it doesn't use timeouts
  (autoload 'buffer-stack-bury "buffer-stack" "Buffer switching")
  (autoload 'buffer-stack-bury-and-kill "buffer-stack" "Buffer switching")
  (autoload 'buffer-stack-up "buffer-stack" "Buffer switching")
  (autoload 'buffer-stack-down "buffer-stack" "Buffer switching")
  (autoload 'buffer-stack-track "buffer-stack" "Buffer switching")
  (autoload 'buffer-stack-untrack "buffer-stack" "Buffer switching")
  (autoload 'buffer-stack-show-position "buffer-stack" "Buffer switching")
  (setq buffer-stack-show-position 'easymacs-buffer-stack-show-position-buffers)

  (defun easymacs-buffer-stack-show-position-buffers (buffer-stack-index buffer-stack)
    "Show position like this: Previous: *Previous Buffer* -- Next: *Next Buffer*"
    (interactive)
    (let (up-buffer-index
          down-buffer-index
          (max-index (- (length buffer-stack) 1)))
      (if (eq buffer-stack-index 0)
          (setq up-buffer-index max-index)
        (setq up-buffer-index (- buffer-stack-index 1)))
      (if (eq buffer-stack-index max-index)
          (setq down-buffer-index 0)
        (setq down-buffer-index (+ buffer-stack-index 1)))
      (message (concat "Previous: "
                       (buffer-name (nth up-buffer-index buffer-stack))
                       " -- " "Next: "
                       (buffer-name (nth down-buffer-index buffer-stack))))))

  ;; Get smarter behavior with switch-to-buffer
  (iswitchb-mode 1)

  (require 'ibuffer)

  ;; organized buffer menu
  (msb-mode 1)
  )

;;;;;;;;;; Bookmarks
(defun easymacs-bookmarks ()
  ;; Visible bookmarks package
  ;; Make sure the repository is loaded as early as possible (for desktop-read)
  (setq bm-restore-repository-on-load t)
  (require 'bm)
  
  (setq-default bm-buffer-persistence t)
  ;; Restoring bookmarks when on file find.
  (add-hook 'find-file-hooks 'bm-buffer-restore)
  ;; Saving bookmark data on killing a buffer
  (add-hook 'kill-buffer-hook 'bm-buffer-save)
  ;; Saving the repository to file when on exit.
  ;; kill-buffer-hook is not called when emacs is killed, so we
  ;; must save all bookmarks first.  Ignore errors, so that Emacs
  ;; doesn't become unkillable.
  (add-hook 'kill-emacs-hook
            '(lambda nil (condition-case nil
                             (progn
                               (bm-buffer-save-all)
                               (bm-repository-save)))))
  ;; Update bookmark repository when saving the file.
  (add-hook 'after-save-hook 'bm-buffer-save)
  ;; Restore bookmarks when buffer is reverted.
  (add-hook 'after-revert-hook 'bm-buffer-restore)

  (defun easymacs-bm-next ()
    (interactive)
    (deactivate-mark)
    (bm-next))

  (defun easymacs-bm-previous ()
    (interactive)
    (deactivate-mark)
    (bm-previous))
  
  )

;;;;;;;;;; Dired
(defun easymacs-dired-extras ()

  ;; Editable filenames in dired (must come before dired loads)
  (autoload 'wdired-change-to-wdired-mode "wdired")
  (defun easymacs-wdired-hook ()
    (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
    (define-key dired-mode-map
      [menu-bar immediate wdired-change-to-wdired-mode]
      '("Edit File Names" . wdired-change-to-wdired-mode)))
  
  ;; dired-x for omitting uninteresting files (f12)
  (setq dired-omit-files
        ;; Omit dot-files and backups, but not ..
        "^\\.?#\\|^\\.$\\|^\\.[^.].*$\\|^_.+$")
 
  ;; dired-x for omitting uninteresting files (f12)
  (add-hook 'dired-mode-hook
              (lambda ()
                (load "dired-x")
                (dired-omit-mode 1)
                ;; Set dired-x global variables here.  
                (define-key dired-mode-map (kbd "<f12>") 'dired-omit-mode)
                (setq dired-omit-extensions
                      (easymacs-delete-from-list ".pdf" dired-omit-extensions))))

  
  ;; Make dired less weird -- it always opens new files or directories
  ;; in the current buffer, rather than endless spawning of new buffers
  (defun easymacs-dired-mouse-find-file-same-window (event)
    ;; Never open a new buffer from dired, even when clicking with the mouse
    ;; Modified from dired.el
    "In Dired, visit the file or directory name you click on."
    (interactive "e")
    (let (window pos file)
      (save-excursion
	(setq window (posn-window (event-end event))
	      pos (posn-point (event-end event)))
	(if (not (windowp window))
	    (error "No file chosen"))
	(set-buffer (window-buffer window))
	(goto-char pos)
	(setq file (dired-get-file-for-visit)))
      (select-window window)
      (find-alternate-file (file-name-sans-versions file t))))
  
  (eval-after-load "dired"
    '(progn
       ;; Never open a new buffer from dired, neither for files nor directories.
       (defadvice dired-find-file (around dired-subst-directory activate)
	 "Replace current buffer if file is a directory."
	 (interactive)
	 (let ((orig (current-buffer))
	       (filename (dired-get-filename nil t)))
	   ad-do-it
	   (kill-buffer orig)))
       (define-key dired-mode-map [mouse-2]
         'easymacs-dired-mouse-find-file-same-window)
       (define-key dired-mode-map "^" (function
				       (lambda nil (interactive) 
					 (find-alternate-file ".."))))))
 
  ;; Always list directories first (From Rene Kyllingstad in
  ;; comp.emacs.xemacs)
  (defun easymacs-dired-sort ()
    "Dired sort hook to list directories first."
    (interactive)
    (save-excursion
      (let (buffer-read-only)
	(forward-line 2) ;; beyond dir. header
	(sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))
	(lambda (a b)
	  (string-lessp
	   (if (string-equal "  l" a) "  -" a)
	   (if (string-equal "  l" b) "  -" b)))))
    (set-buffer-modified-p nil))
  (add-hook 'dired-after-readin-hook 'easymacs-dired-sort)

  ;; Additional sorting for dired
  (defvar dired-sort-map (make-sparse-keymap))
  (add-hook 'dired-mode-hook
            '(lambda () (define-key dired-mode-map "s" dired-sort-map)))
  (defun easymacs-dired-extra-sort (switch doc)
    (dired-sort-other
     (concat dired-listing-switches switch))
    (message "%s" (concat "Sorting by " doc)))

  (define-key dired-sort-map "s"
    '(lambda () (interactive) (easymacs-dired-extra-sort "S" "Size")))
  (define-key dired-sort-map "x"
    '(lambda () (interactive) (easymacs-dired-extra-sort "X" "eXtension")))
  (define-key dired-sort-map "t"
    '(lambda () (interactive) (easymacs-dired-extra-sort "t" "Time")))
  (define-key dired-sort-map "n"
    '(lambda () (interactive) (easymacs-dired-extra-sort "" "Name")))
  (define-key dired-sort-map "S"
    '(lambda () (interactive) (easymacs-dired-extra-sort "Sr" "Size (reverse)")))
  (define-key dired-sort-map "X"
    '(lambda () (interactive) (easymacs-dired-extra-sort
                               "Xr" "eXtension (reverse)")))
  (define-key dired-sort-map "T"
    '(lambda () (interactive) (easymacs-dired-extra-sort "tr" "Time (reverse)")))
  (define-key dired-sort-map "N"
    '(lambda () (interactive) (easymacs-dired-extra-sort "r" "Name (reverse)")))

  )

;;;;;;;;;; Calendar, Diary, etc.
(defun easymacs-calendar ()

  (setq view-diary-entries-initially t
	mark-diary-entries-in-calendar t
	number-of-diary-entries 1)
  (add-hook 'diary-display-hook 'fancy-diary-display)
  (add-hook 'today-visible-calendar-hook 'calendar-mark-today)

  (autoload 'fancy-schedule-display-desk-calendar "cal-desk-calendar")
  (add-hook 'diary-display-hook 'sort-diary-entries)
  (add-hook 'diary-display-hook 'fancy-schedule-display-desk-calendar t)

  (setq diary-duplicate-time-display nil)
  (setq diary-schedule-odd-times-get-separate-entry t)
  (setq diary-schedule-first-time-always-has-hours t)

  (setq auto-mode-alist
	(append '(("\\.?diary$" . diary-mode)) auto-mode-alist))

  (autoload 'org-mode "org" "Org mode" t)
  (autoload 'org-diary "org" "Diary entries from Org mode")
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (add-to-list 'auto-mode-alist '("TODO$" . org-mode))
  (setq org-startup-folded nil)
  (setq org-startup-truncated nil)
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)

  (put 'org-mode 'fold-dwim-outline-style 'nested)

  ;; We use M-S-up/down instead of M-up/down for promoting/demoting
  ;; subtrees
  (defun easymacs-org-up ()
    (interactive)
    (if (org-at-timestamp-p)
	(org-timestamp-up)
      (org-metaup)))
  (defun easymacs-org-down ()
    (interactive)
    (if (org-at-timestamp-p)
	(org-timestamp-down)
      (org-metadown)))
  
  (defun easymacs-org-mode-hook ()
    ;; For terminal mode
    (define-key org-mode-map (kbd "<M-return>") 'org-insert-heading)
    ;; Override some bindings that conflict with easymacs and cua-mode
    (define-key org-mode-map [(shift up)]     nil)
    (define-key org-mode-map [(shift down)]   nil)
    (define-key org-mode-map [(shift left)]   nil)
    (define-key org-mode-map [(shift right)]  nil)
    (define-key org-mode-map [(meta up)]      nil)
    (define-key org-mode-map [(meta down)]    nil)
    (define-key org-mode-map [(control shift left)]  'org-metaleft)
    (define-key org-mode-map [(control shift right)] 'org-metaright)
    (define-key org-mode-map [(shift meta up)]   'easymacs-org-up)
    (define-key org-mode-map [(shift meta down)] 'easymacs-org-down)
    (define-key org-mode-map [(control tab)] 'buffer-stack-down)
    (define-key org-mode-map "\C-a" 'mark-whole-buffer)
    )
  (add-hook 'org-mode-hook 'easymacs-org-mode-hook)
  
  )

;;;;;;;;;; Utility Functions
(defun easymacs-library ()

  ;; Search for duplicated words (taken from the elisp intro tutorial)
  (defun the-the ()
    "Search forward for for a duplicated word."
    (interactive)
    (message "%s" "Searching for for duplicated words ...")
    (push-mark)
    ;; This regexp is not perfect
    ;; but is fairly good over all:
    (if (re-search-forward
	 "\\b\\([^@ \n\t]+\\)[ \n\t]+\\1\\b" nil 'move)
	(message "%s" "Found duplicated word.")
      (message "%s" "End of buffer")))
 
  (defun uncomment-region (beg end)
    (interactive "r")
    (comment-region beg end (universal-argument)))
  
  ;; Many of these functions are adapted from:
  ;; http://www.dotemacs.de/dotfiles/ElijahDaniel.emacs.html

  ;; Duplicate current line
  (autoload 'copy-from-above-command "misc" "" t)
  (defun easymacs-dup-line()
    "Copy the current line and move down to edit it."
    (interactive)                            
    (save-excursion
      (when (= 1 (forward-line 1))
	(insert "\n"))
      (copy-from-above-command)
      (insert "\n"))
    (next-line 1))

  (defun easymacs-dup-region-old ()
    "Duplicates the region."
    (interactive)
    (let* ((deactivate-mark nil)
	   (start (region-beginning))
	   (end (region-end))
	   (contents (buffer-substring start end)))
      (save-excursion
	(goto-char end)
	(insert contents))
      (goto-char start)
      (push-mark end)
      (exchange-point-and-mark)))

  (defun easymacs-dup-region ()
    "Duplicates all lines in the region."
    (interactive)
    (let* ((deactivate-mark nil)
	   (start (region-beginning))
	   (end (region-end))
	   (contents (buffer-substring
                      (progn (goto-char start) (line-beginning-position))
                      (progn (goto-char end) (line-end-position)))))
      (save-excursion
	(goto-char end)
        (forward-line 1)
	(insert contents))
      (goto-char start)
      (push-mark end)
      (exchange-point-and-mark)))
  
  (defun easymacs-dup-region-or-line ()
    (interactive)
    (if mark-active
	(easymacs-dup-region)
      (easymacs-dup-line)))

  ;; See also copy-from-above-command in misc.el
  (defun easymacs-copy-char-above (&optional b)
    "Copy a character exactly below/above the point
to the current point of the cursor (default is above)."
    (interactive "p")
    (let (p col s)
      (setq p (point))
      (setq col (current-column))
      (forward-line (if b -1 1))
      (move-to-column col)
      (setq s (buffer-substring (point) (+ (point) 1)))
      (goto-char p)
      (insert s)))

  (defun easymacs-copy-char-below ()
    (interactive)
    (easymacs-copy-char-above nil))

  (defun easymacs-prepend (start end s &optional append)
    "Add a string in front of all lines in the region.
If APPEND is non-nil, add the string to the end of lines."
    (interactive "*r\nMEnter a string: ")
    (save-excursion
      (save-restriction
	(narrow-to-region
	 (progn (goto-char start) (line-beginning-position))
	 (progn (goto-char end) (line-end-position)))
	(goto-char (point-min))
	(beginning-of-line)
	(while (not (eobp))
	  (if append (end-of-line))
	  (insert s)
	  (forward-line 1))))
    (setq deactivate-mark nil))

  (defun easymacs-prepend-line-or-region (s)
    (interactive "MEnter a string: ")
    (if mark-active
	(easymacs-prepend
	 (region-beginning)
	 (region-end) s)
      (easymacs-prepend (point) (point) s)))
                                
  (defun easymacs-append (start end s)
    "Append a string to the end of all lines in region"
    (interactive "*r\nMEnter a string: ")
    (easymacs-prepend start end s t))

  (defun easymacs-append-line-or-region (s)
    (interactive "MEnter a string: ")
    (if mark-active
	(easymacs-append
	 (region-beginning)
	 (region-end) s)
      (easymacs-append (point) (point) s)))
                                               
  ;; Remove a string from the beginning of all lines in the region
  (defun easymacs-unprepend (start end s)    
    "Remove a string from the front of all lines in the region."
    (interactive "*r\nMEnter a string: ")
    (save-excursion
      (save-restriction
	(narrow-to-region
	 (progn (goto-char start) (beginning-of-line) (point))
	 (progn (goto-char end) (end-of-line) (point)))
	(goto-char (point-min))         
	(while (not (eobp))
	  (if (looking-at (regexp-quote s))
	      (delete-region (match-beginning 0) (match-end 0)))
	  (forward-line 1))))
    (setq deactivate-mark nil))

  (defun easymacs-unprepend-line-or-region (s)
    (interactive "MEnter a string: ")
    (if mark-active
	(easymacs-unprepend
	 (region-beginning)
	 (region-end) s)
      (easymacs-unprepend (point) (point)
		       s)))

  (defun easymacs-unappend (start end s)
    "Remove string from end of each line in region"
    (interactive "*r\nMEnter a string: ")
    (save-excursion
      (save-restriction
	(narrow-to-region
	 (progn (goto-char start) (line-beginning-position))
	 (progn (goto-char end) (line-end-position)))
	(goto-char (point-min))
	(while (not (eobp))
	  (end-of-line)
	  (when (> (point) (line-beginning-position)) ; do nothing for blank line
	    (backward-char 1) ;; so looking-at will work
	    ;; Does last char on line match last char of string?
	    (when (looking-at (regexp-quote (substring s -1)))
	      (forward-char 1)		; so search will work
	      ;; Search backwards for regexp as far as linestart.
	      ;; t means return nil, rather than error, if no match.
	      (if (search-backward-regexp (regexp-quote s) (line-beginning-position) t)
		  (delete-region (match-beginning 0) (match-end 0)))))
	  (forward-line 1))))
    (setq deactivate-mark nil))

  (defun easymacs-unappend-line-or-region (s)
    (interactive "MEnter a string: ")
    (if mark-active
	(easymacs-unappend
	 (region-beginning)
	 (region-end) s)
      (easymacs-unappend (point) (point)
		      s)))
                                                      
  ;; Add a comment character in front of all lines in the region
  (defun easymacs-comment-region (start end)
    "Add one comment character in front of all lines in the region."       
    (interactive "*r")                  
    (or comment-start (setq comment-start (read-input "Comment char?: ")))
    (easymacs-prepend start end comment-start))

  ;; We can't use (interactive "r") here, since we want it to work
  ;; even if the mark has not been set.
  (defun easymacs-comment-line-or-region ()
    (interactive)
    (if mark-active
	(easymacs-comment-region (region-beginning) (region-end))
      (easymacs-comment-region (point) (point))))

  ;; Remove a comment character from in front of all lines in the
  ;; region
  (defun easymacs-uncomment-region (start end)
    "Remove one comment character from in front of all lines in
the region."
    (interactive "*r")                  
    (or comment-start (setq comment-start (read-input "Comment char?: ")))
    (easymacs-unprepend start end comment-start))

  ;; We can't use (interactive "r") here, since we want it to work
  ;; even if the mark has not been set.
  (defun easymacs-uncomment-line-or-region ()
    (interactive)
    (if mark-active
	(easymacs-uncomment-region (region-beginning) (region-end))
      (easymacs-uncomment-region (point) (point))))

  (defun easymacs-toggle-comment-line ()
    (interactive)
    (save-excursion
      (beginning-of-line)
      (if (looking-at (or comment-start
                         (setq comment-start (read-input "Comment char?: "))))
          (easymacs-unprepend (point) (point) comment-start)
        (easymacs-prepend (point) (point) comment-start))))

  (defun easymacs-toggle-comment-region ()
    (interactive)
    (let ((start (region-beginning))
          (end (region-end)))
      (save-excursion
        (save-restriction
          (narrow-to-region
           (progn (goto-char start) (line-beginning-position))
           (progn (goto-char end) (line-end-position)))
          (goto-char (point-min))
          (while (not (eobp))
            (easymacs-toggle-comment-line)
            (forward-line 1))))
      (setq deactivate-mark nil)))

  (defun easymacs-toggle-comment-line-or-region ()
    (interactive)
    (if mark-active
	(easymacs-toggle-comment-region)
      (easymacs-toggle-comment-line)))

  (defun easymacs-dup-and-comment-line-or-region ()
    (interactive)
    (if mark-active
        (save-excursion
          (easymacs-dup-region)
          (when (= 0 (current-column))
            (forward-char -1))
          (easymacs-comment-line-or-region)
          (setq deactivate-mark t))
      (easymacs-dup-line)
      (next-line -1)
      (easymacs-comment-line-or-region)
      (next-line 1)))

  (defun easymacs-kill-buffer ()
    "Kill buffer and delete window if split, taking gnuclient into 
account; do not prompt for confirmation."
    (interactive)
    (if (and (boundp 'server-buffer-clients)
	     server-buffer-clients)
	(server-edit)
      (let ((buffer (current-buffer)))
	(ignore-errors (delete-window (selected-window)))
	(kill-buffer buffer))))

  (defun count-words-region (beg end)
    "Count the number of words in current region;
   print a message in the minibuffer with the result."
    (interactive "r")
    (save-excursion
      (let ((count 0))
	(goto-char beg)
	(while (< (point) end)
	  (forward-word 1)
	  (setq count (1+ count)))
	(message "region contains %d words." count))))

  ;; Home/End keys toggle between the start/end of the screen line and
  ;; the start/end of text on the line.

  (defun easymacs-smart-home ()
    "Odd home to beginning of line, even home to beginning of text, but trying 
to do something useful with over-long lines."
    (interactive)
    (if (= 0 (current-column))
        (beginning-of-line-text)
      (if (and (boundp 'screen-lines-mode) screen-lines-mode)
          (let ((pos (point)))
            (Screen-lines-beginning-of-line)
            (when (= pos (point))
              (beginning-of-line-text)))
        (beginning-of-line))))

  (defun easymacs-smart-end ()
    "Odd home to beginning of line, even home to beginning of text, but trying 
to do something useful with over-long lines."
    (interactive)
    (let ((pos (point)))
      (if (and (boundp 'screen-lines-mode) screen-lines-mode)
          (progn
            (Screen-lines-end-of-line)  
            (when (= pos (point))
              (let ((pos (point)))
                (end-of-line)
                (when (= pos (point))
                  (end-of-line-text)))))
        (end-of-line)
        (when (= pos (point))
          (end-of-line-text)))))      

  (defun end-of-line-text ()  
    "Move to end of current line and skip comments and trailing space.
Requires `font-lock'."
    (interactive)
    (end-of-line)
    (let ((bol (line-beginning-position)))
      (unless (eq font-lock-comment-face (get-text-property bol 'face))
        (while (and (/= bol (point))
                    (eq font-lock-comment-face
                        (get-text-property (point) 'face)))
          (backward-char 1))
        (unless (= (point) bol)
          (unless (eobp) (forward-char 1))
          (skip-chars-backward " \t\n")))))

  (defun top-of-screen ()
    (interactive)
    (move-to-window-line 0))
  
  (defun bottom-of-screen ()
    (interactive)
    (move-to-window-line -1))

  (defun backward-move-to-tab-stop ()
    "Move point to previous (greatest less than point) tab-stop.  The
variable `tab-stop-list' is a list of columns at which there are tab
stops. Use \\[edit-tab-stops] to edit tab stops interactively.  This
is a move-backward version of \\[move-to-tab-stop]."
    (interactive)
    ;; loop to find greatest tab stop less than point
    (let ((tabs (reverse tab-stop-list)))
      (while (and tabs (<= (current-column) (car tabs)))
	(setq tabs (cdr tabs)))
      ;; if tabs not nil, car tabs is that column
      ;; Otherwise, column should be 0.
      ;; So go there.  
      (cond (tabs (move-to-column (car tabs) t))
	    (t  (move-to-column 0 t)))))

  (defun easymacs-rigidly-indent-line-or-region ()
    (interactive)
    (if mark-active
	(increase-left-margin (region-beginning) (region-end) nil)
      (increase-left-margin (point-at-bol) (point-at-eol) nil))
    (setq deactivate-mark nil))

  (defun easymacs-rigidly-unindent-line-or-region ()
    (interactive)
    (if mark-active
	(decrease-left-margin (region-beginning) (region-end) nil)
      (decrease-left-margin (point-at-bol) (point-at-eol) nil))
    (setq deactivate-mark nil))

  (defun easymacs-fill-region-or-paragraph ()
    (interactive)
    (if mark-active
	(fill-region)
      (fill-paragraph)))

  (defun easymacs-fill-region-or-paragraph-nosqueeze ()
    (interactive)
    (if mark-active
	(fill-region (region-beginning) (region-end) 'nil t)
      ;; copied from fill-paragraph, which does not provide for nosqueezing
      (let ((before (point))
	    (paragraph-start paragraph-start))
	(save-excursion
	  ;; To make sure the return value of forward-paragraph is meaningful,
	  ;; we have to start from the beginning of line, otherwise skipping
	  ;; past the last few chars of a paragraph-separator would count as
	  ;; a paragraph (and not skipping any chars at EOB would not count
	  ;; as a paragraph even if it is).
	  (move-to-left-margin)
	  (if (not (zerop (forward-paragraph)))
	      ;; There's no paragraph at or after point: give up.
	      (let ((end (point))
		    (beg (progn (backward-paragraph) (point))))
		(goto-char before)
		(if use-hard-newlines
		    ;; Can't use fill-region-as-paragraph, since this
		    ;; paragraph may still contain hard newlines.  See
		    ;; fill-region.
		    (fill-region beg end 'nil 't)
		  (fill-region-as-paragraph beg end 'nil 't))))))))
 
  (defun easymacs-unfill-paragraph ()
    (interactive)
      (let ((fill-column (point-max)))
        (fill-paragraph nil)))
    
  (defun easymacs-bounce-sexp ()
    "Will bounce between matching parens just like % in vi"
    (interactive)
    (let ((prev-char (char-to-string (preceding-char)))
          (next-char (char-to-string (following-char))))
      (cond ((string-match "[[{(<]" next-char) (forward-sexp 1))
            ((string-match "[\]})>]" prev-char) (backward-sexp 1))
            (t (error "%s" "Not on a paren, brace, or bracket")))))

  (defun easymacs-mark-sexp ()
    "Will bounce between matching parens just like % in vi"
    (interactive)
    (cua-set-mark)
    (let ((prev-char (char-to-string (preceding-char)))
          (next-char (char-to-string (following-char))))
      (cond ((string-match "[[{(<]" next-char) (forward-sexp 1))
            ((string-match "[\]})>]" prev-char) (backward-sexp 1))
            (t (cua-set-mark)
               (error "%s" "Not on a paren, brace, or bracket")))))

  
  (defun easymacs-copy-line ()
    "Copy the line containing point to the kill-ring."
    (interactive)
    (save-excursion
      (beginning-of-line)
      (let ((beg (point)))
	(end-of-line)
	(unless (eobp)
	  (forward-char))
	(copy-region-as-kill beg (point)))))

  (defun easymacs-suspend ()
    "Suspend or Iconify Emacs"
    (interactive)
    (cond
     (window-system
      (iconify-frame)) 
     (t
      (suspend-emacs))))

  (defun easymacs-eshell ()
    "Eshell with switch to directory of current buffer"
    (interactive)
    (let ((dir default-directory))
      (eshell)
      (eshell-kill-input)
      (unless (string= (expand-file-name dir) (expand-file-name default-directory))
	(eshell/cd dir)
	(eshell-send-input))))

  (defun easymacs-delete-to-start-of-buffer ()
    (interactive)
    (delete-region (point) (point-min)))

  (defun easymacs-delete-to-end-of-buffer ()
    (interactive)
    (delete-region (point) (point-max)))

  (defun easymacs-delete-from-list (x l)
    (cond ((null l) nil)
	  ((equal x (car l)) (easymacs-delete-from-list x (cdr l)))
	  (t (cons (car l) (easymacs-delete-from-list x (cdr l))))))

  (defun indent-all ()
    "Indent entire buffer."
    (interactive)
    (indent-region (point-min) (point-max)))

  ;; Can we do something more useful than this?
  ;;  (defun easymacs-highlight-sexp ()
  ;;    (interactive)
  ;;    (beginning-of-sexp)
  ;;    (mark-sexp))

  (defun easymacs-line-of-char (char)
    "Append a string to the end of all lines in region"
    (interactive "cEnter a character: ")
    (insert
     (make-string fill-column char)))

  (defun easymacs-align-cols-compact (&optional arg)
    "Like align-cols, but first minimize whitespace, except for
    indentation"
    (interactive "P")
    (unless mark-active
      (error "Error: no text selected."))
    (let ((beg-mark (set-marker (make-marker) (region-beginning)))
          (end-mark (set-marker (make-marker) (region-end))))
    (save-excursion
      (goto-char (marker-position beg-mark))
      (while (and (re-search-forward "[ \t]+" (marker-position end-mark) t)
                  (not (in-string-p)))
        (save-excursion
          (skip-chars-backward " \t")
          (unless (bolp)
            (replace-match " "))))
      (align-cols (marker-position beg-mark) (marker-position end-mark) arg))))

  (defun easymacs-upcase-word-or-region (&optional arg)
    (interactive "p")
    (if mark-active
        (upcase-region (region-beginning) (region-end))
      (upcase-word arg)))

  (defun easymacs-downcase-word-or-region (&optional arg)
    (interactive "p")
    (if mark-active
        (downcase-region (region-beginning) (region-end))
      (downcase-word arg)))

  (defun easymacs-capitalize-word-or-region (&optional arg)
    (interactive "p")
    (if mark-active
        (capitalize-region (region-beginning) (region-end))
      (capitalize-word arg)))

  (defvar easymacs-currently-defining-macro nil)
  (defvar easymacs-current-macro-counter 0)

  (defun easymacs-macro-start-or-counter ()
    "A (very) poor man's substitute for kmacro.el"
  (interactive)
  (if easymacs-currently-defining-macro
      (progn
        (insert (format "%d" easymacs-current-macro-counter))
        (setq easymacs-current-macro-counter (1+ easymacs-current-macro-counter)))
    (setq easymacs-currently-defining-macro t)
    (start-kbd-macro nil)))

  (defun easymacs-macro-end-or-call ()
    (interactive)
    (if easymacs-currently-defining-macro
        (end-kbd-macro)
      (call-last-kbd-macro))
    (setq easymacs-currently-defining-macro nil))

  ;; From VivekDasmohapatra at
  ;; http://www.emacswiki.org/cgi-bin/wiki/FindingNonAsciiCharacters

  ;; Generalising the above - this function will allow you to find
  ;; characters your current coding system cannot encode: A better
  ;; implementation would use unencodable-char-position,
  ;; but that requires cvs-emacs:

  (defun easymacs-find-next-unsafe-char (&optional coding-system)
    "Find the next character in the buffer that cannot be encoded by
coding-system. If coding-system is unspecified, default to the coding
system that would be used to save this buffer. With prefix argument,
prompt the user for a coding system."
    (interactive "Zcoding-system: ")
    (if (stringp coding-system) (setq coding-system (intern coding-system)))
    (if coding-system nil
      (setq coding-system
            (or save-buffer-coding-system buffer-file-coding-system)))
    (let ((found nil) (char nil) (csets nil) (safe nil))
      (setq safe (coding-system-get coding-system 'safe-chars))
      ;; some systems merely specify the charsets as ones they can encode:
      (setq csets (coding-system-get coding-system 'safe-charsets))
      (save-excursion
        ;;(message "zoom to <")
        (let ((end  (point-max))
              (here (point    ))
              (char  nil))
          (while (and (< here end) (not found))
            (setq char (char-after here))
            (if (or (eq safe t)
                    (< char ?\177)
                    (and safe  (aref safe char))
                    (and csets (memq (char-charset char) csets)))
                nil ;; safe char, noop
              (setq found (cons here char)))
            (setq here (1+ here))) ))
      (and found (goto-char (1+ (car found))))
      found))

;  (defun easymacs-newfile-unkillable-buffer ()
;    (when (and (buffer-modified-p)
;               (yes-or-no-p "Save new file? "))
;      (basic-save-buffer))
;    t)

;  (defun easymacs-newfile ()
;    (interactive)
;    (switch-to-buffer (generate-new-buffer "*New File*"))
;    (cd (expand-file-name "~/"))
;    (make-local-variable 'kill-buffer-query-functions)
;    (add-hook 'kill-buffer-query-functions 'easymacs-newfile-unkillable-buffer)
;    (setq buffer-offer-save t))

  (defun easymacs-kill-some-buffers ()
    "Kill most unmodified buffers, except for a few."
    (interactive)
    (when (yes-or-no-p "Close most unmodified files? ")
      (let ((list (buffer-list)))
        (while list
          (let* ((buffer (car list))
                 (name (buffer-name buffer)))
            (when (if (string-match "^\\*.*\\*$" name)
                      (and (not (string-equal name "*scratch*"))
                           (not (string-equal name "*Messages*"))
                           (not (string-equal name "*info*"))
                           (not (string-equal name "*eshell*")))
                    (not (buffer-modified-p buffer)))
              (kill-buffer buffer)))
          (setq list (cdr list))))
      (Info-goto-node "(Easymacs)Top")
      (delete-other-windows)))

;;  (defun easymacs-elisp-thing-at-point ()
;;    (interactive)
;;    (let ((fn (function-called-at-point))
;;          (var (variable-at-point)))
;;      (when fn
;;        (describe-function fn))
;;      (when (not (= 0 var))
;;        (describe-variable var))))

  (defun easymacs-next-error (&optional arg)
    "For modes with special error handling functions, only call
    next-error if there is a next-error-capable buffer visible in
    the frame; otherwise call the special function.  For other
    modes just call next-error."
    (interactive)
    (if (memq major-mode '(tex-mode latex-mode texinfo-mode nxml-mode))
        ;; Only use next-error if there is a suitable buffer in the
        ;; frame.  Code taken from simple.el
        (if (delq nil (mapcar (lambda (w)
                                (if (next-error-buffer-p (window-buffer w))
                                    (window-buffer w)))
                              (window-list)))
            (next-error arg)
          (cond
           ((memq major-mode'(tex-mode latex-mode texinfo-mode))
            (TeX-next-error nil))
           ((eq major-mode 'nxml-mode)
            (rng-next-error arg))))
      (next-error arg)))
            
  (defun easymacs-previous-error ()
    (interactive)
    (easymacs-next-error -1))

  (defun easymacs-toggle-accents-mode ()
    (interactive)
    (if (equal current-input-method "latin-1-prefix")
        (progn
          (set-input-method nil)
          (message "Accents mode disabled"))
      (set-input-method "latin-1-prefix")
      (message "Accents mode enabled")))

  ;; From emacs-devel
  (defvar caps-lock-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map [remap self-insert-command] 'self-insert-upcased)
      map))
  
  (define-minor-mode caps-lock-mode
    "When enabled, convert all self-inserting characters to uppercase."
    :lighter " CapsLock")
  
  (defun self-insert-upcased (arg)
    (interactive "p")
    (setq last-command-char (upcase last-command-char))
    (self-insert-command arg))
  
  ) ;; end of easymacs-library


;;;;;;;;;; Isearch enhancements
(defun easymacs-isearch-extras ()

  ;; Isearch equivalents for * and # in Vim
  (defun easymacs-vi-star-hash (forward)
    (if mark-active
	(progn
	  (isearch-mode forward nil nil nil nil)
	  (isearch-yank-string (buffer-substring (region-beginning)
                                                 (region-end)))
	  (deactivate-mark))
      (progn
	(isearch-mode forward nil nil nil t)
	(isearch-yank-string (current-word))))
    (isearch-search-and-update))

  (defun easymacs-vi-star ()
    (interactive)
    (easymacs-vi-star-hash t))

  (defun easymacs-vi-hash ()
    (interactive)
    (easymacs-vi-star-hash nil))

  (defun easymacs-vi-star-sensitive ()
    (interactive)
    (let ((case-fold-search))
      (easymacs-vi-star-hash t)))
      
  (defun easymacs-vi-hash-sensitive ()
    (interactive)
    (let ((case-fold-search))
      (easymacs-vi-star-hash nil)))
      
  
  ;; Some remappings specific to isearch-mode

  ;; Use arrows instead of M-p and M-n in isearch-mode
  ;; to scroll through search history.
  (define-key minibuffer-local-isearch-map
    (kbd "<up>") 'isearch-ring-retreat)
  (define-key minibuffer-local-isearch-map
    (kbd "<down>") 'isearch-ring-advance)
  (define-key minibuffer-local-isearch-map
    (kbd "<C-up>") 'isearch-ring-retreat)
  (define-key minibuffer-local-isearch-map
    (kbd "<C-down>") 'isearch-ring-advance)
  (define-key isearch-mode-map (kbd "<C-up>")        'isearch-ring-retreat)
  (define-key isearch-mode-map (kbd "<C-down>")      'isearch-ring-advance)
  (define-key isearch-mode-map (kbd "<pause>")       'isearch-cancel)
  (define-key isearch-mode-map [escape]              'isearch-cancel)

  (define-key isearch-mode-map (kbd "C-f")           'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "C-S-f")         'isearch-repeat-backward)
  
  ;; Repeated striking of f3 continues the current search
  (define-key isearch-mode-map (kbd "<f3>")           'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "<S-f3>")         'isearch-repeat-backward)

  (define-key isearch-mode-map (kbd "<kp-add>") '(lambda() (interactive)
                                                   (if easymacs-bind-numeric-keypad
                                                       (isearch-repeat-forward)
                                                     (insert "+"))))
  (define-key isearch-mode-map (kbd "<kp-subtract>") '(lambda() (interactive)
                                                        (if easymacs-bind-numeric-keypad
                                                            (isearch-repeat-backward)
                                                          (insert "-"))))
  
  ;; Fix to allow pasting in isearch minibuffer.
  (define-key isearch-mode-map (quote [mouse-2]) 'isearch-yank-kill)
  (define-key isearch-mode-map (quote [down-mouse-2]) nil)
  )

;;;;;;;;;; Major-mode-specific extras 

;;;;;;;;;;  Latex-related configuration
(defun easymacs-latex-extras ()

  ;; Enable auctex 
  (load "auctex.el" nil t t)
 
  ;; Where to find the icons
  (setq preview-datadir (concat easymacs-dir  "auctex/preview/"))

  ;; Where to find the .sty files, etc. (only for systems other then
  ;; w32, which uses semicolons rather than colons).  Mik-tex ignores
  ;; the TEXINPUTS env var, so preview-TeX-style-dir doesn't work
  ;; there.  However, preview-latex apparently offers to install the
  ;; files for Mik-tex users when they start it up, using the native
  ;; package system, so better to go with that.
  (unless easymacs-w32-system
    (setq preview-TeX-style-dir
          (concat ".::" easymacs-dir "auctex/preview/latex/:")))

  (setq preview-TeX-style-dir
        (concat
         (if easymacs-w32-system ".;;" ".::")
         easymacs-dir "auctex/preview/latex/"
         (if easymacs-w32-system ";" ":")))

  (load "preview-latex.el" nil t t)

  (when easymacs-w32-system
    (setq preview-gs-command "gswin32c")
    (require 'tex-mik))

  (setq TeX-parse-self t)
  (setq TeX-auto-save t)
  (setq-default TeX-master 'dwim)
  (setq tex-default-mode 'latex-mode)
  (add-hook 'tex-mode-hook (function (lambda () (setq
						 ispell-parser 'tex))))

  ;; NB. Latex seems to inherit from text-mode, so we don't need to turn on
  ;; auto-fill, outline-minor-mode, or flyspell-mode.
  (add-hook 'LaTeX-mode-hook '(lambda ()
                                (TeX-fold-mode 1)
                                ;; For folding comments
                                (hs-minor-mode 1)
                                (require 'flyspell-babel)
                                (flyspell-babel-mode 1)))
  
  ;; add memoir style to list of styles
  (eval-after-load "latex"
    '(add-to-list 'LaTeX-style-list '("memoir")))

  ;; Enable reftex 
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex) ; with AUCTeX LaTeX mode
  (setq reftex-plug-into-AUCTeX t)

  ;; This forces \index{foo} as default, rather than \index[idx]{foo}
  (setq reftex-index-default-macro '(?i ""))

  ;; Enable bib-cite
  (autoload 'turn-on-bib-cite "bib-cite")
  (add-hook 'LaTeX-mode-hook 'turn-on-bib-cite)
  (setq bib-cite-use-reftex-view-crossref t)
  ;; This is buggy
  (setq bib-use-imenu nil)
  ;; So do this instead
  (add-hook 'LaTeX-mode-hook 'imenu-add-menubar-index)

  (autoload 'bst-mode "bst-mode")
  (add-to-list 'auto-mode-alist '("\\.bst\\'" . bst-mode))

  (setq TeX-newline-function 'newline-and-indent)

  ;; For TeX-command-list
  (load "tex")
  (add-to-list 'TeX-command-list
	       (list "DviPS" "dvips %d -o %f" 'TeX-run-command nil nil) t)
  (add-to-list 'TeX-command-list
	       (list "GV" "gv %s.ps" 'TeX-run-background nil nil) t)
  (add-to-list 'TeX-command-list
	       (list "GSview" "gsview %s.ps" 'TeX-run-background nil nil) t)
  (add-to-list 'TeX-command-list
	       (list "PDFLaTeX" "pdflatex \"\\nonstopmode\\input{%t}\""
		     'TeX-run-LaTeX nil t) t)
  (add-to-list 'TeX-command-list
               (cond
                (easymacs-w32-system
                 (list "View PDF" "AcroRd32 %s.pdf"
                       'TeX-run-background nil nil))
                (easymacs-mac-system
                 (list "View PDF" "open %s.pdf"
                       'TeX-run-background nil nil))
                (t
                 (list "View PDF" "acroread %s.pdf"
                       'TeX-run-background nil nil)))
               t)
  (add-to-list 'TeX-command-list
	 (list "ps2pdf" "ps2pdf %s.ps" 'TeX-run-command nil nil) t)
  (add-to-list 'TeX-command-list
	 (list "xpdf" "xpdf %s.pdf" 'TeX-run-background nil nil) t)

  (defun easymacs-run-latex-on-region ()
    (interactive)
    (let ((begin (region-beginning))
          (end (region-end)))
      (TeX-region-create (TeX-region-file TeX-default-extension)
                         (buffer-substring begin end)
                         (file-name-nondirectory (buffer-file-name))
                         (TeX-current-offset begin)))
    (TeX-command (concat (when TeX-PDF-mode "PDF") "LaTeX") 'TeX-region-file -1))


  (when easymacs-mac-system
    (TeX-global-PDF-mode t)
    (push '("^pdf$" "." "open %o") TeX-output-view-style))
  
  ;; functions used for key-mappings
  (defun easymacs-latex ()
    "Save and (PDF)LaTeX the current document or region."
    (interactive)
    (let (TeX-save-query)
      (TeX-save-document (TeX-master-file))) 
    (if mark-active
        (easymacs-run-latex-on-region)
      (TeX-command (concat (when TeX-PDF-mode "PDF") "LaTeX") 'TeX-master-file -1)))
  
  (defun easymacs-view ()
    "View the pdf or dvi output from the current file."
    (interactive)
    (TeX-view))
  (defun easymacs-dvips ()
    "DVIPS the curent file."
    (interactive)
    (TeX-command "DviPS" 'TeX-master-file))
  (defun easymacs-gv ()
    "GV the curent file."
    (interactive)
    (TeX-command "GV" 'TeX-master-file))
  (defun easymacs-pdflatex ()
    "PDFLaTeX the curent file."
    (interactive)
    (save-some-buffers t)    
    (TeX-command "PDFLaTeX" 'TeX-master-file))
  (defun easymacs-ps2pdf ()
    "ps2pdf the curent file."
    (interactive)
    (TeX-command "ps2pdf" 'TeX-master-file))
  (defun easymacs-xpdf ()
    "xpdf the curent file."
    (interactive)
    (TeX-command "xpdf" 'TeX-master-file))

  ;; From Carsten Dominik
  (defun easymacs-latex-and-dvips ()
    "Run LaTeX and dvips." 
    (interactive) 
    (let ((buf (current-buffer)) ret) 
      (easymacs-latex) 
      (set-buffer buf) 
      (while (and (setq ret (sit-for 1)) 
                  (TeX-process (TeX-master-file)))) 
      (and ret (easymacs-dvips))))
  
  (defun LaTeX-insert-footnote ()
    "Insert a \\footnote{} macro in a LaTeX-document."
    (interactive)
    (TeX-insert-macro "footnote")
    (insert "\n")
    (forward-char)
    (insert " %")
    (unless (looking-at "\n")
      (insert "\n"))
    (backward-char 4))

  (defun LaTeX-insert-emph ()
    "Insert an \\emph{} macro in a LaTeX-document."
    (interactive)
    (TeX-insert-macro "emph"))

  (defun LaTeX-insert-textbf ()
    "Insert a \\textbf{} macro in a LaTeX-document."
    (interactive)
    (TeX-insert-macro "textbf"))

  (defun LaTeX-insert-textsc ()
    "Insert a \\textsc{} macro in a LaTeX-document."
    (interactive)
    (TeX-insert-macro "textsc"))

  (defun LaTeX-insert-uline ()
    "Insert a \\uline{} macro in a LaTeX-document."
    (interactive)
    (TeX-insert-macro "uline"))

;  (add-hook 'LaTeX-mode-hook        '(lambda () 
;				       (enfold-mode 1)))
;  (add-hook 'bibtex-mode-hook       '(lambda () 
;				       (enfold-mode 1)))

  ;; Especially for greek text that uses ( and ) for diacritics,
  ;; we don't want to count () as special matching things.
  ;; Treat hyphens as word chars for word searching
  (add-hook 'LaTeX-mode-hook (lambda ()
			       (modify-syntax-entry ?\( ".")
			       (modify-syntax-entry ?\) ".")
			       (modify-syntax-entry ?-  "w")))

  ;; Make end-sentence take notice of foo.\footnote{bar} Here is the
  ;; default: "[.?!][]\"')}]*\\($\\| $\\| \\| \\)[ \n]*" Should the
  ;; cursor go after the . rather than at the start of the footnote?
  ;; If so, it would be harder to do, since forward-sentence just
  ;; skips back over whitespace (which is wired in).  Or maybe this is
  ;; correct anyway -- it sends us to the first place where whitespace
  ;; can in most instances safely be put.
  (add-hook
   'LaTeX-mode-hook
   (lambda ()
     (setq sentence-end
	   "[.?!][]\"')}]*\\($\\| $\\|	\\|  \\|\\\\footnote{\\)[ 	\n]*")))

  (autoload 'LaTeX-itemize-region "itemize" "Turn bullets into items" t)

  ;; Latex skeletons (file templates)
  ;; For each of these, we need a latex-*-skeleton function
  (setq latex-skeletons '(
			  ("article") 
			  ("letter") 
			  ("letterhead") 
			  ("quickie") ))

  (defvar easymacs-latex-preamble "")
  (defvar easymacs-latex-name "")
  (defvar easymacs-latex-address "")
  
  (defun easymacs-latex-skeletons ()
    
    (define-skeleton latex-article-skeleton
      "Inserts a Latex article skeleton into current buffer."
      'nil
      "\\documentclass[" LaTeX-default-options "]{article}\n"
      easymacs-latex-preamble
      "\\begin{document}\n"
      _ "\n"
      "\\end{document}\n")

    (define-skeleton latex-letter-skeleton
      "Inserts a Latex letter skeleton into current buffer."
      "Recipient: "
      "\\documentclass[" LaTeX-default-options "]{letter}\n"
      easymacs-latex-preamble
      "\\name{" easymacs-latex-name "}\n"
      "\\address{" easymacs-latex-address "}\n"
      "\\begin{document}\n"
      "\\begin{letter}{" str | " *** Recipient *** " "}\n"
      "\\opening{" _ "}\n\n"
      "\\closing{Yours faithfully,}\n"
      "\\end{letter}\n"
      "\\end{document}\n")

    (define-skeleton latex-letterhead-skeleton
      "Inserts a Latex letter skeleton into current buffer.
 This only makes sense for empty buffers."
      "Recipient: "
      "\\documentclass[" LaTeX-default-options "]{letter}\n"
      easymacs-latex-preamble
      "\\name{" easymacs-latex-name "}\n"
      "\\address{~\\\\~\\\\~\\\\}\n"
      "\\begin{document}\n"
      "\\begin{letter}{" str | " *** Recipient *** " "}\n"
      "\\opening{" _ "}\n\n"
      "\\closing{Yours faithfully,}\n"
      "\\end{letter}\n"
      "\\end{document}\n")

    (define-skeleton latex-quickie-skeleton
      "Inserts a Latex skeleton for a flyer, announcement, etc."
      'nil
      "\\documentclass[" LaTeX-default-options "]{article}\n"
      easymacs-latex-preamble
      "%\\usepackage{fullpage}\n"
      "\\usepackage{parskip}\n"
      "\\pagestyle{empty}\n"
      "\\begin{document}\n"
      _ "\n"
      "\\end{document}\n")

    (defun insert-latex-skeleton ()
      (interactive)
      (funcall (intern
		(concat "latex-"
			(completing-read "Type of document: " 
					 latex-skeletons)
			"-skeleton")))))

  (add-hook 'LaTeX-mode-hook 'easymacs-latex-skeletons)

  ;; Prompt for bibtex entry types
  (defun easymacs-insert-bibtex-entry ()
    (interactive)
    (funcall (intern
	      (concat "bibtex-"
		      (completing-read
		       "Entry type (tab for list): " 
		       (mapcar 'car bibtex-entry-field-alist))))))

  (defun easymacs-auctex-help-at-point ()
    (interactive)
    (save-excursion
      (goto-char (or (TeX-find-macro-start)
                     (re-search-backward (regexp-quote TeX-esc) nil t)))
      (re-search-forward (concat (regexp-quote TeX-esc) "\\sw+") nil t)
      (info-lookup-symbol (match-string 0))))

  (defmacro easymacs-restriction (&rest body)
    `(save-excursion
       (save-restriction
         (save-match-data
           (when mark-active
             (narrow-to-region (region-beginning) (region-end))
             (goto-char (point-min)))
           ,@body))))

  ;; This is modified from convert-quotes to respect narrowing and the
  ;; region, and to only convert from point forwards if there is no region.
  (defun easymacs-convert-quotes ()
    "Convert quotes in the region or forward from point for
  proper processing in TeX/LaTeX."
    (interactive)
    ;; convert left double quotes
    (easymacs-restriction
     (while (re-search-forward "\\(^\\|[ \t\n]+\\|(\\|<\\|{\\)\"" nil t nil)
       (replace-match "\\1``" t nil nil nil)))
    ;; convert right double quotes
    (easymacs-restriction
     (while (re-search-forward "\\([^\\\\]\\)\"" nil t nil)
       (replace-match "\\1''" t nil nil nil)))
    ;; convert left single quotes
    (easymacs-restriction
     (while (re-search-forward "\\(^\\|[ \t\n]+\\|(\\|<\\|{\\)'" nil t nil)
       (replace-match "\\1`" t nil nil nil)))
    nil)
  
  (setq TeX-complete-word 'LaTeX-close-environment)

  ;; Beamer stuff from Stephen Eglen
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (when (member "beamer" TeX-active-styles)
                (TeX-add-style-hook "beamer" 'my-beamer-mode))))

  (defun my-beamer-mode ()
    "My adds on for when in beamer."

    ;; when in a Beamer file I want to use pdflatex.
    ;; Thanks to Ralf Angeli for this.
    (TeX-PDF-mode 1)			;turn on PDF mode.

    ;; Tell reftex to treat \lecture and \frametitle as section commands
    ;; so that C-c = gives you a list of frametitles and you can easily
    ;; navigate around the list of frames.
    ;; If you change reftex-section-level, reftex needs to be reset so that
    ;; reftex-section-regexp is correctly remade.
    (require 'reftex)
    (set (make-local-variable 'reftex-section-levels)
         '(("lecture" . 1) ("frametitle" . 2)))
    (reftex-reset-mode)

    ;; add some extra functions.
    (define-key LaTeX-mode-map "\C-cf" 'beamer-template-frame)
    (define-key LaTeX-mode-map "\C-\M-x" 'tex-frame))

  (defun tex-frame ()
    "Run pdflatex on current frame.  
Frame must be declared as an environment."
    (interactive)
    (let (beg)
      (save-excursion
        (search-backward "\\begin{frame}")
        (setq beg (point))
        (forward-char 1)
        (LaTeX-find-matching-end)
        (TeX-pin-region beg (point))
        (letf (( (symbol-function 'TeX-command-query) (lambda (x) "LaTeX")))
          (TeX-command-region)))))

  (defun beamer-template-frame ()
    "Create a simple template and move point to after \\frametitle."
    (interactive)
    (LaTeX-environment-menu "frame")
    (insert "\\frametitle{}")
    (backward-char 1))

  ;; Work-around for MS Windows files with spaces in the name
  (defun easymacs-makeinfo-buffer ()
    (interactive)
    (let ((buffer-file-name
           (when (buffer-file-name)
             (concat "\"" (buffer-file-name) "\""))))
      (makeinfo-buffer)))

)

;;;;;;;;;; HTML/XML editing
(defun easymacs-xml-editing ()

  (load "rng-auto")
  (require 'html-script)

  (autoload 'php-mode "php-mode" "PHP mode" t)
  (autoload 'css-mode "css-mode" "CSS mode" t)
  (autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)

  (autoload 'javascript-mode "javascript-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
  
  (when (boundp 'magic-mode-alist)
    (setq magic-mode-alist
          (cons '("<\\?xml\\s " . nxml-mode) magic-mode-alist)))
;          '(("<\\?xml\\s " . nxml-mode))))
  (fset 'xhtml-mode 'nxml-mode)
  (setq auto-mode-alist (cons '("\\.x?html?$" . html-chooser-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.php[34]?$" . html-chooser-mode) auto-mode-alist))
  (autoload 'html-chooser-mode "html-chooser-mode")

  (setq auto-mode-alist
        (cons '("\\.\\(xml\\|xsl\\|rng\\|tei\\|xsp\\|odd\\)\\'" . nxml-mode)
              auto-mode-alist))

  (add-to-list 'rng-schema-locating-files-default
               (concat easymacs-dir "schemata.xml"))

  ;; CSS-Mode
  (add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
  (setq cssm-indent-function #'cssm-c-style-indenter)
  (setq cssm-indent-level '2)

  ;; DTD mode
  (autoload 'dtd-mode "tdtd" "Major mode for SGML and XML DTDs." t)
  (autoload 'dtd-etags "tdtd"
    "Execute etags on FILESPEC and match on DTD-specific regular expressions."
    t)
  (autoload 'dtd-grep "tdtd" "Grep for PATTERN in files matching FILESPEC." t)
  (setq auto-mode-alist (append (list
                                 '("\\.dcl$" . dtd-mode)
                                 '("\\.dec$" . dtd-mode)
                                 '("\\.dtd$" . dtd-mode)
                                 '("\\.ele$" . dtd-mode)
                                 '("\\.ent$" . dtd-mode)
                                 '("\\.mod$" . dtd-mode))
                                auto-mode-alist))

  
  (autoload 'flyspell-xml-lang-setup "flyspell-xml-lang")
  (autoload 'flyspell-xml-lang-mode "flyspell-xml-lang")
  (add-hook 'nxml-mode-hook 'flyspell-xml-lang-setup)

  (autoload 'htmlize-buffer "htmlize")

  (defun easymacs-insert-tei-skeleton ()
    (interactive)
    (let ((file
           (read-file-name "Template: " (concat easymacs-dir "TEI/p5-templates/") "" t)))
      (unless (equal file "")
        (insert-file file)
        (rng-auto-set-schema))))
  
  (load-library "nxml-mode-os-additions")

  ;; http://groups.yahoo.com/group/emacs-nxml-mode/message/1018
  (defun surround-region-with-tag (tag-name beg end)
    (interactive "sTag name: \nr")
    (save-excursion
      (goto-char beg)
      (insert "<" tag-name ">")
      (goto-char (+ end 2 (length tag-name)))
      (insert "</" tag-name ">")))
  

  ;; This loads unichars.el and xmlunicode.el, neither of which have
  ;; provide statements; so this is to make sure we only load them once.
  (require 'easymacs-xml-unicode)
  (require 'tei-html-docs-p5)
  (require 'nxml-fixes)

  (defun easymacs-unicode-insert ()
    "Always try to insert the correct glyph, even if
xmlunicode.el thinks that Emacs can't display it and so puts in a
character entity instead.  Just provides a prefix arg to
unicode-character-insert."
    (interactive)
    (unicode-character-insert t))
  
  (defun easymacs-nxml-mode-hook ()

    (setq tei-html-docs-p5-dir (concat easymacs-dir "TEI/p5-doc/"))

    (put 'nxml-mode 'flyspell-mode-predicate 'sgml-mode-flyspell-verify)
    (easymacs-flyspell-mode)
    (goto-address)

    (define-key nxml-mode-map "\C-m"   'newline-and-indent)
    (local-set-key (kbd "<S-return>")  'nxml-complete)

    (local-set-key (kbd "<S-f1>")      'tei-html-docs-p5-element-at-point)

    (local-set-key (kbd "<M-f10>")     'drkm-nxml:ancestor-axis-path)
    (local-set-key (kbd "<M-S-f10>")   'iso8879-character-insert)
    (local-set-key (kbd "<C-f10>")     'easymacs-insert-tei-skeleton)

    (local-set-key (kbd "<f11>")       'browse-url-of-buffer)
    (local-set-key (kbd "<S-f11>")     'easymacs-xslt-process-buffer)
    (local-set-key (kbd "<C-f11>")     'rng-reload-schema-file)
    (local-set-key (kbd "<C-S-f11>")   'rng-find-schema-file)
    (local-set-key (kbd "<M-f11>")     'surround-region-with-tag)

    (local-set-key (kbd "<S-f12>")     'nxml-finish-element)
    (local-set-key (kbd "<M-f12>")     'nxml-dynamic-markup-word)
    (local-set-key (kbd "<M-S-f12>")   'nxml-split-element)
    (local-set-key (kbd "<C-f12>")     'nxml-balanced-close-start-tag-block)
    (local-set-key (kbd "<C-S-f12>")   'nxml-balanced-close-start-tag-inline)
    (local-set-key (kbd "M-p")         'easymacs-nxml-insert-paragraph)
    (define-key    nxml-mode-map [menu-bar      unichar]
          (cons "UniChar" unicode-character-menu-map))
    (when (and (buffer-file-name)
               (string-match "\\.\\(x?html\\|php[34]?\\)$"
                             (file-name-sans-versions (buffer-file-name))))
      (easymacs-xhtml-extras))
    )
  (add-hook 'nxml-mode-hook 'easymacs-nxml-mode-hook)
  (add-hook 'nxml-mode-hook 'turn-on-auto-fill)

  (defun easymacs-html-mode-hook () 
    (local-set-key (kbd "<f11>")     'browse-url-of-buffer)
    (local-set-key (kbd "<S-f11>")   'sgml-validate)
    (local-set-key (kbd "M-p")       'html-paragraph)
    (local-set-key "\C-m"   'newline-and-indent)
    (require 'compile) ; for sgml-validate
    )
  (add-hook 'html-mode-hook 'easymacs-html-mode-hook)
  (add-hook 'html-mode-hook 'turn-on-auto-fill)

  (defun easymacs-nxml-insert-paragraph ()
    (interactive)
    (insert "<p")
    (call-interactively 'nxml-balanced-close-start-tag-block))

  
  (defun easymacs-xhtml-outline-level ()
    (let ((tag (buffer-substring (match-beginning 1) (match-end 1))))
      (if (eq (length tag) 2)
          (- (aref tag 1) ?0)
        0)))

  (defun easymacs-xhtml-extras ()
    (make-local-variable 'outline-regexp)
    (setq outline-regexp "\\s *<\\([h][1-6]\\|html\\|body\\|head\\)\\b")
    (make-local-variable 'outline-level)
    (setq outline-level 'easymacs-xhtml-outline-level)
    (outline-minor-mode 1)
    (hs-minor-mode 1))

  (add-to-list 'hs-special-modes-alist
	       '(nxml-mode
		 "<!--\\|<[^/>]>\\|<[^/][^>]*[^/]>"
                 ""
		 "<!--" ;; won't work on its own; uses syntax table
                 (lambda (arg) (easymacs-nxml-forward-element))
		 nil
		 ))

  (defun easymacs-nxml-forward-element ()
    (let ((nxml-sexp-element-flag))
      (setq nxml-sexp-element-flag (not (looking-at "<!--")))
      (unless (looking-at outline-regexp)
        (condition-case nil
            (nxml-forward-balanced-item 1)
          (error nil)))))

  ;; Find an installed xslt processor
  (defvar easymacs-xslt-processor-found nil)
  (if (executable-find "xsltproc")
      (setq easymacs-xslt-processor-found 'xsltproc)
    (if (executable-find "java")
        (save-excursion
          (set-buffer (get-buffer-create " *easymacs-xslt-tmp*"))
          (erase-buffer)
          (let ((status (call-process "java" nil t nil
                                      "org.apache.xalan.xslt.Process" "-v")))
            (goto-char (point-min))
            (if (re-search-forward ">> Xalan" nil t)
                (setq easymacs-xslt-processor-found 'xalan)
              (erase-buffer)
              (let ((status (call-process "java" nil t nil
                                          "com.icl.saxon.StyleSheet" "-?")))
                (goto-char (point-min))
                (when (re-search-forward "Usage:" nil t)
                  (setq easymacs-xslt-processor-found 'saxon)))))
          (kill-buffer nil))))
  
  (defcustom easymacs-xslt-processor easymacs-xslt-processor-found
    "*The default XSLT processor to be applied to an XML document."
    :group 'easymacs
    :type '(choice
            (const :tag "xsltproc" xsltproc)
            (const :tag "Xalan" xalan)
            (const :tag "Saxon" saxon)))

  (defun easymacs-xslt-processor ()
    (interactive)
    (let ((processor
           (intern 
            (completing-read "What XSL processor to use? "
                             '("xsltproc" "xalan" "saxon")
                             nil t))))
      (customize-set-variable 'easymacs-xslt-processor processor)
      (customize-save-customized)))


  (defun easymacs-xslt-process-buffer ()
    "Run buffer through an xslt processor, where the stylesheet
    is specified via an xml-stylesheet processing directive"
    (interactive)
    (save-buffer)
    (let ((input-file buffer-file-name)
          (stylesheet (easymacs-get-tei-stylesheet)))
      (switch-to-buffer (get-buffer-create "*xslt-output*"))
      (erase-buffer)
      (when stylesheet
        (cond
         ;; no point -- file needs to be saved to run latex anyway
;         ((string-match "latex" stylesheet) (LaTeX-mode))
         ;; TEI html stylesheets do not produce xhtml
         ((string-match "html" stylesheet) (html-mode))))
      (easymacs-xslt-run input-file stylesheet)))
      
  (defun easymacs-xslt-run (input-file &optional stylesheet)
    (if (and stylesheet
             (not (file-exists-p stylesheet)))
        (error (concat "File doesn't exist: " stylesheet))
      (cond
       ((eq easymacs-xslt-processor 'xsltproc)
        (if stylesheet
            (call-process "xsltproc" nil t t
                          stylesheet input-file)
          (call-process "xsltproc" nil t t
                        input-file)))
       ((eq easymacs-xslt-processor'xalan)
        (if stylesheet
            (call-process "java" nil t t
                          "org.apache.xalan.xslt.Process"
                          "-XSL" stylesheet "-IN" input-file)
          (call-process "java" nil t t
                        "org.apache.xalan.xslt.Process"
                        "-IN" input-file)))
       ((eq easymacs-xslt-processor 'saxon)
        (if stylesheet
            (call-process "java" nil t t
                          "com.icl.saxon.StyleSheet" input-file stylesheet)
          (call-process "java" nil t t
                        "com.icl.saxon.StyleSheet" input-file)))
       (t
        (message "Unknown xslt processor")))))

  
  (defvar easymacs-xslt-buffer-stylesheet)
  (make-variable-buffer-local 'easymacs-xslt-buffer-stylesheet)

  (defun easymacs-get-tei-stylesheet (&optional force)
    "Return stylesheet for this buffer. Unless one has aleady
    been specified, ask user whether to use one of the
    stylesheets distributed by the TEI, or some other.  Returns
    NIL if the stylesheet is specified in the file via a
    processing directive.  With a prefix, ignore a previously
    selected stylesheet for this buffer."
    (interactive "p")
    (if (and (not force)
             easymacs-xslt-buffer-stylesheet)
        easymacs-xslt-buffer-stylesheet
      (unless (save-excursion
                (goto-char (point-min))
                (re-search-forward
                 ;; Keep it simple -- no need to capture attribute -- let processor find it
                 ;;"<\\?xml-stylesheet.* href[ \t\n\r]*=[ \t\n\r]*\"\\([^\"]+\\)\"")
                 "<\\?xml-stylesheet.* href" nil t))
        (let* ((tei-version (case (intern (third (easymacs-document-element)))
                              ('TEI.2 "p4")
                              ('TEI "p5")))
               (path (when tei-version
                       (concat easymacs-dir "TEI/xsl/" tei-version "/"))))
          (setq easymacs-xslt-buffer-stylesheet
                (if tei-version
                    (ecase
                        (intern (completing-read "What TEI stylesheet to use? "
                                                 '("html" "latex" "fo" "other")
                                                 nil t ))
                      ('html  (concat path "html/tei.xsl")) 
                      ('latex (concat path "latex/tei.xsl")) 
                      ('fo    (concat path "fo/tei.xsl"))
                      ('other (easymacs-read-file-stylesheet)))
                  (easymacs-read-file-stylesheet)))))))

  
  (defun easymacs-read-file-stylesheet ()
    (expand-file-name
     (read-file-name "Stylesheet: "
                     nil nil t)))

  (defun easymacs-document-element ()
    (interactive)
    (let ((rng-cached-document-element nil))
      (rng-document-element)))
  
  )



;;;;;;;;;; Enable other commonly desired programming modes 
(defun easymacs-programming-extras ()

  ;; C
  (autoload 'blockcomment-mode "block-comm" "" t)
  (autoload 'turn-on-blockcomment-mode "block-comm" "" t)
  (defun easymacs-c-mode-hook ()
    (define-key c-mode-base-map "\C-m" 'newline-and-indent)
    (turn-on-blockcomment-mode))
  (add-hook 'c-mode-hook 'easymacs-c-mode-hook)

  ;; Perl
  (setq auto-mode-alist
        (append
         '(("\\.\\([pP]\\([Llm]\\|erl\\)\\|al\\|pod\\|cgi\\)\\'" . perl-mode))
         auto-mode-alist))

  (add-to-list 'hs-special-modes-alist
               '(perl-mode "{" "}" "#" nil hs-c-like-adjust-block-beginning))

  ;; From cperl
  (defvar easymacs-function-name-regexp-perl
    (concat
     "^\\("
     "[ \t]*\\(sub\\|package\\)[ \t\n]+\\([a-zA-Z_0-9:']+\\)[ \t]*\\(([^()]*)[ \t]*\\)?"
     "\\|"
     "=head\\([12]\\)[ \t]+\\([^\n]+\\)$"
     "\\)"))

  (defvar easymacs-perl-outline-regexp
    (concat easymacs-function-name-regexp-perl "\\|" "\\`"))

  (defun easymacs-perl-outline-level ()
    (looking-at outline-regexp)
    (cond ((not (match-beginning 1)) 0)	; beginning-of-file
          ((match-beginning 2)
           (if (eq (char-after (match-beginning 2)) ?p)
               0				; package
             1))				; sub
          ((match-beginning 5)
           (if (eq (char-after (match-beginning 5)) ?1)
               1				; head1
             2))				; head2
          (t 3)))				; should not happen

  
  (add-hook 'perl-mode-hook 'easymacs-perl-mode-hook)
  (defun easymacs-perl-mode-hook ()
    (require 'perldoc)

    (make-local-variable 'outline-regexp)
    (setq outline-regexp easymacs-perl-outline-regexp)
    (make-local-variable 'outline-level)
    (setq outline-level 'easymacs-perl-outline-level)
    
    (define-key perl-mode-map "\C-m" 'newline-and-indent)
    (local-set-key (kbd "<S-f1>") 'perldoc-at-point))

  ;; elisp

  (defun easymacs-elisp-help ()
    (interactive)
    (let ((sym (intern-soft (thing-at-point 'symbol))))
      (cond
        ((and sym
              (fboundp sym)
              (not (boundp sym)))
         (describe-function sym))
        ((and sym
              (not (fboundp sym))
              (boundp sym))
         (describe-variable sym))
        ((and sym
              (fboundp sym)
              (boundp sym))
         (if (yes-or-no-p "Both value and function are bound; describe function? ")
             (describe-function sym)
             (describe-variable sym)))
        (t
         (call-interactively 'describe-function)))))
  
  (defun easymacs-elisp-mode-hook () 
    (local-set-key "\C-m" 'newline-and-indent)
    (local-set-key (kbd "<S-return>") 'lisp-complete-symbol)

    (local-set-key (kbd "<S-f1>") 'easymacs-elisp-help)
    (local-set-key (kbd "<f12>") 'eval-defun)
    (local-set-key (kbd "<M-f12>") 'eval-buffer)
    (local-set-key (kbd "<f11>") 'eval-last-sexp)
    (local-set-key (kbd "<M-f10>") 'check-parens)
  )
  (add-hook 'emacs-lisp-mode-hook 'easymacs-elisp-mode-hook)

  (autoload 'turn-on-eldoc-mode "eldoc" nil t)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
 
  ;; makefiles
  (defun easymacs-makefile-mode-hook ()
    (setq fold-dwim-toggle-selective-display t))
  (add-hook 'makefile-mode-hook 'easymacs-makefile-mode-hook)
  
  ;; For various programming languages, turn on folding, outlining,
  ;; index-menu and flyspell for comments

  (defun easymacs-programming-keys ()
    (local-set-key (kbd "<f12>") 'mode-compile))


  (mapcar '(lambda (hook)
	     (add-hook hook '(lambda () (progn 
					  (easymacs-flyspell-mode 't)
					  (easymacs-programming-keys)
                                          ))))
	  '(c-mode-hook         
	    c++-mode-hook       
	    cperl-mode-hook     
	    perl-mode-hook     
	    python-mode-hook    
	    sh-mode-hook        
	    java-mode-hook      
	    lisp-mode-hook      
	    emacs-lisp-mode-hook
	    css-mode-hook
	    php-mode-hook
            makefile-mode-hook
	    ))      

  (mapcar '(lambda (hook)
	     (add-hook hook '(lambda () (progn 
					  (hs-minor-mode 1) 
                                          ))))
	  '(c-mode-hook         
	    c++-mode-hook       
	    sh-mode-hook        
	    java-mode-hook      
	    css-mode-hook
	    php-mode-hook
	    ))      

  (mapcar '(lambda (hook)
	     (add-hook hook '(lambda () (progn 
					  (outline-minor-mode)
                                          ))))
	  '(emacs-lisp-mode-hook
	    perl-mode-hook     
	    python-mode-hook    
	    lisp-mode-hook      
	    ))      


  )


;;;;;;;;;; Key mappings ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Basic keyboard mappings that correspond to the usage of widespread
;; human interface guidelines
(defun easymacs-basic-keys ()

  (global-set-key (kbd "C-n")   '(lambda () (interactive)
                                   (let ((last-nonmenu-event nil))
                                     (call-interactively 'find-file))))
  (defvar easymacs-open-file-function
    (if (fboundp 'find-file-existing) 'find-file-existing 'find-file))
  (global-set-key (kbd "C-o")   '(lambda () (interactive)
                                   (let ((last-nonmenu-event nil))
                                     (call-interactively easymacs-open-file-function))))
  (global-set-key (kbd "C-S-o") '(lambda () (interactive)
                                   (let ((last-nonmenu-event nil))
                                     (call-interactively 'insert-file))))
  (global-set-key (kbd "C-s")   '(lambda () (interactive)
                                   (let ((last-nonmenu-event nil))
                                     (call-interactively 'save-buffer))))
  (global-set-key (kbd "C-S-s") '(lambda () (interactive)
                                   (let ((last-nonmenu-event nil))
                                     (call-interactively
                                      'write-file))))
  (global-set-key (kbd "M-s") '(lambda () (interactive)
                                   (let ((last-nonmenu-event nil))
                                     (save-some-buffers nil t))))
  (global-set-key (kbd "C-w")   'easymacs-kill-buffer)
  (global-set-key (kbd "C-S-w") 'easymacs-kill-some-buffers)

  (global-set-key (kbd "C-f")   'isearch-forward)
  (global-set-key (kbd "C-S-f") 'isearch-backward)
  (global-set-key (kbd "C-r")   'replace-string)
  (global-set-key (kbd "C-S-r") 'query-replace)
  (global-set-key (kbd "M-r")   'replace-regexp)
  (global-set-key (kbd "M-S-r") 'query-replace-regexp)

  (global-set-key (kbd "C-q")   'save-buffers-kill-emacs)
  (global-set-key (kbd "C-S-q") 'kill-emacs)
  (global-set-key (kbd "C-@") 'quoted-insert)
  
  (global-set-key (kbd "C-p")   '(lambda () (interactive)
                                   (pr-ps-buffer-preview 1 nil))) 
  (global-set-key (kbd "C-a")   'mark-whole-buffer)

  (global-set-key (kbd "<home>")    'easymacs-smart-home)
  (global-set-key (kbd "<end>")     'easymacs-smart-end)
  (global-set-key (kbd "<C-prior>") 'top-of-screen)
  (global-set-key (kbd "<C-next>")  'bottom-of-screen)

  (global-set-key (kbd "<M-insert>")      'easymacs-copy-line)
  (global-set-key (kbd "<M-S-delete>")    'easymacs-copy-line) ; when insert key is missing
  (global-set-key (kbd "<M-clear>")      'easymacs-copy-line) ; for Mac
  (global-set-key (kbd "<M-delete>")      '(lambda () (interactive) 
                                             (beginning-of-line) (kill-line)))
  (global-set-key (kbd "<S-backspace>")   'backward-kill-word)
  (global-set-key (kbd "<C-backspace>")   'backward-kill-word)
  (global-set-key (kbd "<C-delete>")      'kill-word)
  (global-set-key (kbd "<M-home>")        'easymacs-delete-to-start-of-buffer)
  (global-set-key (kbd "<M-end>")         'easymacs-delete-to-end-of-buffer) 
  
  (global-set-key (kbd "<M-down>")        'scroll-up-one-line-sticky)
  (global-set-key (kbd "<M-up>")          'scroll-down-one-line-sticky)

  ;; C-\ to toggle the current input method off and on; M-\ to choose a
  ;; different input method
  (global-set-key (kbd "M-\\")   'set-input-method)
  (global-set-key (kbd "<M-S-SPC>") 'delete-horizontal-space)

  (global-set-key (kbd "C-z")   'aquamacs-undo)
  (global-set-key (kbd "C-y")   'aquamacs-redo)
  
  (global-set-key (kbd "<C-tab>")           'buffer-stack-down)
  (global-set-key (kbd "<C-S-tab>")         'buffer-stack-up)
  (global-set-key (kbd "<C-S-iso-lefttab>") 'buffer-stack-up)

  ;; For Mac (with Mac-style modifiers)
  (global-set-key (kbd "<M-tab>")           'buffer-stack-down)
  (global-set-key (kbd "<M-S-tab>")         'buffer-stack-up)

  (global-set-key (kbd "M-u")   'easymacs-upcase-word-or-region)
  (global-set-key (kbd "M-l")   'easymacs-downcase-word-or-region)
  (global-set-key (kbd "M-c")   'easymacs-capitalize-word-or-region)

  (global-set-key (kbd "M-C")   '(lambda () (interactive) (capitalize-word -1)))
  (global-set-key (kbd "M-L")   '(lambda () (interactive) (downcase-word -1)))
  (global-set-key (kbd "M-U")   '(lambda () (interactive) (upcase-word -1)))

  (global-set-key (kbd "C-e")   'joc-toggle-case)
  (global-set-key (kbd "C-S-e") 'joc-toggle-case-backwards)

  (global-set-key [escape]          'keyboard-escape-quit)
  (global-set-key (kbd "<C-break>") 'easymacs-suspend) 
  (global-set-key (kbd "<C-pause>") 'easymacs-suspend) 
  (global-set-key (kbd "<M-pause>") 'calendar)

  ;; In "scroll-lock" position -- useful only on Macs
  (global-set-key (kbd "<C-f14>") 'easymacs-mac-swap-modifiers)
  (global-set-key (kbd "<M-f14>") 'easymacs-mac-swap-modifiers)
  
  (global-set-key (kbd "M-g")       'goto-line)
  (global-set-key (kbd "M-Q")       'easymacs-unfill-paragraph) 
  (global-set-key (kbd "C-S-v")     'browse-kill-ring)       

  (global-set-key (kbd "M-`")   'easymacs-next-error)
  (global-set-key (kbd "C-`")   'easymacs-previous-error)
  (global-set-key (kbd "C-/")   'easymacs-bounce-sexp)
  (global-set-key (kbd "C-M-/") 'easymacs-mark-sexp)
  (global-set-key (kbd "M-/")   'browse-url-at-point)
 
  )

(defmacro easymacs-kp-bind (key num-on num-off)
  `(global-set-key (kbd ,key)
                   '(lambda ()
                      (interactive)
                      (if easymacs-bind-numeric-keypad
                          (funcall ,num-on)
                        (insert ,num-off)))))
  
(defun easymacs-numeric-keypad-keys () 

  ;; These are irrespective of NumLock status
  (global-set-key (kbd "<pause>")      'easymacs-keypad-toggle)
  (global-set-key (kbd "<S-pause>")    'easymacs-keypad-show)
  (easymacs-kp-bind "<kp-enter>"       'save-buffer "\n")

  (easymacs-kp-bind "<kp-add>"         'isearch-forward "+")
  (easymacs-kp-bind "<kp-subtract>"    'isearch-backward "-")
  
  (easymacs-kp-bind "<M-kp-add>"       'align-cols "+")
  (easymacs-kp-bind "<M-kp-subtract>"  'easymacs-align-cols-compact "-")
  
  (easymacs-kp-bind "<kp-multiply>"    'joc-toggle-case "*")
  (easymacs-kp-bind "<S-kp-multiply>"  'joc-toggle-case-backwards "*") 
  (easymacs-kp-bind "<kp-divide>"      'easymacs-bounce-sexp "/")
  (easymacs-kp-bind "<M-kp-divide>"    'easymacs-mark-sexp "/")

  ; numbers
  (easymacs-kp-bind "<kp-2>"           'scroll-up-one-line "2")
  (easymacs-kp-bind "<kp-down>"        'scroll-up-one-line "2")
  (easymacs-kp-bind "<kp-8>"           'scroll-down-one-line "8")
  (easymacs-kp-bind "<kp-up>"          'scroll-down-one-line "8")

  (easymacs-kp-bind "<kp-6>"           'buffer-stack-down "6")
  (easymacs-kp-bind "<kp-right>"       'buffer-stack-down "6")
  (easymacs-kp-bind "<kp-4>"           'buffer-stack-up "4")
  (easymacs-kp-bind "<kp-left>"        'buffer-stack-up "4")

  (easymacs-kp-bind "<kp-5>"           '(lambda () (interactive)
                                          (buffer-stack-show-position)) "5")
  (easymacs-kp-bind "<kp-begin>"       '(lambda () (interactive)
                                          (buffer-stack-show-position)) "5")

  (easymacs-kp-bind "<kp-7>"           'beginning-of-buffer "7")
  (easymacs-kp-bind "<kp-home>"        'beginning-of-buffer "7")
  (easymacs-kp-bind "<kp-1>"           'end-of-buffer "1")
  (easymacs-kp-bind "<kp-end>"         'end-of-buffer "1")
  (easymacs-kp-bind "<C-kp-7>"           'beginning-of-buffer "7")
  (easymacs-kp-bind "<C-kp-home>"        'beginning-of-buffer "7")
  (easymacs-kp-bind "<C-kp-1>"           'end-of-buffer "1")
  (easymacs-kp-bind "<C-kp-end>"         'end-of-buffer "1")

  (easymacs-kp-bind "<kp-9>"           'top-of-screen "9")
  (easymacs-kp-bind "<kp-prior>"       'top-of-screen "9")
  (easymacs-kp-bind "<kp-3>"           'bottom-of-screen "3")
  (easymacs-kp-bind "<kp-next>"        'bottom-of-screen "3")
  
  ;; decimal
  (easymacs-kp-bind "<kp-decimal>"     'easymacs-toggle-accents-mode ".")
  (easymacs-kp-bind "<kp-delete>"      'easymacs-toggle-accents-mode ".")
  (easymacs-kp-bind "<S-kp-decimal>"   'easymacs-find-next-unsafe-char ".")
  (easymacs-kp-bind "<S-kp-delete>"    'easymacs-find-next-unsafe-char ".")
  (easymacs-kp-bind "<M-kp-decimal>"   'repeat-complex-command ".")
  (easymacs-kp-bind "<M-kp-delete>"    'repeat-complex-command ".")
  (easymacs-kp-bind "<C-kp-decimal>"   'quoted-insert ".")
  (easymacs-kp-bind "<C-kp-delete>"    'quoted-insert ".")
  ;; 0
  (easymacs-kp-bind "<kp-0>"           '(lambda () (fill-paragraph nil)) "0")
  (easymacs-kp-bind "<kp-insert>"      '(lambda () (fill-paragraph nil)) "0")
  (easymacs-kp-bind "<M-kp-0>"         'blank-mode "0")
  (easymacs-kp-bind "<M-kp-insert>"    'blank-mode "0")
  (easymacs-kp-bind "<C-kp-0>"         'linum "0")
  (easymacs-kp-bind "<C-kp-insert>"    'linum "0")
  
  )

;; Bindings for function keys
(defun easymacs-function-keys ()

;;;;;; F1: File operations and finding things

  (global-set-key (kbd "<S-M-f1>")     '(lambda () (interactive)
                                      (Info-goto-node "(Easymacs)Top")
                                      (delete-other-windows)))
  (global-set-key (kbd "<S-f1>")   '(lambda () (interactive)
                                      (message
    "This type of file does not provide context-sensitive help")))
  (global-set-key (kbd "<f1>")   'find-file)
  (global-set-key (kbd "<M-f1>") 'recentf-open-files) 
  (global-set-key (kbd "<C-f1>")   'insert-file)
  (global-set-key (kbd "<S-C-f1>") 'locate)
  ;; For Mac (with pc-style modifiers)
  (global-set-key (kbd "<C-M-f1>")   'insert-file)
  
;;;;;; F2 Bookmark operations

  (global-set-key (kbd "<f2>")     'easymacs-bm-next)
  (global-set-key (kbd "<S-f2>")   'easymacs-bm-previous)
  (global-set-key (kbd "<M-f2>")   'bm-toggle)
  (global-set-key (kbd "<C-f2>")   'bookmark-bmenu-list)
  (global-set-key (kbd "<S-C-f2>") 'bookmark-set)

;;;;;; F3 Search operations

  (global-set-key (kbd "<f3>")     'easymacs-vi-star)
  (global-set-key (kbd "<S-f3>")   'easymacs-vi-hash)
  (global-set-key (kbd "<M-f3>")   'replace-regexp)
  (global-set-key (kbd "<M-S-f3>") 'query-replace-regexp)
  (global-set-key (kbd "<C-f3>")   'occur)

;;;;;; F4: Window operations

  (global-set-key (kbd "<f4>")     'delete-other-windows)
  (global-set-key (kbd "<S-f4>")   'other-window)
  (global-set-key (kbd "<C-f4>")   'easymacs-kill-buffer)
  (global-set-key (kbd "<M-f4>")   'save-buffers-kill-emacs)
  (global-set-key (kbd "<S-C-f4>") 'split-window-horizontally)
  (global-set-key (kbd "<S-M-f4>") 'split-window-vertically)

;;;;;; F5: Text shortcuts

;  (global-set-key (kbd "<f5>")     'easymacs-dabbrev-expand)
  (global-set-key (kbd "<f5>")     'dabbrev-expand)
  (global-set-key (kbd "<S-f5>")   'easymacs-dup-region-or-line)
  (global-set-key (kbd "<C-f5>")   'easymacs-copy-char-above)
  (global-set-key (kbd "<S-C-f5>") 'easymacs-copy-char-below)
  (if (featurep 'kmacro)
      (progn
        (global-set-key (kbd "<M-f5>")   'kmacro-end-or-call-macro)
        (global-set-key (kbd "<S-M-f5>") 'kmacro-start-macro-or-insert-counter))
    (global-set-key (kbd "<M-f5>")   'easymacs-macro-end-or-call)
    (global-set-key (kbd "<S-M-f5>") 'easymacs-macro-start-or-counter))

  ;; For Mac (with Mac-style modifiers)
  (global-set-key (kbd "<C-M-f5>")   'easymacs-copy-char-above)

;;;;;; F6: Commenting, etc

;  (global-set-key (kbd "<f6>")      'easymacs-comment-line-or-region)
  (global-set-key (kbd "<f6>")      'easymacs-toggle-comment-line-or-region)
;  (global-set-key (kbd "<S-f6>")    'easymacs-uncomment-line-or-region)
  (global-set-key (kbd "<S-f6>")    'easymacs-dup-and-comment-line-or-region)
  (global-set-key (kbd "<C-f6>")    'easymacs-prepend-line-or-region)
  (global-set-key (kbd "<S-C-f6>")  'easymacs-unprepend-line-or-region)
  (global-set-key (kbd "<M-f6>")    'easymacs-append-line-or-region)
  (global-set-key (kbd "<S-M-f6>")  'easymacs-unappend-line-or-region)

;;;;;; F7: Folding 

 (global-set-key (kbd "<f7>")       'fold-dwim-toggle)
; (global-set-key (kbd "<S-f7>")     'fold-dwim-toggle-all)
 (global-set-key (kbd "<M-f7>")     'fold-dwim-hide-all)
 (global-set-key (kbd "<S-M-f7>")   'fold-dwim-show-all)
 (global-set-key (kbd "<C-f7>")     'outline-next-visible-heading)
 (global-set-key (kbd "<S-C-f7>")   'outline-previous-visible-heading)

   ;; For Mac (with pc-style modifiers)
 (global-set-key (kbd "<C-M-f7>")     'outline-next-visible-heading)
 (global-set-key (kbd "<S-C-M-f7>")   'outline-previous-visible-heading)

;;;;;; F8 Dictionary and Spelling

  (global-set-key (kbd "<f8>")      'flyspell-auto-correct-previous-word)
  (global-set-key (kbd "<S-f8>")    'ispell-complete-word)
  (global-set-key (kbd "<C-f8>")    '(lambda () (interactive)
                                       (if mark-active
                                           (flyspell-region)
                                         (flyspell-buffer))))
  (global-set-key (kbd "<S-C-f8>")  '(lambda () (interactive)
                                       (if mark-active
                                           (ispell-region)
                                         (ispell-buffer))))
  (global-set-key (kbd "<M-f8>")    'dictionary-search)
  (global-set-key (kbd "<S-M-f8>")  'ispell-change-dictionary)
;  (global-set-key (kbd "<S-M-f8>")  'easymacs-toggle-flyspell-mode)

  ;; For Mac (with pc-style modifiers)
  (global-set-key (kbd "<C-M-f8>")    '(lambda () (interactive)
                                       (if mark-active
                                           (flyspell-region)
                                         (flyspell-buffer))))


;;;;;; F9: Buffers, etc.

  (global-set-key (kbd "<f9>")      'ibuffer)
  (global-set-key (kbd "<S-f9>")    'iswitchb-buffer)
  (global-set-key (kbd "<C-f9>")    'speedbar-get-focus)
  (global-set-key (kbd "<M-f9>")    'easymacs-eshell)
;  (global-set-key (kbd "<S-M-f9>")  'locate)
  (global-set-key (kbd "<S-C-f9>")  'grep) ; NB. overridden in latex-mode

;;;;;; F10 -- F12 are for mode-specific functions
;;; (except that F10 is used to access the menus)
;;; Individual modes may override these defaults

  (global-set-key (kbd "<S-f10>")   'easymacs-unicode-insert)
  (global-set-key (kbd "<f12>")     'compile)

  
;;;;;; Latex-mode key mappings

  (add-hook 'LaTeX-mode-hook
    '(lambda () (interactive)
       
       (define-key LaTeX-mode-map (kbd "<f12>")   'easymacs-latex)
       (define-key LaTeX-mode-map (kbd "<f11>")   'easymacs-view)
       (define-key LaTeX-mode-map (kbd "<S-f12>") 'TeX-command-master)
       (define-key LaTeX-mode-map (kbd "<S-f11>") 'TeX-recenter-output-buffer)
       (define-key LaTeX-mode-map (kbd "<M-f12>") 'easymacs-dvips)
       (define-key LaTeX-mode-map (kbd "<M-f11>") 'easymacs-gv)
       (define-key LaTeX-mode-map (kbd "<M-S-f12>") 'easymacs-latex-and-dvips)

       (define-key LaTeX-mode-map (kbd "<C-f12>") 'TeX-PDF-mode)
       (define-key LaTeX-mode-map (kbd "<C-S-f12>") 'easymacs-ps2pdf)
       
       (define-key LaTeX-mode-map (kbd "<S-f1>") 'easymacs-auctex-help-at-point)
       (define-key LaTeX-mode-map (kbd "<C-S-f10>") 'tex-frame)
       
       (define-key LaTeX-mode-map (kbd "<C-f10>") 'insert-latex-skeleton)
       (define-key LaTeX-mode-map (kbd "<M-f10>") 'LaTeX-environment)
       (define-key LaTeX-mode-map (kbd "<M-S-f10>") '(lambda () (interactive)
                                                       (LaTeX-environment t)))
       
       (define-key LaTeX-mode-map (kbd "<S-C-f9>")  'reftex-grep-document))) 

  (add-hook 'Texinfo-mode-hook
            '(lambda () (interactive)
              ;; Hmmm.  These two shouldn't be necessary.
              (require 'makeinfo)
              (require 'compile)
              (define-key Texinfo-mode-map (kbd "<f12>")  'easymacs-makeinfo-buffer)
              (define-key Texinfo-mode-map (kbd "<S-f1>") 'easymacs-auctex-help-at-point)
              (define-key Texinfo-mode-map (kbd "<M-f10>") 'Texinfo-environment)))
  
  (defun easymacs-latex-shortcuts ()
    (local-set-key (kbd "C-e") 'LaTeX-insert-emph)
    (local-set-key (kbd "C-b") 'LaTeX-insert-textbf)
    (local-set-key (kbd "M-p") 'LaTeX-insert-textsc)
    (local-set-key (kbd "C-_") 'LaTeX-insert-uline)
    (local-set-key (kbd "M-f") 'LaTeX-insert-footnote)
    (local-set-key (kbd "C-\"") 'easymacs-convert-quotes)
    (local-set-key (kbd "<S-return>") 'TeX-complete-symbol))

  (add-hook 'LaTeX-mode-hook 'easymacs-latex-shortcuts)

;;;;;; Bibtex

  (add-hook 'bibtex-mode-hook 'easymacs-latex-shortcuts)
  (add-hook 'bibtex-mode-hook (lambda ()
				(local-set-key (kbd "<f12>")
					       'easymacs-insert-bibtex-entry)
				(local-set-key (kbd "M-q")
					       'bibtex-fill-entry)))
 
   
  (add-hook 'reftex-index-phrases-mode-hook 'easymacs-latex-shortcuts)
  )

;;;;;;;;;; Here we call the functions defined above

(easymacs-setup-paths)

(easymacs-general-settings)
(easymacs-advice)
(easymacs-library)

(easymacs-scrolling)
(easymacs-cua-cursor)
(easymacs-folding)
(easymacs-flyspell-dict)
(easymacs-misc-packages)

(easymacs-switching-buffers)
(easymacs-bookmarks)
(easymacs-dired-extras)
(easymacs-calendar)

(easymacs-isearch-extras)
(easymacs-latex-extras)
(easymacs-xml-editing)
(easymacs-programming-extras)
;; load the key bindings
(easymacs-basic-keys)
(easymacs-function-keys)
(easymacs-numeric-keypad-keys)

;; do the right thing for windowed and non-windowed use
(cond
 (window-system
  ;; Window system in use
  ;;  (easymacs-load-desktop)
  ) 
 (t
  ;; Non-windowed -- check env for xterm and error if not?
  ;;(load "emacs-keys")
  (load "easymacs-terminal")
  ))

;; Kill *scratch*, and make *messages* and Easymacs help screen
;; unkillable
(kill-buffer (get-buffer "*scratch*"))

(defun kill-unkillable-buffer ()
  (message "%s" "Error: you cannot close this file.")
  nil)

(set-buffer (get-buffer "*Messages*"))
(make-local-variable 'kill-buffer-query-functions)
(add-hook 'kill-buffer-query-functions 'kill-unkillable-buffer)

(setq inhibit-startup-message t)
(require 'info)
(Info-goto-node "(Easymacs)Top")
(delete-other-windows)
(cd (expand-file-name "~/"))

; Making the info buffer unkillable interferes with viewing Texinfo files.
;(make-local-variable 'kill-buffer-query-functions)
;(add-hook 'kill-buffer-query-functions 'kill-unkillable-buffer)

;(require 'time-date)
;(message "Load of easymacs.el took %d seconds."
;         (time-to-seconds (time-since my-start-time)))

(setq easymacs-loaded t)
(message "Easymacs loaded successfully")
