;;; This file should be loaded for support of running in a terminal emulator  

(menu-bar-mode -1)

;; gold3 may not be available on a tty
(custom-set-faces
'(flyspell-duplicate-face ((((class color))
			    (:foreground "yellow" :underline t :weight bold)))))

(defun easymacs-term-rebindings ()
"Not sure why, but we need to bind these explicitly so that cua-mode will
detect that they are shifted"
  (global-set-key (kbd "<S-right>")   'forward-char)
  (global-set-key (kbd "<S-left>")    'backward-char)
  (global-set-key (kbd "<S-down>")    'next-line)
  (global-set-key (kbd "<S-up>")      'previous-line)
  (global-set-key (kbd "<S-C-right>") 'forward-word)
  (global-set-key (kbd "<S-C-left>")  'backward-word)
  (global-set-key (kbd "<C-E>")       'end-of-line)
  (global-set-key (kbd "<C-A>")       'beginning-of-line)
  (global-set-key (kbd "<S-C-end>")   'end-of-buffer)
  (global-set-key (kbd "<S-C-home>")  'beginning-of-buffer)
  (global-set-key (kbd "<S-next>")    'scroll-up)
  (global-set-key (kbd "<S-prior>")   'scroll-down)
  (global-set-key (kbd "<S-C-down>")  'forward-paragraph)
  (global-set-key (kbd "<S-C-up>")    'backward-paragraph)

  (global-set-key (kbd "<S-M-up>")    'scroll-down-one-line)
  (global-set-key (kbd "<S-M-down>")  'scroll-up-one-line)
  (global-set-key (kbd "<S-M-home>")  'beginning-of-buffer-other-window)
  (global-set-key (kbd "<S-M-end>")   'end-of-buffer-other-window)
  (global-set-key (kbd "<S-M-prior>") 'scroll-other-window-down)
  (global-set-key (kbd "<S-M-next>")  'scroll-other-window)

  (global-set-key (kbd "<S-end>")     'easymacs-smart-end)
  (global-set-key (kbd "<S-home>")    'easymacs-smart-home)
  (global-set-key (kbd "ESC E")       'forward-sentence)
  (global-set-key (kbd "ESC A")       'backward-sentence)

  (put 'C-E 'event-symbol-elements '(?E shift control))
  (put 'C-A 'event-symbol-elements '(?A shift control))
  (put 'M-E 'event-symbol-elements '(?E shift meta))
  (put 'M-A 'event-symbol-elements '(?A shift meta))

  ;; Not sure why this is necessary (screen with xterm-256color)
  (define-key key-translation-map [(delete)]   "\C-d")
  
  )


(let ((term (getenv "TERM")))
  (if (string-match "^xterm" term)
      (progn 
	(load-library "xterm-extras")
	(xterm-extra-keys))
    (if (or (string= term "rxvt") 
	    (string= term "aterm") 
	    (string= term "wterm") 
	    (string= term "Eterm") 
	    (string= term "eterm"))
	(load-library "rxvt-keys")
      (message "%s" "Unknown terminal type"))))

(easymacs-term-rebindings)

