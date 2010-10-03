;-*- coding: utf-8 -*-
;; Shortcuts for ERGOEMACS_KEYBOARD_LAYOUT=gb-dv
;; UK Dvorak

;;; --------------------------------------------------
;;; Load US layout

(load "ergoemacs-layout-dv")

;;; --------------------------------------------------
;;; Modify to UK
;;;    US ~ maps to ¬
;;;    US @ maps to "
;;;    US # maps to £
;;;    US " maps to @

(setq ergoemacs-switch-to-previous-frame-key (kbd "M-¬"))
(setq ergoemacs-split-window-horizontally-key (kbd "M-\""))
