;; This extends the work done in "term/rxvt.el", which is assumed to
;; have been loaded already -- this should have happened
;; automatically.

;; We set some bindings that usually are set by rxvt termcap, in case
;; we are running under screen, in which case it installs its own
;; termcap capabilities.  If we run screen with or without -T rxvt, we
;; get a hybrid of rxvt and screen escapes.  So best to run without --
;; that way, termcap gives us the screen escapes, and here we can add
;; the rxvt ones usually gotten from termcap.  The other way of doing
;; it would be to add the screen escapes here, which would be messier.

; Even when $TERM is not rxvt, we still need to load term/rxvt.el,
; since it could be GNU screen running inside rxvt ($TERM=screen), or
; eterm, etc (aterm gives $TERM as rxvt, so it's OK; Eterm gives Eterm
; on my system, which is totally broken, but with export TERM=eterm or
; =rxvt, everything works fine).  So we only refrain from loading it
; when it has already been loaded.

(unless (string= (getenv "TERM") "rxvt")
     (load-library "term/rxvt"))

(defun easymacs-rxvt-default ()
  "Additional key combinations that should work with rxvt, and are 
compatible with our gui keybindings"

  ;; These are already given by rxvt termcap, but we need them when
  ;; running under GNU screen.  S-F1 and -F2 cannot be distinguished
  ;; from F11 and F12
  (define-key function-key-map "\e[25~" [S-f3])
  (define-key function-key-map "\e[26~" [S-f4])
  (define-key function-key-map "\e[28~" [S-f5])
  (define-key function-key-map "\e[29~" [S-f6])
  (define-key function-key-map "\e[31~" [S-f7])
  (define-key function-key-map "\e[32~" [S-f8])
  (define-key function-key-map "\e[33~" [S-f9])
  (define-key function-key-map "\e[34~" [S-f10])
  (define-key function-key-map "\e[23$" [S-f11])
  (define-key function-key-map "\e[24$" [S-f12])

  ;; These are not defined elsewhere

;  (define-key function-key-map "\e[23^" [S-C-f1])
;  (define-key function-key-map "\e[24^" [S-C-f2])
  (define-key function-key-map "\e[25^" [S-C-f3])
  (define-key function-key-map "\e[26^" [S-C-f4])
  (define-key function-key-map "\e[28^" [S-C-f5])
  (define-key function-key-map "\e[29^" [S-C-f6])
  (define-key function-key-map "\e[31^" [S-C-f7])
  (define-key function-key-map "\e[32^" [S-C-f8])
  (define-key function-key-map "\e[33^" [S-C-f9])
  (define-key function-key-map "\e[34^" [S-C-f10])
  (define-key function-key-map "\e[23@" [S-C-f11])
  (define-key function-key-map "\e[24@" [S-C-f12])

  (define-key function-key-map "\e\e[25~" [M-S-f3])
  (define-key function-key-map "\e\e[26~" [M-S-f4])
  (define-key function-key-map "\e\e[28~" [M-S-f5])
  (define-key function-key-map "\e\e[29~" [M-S-f6])
  (define-key function-key-map "\e\e[31~" [M-S-f7])
  (define-key function-key-map "\e\e[32~" [M-S-f8])
  (define-key function-key-map "\e\e[33~" [M-S-f9])
  (define-key function-key-map "\e\e[34~" [M-S-f10])
  (define-key function-key-map "\e\e[23$" [M-S-f11])
  (define-key function-key-map "\e\e[24$" [M-S-f12])

;; Do these better
  (define-key function-key-map "\e\e[A"  [M-up])
  (define-key function-key-map "\e\e[B"  [M-down])
  (define-key function-key-map "\e\e[C"  [M-right])
  (define-key function-key-map "\e\e[D"  [M-left])
  (define-key function-key-map "\e\e[7~" [M-home])
  (define-key function-key-map "\e\e[8~" [M-end])
  (define-key function-key-map "\e\e[2~" [M-insert])
  (define-key function-key-map "\e\e[3~" [M-delete])
  (define-key function-key-map "\e\e[4~" [M-select])
  (define-key function-key-map "\e\e[5~" [M-prior])
  (define-key function-key-map "\e\e[6~" [M-next])
  
  (define-key function-key-map "\e[7^" [C-home])
  (define-key function-key-map "\e[8^" [C-end])

  (define-key function-key-map "\e[7@" [S-C-home])
  (define-key function-key-map "\e[8@" [S-C-end])

  (define-key function-key-map "\e\e[7@" [M-S-C-home])
  (define-key function-key-map "\e\e[8@" [M-S-C-end])
  (define-key function-key-map "\e\e[2@" [M-S-C-insert])
  (define-key function-key-map "\e\e[3@" [M-S-C-delete])

  ;; These don't seem to work. I don't know why ...
  (define-key function-key-map "\e\e[a"  [S-M-up])
  (define-key function-key-map "\e\e[b"  [S-M-down])
  (define-key function-key-map "\e\e[c"  [S-M-right])
  (define-key function-key-map "\e\e[d"  [S-M-left])
  (define-key function-key-map "\e\e[7$" [M-S-home])
  (define-key function-key-map "\e\e[8$" [M-S-end])
  (define-key function-key-map "\e[2@" [C-S-insert])
  (define-key function-key-map "\e[3@" [C-S-delete])
  

  ;; We override this below
  ;;(define-key function-key-map "\e[Z" [S-tab])
)

;; In case termcap has already bound these
    (define-key key-translation-map [(f13)] [S-f3])
    (define-key key-translation-map [(f14)] [S-f4])
    (define-key key-translation-map [(f15)] [S-f5])
    (define-key key-translation-map [(f16)] [S-f6])
    (define-key key-translation-map [(f17)] [S-f7])
    (define-key key-translation-map [(f18)] [S-f8])
    (define-key key-translation-map [(f19)] [S-f9])
    (define-key key-translation-map [(f20)] [S-f10])
    (define-key key-translation-map [(f21)] [S-f11])
    (define-key key-translation-map [(f22)] [S-f12])


  (define-key key-translation-map (kbd "ESC <f1>")  [M-f1])
  (define-key key-translation-map (kbd "ESC <f2>")  [M-f2])
  (define-key key-translation-map (kbd "ESC <f3>")  [M-f3])
  (define-key key-translation-map (kbd "ESC <f4>")  [M-f4])
  (define-key key-translation-map (kbd "ESC <f5>")  [M-f5])
  (define-key key-translation-map (kbd "ESC <f6>")  [M-f6])
  (define-key key-translation-map (kbd "ESC <f7>")  [M-f7])
  (define-key key-translation-map (kbd "ESC <f8>")  [M-f8])
  (define-key key-translation-map (kbd "ESC <f9>")  [M-f9])
  (define-key key-translation-map (kbd "ESC <f10>") [M-f10])
  (define-key key-translation-map (kbd "ESC <f11>") [M-f11])
  (define-key key-translation-map (kbd "ESC <f12>") [M-f12])



(defun easymacs-rxvt-fake ()
  "These are some additional key combinations, which rxvt does not itself
generate, but which might be generated by an rxvt-emulator, such as konsole"

;; These don't work
  (define-key function-key-map "\e[1a"  [S-C-up])
  (define-key function-key-map "\e[1b"  [S-C-down])
  (define-key function-key-map "\e[1c"  [S-C-right])
  (define-key function-key-map "\e[1d"  [S-C-left])


  (define-key function-key-map "\e\e1a"  [S-C-M-up])
  (define-key function-key-map "\e\e1b"  [S-C-M-down])
  (define-key function-key-map "\e\e1c"  [S-C-M-right])
  (define-key function-key-map "\e\e1d"  [S-C-M-left])
  
;key Tab    -Shift -Alt +Control : "\E0Z"
;key Tab    +Shift -Alt +Control : "\E0z"
;
;key Tab    -Shift +Alt +Control : "\E\E0Z"
;key Tab    +Shift +Alt +Control : "\E\E0z"
;
;key Backspace  -Shift -Alt +Control : "\E1y"
;key Backspace  +Shift -Alt +Control : "\E1Y"
;key Backspace  +Shift -Alt -Control : "\E0Y"
;
;key Backspace  -Shift +Alt +Control : "\E\E1y"
;key Backspace  +Shift +Alt +Control : "\E\E1Y"
;key Backspace  +Shift +Alt -Control : "\E\E0Y"
;
;key Return  +Shift -Alt -Control : "\E0X"
;key Return  -Shift -Alt +Control : "\E1x"
;key Return  +Shift -Alt +Control : "\E1X"
;
;key Return  +Shift +Alt -Control : "\E\E0X"
;key Return  -Shift +Alt +Control : "\E\E1x"
;key Return  +Shift +Alt +Control : "\E\E1X"
;
;key Prior +Shift -Alt +Control : "\E[5@"
;key Prior +Shift +Alt +Control : "\E\E[5@"
;
;key Next +Shift -Alt +Control : "\E[6@"
;key Next +Shift +Alt +Control : "\E\E[6@"
;
;key Pause -Shift -Alt -Control : "\E0w"
;key Pause +Shift -Alt -Control : "\E0W"
;key Pause -Shift -Alt +Control : "\E1w"
;key Pause +Shift -Alt +Control : "\E1W"
;key Pause -Shift +Alt -Control : "\E\E0w"
;key Pause +Shift +Alt -Control : "\E\E0W"
;key Pause -Shift +Alt +Control : "\E\E1w"
;key Pause +Shift +Alt +Control : "\E\E1W"
;
;key SysReq -Shift -Alt -Control : "\E0v"
;key SysReq +Shift -Alt -Control : "\E0V"
;key SysReq -Shift -Alt +Control : "\E1v"
;key SysReq +Shift -Alt +Control : "\E1V"
;key SysReq -Shift +Alt -Control : "\E\E0v"
;key SysReq +Shift +Alt -Control : "\E\E0V"
;key SysReq -Shift +Alt +Control : "\E\E1v"
;key SysReq +Shift +Alt +Control : "\E\E1V"
;
;key F1 +Shift -Alt -Control : "\E[11$"
;key F1 +Shift +Alt -Control : "\E\E[11$"
;
;key F2 +Shift -Alt -Control : "\E[12$"
;key F2 +Shift +Alt -Control : "\E\E[12$"


)


;; In rxvt, the following keys don't work: [S-F1], [S-f2], [S-M-F1],
;; [S-M-F2] [pause] [C-tab] [S-C-tab] [S-C-down/up/left/right],
;; [C-backspace] [S-backspace]
;; So we try to provide 
(defun easymacs-rxvt-hacks ()
"These are some hacks to help work around the absence of some useful
key combinations in rxvt"

;; use [Esc] [backspace] for [M-backspace] which runs the same
;; function as [C-backspace]

;; use S-tab for C-tab, and <esc> S-tab for S-C-tab
(define-key function-key-map "\e[Z" [C-tab])
(define-key function-key-map "\e\e[Z" [S-C-tab])

;; use S-M-left, right, etc instead of S-C-left, right, etc.
(define-key function-key-map "\e\e[d" [S-C-left])
(define-key function-key-map "\e\e[c" [S-C-right])
(define-key function-key-map "\e\e[a" [S-C-up])
(define-key function-key-map "\e\e[b" [S-C-down])

;; use C-M-F1 and C-M-F2 instead of S-F1 and S-F2, but under
;; X-windows, this will usually have to be [Esc] C-F1 and [Esc] C-F2
(define-key function-key-map "\e\e[11^" [S-f1])
(define-key function-key-map "\e\e[12^" [S-f2])

;; What to do for S-M-F1, and S-M-F2?

)

(easymacs-rxvt-default)

(if (getenv "KONSOLE_DCOP")
    (easymacs-rxvt-fake)
  (easymacs-rxvt-hacks))

