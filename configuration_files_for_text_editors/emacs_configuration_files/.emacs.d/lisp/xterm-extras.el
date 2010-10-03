;; xterm-extras.el --- define additional function key sequences for
;; recent versions of xterm
;;
;; Copyright (C) 2004 P J Heslin
;;
;; Author: Peter Heslin <p.j.heslin@dur.ac.uk>
;; URL: http://www.dur.ac.uk/p.j.heslin/emacs/download/xterm-extras.el
;; Version: 1.1
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
;; If you do not have a copy of the GNU General Public License, you
;; can obtain one by writing to the Free Software Foundation, Inc., 59
;; Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;;;;;;;;; Commentary:
;;
;; This file provides some extra emacs keybindings for the escape
;; sequences transmitted by recent versions of xterm.  It will not
;; work with the older versions of xterm that are still often found
;; in use.  An up-to-date version of xterm can be obtained from
;; http://dickey.his.com/xterm/xterm.html. It includes a file called
;; ctlseqs.ms, which documents the treatment of control sequences by
;; recent xterms.
;;
;; This implementation is somewhat limited in that it assumes a
;; standard PC keyboard and that the Alt key is being used for Meta.
;; Since both xterm and emacs support the use of a Meta key in
;; addition to the Alt key, this file probably should support that
;; usage, but as of yet it does not.  In fact, this file takes the
;; modifier that xterm refers to as "alt" and maps it right to the
;; emacs "meta".  For many users this is convenient, but for some it
;; will be wrong.
;;
;; These key combinations should also work with GNU screen, provided
;; that the TERM environment variable is set to "xterm" rather than
;; "screen".  This can be done in the .screenrc configuration file
;; or by using the "-T" switch like so: screen -T xterm
;;
;; To use, put this file in your load-path, and put these lines in
;; your .emacs file:
;;
;;   (when (string-match "^xterm" (getenv "TERM"))
;;     (require 'xterm-extras)
;;     (xterm-extra-keys))

;;;;;;;;;; X resource settings:
;;
;; For some extra keybindings, beyond those available from xterm by
;; default, try putting the following settings in your X resources
;; file:
;;
;; XTerm*eightBitInput:        false
;; XTerm*metaSendsEscape:      true
;; XTerm*backarrowKey:         false
;; XTerm*modifier:             meta
;;
;; XTerm.VT100.Translations: #override \
;; ~Ctrl ~Meta Shift  <Key> Tab:         string(0x1b) string("[z2a") \n\
;; ~Ctrl Meta  ~Shift <Key> Tab:         string(0x1b) string("[z3a") \n\
;; ~Ctrl Meta  Shift  <Key> Tab:         string(0x1b) string("[z4a") \n\
;; Ctrl  ~Meta ~Shift <Key> Tab:         string(0x1b) string("[z5a") \n\
;; Ctrl  ~Meta Shift  <Key> Tab:         string(0x1b) string("[z6a") \n\
;; Ctrl  Meta  ~Shift <Key> Tab:         string(0x1b) string("[z7a") \n\
;; Ctrl  Meta  Shift  <Key> Tab:         string(0x1b) string("[z8a") \n\
;; \
;; ~Ctrl ~Meta Shift  <Key> Return:      string(0x1b) string("[z2b") \n\
;; ~Ctrl Meta  ~Shift <Key> Return:      string(0x1b) string("[z3b") \n\
;; ~Ctrl Meta  Shift  <Key> Return:      string(0x1b) string("[z4b") \n\
;; Ctrl  ~Meta ~Shift <Key> Return:      string(0x1b) string("[z5b") \n\
;; Ctrl  ~Meta Shift  <Key> Return:      string(0x1b) string("[z6b") \n\
;; Ctrl  Meta  ~Shift <Key> Return:      string(0x1b) string("[z7b") \n\
;; Ctrl  Meta  Shift  <Key> Return:      string(0x1b) string("[z8b") \n\
;; \
;; ~Ctrl ~Meta Shift  <Key> BackSpace:   string(0x1b) string("[z2c") \n\
;; ~Ctrl Meta  ~Shift <Key> BackSpace:   string(0x1b) string("[z3c") \n\
;; ~Ctrl Meta  Shift  <Key> BackSpace:   string(0x1b) string("[z4c") \n\
;; Ctrl  ~Meta ~Shift <Key> BackSpace:   string(0x1b) string("[z5c") \n\
;; Ctrl  ~Meta Shift  <Key> BackSpace:   string(0x1b) string("[z6c") \n\
;; Ctrl  Meta  ~Shift <Key> BackSpace:   string(0x1b) string("[z7c") \n\
;; Ctrl  Meta  Shift  <Key> BackSpace:   string(0x1b) string("[z8c") \n\
;; \
;; ~Ctrl ~Meta ~Shift <Key> Pause:       string(0x1b) string("[zd")  \n\
;; ~Ctrl ~Meta Shift  <Key> Pause:       string(0x1b) string("[z2d") \n\
;; ~Ctrl Meta  ~Shift <Key> Pause:       string(0x1b) string("[z3d") \n\
;; ~Ctrl Meta  Shift  <Key> Pause:       string(0x1b) string("[z4d") \n\
;; Ctrl  ~Meta ~Shift <Key> Pause:       string(0x1b) string("[z5d") \n\
;; Ctrl  ~Meta Shift  <Key> Pause:       string(0x1b) string("[z6d") \n\
;; Ctrl  Meta  ~Shift <Key> Pause:       string(0x1b) string("[z7d") \n\
;; Ctrl  Meta  Shift  <Key> Pause:       string(0x1b) string("[z8d") \n\
;; \
;; Ctrl  ~Meta ~Shift <Key> Break:       string(0x1b) string("[z5d") \n\
;; Ctrl  ~Meta Shift  <Key> Break:       string(0x1b) string("[z6d") \n\
;; Ctrl  Meta  ~Shift <Key> Break:       string(0x1b) string("[z7d") \n\
;; Ctrl  Meta  Shift  <Key> Break:       string(0x1b) string("[z8d") \n\
;; \
;; ~Ctrl ~Meta ~Shift <Key> Sys_Req:     string(0x1b) string("[ze")  \n\
;; ~Ctrl ~Meta Shift  <Key> Sys_Req:     string(0x1b) string("[z2e") \n\
;; Ctrl  ~Meta ~Shift <Key> Sys_Req:     string(0x1b) string("[z5e") \n\
;; Ctrl  ~Meta Shift  <Key> Sys_Req:     string(0x1b) string("[z6e") \n\
;; \
;; ~Ctrl ~Meta ~Shift <Key> KP_Enter:    string(0x1b) string("[zf")  \n\
;; ~Ctrl ~Meta Shift  <Key> KP_Enter:    string(0x1b) string("[z2f") \n\
;; ~Ctrl Meta  ~Shift <Key> KP_Enter:    string(0x1b) string("[z3f") \n\
;; ~Ctrl Meta  Shift  <Key> KP_Enter:    string(0x1b) string("[z4f") \n\
;; Ctrl  ~Meta ~Shift <Key> KP_Enter:    string(0x1b) string("[z5f") \n\
;; Ctrl  ~Meta Shift  <Key> KP_Enter:    string(0x1b) string("[z6f") \n\
;; Ctrl  Meta  ~Shift <Key> KP_Enter:    string(0x1b) string("[z7f") \n\
;; Ctrl  Meta  Shift  <Key> KP_Enter:    string(0x1b) string("[z8f") \n\
;; \
;; ~Ctrl ~Meta ~Shift <Key> KP_Add:      string(0x1b) string("[zg")  \n\
;; ~Ctrl ~Meta Shift  <Key> KP_Add:      string(0x1b) string("[z2g") \n\
;; ~Ctrl Meta  ~Shift <Key> KP_Add:      string(0x1b) string("[z3g") \n\
;; ~Ctrl Meta  Shift  <Key> KP_Add:      string(0x1b) string("[z4g") \n\
;; Ctrl  ~Meta ~Shift <Key> KP_Add:      string(0x1b) string("[z5g") \n\
;; Ctrl  ~Meta Shift  <Key> KP_Add:      string(0x1b) string("[z6g") \n\
;; Ctrl  Meta  ~Shift <Key> KP_Add:      string(0x1b) string("[z7g") \n\
;; Ctrl  Meta  Shift  <Key> KP_Add:      string(0x1b) string("[z8g") \n\
;; \
;; ~Ctrl ~Meta ~Shift <Key> KP_Subtract: string(0x1b) string("[zh")  \n\
;; ~Ctrl ~Meta Shift  <Key> KP_Subtract: string(0x1b) string("[z2h") \n\
;; ~Ctrl Meta  ~Shift <Key> KP_Subtract: string(0x1b) string("[z3h") \n\
;; ~Ctrl Meta  Shift  <Key> KP_Subtract: string(0x1b) string("[z4h") \n\
;; Ctrl  ~Meta ~Shift <Key> KP_Subtract: string(0x1b) string("[z5h") \n\
;; Ctrl  ~Meta Shift  <Key> KP_Subtract: string(0x1b) string("[z6h") \n\
;; Ctrl  Meta  ~Shift <Key> KP_Subtract: string(0x1b) string("[z7h") \n\
;; Ctrl  Meta  Shift  <Key> KP_Subtract: string(0x1b) string("[z8h") \n\
;; \
;; ~Ctrl ~Meta ~Shift <Key> KP_Decimal:  string(0x1b) string("[zi")  \n\
;; ~Ctrl ~Meta Shift  <Key> KP_Decimal:  string(0x1b) string("[z2i") \n\
;; ~Ctrl Meta  ~Shift <Key> KP_Decimal:  string(0x1b) string("[z3i") \n\
;; ~Ctrl Meta  Shift  <Key> KP_Decimal:  string(0x1b) string("[z4i") \n\
;; Ctrl  ~Meta ~Shift <Key> KP_Decimal:  string(0x1b) string("[z5i") \n\
;; Ctrl  ~Meta Shift  <Key> KP_Decimal:  string(0x1b) string("[z6i") \n\
;; Ctrl  Meta  ~Shift <Key> KP_Decimal:  string(0x1b) string("[z7i") \n\
;; Ctrl  Meta  Shift  <Key> KP_Decimal:  string(0x1b) string("[z8i") \n\
;; \
;; ~Ctrl ~Meta ~Shift <Key> KP_Divide:   string(0x1b) string("[zj")  \n\
;; ~Ctrl ~Meta Shift  <Key> KP_Divide:   string(0x1b) string("[z2j") \n\
;; ~Ctrl Meta  ~Shift <Key> KP_Divide:   string(0x1b) string("[z3j") \n\
;; ~Ctrl Meta  Shift  <Key> KP_Divide:   string(0x1b) string("[z4j") \n\
;; Ctrl  ~Meta ~Shift <Key> KP_Divide:   string(0x1b) string("[z5j") \n\
;; Ctrl  ~Meta Shift  <Key> KP_Divide:   string(0x1b) string("[z6j") \n\
;; Ctrl  Meta  ~Shift <Key> KP_Divide:   string(0x1b) string("[z7j") \n\
;; Ctrl  Meta  Shift  <Key> KP_Divide:   string(0x1b) string("[z8j") \n\
;; \
;; ~Ctrl ~Meta ~Shift <Key> KP_Multiply: string(0x1b) string("[zk")  \n\
;; ~Ctrl ~Meta Shift  <Key> KP_Multiply: string(0x1b) string("[z2k") \n\
;; ~Ctrl Meta  ~Shift <Key> KP_Multiply: string(0x1b) string("[z3k") \n\
;; ~Ctrl Meta  Shift  <Key> KP_Multiply: string(0x1b) string("[z4k") \n\
;; Ctrl  ~Meta ~Shift <Key> KP_Multiply: string(0x1b) string("[z5k") \n\
;; Ctrl  ~Meta Shift  <Key> KP_Multiply: string(0x1b) string("[z6k") \n\
;; Ctrl  Meta  ~Shift <Key> KP_Multiply: string(0x1b) string("[z7k") \n\
;; Ctrl  Meta  Shift  <Key> KP_Multiply: string(0x1b) string("[z8k") \n\
;; \
;; ~Ctrl ~Meta ~Shift <Key> KP_0:        string(0x1b) string("[zl")  \n\
;; ~Ctrl ~Meta Shift  <Key> KP_0:        string(0x1b) string("[z2l") \n\
;; ~Ctrl Meta  ~Shift <Key> KP_0:        string(0x1b) string("[z3l") \n\
;; ~Ctrl Meta  Shift  <Key> KP_0:        string(0x1b) string("[z4l") \n\
;; Ctrl  ~Meta ~Shift <Key> KP_0:        string(0x1b) string("[z5l") \n\
;; Ctrl  ~Meta Shift  <Key> KP_0:        string(0x1b) string("[z6l") \n\
;; Ctrl  Meta  ~Shift <Key> KP_0:        string(0x1b) string("[z7l") \n\
;; Ctrl  Meta  Shift  <Key> KP_0:        string(0x1b) string("[z8l") \n\
;; \
;; ~Ctrl ~Meta ~Shift <Key> KP_1:        string(0x1b) string("[zm")  \n\
;; ~Ctrl ~Meta Shift  <Key> KP_1:        string(0x1b) string("[z2m") \n\
;; ~Ctrl Meta  ~Shift <Key> KP_1:        string(0x1b) string("[z3m") \n\
;; ~Ctrl Meta  Shift  <Key> KP_1:        string(0x1b) string("[z4m") \n\
;; Ctrl  ~Meta ~Shift <Key> KP_1:        string(0x1b) string("[z5m") \n\
;; Ctrl  ~Meta Shift  <Key> KP_1:        string(0x1b) string("[z6m") \n\
;; Ctrl  Meta  ~Shift <Key> KP_1:        string(0x1b) string("[z7m") \n\
;; Ctrl  Meta  Shift  <Key> KP_1:        string(0x1b) string("[z8m") \n\
;; \
;; ~Ctrl ~Meta ~Shift <Key> KP_2:        string(0x1b) string("[zn")  \n\
;; ~Ctrl ~Meta Shift  <Key> KP_2:        string(0x1b) string("[z2n") \n\
;; ~Ctrl Meta  ~Shift <Key> KP_2:        string(0x1b) string("[z3n") \n\
;; ~Ctrl Meta  Shift  <Key> KP_2:        string(0x1b) string("[z4n") \n\
;; Ctrl  ~Meta ~Shift <Key> KP_2:        string(0x1b) string("[z5n") \n\
;; Ctrl  ~Meta Shift  <Key> KP_2:        string(0x1b) string("[z6n") \n\
;; Ctrl  Meta  ~Shift <Key> KP_2:        string(0x1b) string("[z7n") \n\
;; Ctrl  Meta  Shift  <Key> KP_2:        string(0x1b) string("[z8n") \n\
;; \
;; ~Ctrl ~Meta ~Shift <Key> KP_3:        string(0x1b) string("[zo")  \n\
;; ~Ctrl ~Meta Shift  <Key> KP_3:        string(0x1b) string("[z2o") \n\
;; ~Ctrl Meta  ~Shift <Key> KP_3:        string(0x1b) string("[z3o") \n\
;; ~Ctrl Meta  Shift  <Key> KP_3:        string(0x1b) string("[z4o") \n\
;; Ctrl  ~Meta ~Shift <Key> KP_3:        string(0x1b) string("[z5o") \n\
;; Ctrl  ~Meta Shift  <Key> KP_3:        string(0x1b) string("[z6o") \n\
;; Ctrl  Meta  ~Shift <Key> KP_3:        string(0x1b) string("[z7o") \n\
;; Ctrl  Meta  Shift  <Key> KP_3:        string(0x1b) string("[z8o") \n\
;; \
;; ~Ctrl ~Meta ~Shift <Key> KP_4:        string(0x1b) string("[zp")  \n\
;; ~Ctrl ~Meta Shift  <Key> KP_4:        string(0x1b) string("[z2p") \n\
;; ~Ctrl Meta  ~Shift <Key> KP_4:        string(0x1b) string("[z3p") \n\
;; ~Ctrl Meta  Shift  <Key> KP_4:        string(0x1b) string("[z4p") \n\
;; Ctrl  ~Meta ~Shift <Key> KP_4:        string(0x1b) string("[z5p") \n\
;; Ctrl  ~Meta Shift  <Key> KP_4:        string(0x1b) string("[z6p") \n\
;; Ctrl  Meta  ~Shift <Key> KP_4:        string(0x1b) string("[z7p") \n\
;; Ctrl  Meta  Shift  <Key> KP_4:        string(0x1b) string("[z8p") \n\
;; \
;; ~Ctrl ~Meta ~Shift <Key> KP_5:        string(0x1b) string("[zq")  \n\
;; ~Ctrl ~Meta Shift  <Key> KP_5:        string(0x1b) string("[z2q") \n\
;; ~Ctrl Meta  ~Shift <Key> KP_5:        string(0x1b) string("[z3q") \n\
;; ~Ctrl Meta  Shift  <Key> KP_5:        string(0x1b) string("[z4q") \n\
;; Ctrl  ~Meta ~Shift <Key> KP_5:        string(0x1b) string("[z5q") \n\
;; Ctrl  ~Meta Shift  <Key> KP_5:        string(0x1b) string("[z6q") \n\
;; Ctrl  Meta  ~Shift <Key> KP_5:        string(0x1b) string("[z7q") \n\
;; Ctrl  Meta  Shift  <Key> KP_5:        string(0x1b) string("[z8q") \n\
;; \
;; ~Ctrl ~Meta ~Shift <Key> KP_6:        string(0x1b) string("[zr")  \n\
;; ~Ctrl ~Meta Shift  <Key> KP_6:        string(0x1b) string("[z2r") \n\
;; ~Ctrl Meta  ~Shift <Key> KP_6:        string(0x1b) string("[z3r") \n\
;; ~Ctrl Meta  Shift  <Key> KP_6:        string(0x1b) string("[z4r") \n\
;; Ctrl  ~Meta ~Shift <Key> KP_6:        string(0x1b) string("[z5r") \n\
;; Ctrl  ~Meta Shift  <Key> KP_6:        string(0x1b) string("[z6r") \n\
;; Ctrl  Meta  ~Shift <Key> KP_6:        string(0x1b) string("[z7r") \n\
;; Ctrl  Meta  Shift  <Key> KP_6:        string(0x1b) string("[z8r") \n\
;; \
;; ~Ctrl ~Meta ~Shift <Key> KP_7:        string(0x1b) string("[zs")  \n\
;; ~Ctrl ~Meta Shift  <Key> KP_7:        string(0x1b) string("[z2s") \n\
;; ~Ctrl Meta  ~Shift <Key> KP_7:        string(0x1b) string("[z3s") \n\
;; ~Ctrl Meta  Shift  <Key> KP_7:        string(0x1b) string("[z4s") \n\
;; Ctrl  ~Meta ~Shift <Key> KP_7:        string(0x1b) string("[z5s") \n\
;; Ctrl  ~Meta Shift  <Key> KP_7:        string(0x1b) string("[z6s") \n\
;; Ctrl  Meta  ~Shift <Key> KP_7:        string(0x1b) string("[z7s") \n\
;; Ctrl  Meta  Shift  <Key> KP_7:        string(0x1b) string("[z8s") \n\
;; \
;; ~Ctrl ~Meta ~Shift <Key> KP_8:        string(0x1b) string("[zt")  \n\
;; ~Ctrl ~Meta Shift  <Key> KP_8:        string(0x1b) string("[z2t") \n\
;; ~Ctrl Meta  ~Shift <Key> KP_8:        string(0x1b) string("[z3t") \n\
;; ~Ctrl Meta  Shift  <Key> KP_8:        string(0x1b) string("[z4t") \n\
;; Ctrl  ~Meta ~Shift <Key> KP_8:        string(0x1b) string("[z5t") \n\
;; Ctrl  ~Meta Shift  <Key> KP_8:        string(0x1b) string("[z6t") \n\
;; Ctrl  Meta  ~Shift <Key> KP_8:        string(0x1b) string("[z7t") \n\
;; Ctrl  Meta  Shift  <Key> KP_8:        string(0x1b) string("[z8t") \n\
;; \
;; ~Ctrl ~Meta ~Shift <Key> KP_9:        string(0x1b) string("[zu")  \n\
;; ~Ctrl ~Meta Shift  <Key> KP_9:        string(0x1b) string("[z2u") \n\
;; ~Ctrl Meta  ~Shift <Key> KP_9:        string(0x1b) string("[z3u") \n\
;; ~Ctrl Meta  Shift  <Key> KP_9:        string(0x1b) string("[z4u") \n\
;; Ctrl  ~Meta ~Shift <Key> KP_9:        string(0x1b) string("[z5u") \n\
;; Ctrl  ~Meta Shift  <Key> KP_9:        string(0x1b) string("[z6u") \n\
;; Ctrl  Meta  ~Shift <Key> KP_9:        string(0x1b) string("[z7u") \n\
;; Ctrl  Meta  Shift  <Key> KP_9:        string(0x1b) string("[z8u") \n\
;; \
;; ~Ctrl ~Meta ~Shift <Key> Menu:        string(0x1b) string("[zv")  \n\
;; ~Ctrl ~Meta Shift  <Key> Menu:        string(0x1b) string("[z2v") \n\
;; ~Ctrl Meta  ~Shift <Key> Menu:        string(0x1b) string("[z3v") \n\
;; ~Ctrl Meta  Shift  <Key> Menu:        string(0x1b) string("[z4v") \n\
;; Ctrl  ~Meta ~Shift <Key> Menu:        string(0x1b) string("[z5v") \n\
;; Ctrl  ~Meta Shift  <Key> Menu:        string(0x1b) string("[z6v") \n\
;; Ctrl  Meta  ~Shift <Key> Menu:        string(0x1b) string("[z7v") \n\
;; Ctrl  Meta  Shift  <Key> Menu:        string(0x1b) string("[z8v") \n

;;; .inputrc
;;
;; The above X resources change the escape sequences even of the
;; unmodified numeric keypad keys (so that they can be used for
;; distinct purposes within Emacs), but this will be a nuisance if you
;; try to use the numeric keypad in other terminal applications.  To
;; fix that, put the following lines in your ~/.inputrc:
;;  
;; # For xterm-extras:
;;  
;; "\e[zf": "\n"
;; "\e[zg": "+"
;; "\e[zh": "-"
;; "\e[zi": "."
;; "\e[zj": "/"
;; "\e[zk": "*"
;; "\e[zl": "0"
;; "\e[zm": "1"
;; "\e[zn": "2"
;; "\e[zo": "3"
;; "\e[zp": "4"
;; "\e[zq": "5"
;; "\e[zr": "6"
;; "\e[zs": "7"
;; "\e[zt": "8"
;; "\e[zu": "9"



;; Code:

(defun xterm-extra-remap-function-keys ()
  ;; If the system has an up-to-date xterm termcap entry, these escapes
  ;; have already been bound, so we remap them to suit our purposes
  (define-key key-translation-map [(f13)] [S-f1])
  (define-key key-translation-map [(f14)] [S-f2])
  (define-key key-translation-map [(f15)] [S-f3])
  (define-key key-translation-map [(f16)] [S-f4])
  (define-key key-translation-map [(f17)] [S-f5])
  (define-key key-translation-map [(f18)] [S-f6])
  (define-key key-translation-map [(f19)] [S-f7])
  (define-key key-translation-map [(f20)] [S-f8])
  (define-key key-translation-map [(f21)] [S-f9])
  (define-key key-translation-map [(f22)] [S-f10])
  (define-key key-translation-map [(f23)] [S-f11])
  (define-key key-translation-map [(f24)] [S-f12])

  (define-key key-translation-map [(f25)] [C-f1])
  (define-key key-translation-map [(f26)] [C-f2])
  (define-key key-translation-map [(f27)] [C-f3])
  (define-key key-translation-map [(f28)] [C-f4])
  (define-key key-translation-map [(f29)] [C-f5])
  (define-key key-translation-map [(f30)] [C-f6])
  (define-key key-translation-map [(f31)] [C-f7])
  (define-key key-translation-map [(f32)] [C-f8])
  (define-key key-translation-map [(f33)] [C-f9])
  (define-key key-translation-map [(f34)] [C-f10])
  (define-key key-translation-map [(f35)] [C-f11])
  (define-key key-translation-map [(f36)] [C-f12])

  (define-key key-translation-map [(f37)] [S-C-f1])
  (define-key key-translation-map [(f38)] [S-C-f2])
  (define-key key-translation-map [(f39)] [S-C-f3])
  (define-key key-translation-map [(f40)] [S-C-f4])
  (define-key key-translation-map [(f41)] [S-C-f5])
  (define-key key-translation-map [(f42)] [S-C-f6])
  (define-key key-translation-map [(f43)] [S-C-f7])
  (define-key key-translation-map [(f44)] [S-C-f8])
  (define-key key-translation-map [(f45)] [S-C-f9])
  (define-key key-translation-map [(f46)] [S-C-f10])
  (define-key key-translation-map [(f47)] [S-C-f11])
  (define-key key-translation-map [(f48)] [S-C-f12]))

(defun xterm-extra-bind-keys ()
    ;; This section is taken right from term/xterm.el, which we are
    ;; going to overrride here, so we include these bindings again

    (define-key function-key-map "\e[A" [up])
    (define-key function-key-map "\e[B" [down])
    (define-key function-key-map "\e[C" [right])
    (define-key function-key-map "\e[D" [left])
    (define-key function-key-map "\e[1~" [home])
    (define-key function-key-map "\e[2~" [insert])
    (define-key function-key-map "\e[3~" [delete])
    (define-key function-key-map "\e[4~" [select])
    (define-key function-key-map "\e[5~" [prior])
    (define-key function-key-map "\e[6~" [next])
    (define-key function-key-map "\e[11~" [f1])
    (define-key function-key-map "\e[12~" [f2])
    (define-key function-key-map "\e[13~" [f3])
    (define-key function-key-map "\e[14~" [f4])
    (define-key function-key-map "\e[15~" [f5])
    (define-key function-key-map "\e[17~" [f6])
    (define-key function-key-map "\e[18~" [f7])
    (define-key function-key-map "\e[19~" [f8])
    (define-key function-key-map "\e[20~" [f9])
    (define-key function-key-map "\e[21~" [f10])
    (define-key function-key-map "\e[23~" [f11])
    (define-key function-key-map "\e[24~" [f12])
    (define-key function-key-map "\e[29~" [print])

    (define-key function-key-map "\e[2;2~" [S-insert])
    (define-key function-key-map "\e[3;2~" [S-delete])
    (define-key function-key-map "\e[5;2~" [S-prior])
    (define-key function-key-map "\e[6;2~" [S-next])

    (define-key function-key-map "\e[2;5~" [C-insert])
    (define-key function-key-map "\e[3;5~" [C-delete])
    (define-key function-key-map "\e[5;5~" [C-prior])
    (define-key function-key-map "\e[6;5~" [C-next])

    (define-key function-key-map "\eOA" [up])
    (define-key function-key-map "\eOB" [down])
    (define-key function-key-map "\eOC" [right])
    (define-key function-key-map "\eOD" [left])
    (define-key function-key-map "\eOF" [end])
    (define-key function-key-map "\eOH" [home])

    (define-key function-key-map "\eO2A" [S-up])
    (define-key function-key-map "\eO2B" [S-down])
    (define-key function-key-map "\eO2C" [S-right])
    (define-key function-key-map "\eO2D" [S-left])
    (define-key function-key-map "\eO2F" [S-end])
    (define-key function-key-map "\eO2H" [S-home])

    (define-key function-key-map "\eO5A" [C-up])
    (define-key function-key-map "\eO5B" [C-down])
    (define-key function-key-map "\eO5C" [C-right])
    (define-key function-key-map "\eO5D" [C-left])
    (define-key function-key-map "\eO5F" [C-end])
    (define-key function-key-map "\eO5H" [C-home])

    ;; Even if the local machine has an up-to-date xterm and termcap
    ;; entry, the remote machine may not have an up-to-date termcap,
    ;; in which case, the escapes dealt with in the function above
    ;; (xterm-extras-refunction-key-map-function-keys) will not have
    ;; been bound to any function-key-mapping, so we must bind the
    ;; escapes here.  We probably ought not to do both, but I don't
    ;; know how to detect what sort of termcap entry for xterm the
    ;; host machine has.

    (define-key function-key-map "\eO2P"    [S-f1])
    (define-key function-key-map "\eO2Q"    [S-f2])
    (define-key function-key-map "\eO2R"    [S-f3])
    (define-key function-key-map "\eO2S"    [S-f4])
    (define-key function-key-map "\e[15;2~" [S-f5])
    (define-key function-key-map "\e[17;2~" [S-f6])
    (define-key function-key-map "\e[18;2~" [S-f7])
    (define-key function-key-map "\e[19;2~" [S-f8])
    (define-key function-key-map "\e[20;2~" [S-f9])
    (define-key function-key-map "\e[21;2~" [S-f10])
    (define-key function-key-map "\e[23;2~" [S-f11])
    (define-key function-key-map "\e[24;2~" [S-f12])

    (define-key function-key-map "\eO5P"    [C-f1])
    (define-key function-key-map "\eO5Q"    [C-f2])
    (define-key function-key-map "\eO5R"    [C-f3])
    (define-key function-key-map "\eO5S"    [C-f4])
    (define-key function-key-map "\e[15;5~" [C-f5])
    (define-key function-key-map "\e[17;5~" [C-f6])
    (define-key function-key-map "\e[18;5~" [C-f7])
    (define-key function-key-map "\e[19;5~" [C-f8])
    (define-key function-key-map "\e[20;5~" [C-f9])
    (define-key function-key-map "\e[21;5~" [C-f10])
    (define-key function-key-map "\e[23;5~" [C-f11])
    (define-key function-key-map "\e[24;5~" [C-f12])

    (define-key function-key-map "\eO6P"    [S-C-f1])
    (define-key function-key-map "\eO6Q"    [S-C-f2])
    (define-key function-key-map "\eO6R"    [S-C-f3])
    (define-key function-key-map "\eO6S"    [S-C-f4])
    (define-key function-key-map "\e[15;6~" [S-C-f5])
    (define-key function-key-map "\e[17;6~" [S-C-f6])
    (define-key function-key-map "\e[18;6~" [S-C-f7])
    (define-key function-key-map "\e[19;6~" [S-C-f8])
    (define-key function-key-map "\e[20;6~" [S-C-f9])
    (define-key function-key-map "\e[21;6~" [S-C-f10])
    (define-key function-key-map "\e[23;6~" [S-C-f11])
    (define-key function-key-map "\e[24;6~" [S-C-f12])

    ;; These meta-modified combinations are not defined in termcap or
    ;; term/xterm.el at all.  I am assuming here that the Alt key is
    ;; being used for Meta

    (define-key function-key-map "\eO3P"    [M-f1])
    (define-key function-key-map "\eO3Q"    [M-f2])
    (define-key function-key-map "\eO3R"    [M-f3])
    (define-key function-key-map "\eO3S"    [M-f4])
    (define-key function-key-map "\e[15;3~" [M-f5])
    (define-key function-key-map "\e[17;3~" [M-f6])
    (define-key function-key-map "\e[18;3~" [M-f7])
    (define-key function-key-map "\e[19;3~" [M-f8])
    (define-key function-key-map "\e[20;3~" [M-f9])
    (define-key function-key-map "\e[21;3~" [M-f10])
    (define-key function-key-map "\e[23;3~" [M-f11])
    (define-key function-key-map "\e[24;3~" [M-f12])

    (define-key function-key-map "\eO4P"    [S-M-f1])
    (define-key function-key-map "\eO4Q"    [S-M-f2])
    (define-key function-key-map "\eO4R"    [S-M-f3])
    (define-key function-key-map "\eO4S"    [S-M-f4])
    (define-key function-key-map "\e[15;4~" [S-M-f5])
    (define-key function-key-map "\e[17;4~" [S-M-f6])
    (define-key function-key-map "\e[18;4~" [S-M-f7])
    (define-key function-key-map "\e[19;4~" [S-M-f8])
    (define-key function-key-map "\e[20;4~" [S-M-f9])
    (define-key function-key-map "\e[21;4~" [S-M-f10])
    (define-key function-key-map "\e[23;4~" [S-M-f11])
    (define-key function-key-map "\e[24;4~" [S-M-f12])

    ;; These are often unavailable under X (because Alt-Ctrl-Fx is
    ;; used by default to change VT), but for the sake of completeness
    ;; they are included here.

    (define-key function-key-map "\eO7P"    [M-C-f1])
    (define-key function-key-map "\eO7Q"    [M-C-f2])
    (define-key function-key-map "\eO7R"    [M-C-f3])
    (define-key function-key-map "\eO7S"    [M-C-f4])
    (define-key function-key-map "\e[15;7~" [M-C-f5])
    (define-key function-key-map "\e[17;7~" [M-C-f6])
    (define-key function-key-map "\e[18;7~" [M-C-f7])
    (define-key function-key-map "\e[19;7~" [M-C-f8])
    (define-key function-key-map "\e[20;7~" [M-C-f9])
    (define-key function-key-map "\e[21;7~" [M-C-f10])
    (define-key function-key-map "\e[23;7~" [M-C-f11])
    (define-key function-key-map "\e[24;7~" [M-C-f12])

    (define-key function-key-map "\eO8P"    [S-M-C-f1])
    (define-key function-key-map "\eO8Q"    [S-M-C-f2])
    (define-key function-key-map "\eO8R"    [S-M-C-f3])
    (define-key function-key-map "\eO8S"    [S-M-C-f4])
    (define-key function-key-map "\e[15;8~" [S-M-C-f5])
    (define-key function-key-map "\e[17;8~" [S-M-C-f6])
    (define-key function-key-map "\e[18;8~" [S-M-C-f7])
    (define-key function-key-map "\e[19;8~" [S-M-C-f8])
    (define-key function-key-map "\e[20;8~" [S-M-C-f9])
    (define-key function-key-map "\e[21;8~" [S-M-C-f10])
    (define-key function-key-map "\e[23;8~" [S-M-C-f11])
    (define-key function-key-map "\e[24;8~" [S-M-C-f12])

    ;; A number of these are usually not available: for example, Shift +
    ;; Prior (and any other modifier) normally scrolls up; but for
    ;; completeness they are included here.

    (define-key function-key-map "\e[5;2~"    [S-prior])
    (define-key function-key-map "\e[5;3~"    [M-prior])
    (define-key function-key-map "\e[5;4~"    [M-S-prior])
    (define-key function-key-map "\e[5;5~"    [C-prior])
    (define-key function-key-map "\e[5;6~"    [C-S-prior])
    (define-key function-key-map "\e[5;7~"    [M-C-prior])
    (define-key function-key-map "\e[5;8~"    [M-C-S-prior])

    (define-key function-key-map "\e[6;2~"    [S-next])
    (define-key function-key-map "\e[6;3~"    [M-next])
    (define-key function-key-map "\e[6;4~"    [M-S-next])
    (define-key function-key-map "\e[6;5~"    [C-next])
    (define-key function-key-map "\e[6;6~"    [C-S-next])
    (define-key function-key-map "\e[6;7~"    [M-C-next])
    (define-key function-key-map "\e[6;8~"    [M-C-S-next])

    ;; The simple shift-key and control-key combinations are already
    ;; defined in the section from term/xterm.el reproduced above.

    (define-key function-key-map "\eO3H"    [M-home])
    (define-key function-key-map "\eO4H"    [M-S-home])
    (define-key function-key-map "\eO6H"    [C-S-home])
    (define-key function-key-map "\eO7H"    [M-C-home])
    (define-key function-key-map "\eO8H"    [M-C-S-home])

    (define-key function-key-map "\eO3F"    [M-end])
    (define-key function-key-map "\eO4F"    [M-S-end])
    (define-key function-key-map "\eO6F"    [C-S-end])
    (define-key function-key-map "\eO7F"    [M-C-end])
    (define-key function-key-map "\eO8F"    [M-C-S-end])

    (define-key function-key-map "\eO3A"    [M-up])
    (define-key function-key-map "\eO4A"    [M-S-up])
    (define-key function-key-map "\eO6A"    [C-S-up])
    (define-key function-key-map "\eO7A"    [M-C-up])
    (define-key function-key-map "\eO8A"    [M-C-S-up])

    (define-key function-key-map "\eO3B"    [M-down])
    (define-key function-key-map "\eO4B"    [M-S-down])
    (define-key function-key-map "\eO6B"    [C-S-down])
    (define-key function-key-map "\eO7B"    [M-C-down])
    (define-key function-key-map "\eO8B"    [M-C-S-down])

    (define-key function-key-map "\eO3C"    [M-right])
    (define-key function-key-map "\eO4C"    [M-S-right])
    (define-key function-key-map "\eO6C"    [C-S-right])
    (define-key function-key-map "\eO7C"    [M-C-right])
    (define-key function-key-map "\eO8C"    [M-C-S-right])

    (define-key function-key-map "\eO3D"    [M-left])
    (define-key function-key-map "\eO4D"    [M-S-left])
    (define-key function-key-map "\eO6D"    [C-S-left])
    (define-key function-key-map "\eO7D"    [M-C-left])
    (define-key function-key-map "\eO8D"    [M-C-S-left])

    ;; Shift+insert combinations may not be available, if they are set
    ;; to paste from the clipboard

    (define-key function-key-map "\e[2;2~"    [S-insert])
    (define-key function-key-map "\e[2;3~"    [M-insert])
    (define-key function-key-map "\e[2;4~"    [M-S-insert])
    (define-key function-key-map "\e[2;5~"    [C-insert])
    (define-key function-key-map "\e[2;6~"    [C-S-insert])
    (define-key function-key-map "\e[2;7~"    [M-C-insert])
    (define-key function-key-map "\e[2;8~"    [M-C-S-insert])

    (define-key function-key-map "\e[3;2~"    [S-delete])
    (define-key function-key-map "\e[3;3~"    [M-delete])
    (define-key function-key-map "\e[3;4~"    [M-S-delete])
    (define-key function-key-map "\e[3;5~"    [C-delete])
    (define-key function-key-map "\e[3;6~"    [C-S-delete])
    (define-key function-key-map "\e[3;7~"    [M-C-delete])
    (define-key function-key-map "\e[3;8~"    [M-C-S-delete]))

(defun xterm-alternative-bind-keys ()
  
    ;; Newer versions of xterm seem to have different escape sequences
    ;; for some of the above

    (define-key function-key-map "\e[1;2A" [S-up])
    (define-key function-key-map "\e[1;2B" [S-down])
    (define-key function-key-map "\e[1;2C" [S-right])
    (define-key function-key-map "\e[1;2D" [S-left])
    (define-key function-key-map "\e[1;2F" [S-end])
    (define-key function-key-map "\e[1;2H" [S-home])

    (define-key function-key-map "\e[1;3A" [M-up])
    (define-key function-key-map "\e[1;3B" [M-down])
    (define-key function-key-map "\e[1;3C" [M-right])
    (define-key function-key-map "\e[1;3D" [M-left])
    (define-key function-key-map "\e[1;3F" [M-end])
    (define-key function-key-map "\e[1;3H" [M-home])

    (define-key function-key-map "\e[1;4A" [M-S-up])
    (define-key function-key-map "\e[1;4B" [M-S-down])
    (define-key function-key-map "\e[1;4C" [M-S-right])
    (define-key function-key-map "\e[1;4D" [M-S-left])
    (define-key function-key-map "\e[1;4F" [M-S-end])
    (define-key function-key-map "\e[1;4H" [M-S-home])
    
    (define-key function-key-map "\e[1;5A" [C-up])
    (define-key function-key-map "\e[1;5B" [C-down])
    (define-key function-key-map "\e[1;5C" [C-right])
    (define-key function-key-map "\e[1;5D" [C-left])
    (define-key function-key-map "\e[1;5F" [C-end])
    (define-key function-key-map "\e[1;5H" [C-home])

    (define-key function-key-map "\e[1;6A" [C-S-up])
    (define-key function-key-map "\e[1;6B" [C-S-down])
    (define-key function-key-map "\e[1;6C" [C-S-right])
    (define-key function-key-map "\e[1;6D" [C-S-left])
    (define-key function-key-map "\e[1;6F" [C-S-end])
    (define-key function-key-map "\e[1;6H" [C-S-home])

    (define-key function-key-map "\e[1;7A" [M-C-up])
    (define-key function-key-map "\e[1;7B" [M-C-down])
    (define-key function-key-map "\e[1;7C" [M-C-right])
    (define-key function-key-map "\e[1;7D" [M-C-left])
    (define-key function-key-map "\e[1;7F" [M-C-end])
    (define-key function-key-map "\e[1;7H" [M-C-home])

    (define-key function-key-map "\e[1;8A" [S-M-C-up])
    (define-key function-key-map "\e[1;8B" [S-M-C-down])
    (define-key function-key-map "\e[1;8C" [S-M-C-right])
    (define-key function-key-map "\e[1;8D" [S-M-C-left])
    (define-key function-key-map "\e[1;8F" [S-M-C-end])
    (define-key function-key-map "\e[1;8H" [S-M-C-home]))

(defun xterm-extra-extra-keys ()

  ;; These are escapes that xterm does not provide by default, but you
  ;; can induce xterm to send them, if you configure it to do so, by
  ;; using the X resource settings given at the start of this file

  (define-key function-key-map "\e[z2a"    [S-tab])
  (define-key function-key-map "\e[z3a"    [M-tab])
  (define-key function-key-map "\e[z4a"    [M-S-tab])
  (define-key function-key-map "\e[z5a"    [C-tab])
  (define-key function-key-map "\e[z6a"    [S-C-tab])
  (define-key function-key-map "\e[z7a"    [M-C-tab])
  (define-key function-key-map "\e[z8a"    [M-C-S-tab])

  (define-key function-key-map "\e[z2b"    [S-return])
  (define-key function-key-map "\e[z3b"    [M-return])
  (define-key function-key-map "\e[z4b"    [M-S-return])
  (define-key function-key-map "\e[z5b"    [C-return])
  (define-key function-key-map "\e[z6b"    [C-S-return])
  (define-key function-key-map "\e[z7b"    [M-C-return])
  (define-key function-key-map "\e[z8b"    [M-C-S-return])

  (define-key function-key-map "\e[z2c"    [S-backspace])
  (define-key function-key-map "\e[z3c"    [M-backspace])
  (define-key function-key-map "\e[z4c"    [M-S-backspace])
  (define-key function-key-map "\e[z5c"    [C-backspace])
  (define-key function-key-map "\e[z6c"    [C-S-backspace])
  (define-key function-key-map "\e[z7c"    [M-C-backspace])
  (define-key function-key-map "\e[z8c"    [M-C-S-backspace])

  (define-key function-key-map "\e[zd"     [pause])
  (define-key function-key-map "\e[z2d"    [S-pause])
  (define-key function-key-map "\e[z3d"    [M-pause])
  (define-key function-key-map "\e[z4d"    [M-S-pause])
  (define-key function-key-map "\e[z5d"    [C-pause])
  (define-key function-key-map "\e[z6d"    [C-S-pause])
  (define-key function-key-map "\e[z7d"    [M-C-pause])
  (define-key function-key-map "\e[z8d"    [S-M-C-pause])

  (define-key function-key-map "\e[ze"     [Sys_Req])
  (define-key function-key-map "\e[z2e"    [S-Sys_Req])
  (define-key function-key-map "\e[z5e"    [C-Sys_Req])
  (define-key function-key-map "\e[z6e"    [C-S-Sys_Req])

  (define-key function-key-map "\e[zf"     [kp-enter])
  (define-key function-key-map "\e[z2f"    [S-kp-enter])
  (define-key function-key-map "\e[z3f"    [M-kp-enter])
  (define-key function-key-map "\e[z4f"    [M-S-kp-enter])
  (define-key function-key-map "\e[z5f"    [C-kp-enter])
  (define-key function-key-map "\e[z6f"    [C-S-kp-enter])
  (define-key function-key-map "\e[z7f"    [M-C-kp-enter])
  (define-key function-key-map "\e[z8f"    [S-M-C-kp-enter])

  (define-key function-key-map "\e[zg"     [kp-add])
  (define-key function-key-map "\e[z2g"    [S-kp-add])
  (define-key function-key-map "\e[z3g"    [M-kp-add])
  (define-key function-key-map "\e[z4g"    [M-S-kp-add])
  (define-key function-key-map "\e[z5g"    [C-kp-add])
  (define-key function-key-map "\e[z6g"    [C-S-kp-add])
  (define-key function-key-map "\e[z7g"    [M-C-kp-add])
  (define-key function-key-map "\e[z8g"    [S-M-C-kp-add])

  (define-key function-key-map "\e[zh"     [kp-subtract])
  (define-key function-key-map "\e[z2h"    [S-kp-subtract])
  (define-key function-key-map "\e[z3h"    [M-kp-subtract])
  (define-key function-key-map "\e[z4h"    [M-S-kp-subtract])
  (define-key function-key-map "\e[z5h"    [C-kp-subtract])
  (define-key function-key-map "\e[z6h"    [C-S-kp-subtract])
  (define-key function-key-map "\e[z7h"    [M-C-kp-subtract])
  (define-key function-key-map "\e[z8h"    [S-M-C-kp-subtract])

  (define-key function-key-map "\e[zi"     [kp-decimal])
  (define-key function-key-map "\e[z2i"    [S-kp-decimal])
  (define-key function-key-map "\e[z3i"    [M-kp-decimal])
  (define-key function-key-map "\e[z4i"    [M-S-kp-decimal])
  (define-key function-key-map "\e[z5i"    [C-kp-decimal])
  (define-key function-key-map "\e[z6i"    [C-S-kp-decimal])
  (define-key function-key-map "\e[z7i"    [M-C-kp-decimal])
  (define-key function-key-map "\e[z8i"    [S-M-C-kp-decimal])

  (define-key function-key-map "\e[zj"     [kp-divide])
  (define-key function-key-map "\e[z2j"    [S-kp-divide])
  (define-key function-key-map "\e[z3j"    [M-kp-divide])
  (define-key function-key-map "\e[z4j"    [M-S-kp-divide])
  (define-key function-key-map "\e[z5j"    [C-kp-divide])
  (define-key function-key-map "\e[z6j"    [C-S-kp-divide])
  (define-key function-key-map "\e[z7j"    [M-C-kp-divide])
  (define-key function-key-map "\e[z8j"    [S-M-C-kp-divide])

  (define-key function-key-map "\e[zk"     [kp-multiply])
  (define-key function-key-map "\e[z2k"    [S-kp-multiply])
  (define-key function-key-map "\e[z3k"    [M-kp-multiply])
  (define-key function-key-map "\e[z4k"    [M-S-kp-multiply])
  (define-key function-key-map "\e[z5k"    [C-kp-multiply])
  (define-key function-key-map "\e[z6k"    [C-S-kp-multiply])
  (define-key function-key-map "\e[z7k"    [M-C-kp-multiply])
  (define-key function-key-map "\e[z8k"    [S-M-C-kp-multiply])

  (define-key function-key-map "\e[zl"     [kp-0])
  (define-key function-key-map "\e[z2l"    [S-kp-0])
  (define-key function-key-map "\e[z3l"    [M-kp-0])
  (define-key function-key-map "\e[z4l"    [M-S-kp-0])
  (define-key function-key-map "\e[z5l"    [C-kp-0])
  (define-key function-key-map "\e[z6l"    [C-S-kp-0])
  (define-key function-key-map "\e[z7l"    [M-C-kp-0])
  (define-key function-key-map "\e[z8l"    [S-M-C-kp-0])

  (define-key function-key-map "\e[zm"     [kp-1])
  (define-key function-key-map "\e[z2m"    [S-kp-1])
  (define-key function-key-map "\e[z3m"    [M-kp-1])
  (define-key function-key-map "\e[z4m"    [M-S-kp-1])
  (define-key function-key-map "\e[z5m"    [C-kp-1])
  (define-key function-key-map "\e[z6m"    [C-S-kp-1])
  (define-key function-key-map "\e[z7m"    [M-C-kp-1])
  (define-key function-key-map "\e[z8m"    [S-M-C-kp-1])

  (define-key function-key-map "\e[zn"     [kp-2])
  (define-key function-key-map "\e[z2n"    [S-kp-2])
  (define-key function-key-map "\e[z3n"    [M-kp-2])
  (define-key function-key-map "\e[z4n"    [M-S-kp-2])
  (define-key function-key-map "\e[z5n"    [C-kp-2])
  (define-key function-key-map "\e[z6n"    [C-S-kp-2])
  (define-key function-key-map "\e[z7n"    [M-C-kp-2])
  (define-key function-key-map "\e[z8n"    [S-M-C-kp-2])

  (define-key function-key-map "\e[zo"     [kp-3])
  (define-key function-key-map "\e[z2o"    [S-kp-3])
  (define-key function-key-map "\e[z3o"    [M-kp-3])
  (define-key function-key-map "\e[z4o"    [M-S-kp-3])
  (define-key function-key-map "\e[z5o"    [C-kp-3])
  (define-key function-key-map "\e[z6o"    [C-S-kp-3])
  (define-key function-key-map "\e[z7o"    [M-C-kp-3])
  (define-key function-key-map "\e[z8o"    [S-M-C-kp-3])

  (define-key function-key-map "\e[zp"     [kp-4])
  (define-key function-key-map "\e[z2p"    [S-kp-4])
  (define-key function-key-map "\e[z3p"    [M-kp-4])
  (define-key function-key-map "\e[z4p"    [M-S-kp-4])
  (define-key function-key-map "\e[z5p"    [C-kp-4])
  (define-key function-key-map "\e[z6p"    [C-S-kp-4])
  (define-key function-key-map "\e[z7p"    [M-C-kp-4])
  (define-key function-key-map "\e[z8p"    [S-M-C-kp-4])

  (define-key function-key-map "\e[zq"     [kp-5])
  (define-key function-key-map "\e[z2q"    [S-kp-5])
  (define-key function-key-map "\e[z3q"    [M-kp-5])
  (define-key function-key-map "\e[z4q"    [M-S-kp-5])
  (define-key function-key-map "\e[z5q"    [C-kp-5])
  (define-key function-key-map "\e[z6q"    [C-S-kp-5])
  (define-key function-key-map "\e[z7q"    [M-C-kp-5])
  (define-key function-key-map "\e[z8q"    [S-M-C-kp-5])

  (define-key function-key-map "\e[zr"     [kp-6])
  (define-key function-key-map "\e[z2r"    [S-kp-6])
  (define-key function-key-map "\e[z3r"    [M-kp-6])
  (define-key function-key-map "\e[z4r"    [M-S-kp-6])
  (define-key function-key-map "\e[z5r"    [C-kp-6])
  (define-key function-key-map "\e[z6r"    [C-S-kp-6])
  (define-key function-key-map "\e[z7r"    [M-C-kp-6])
  (define-key function-key-map "\e[z8r"    [S-M-C-kp-6])

  (define-key function-key-map "\e[zs"     [kp-7])
  (define-key function-key-map "\e[z2s"    [S-kp-7])
  (define-key function-key-map "\e[z3s"    [M-kp-7])
  (define-key function-key-map "\e[z4s"    [M-S-kp-7])
  (define-key function-key-map "\e[z5s"    [C-kp-7])
  (define-key function-key-map "\e[z6s"    [C-S-kp-7])
  (define-key function-key-map "\e[z7s"    [M-C-kp-7])
  (define-key function-key-map "\e[z8s"    [S-M-C-kp-7])

  (define-key function-key-map "\e[zt"     [kp-8])
  (define-key function-key-map "\e[z2t"    [S-kp-8])
  (define-key function-key-map "\e[z3t"    [M-kp-8])
  (define-key function-key-map "\e[z4t"    [M-S-kp-8])
  (define-key function-key-map "\e[z5t"    [C-kp-8])
  (define-key function-key-map "\e[z6t"    [C-S-kp-8])
  (define-key function-key-map "\e[z7t"    [M-C-kp-8])
  (define-key function-key-map "\e[z8t"    [S-M-C-kp-8])

  (define-key function-key-map "\e[zu"     [kp-9])
  (define-key function-key-map "\e[z2u"    [S-kp-9])
  (define-key function-key-map "\e[z3u"    [M-kp-9])
  (define-key function-key-map "\e[z4u"    [M-S-kp-9])
  (define-key function-key-map "\e[z5u"    [C-kp-9])
  (define-key function-key-map "\e[z6u"    [C-S-kp-9])
  (define-key function-key-map "\e[z7u"    [M-C-kp-9])
  (define-key function-key-map "\e[z8u"    [S-M-C-kp-9])

  (define-key function-key-map "\e[zv"     [menu])
  (define-key function-key-map "\e[z2v"    [S-menu])
  (define-key function-key-map "\e[z3v"    [M-menu])
  (define-key function-key-map "\e[z4v"    [M-S-menu])
  (define-key function-key-map "\e[z5v"    [C-menu])
  (define-key function-key-map "\e[z6v"    [C-S-menu])
  (define-key function-key-map "\e[z7v"    [M-C-menu])
  (define-key function-key-map "\e[z8v"    [S-M-C-menu]))

(defun xterm-extra-screen-keys ()
  ;; These may be necessary when running GNU screen inside an xterm,
  ;; since even when running "screen -t xterm", the $TERMCAP screen
  ;; installs for xterm does not always seem to correspond to the actual
  ;; escapes that are sent for a few keys:
  (define-key function-key-map "\e[1~"    [home])
  (define-key function-key-map "\e[2~"    [insert])
  (define-key function-key-map "\e[4~"    [end]))

(defun xterm-extra-keys ()
  (xterm-extra-remap-function-keys)
  (xterm-extra-bind-keys)
  (xterm-alternative-bind-keys)
  (xterm-extra-extra-keys)
  (xterm-extra-screen-keys))

(provide 'xterm-extras)
