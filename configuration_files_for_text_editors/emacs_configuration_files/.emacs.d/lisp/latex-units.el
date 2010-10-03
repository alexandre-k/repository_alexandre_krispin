;; latex-units.el - Add a Units sub-menu to auctex's math-mode's menu.

;; Copyright (C) 1995, 1996 Peter S. Galbraith
 
;; Author:    Peter S. Galbraith <GalbraithP@dfo-mpo.gc.ca>
;; Created:   18 December 1995
;; Version:   1.02 (25 October 96)
;; Keywords:  auctex, emacs

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
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;
;; This code adds a `Units' submenu to the auctex's LaTeX-math-mode pull-down
;; menu.  The commands are available in LaTeX-math-mode only as "C-` letter"
;; This code needs latex.el v 5.43 or later.  Add your own entries to the
;; menu by editing the variable LaTeX-math-units-list similarly to the constant
;; LaTeX-math-units-default.  I have already entered some examples which I
;; use, but you are of course free to delete them.
;;
;; All you need to do is add this line to your .emacs file:
;;   (require 'latex-units)
;;
;; New versions of this package (if they exist) may be found at:
;;  http://people.debian.org/~psg/elisp/latex-units.el

;; The method of dynamic key defintions in this package is derived from 
;; latex.el, part of auctex, Copyright 1991 Kresten Krab Thorup
;; Copyright 1993, 1994, 1995 Per Abrahamsen

;;; Change log:
;; V1.00 Dec 18 95 - Peter Galbraith - Created
;; V1.01 Jan 22 96 - Peter Galbraith - Added Diffusivity.
;; V1.02 Oct 25 96 - Peter Galbraith - Changed "Insert Units" to simply "Units"
;;                                   - Added Lat an Lon.

;;; Code:

(require 'latex)                        ;Requires auctex's latex.el

(defvar LaTeX-math-units-list
  '(( ?a "APEF"            "mbox{${\\rm m}^{2}\\,{\\rm s}^{-2}$}")
    ( ?k "Diffusivity"     "mbox{${\\rm m}^{2}\\,{\\rm s}^{-1}$}")
  ;;( ?D "Dissipation"     "mbox{${\\rm m}^{2}\\,{\\rm s}^{-3}$}")
    ( ?D "Dissipation"     "mbox{${\\rm W\\,kg}^{-1}$}")
    ( ?h "Heat Flux"       "mbox{${\\rm W\\,m}^{-2}$}")
    ( ?x "Longitude"       "mbox{$^\\circ{\\rm W}$}") ;North America default!
    ( ?y "Latitude"        "mbox{$^\\circ{\\rm N}$}"));North America default!
  "List of your personal LaTeX math units.  
Each list entry is a list of the key to be bound, menu label 
and string to insert")

(defconst LaTeX-math-units-default
  '(( ?l "Length      m"   "mbox{${\\rm m}$}")
    ( ?m "Mass        kg"  "mbox{${\\rm kg}$}")
    ( ?T "Temperature C"   "mbox{$^\\circ{\\rm C}$}")
    ( ?t "Time        s"   "mbox{${\\rm s}$}")
    ( ?e "Energy      J"   "mbox{${\\rm J}$}")
    ( ?p "Power       W"   "mbox{${\\rm W}$}")
    ( ?S "Shear"           "mbox{${\\rm s}^{-1}$}")
    ( ?d "Density"         "mbox{${\\rm kg\\,m}^{-3}$}")
    ( ?v "Velocity"        "mbox{${\\rm m\\,s}^{-1}$}")))

(defvar LaTeX-math-units-menu nil       ;'("Insert Units"))
  "Menu containing LaTeX units commands.")
(defvar LaTeX-math-units-keymap (make-sparse-keymap)
  "Keymap containing LaTeX units commands.")

(define-key LaTeX-math-keymap [?\C-`] LaTeX-math-units-keymap)

(let ((units (append LaTeX-math-units-default LaTeX-math-units-list))
      (menu))
  (while units
    (let* ((entry (car units))   
           (key (nth 0 entry)) (label (nth 1 entry)) (value (nth 2 entry)))
      (setq units (cdr units))
      ;; key should usually be `numberp'  e.g. ?l converts to "l"
      (setq key (if (numberp key) (char-to-string key) (vector key)))
      (string-match "[^ ]+" label)
      (let ((name (intern (concat "LaTeX-units-" 
                                  (substring label 0 (match-end 0))))))
        (fset name (list 'lambda (list 'arg) (list 'interactive "*P")
                         (list 'LaTeX-math-insert value 'arg)))
        (define-key LaTeX-math-units-keymap key name)
        ;; Here I should add a check for user overriden key definition.
        ;; e.g. avoid multiply defined key in menu.
        (setq menu (cons (vector label name t) menu))
        menu)))
  (setq LaTeX-math-units-menu 
       ;;(cons "Insert Units" (sort menu 'LaTeX-elt-string-lessp))
       (cons "Units" (reverse menu))))
 
;;(defun LaTeX-elt-string-lessp (a b)
;;  (string-lessp (elt a 0) (elt b 0)))

;; In latex.el, LaTeX-math-menu is constructed and then passed to 
;; easy-menu-define to build LaTeX-math-mode-menu and add it to 
;; LaTeX-math-keymap.  We do it again here, adding our `Units' menu.
(easy-menu-define LaTeX-math-mode-menu
    LaTeX-math-keymap
    "Menu used in math minor mode."
    (append  LaTeX-math-menu (list LaTeX-math-units-menu)))

(provide 'latex-units)
;;; latex-units.el ends here
