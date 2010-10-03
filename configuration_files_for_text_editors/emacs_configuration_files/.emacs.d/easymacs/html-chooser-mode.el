;;; html-chooser-mode.el -- use nxml-mode or html-mode, depending, and
;;                          insert templates for HTML and XHTML.
;;
;; Author: Peter Heslin <p.j.heslin@dur.ac.uk>
;; Version: 1.1
;; 
;; Copyright (C) 2005 Peter Heslin
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
;;
;;; Commentary:
;;
;; nxml-mode is an excellent mode for authoring XHTML (see
;; http://www.emacswiki.org/cgi-bin/wiki/NxmlModeForXHTML), but it
;; only works for XHTML. Even people who prefer XHTML still must often
;; deal with legacy HTML documents, and so many people need to work
;; with files in both flavors.  Unfortunately, both usually have the
;; same .html or .htm extension, and so are treated identically by
;; auto-mode-alist.  If, as a result, you accidentally open an HTML
;; file in nxml-mode, Emacs churns CPU while it tries in vain to
;; validate it, until you kill the buffer.
;;
;; This file provides a dummy major mode called html-chooser-mode that
;; looks at a file, and if it seems to be XHTML, switches to
;; nxml-mode, and if it seems to be regular HTML, switches to
;; html-mode (or html-helper-mode, or whatever).  If the file is empty
;; or otherwise cannot be understood, the user is prompted to choose
;; which mode, and if empty, is offered the possibility of inserting
;; several skeletons or templates appropriate to that mode.
;;
;; In fact, you may not need html-chooser-mode at all, since you can
;; get most of its functionality by putting this in your .emacs file:
;;
;;  (setq magic-mode-alist (cons '("<\\?xml\\s " . nxml-mode) magic-mode-alist))
;;  (setq auto-mode-alist  (cons '("\\.x?html?$" . html-mode) auto-mode-alist))
;;
;; In this way, all files with a .html extension will be opened in
;; html-mode, except for those containing <?xml at the start, which
;; will be opened in nxml-mode.  This doesn't handle new files well,
;; though: it puts them into html-mode, even if your intention is to
;; create an XHTML file with nxml-mode.  It also completely fails to
;; work on older versions of Emacs that don't implement
;; magic-mode-alist.  Hence html-chooser-mode.
;;
;;; Installation:
;;
;; Put these lines in your .emacs file:
;;
;;  (when (boundp 'magic-mode-alist)
;;    (setq magic-mode-alist
;;                (cons '("<\\?xml\\s " . nxml-mode) magic-mode-alist)))
;;
;;  (setq auto-mode-alist (cons '("\\.x?html?$" . html-chooser-mode) auto-mode-alist))
;;  (autoload 'html-chooser-mode "html-chooser-mode")

;; Changes:
;;
;; 1.0 Initial release
;; 1.1 Don't offer to insert a template if there's already text in the
;;     buffer (as for HTML fragments)

;;; Code:

(defvar html-chooser-mode-xhtml-mode 'nxml-mode)
(defvar html-chooser-mode-html-mode  'html-mode)

(defvar html-chooser-mode-xml-declaration
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
")

(defvar html-chooser-mode-doctype-alist
  '((xhtml11 . "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">")
    (trxhtml10 . "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">")
    (strxhtml10 . "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
    (frxhtml10 . "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">")
    (trhtml401 . "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"
\"http://www.w3.org/TR/html4/loose.dtd\">")
    (strhtml401 . "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"
\"http://www.w3.org/TR/html4/strict.dtd\">")
    (frhtml401 . "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\"
\"http://www.w3.org/TR/html4/frameset.dtd\">")
    (html32 . "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">")
    (html20 . "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">")))

(defvar html-chooser-mode-default-doctypes
  '((xhtml . "xhtml 1.1")
    (html  . "transitional html 4.01")))

(defvar html-chooser-mode-skeleton-alist
  '((xhtml .
"<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">
  <head>
    <title></title>
  </head>
  <body>
    <p>
    </p>
  </body>
</html>
")
    (html .
"<HTML>
  <HEAD>
    <TITLE></TITLE>
  </HEAD>
  <BODY>
    <P>
    </P>
  </BODY>
</HTML>
")))


(defun html-chooser-mode ()
  (interactive)
  (goto-char (point-min))
  (if (or (looking-at "\\s *<\\?xml\\s ")
          (re-search-forward "<[Hh][Tt][Mm][Ll]\\s xmlns=.*xhtml" nil t))
      ;; looks like XHTML
      (funcall html-chooser-mode-xhtml-mode)
    (goto-char (point-min))
    (if (re-search-forward
         "<\\([hH][tT][mM][lL]\\|[hH][eE][aA][dD]\\|[bB][oO][dD][yY]\\)" nil t)
        ;; looks like HTML
        (funcall html-chooser-mode-html-mode)
      (let ((type (if (y-or-n-p "Is this an XHTML file? ") 'xhtml 'html)))
        ;; Don't offer to insert template if there's already text
        (unless (> (point-max) 1)
          (when (y-or-n-p "Do you want to insert a template? ")
            (html-chooser-mode-insert-template type)))
        (funcall (if (eq type 'xhtml)
                     html-chooser-mode-xhtml-mode
                   html-chooser-mode-html-mode))))))

(defun html-chooser-mode-insert-template (type)
  (let* ((choices
          (if (eq type 'xhtml)
              '(("xhtml 1.1" . xhtml11)
                ("transitional xhtml 1.0" . trxhtml10)
                ("strict xhtml 1.0" . strxhtml10)
                ("frameset xhtml 1.0" . frxhtml10))
            '(("transitional html 4.01" . trhtml401)
              ("strict html 4.01" . strhtml401)
              ("frameset html 4.01" . frhtml401)
              ("html 3.2" . html32)
              ("html 2.0" . html20))))
         (default
           (cdr (assoc type html-chooser-mode-default-doctypes)))
         (style (cdr (assoc
                 (completing-read "Style: " choices nil t default) choices))))
    (goto-char (point-min))
    (when (eq type 'xhtml)
      (insert html-chooser-mode-xml-declaration))
    (when style
      (insert (cdr (assoc style html-chooser-mode-doctype-alist))
              "\n"))
    (insert (cdr (assoc type html-chooser-mode-skeleton-alist)))
    (goto-char (point-min))
    (re-search-forward "<[Tt][Ii][Tt][Ll][Ee]>" nil t)))
    

(provide 'html-chooser-mode)