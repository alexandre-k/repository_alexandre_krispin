;;; zenburn.el

(defun color-theme-zenburn ()
  "Color theme by Daniel Brockman <daniel@brockman.nu>.
Created 2003-11-22; modified 2003-11-24.

All thanks to Jani Nurminen, the maintainer of the original zenburn color
scheme for Vim, in turn based primarily on the BlackDust, Camo, and Desert Vim
color schemes.  Jani had the following to say about the magnificent
configuration of colors:

  Nothing too fancy, just some alien fruit salad to keep you in the zone.

To match your zenburned choice of text editor, I've created matching color
themes for XTerm and Ion -- so just drop me a note if you want those!  In case
I'm being bombarded by spam and still haven't figured out how to set up
SpamAssassin, they might be here:

  http://teepee.ath.cx/~drlion/

zenburn.el is free software. It's licensed under the GPL."

  (interactive)
  (color-theme-install
   '(color-theme-zenburn
     ((background-color . "#3f3f3f")
      (background-mode . dark)
      (border-color . "#3f3f3f")
;;       (cursor-color . "#8faf9f")
      (foreground-color . "#dcdccc")
      (mouse-color . "#dcdccc"))
     ((goto-address-mail-face . italic)
      (goto-address-mail-mouse-face . secondary-selection)
      (goto-address-url-face . bold)
      (goto-address-url-mouse-face . highlight)
      (help-highlight-face . underline)
      (list-matching-lines-face . bold)
      (view-highlight-face . highlight)
      (widget-mouse-face . highlight))
     (default
       ((t (:stipple nil :background "#3f3f3f" :foreground "#dcdccc"
                     :inverse-video nil :box nil :strike-through nil
                     :overline nil :underline nil :slant normal
                     :weight normal :height 120 :width normal
                     :family "b&h-lucidatypewriter"))))
     (bold ((t (:bold t :weight bold))))
     (bold-italic ((t (:italic t :bold t :slant italic :weight bold))))
     (border ((t (:background "black"))))
     (comint-highlight-input ((t (:bold t :weight bold))))
     (comint-highlight-prompt ((t (:foreground "cyan"))))
     (cursor ((t (:bold t :background "#dcdccc" :weight bold))))
     (custom-button-face
      ((t (:box (:line-width 1 :style released-button)
                :background "#606060" :foreground "#dcdccc"))))
     (custom-button-pressed-face
      ((t (:box (:line-width 1 :style pressed-button)
                :foreground "#acac9c" :background "#464646"))))
     (custom-changed-face
      ((t (:family "b&h-lucidatypewriter" :width normal :weight normal
                   :slant normal :underline nil :overline nil
                   :strike-through nil :box nil :inverse-video nil
                   :background "#3f3f3f" :stipple nil
                   :foreground "#8cd0d3"))))
     (custom-comment-face ((t (:background "dim gray"))))
     (custom-comment-tag-face ((t (:foreground "gray80"))))
     (custom-documentation-face ((t (nil))))
     (custom-face-tag-face
      ((t (:bold t :family "helv" :weight bold :height 1.2))))
     (custom-group-tag-face
      ((t (:bold t :foreground "#94bff3" :weight bold :height 1.2))))
     (custom-group-tag-face-1
      ((t (:bold t :family "helv" :foreground "#dca3a3" :weight bold
                 :height 1.2))))
     (custom-invalid-face
      ((t (:foreground "#e37170" :background "#332323"))))
     (custom-modified-face
      ((t (:foreground "#8cd0d3" :stipple nil :background "#3f3f3f"
                       :inverse-video nil :box nil :strike-through nil
                       :overline nil :underline nil :slant normal
                       :weight normal :family "b&h-lucidatypewriter"))))
     (custom-rogue-face
      ((t (:background "black" :foreground "pink"))))
     (custom-saved-face ((t (:underline t))))
     (custom-set-face
      ((t (:family "b&h-lucidatypewriter"
                   :weight normal :slant normal :underline nil
                   :overline nil :strike-through nil :box nil
                   :foreground "#3f3f3f" :stipple nil
                   :background "#8cd0d3"))))
     (custom-state-face ((t (:foreground "#7f9f7f"))))
     (custom-variable-button-face
      ((t (:bold t :underline t :weight bold))))
     (custom-variable-tag-face
      ((t (:bold t :family "helv" :foreground "#94bff3"
                 :weight bold :height 1.2))))
;;      (ediff-current-diff-face-A ((t (:background "pale green" :foreground "firebrick"))))
;;      (ediff-current-diff-face-Ancestor ((t (:background "VioletRed" :foreground "Black"))))
;;      (ediff-current-diff-face-B ((t (:background "Yellow" :foreground "DarkOrchid"))))
;;      (ediff-current-diff-face-C ((t (:background "Pink" :foreground "Navy"))))
;;      (ediff-even-diff-face-A ((t (:background "light grey" :foreground "Black"))))
;;      (ediff-even-diff-face-Ancestor ((t (:background "Grey" :foreground "White"))))
;;      (ediff-even-diff-face-B ((t (:background "Grey" :foreground "White"))))
;;      (ediff-even-diff-face-C ((t (:background "light grey" :foreground "Black"))))
;;      (ediff-fine-diff-face-A ((t (:background "sky blue" :foreground "Navy"))))
;;      (ediff-fine-diff-face-Ancestor ((t (:background "Green" :foreground "Black"))))
;;      (ediff-fine-diff-face-B ((t (:background "cyan" :foreground "Black"))))
;;      (ediff-fine-diff-face-C ((t (:background "Turquoise" :foreground "Black"))))
;;      (ediff-odd-diff-face-A ((t (:background "Grey" :foreground "White"))))
;;      (ediff-odd-diff-face-Ancestor ((t (:background "light grey" :foreground "Black"))))
;;      (ediff-odd-diff-face-B ((t (:background "light grey" :foreground "Black"))))
;;      (ediff-odd-diff-face-C ((t (:background "Grey" :foreground "White"))))
     (erc-action-face ((t (:bold t :weight bold))))
     (erc-bold-face ((t (:bold t :weight bold))))
     (erc-current-nick-face ((t (:bold t :foreground "#f0dfaf" :weight bold))))
     (erc-default-face ((t (nil))))
     (erc-direct-msg-face ((t (:foreground "#dfaf8f"))))
     (erc-error-face ((t (:bold t :foreground "#e37170" :background "#332323"
                                :weight bold))))
     (erc-highlight-face ((t (:underline t :foreground "#f8f893"))))
     (erc-host-danger-face ((t (:foreground "#dca3a3"))))
     (erc-input-face ((t (:bold t :foreground "#f0dfaf" :weight bold))))
     (erc-inverse-face ((t (:background "#dcdccc" :foreground "#3f3f3f"))))
     (erc-notice-face ((t (:foreground "#7f9f7f"))))
     (erc-pal-face ((t (:foreground "#c3bf9f"))))
     (erc-prompt-face ((t (:bold t :foreground "#dfdfbf" :weight bold))))
     (erc-timestamp-face ((t (:bold t :foreground "#c3bf9f" :weight bold))))
     (erc-underline-face ((t (:underline t))))
     (fixed-pitch ((t (nil))))
     (font-lock-builtin-face
      ((t (:foreground "#7f9f7f"))))
     (font-lock-comment-face
      ((t (:foreground "#7f9f7f"))))
     (font-lock-constant-face
      ((t (:bold t :foreground "#dca3a3" :weight bold))))
     (font-lock-doc-face
      ((t (:foreground "#7f9f7f"))))
     (font-lock-function-name-face
      ((t (:foreground "#efdcbc"))))
     (font-lock-keyword-face
      ((t (:bold t :foreground "#f0dfaf" :weight bold))))
     (font-lock-string-face
      ((t (:foreground "#cc9393"))))
     (font-lock-type-face
      ((t (:bold t :foreground "#dfdfbf" :weight bold))))
     (font-lock-variable-name-face
      ((t (:foreground "#efdcbc"))))
     (font-lock-warning-face
      ((t (:background "#332323" :foreground "#e37170"))))
     (fringe ((t (:background "#464646"))))
;;      (header-line
;;       ((t (:foreground "#acbc90"
;;                  :background "#464646" :weight normal))))
     (header-line
      ((t (:foreground "#88b090" :background "#2e3330"))))
     (highlight ((t (:underline t :foreground "#f8f893"))))
     (info-xref ((t (:bold t :foreground "#94bff3" :weight bold))))
     (info-menu-5 ((t (:foreground "white"))))
     (isearch ((t (:bold t :background "black" :foreground "white"
                         :weight bold))))
     (isearch-lazy-highlight-face ((t (:background "black"))))
     (italic ((t (:italic t :slant italic))))
     (menu ((t (nil))))
     (mode-line ((t (:foreground "#acbc90" :background "#1e2320"))))
;;      (mode-line ((t (:box (:line-width 1 :style released-button)
;;                           :foreground "#dcdccc"
;;                           :background "#606060"))))
     (mouse ((t (:background "#dcdccc" :foreground "#464646"))))
     (region ((t (:foreground "#71d3b4" :background "#233323"))))
     (scroll-bar ((t (:background "#606060"))))
     (secondary-selection ((t (:background "#506070"))))
     (strokes-char-face ((t (:background "lightgray"))))
     (tool-bar ((t (:box (:line-width 1 :style released-button)
                         :background "#606060"
                         :foreground "#dcdccc"))))
     (tooltip ((t (:background "lightyellow" :foreground "black"))))
     (trailing-whitespace
      ((t (:foreground "#e37170" :background "#332323"))))
     (tuareg-font-lock-governing-face
      ((t (:bold t :foreground "#dfaf8f" :slant normal :weight bold))))
     (tuareg-font-lock-interactive-error-face
      ((t (:italic t :bold t :foreground "#ec93d3" :slant italic
                   :weight bold))))
     (tuareg-font-lock-interactive-output-face
      ((t (:foreground "#93e0e3" :slant normal :weight normal))))
     (tuareg-font-lock-operator-face
      ((t (:bold t :foreground "#dfaf8f" :slant normal
                 :weight bold))))
     (underline ((t (:underline t))))
     (variable-pitch ((t (:family "new century schoolbook"))))
     (widget-button-face
      ((t (:bold t :weight bold))))
;;      (widget-button-face
;;       ((t (:box (:line-width 1 :style released-button)
;;                 :foreground "#dcdccc" :background "#606060"))))
     (widget-button-pressed-face
      ((t (:box (:line-width 1 :style pressed-button)
                :background "#464646" :foreground "#acac9c"))))
     (widget-documentation-face ((t (:foreground "#7f9f7f"))))
     (widget-field-face ((t (:background "#606060"))))
     (widget-inactive-face ((t (nil))))
     (widget-single-line-field-face ((t (:background "#606060")))))))

(provide 'zenburn)

;;; zenburn.el ends here.