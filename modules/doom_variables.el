;; Open fullscreen without Daemon
(add-hook 'window-setup-hook 'toggle-frame-maximized t) 
;; Open fullscreen with Daemon
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (set-frame-parameter frame 'fullscreen 'maximized))))

;; Font Settings
(setq doom-font (font-spec :family "CaskaydiaCove NFM" :size 24))
(setq doom-symbol-font (font-spec :family "Symbols Nerd Font"))

;; This changes the base color for numbers, if Theme has set then it's overwritten
(set-face-attribute 'font-lock-number-face nil :foreground "#D27E99")

;; SPC h t - Choose a new theme inside emacs directly
(load-theme 'kanagawa-wave t)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Org Mode Directory
(setq org-directory "D:/GitProjects/DiscipleOfPermanence")

;; Set Identation Options
(electric-indent-mode -1)
(setq-default indent-tabs-mode t)
(setq-default c-ts-mode-indent-offset 4)

;; Set Identation Styles for Major Modes 
(setq c-default-style '((c-mode . "k&r")))
(setq c-ts-mode-indent-style 'bsd)

;; Remove the annyong "Do you REALLY want to quit" message
(setq confirm-kill-emacs nil)

;; Auto update buffer when file changes on disk
(setq global-auto-revert-mode t)

;; Set Dired in Hide Details Mode by default
(after! dirvish
  (setq-default dirvish-hide-details t))

;; Hack to fix Dired with Icons
(after! dired
  (require 'dired-x))


;; Set Find Sibling to change from .h to .c (SPC p o)
(after! files
  (setq find-sibling-rules
        '(("\\(.*\\)\\.c\\'" "\\1.h")
          ("\\(.*\\)\\.h\\'" "\\1.c"))))


;; Set Bookmark Directory so I can back it up
(setq bookmark-default-file (expand-file-name "bookmarks.txt" doom-user-dir))


;; Recenter After Bookmark Jump
(add-hook 'bookmark-after-jump-hook #'recenter)
;; Recenter After Consult Jump
(add-hook 'consult-after-jump-hook #'recenter)
;; Recenter After Searching
(after! evil
  (setq evil-search-wrap t)
  (add-hook 'evil-jumps-post-jump-hook #'recenter))


;; Set Compilation Buffer to open with a specific size in lines
(set-popup-rule! "^\\*compilation\\*"
  :side 'bottom
  :size 10)

;; Set customized colors!
(custom-set-faces!
  '(org-code :foreground "firebrick2" :background "grey20")
  '(org-block-begin-line :foreground "#646489" :background "#282835")
  '(org-block :background "#282835")
  '(org-block-end-line :foreground "#646489" :background "#282835"))



;; Disable the DOOM default S key behavior in Normal mode (snipe)
;; (remove-hook 'doom-first-input-hook #'evil-snipe-mode)
;; (after! evil-snipe
;;   (evil-snipe-mode -1)
;;   (evil-snipe-override-mode -1))
