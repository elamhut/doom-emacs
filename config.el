;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

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

;; Set Identation Options
(electric-indent-mode -1)
(setq-default indent-tabs-mode t)
(setq-default c-ts-mode-indent-offset 4)

;; Set Identation Styles for Major Modes 
(setq c-default-style '((c-mode . "k&r")))
(setq c-ts-mode-indent-style 'bsd)

;; Tree-Sitter Config
(add-hook 'c-ts-mode-hook
          (lambda ()
            (setq treesit-indent-mode nil)))

;; This controls how much Tree Sitter Syntex Highlight color you want
(setq treesit-font-lock-level 3)


;; Configuring TreeSitter for Syntax Highlighting of Numbers
(add-hook 'c-ts-mode-hook
	  (lambda ()
	    (setq-local treesit-font-lock-settings
			(append
			 treesit-font-lock-settings
			 (treesit-font-lock-rules
			  :language 'c
			  :feature 'number
			  '((number_literal)
			    @font-lock-number-face))))
	    (font-lock-flush)))

;; This controls the colors for a symbol on each level. Each parenthesis is a level
;; we added the 'number' option in the function above
(setq c-ts-mode--feature-list
      '((comment definition)
	(keyword preprocessor string type)
	(assignment constant function escape-sequence label literal variable number)
	(bracket delimiter error operator property)))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Set Compilation Buffer to open with a specific size in lines
(set-popup-rule! "^\\*compilation\\*"
  :side 'bottom
  :size 10)

;; Detect build.bat in room and auto-build
(after! projectile
  (defun my/project-build ()
    "Run build.bat if it exists in the project root, otherwise repeat last Projectile command with prompt."
    (interactive)
    (if-let* ((root (projectile-project-root))
	      (build (expand-file-name "build.bat" root))
	      ((file-exists-p build)))
	;; Run build.bat from project root
	(let ((default-directory root))
	  (compile "build.bat"))
      ;; Fallback
      (projectile-repeat-last-command 'show-prompt))))

;; Activating Drag-Stuff
(after! drag-stuff
  (drag-stuff-global-mode 1))

;; Keybinds
(map! :n "j"   #'evil-next-visual-line
      :n "k"   #'evil-previous-visual-line
      :v "j"   #'evil-next-visual-line
      :v "k"   #'evil-previous-visual-line

      :n "C-j" #'evil-scroll-down
      :n "C-k" #'evil-scroll-up
      :v "C-j" #'evil-scroll-down
      :v "C-k" #'evil-scroll-up
      
      :n "M-n" #'evil-scroll-line-down
      :n "M-p" #'evil-scroll-line-up
      :v "M-n" #'evil-scroll-line-down
      :v "M-p" #'evil-scroll-line-up

      :n "C-e" #'move-end-of-line
      :n "C-y" #'yank
      :v "C-e" #'move-end-of-line
      :v "C-y" #'yank

      :leader :desc "project build (auto-detect)" "b" #'my/project-build)

(after! drag-stuff
  (map! :n "M-j" #'drag-stuff-down
        :n "M-k" #'drag-stuff-up
        :v "M-j" #'drag-stuff-down
        :v "M-k" #'drag-stuff-up))

;; Disable the DOOM default S key behavior in Normal mode (snipe)
(remove-hook 'doom-first-input-hook #'evil-snipe-mode)

;; Hack to fix Dired with Icons
(after! dired
  (require 'dired-x))

;; Remove the annyong "Do you REALLY want to quit" message
(setq confirm-kill-emacs nil)

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
