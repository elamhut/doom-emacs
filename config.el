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

;; Remove the annyong "Do you REALLY want to quit" message
(setq confirm-kill-emacs nil)

;; Set Find Sibling to change from .h to .c
(after! files
  (setq find-sibling-rules
        '(("\\(.*\\)\\.c\\'" "\\1.h")
          ("\\(.*\\)\\.h\\'" "\\1.c"))))

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

;; Hack to fix Dired with Icons
(after! dired
  (require 'dired-x))

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

;; Ctrl V will open Search Result in Other Window
;; 1. The Action: logic to parse the string and open the file
(defun +vertico/open-candidate-other-window (candidate)
  "Parses 'file:line:content' and opens it in another window."
  (interactive "sCandidate: ")
  (let* ((clean-cand (substring-no-properties candidate))
         (file clean-cand)
         (line nil))
    
    ;; A. Parse "File:Line:" pattern
    (when (string-match "^\\([^:]+\\):\\([0-9]+\\):" clean-cand)
      (setq file (match-string 1 clean-cand))
      (setq line (string-to-number (match-string 2 clean-cand))))

    ;; B. Open the file (relative to current search context)
    ;; We use expand-file-name to ensure we don't lose the path
    (find-file-other-window (expand-file-name file))

    ;; C. Jump to line
    (when line
      (goto-char (point-min))
      (forward-line (1- line))
      (recenter)
      (pulse-momentary-highlight-one-line (point)))))

;; 2. The Trigger: A specific command to bind to C-v
(defun +vertico/trigger-open-other-window ()
  "Trigger embark to act on the current candidate with our custom function."
  (interactive)
  (let ((embark-prompter (lambda (&rest _) #'+vertico/open-candidate-other-window)))
    (embark-act)))

;; 3. The Bind: clear and explicit
(map! :after vertico
      :map vertico-map
      "C-v" #'+vertico/trigger-open-other-window)

;; Narrow to Scope then select all occurrences of word
(defun my/multiedit-defun ()
  "Narrow to defun, multiedit symbol at point, then widen on exit."
  (interactive)
  (narrow-to-defun)

  ;; Add temporary advice to widen after multiedit exits
  (advice-add
   'evil-multiedit-abort :after
   (defun my/multiedit--widen (&rest _)
     (when (buffer-narrowed-p)
       (widen))
     ;; Remove advice after running once
     (advice-remove 'evil-multiedit-abort #'my/multiedit--widen)))

  ;; Start multiedit
  (evil-multiedit-match-all))

(map! :leader
      :desc "Multiedit symbol in defun"
      "r" #'my/multiedit-defun)

;; Grep functions and names in another Project
(defun doom/grep-in-other-project ()
  "Ripgrep in another Projectile project."
  (interactive)
  (require 'consult)
  (require 'projectile)
  (let* ((projects (projectile-relevant-known-projects))
         (project (projectile-completing-read
                   "Grep in project: "
                   projects)))
    (consult-ripgrep project)))

;; Fuzzy Find files in another Project
(defun doom/projectile-find-file-in-other-project ()
  "Use Projectile to find a file in another known project."
  (interactive)
  (require 'projectile)
  (let* ((projects (projectile-relevant-known-projects))
         (project (projectile-completing-read
                   "Find file in project: "
                   projects))
         (default-directory project))
    (projectile-find-file)))

;; Add Lines without entering Insert Mode
(defun my/evil-open-below-no-insert ()
  "Add a line below without entering insert mode."
  (interactive)
  (evil-open-below 1)
  (evil-normal-state))

(defun my/evil-open-above-no-insert ()
  "Add a line above without entering insert mode."
  (interactive)
  (evil-open-above 1)
  (evil-normal-state))

;; Magit Stage all Modified and Commit with message
(defun my-magit-stage-all-and-commit (message)
  (interactive "sCommit message: ")
  (magit-stage-modified)
  (magit-commit-create `("-m" ,message)))

;; Disable the DOOM default S key behavior in Normal mode (snipe)
(remove-hook 'doom-first-input-hook #'evil-snipe-mode)
(after! evil-snipe
  (evil-snipe-mode -1)
  (evil-snipe-override-mode -1))

;; Keybinds
(map! :n "j"   #'evil-next-visual-line
      :n "k"   #'evil-previous-visual-line
      :v "j"   #'evil-next-visual-line
      :v "k"   #'evil-previous-visual-line

      :n "C-j" #'evil-scroll-down
      :n "C-k" #'evil-scroll-up
      :v "C-j" #'evil-scroll-down
      :v "C-k" #'evil-scroll-up
      
      :n "M-j" #'evil-scroll-line-down
      :n "M-k" #'evil-scroll-line-up
      :v "M-j" #'evil-scroll-line-down
      :v "M-k" #'evil-scroll-line-up

      :n "M-n" #'my/evil-open-below-no-insert
      :n "M-p" #'my/evil-open-above-no-insert
      :v "M-n" #'my/evil-open-below-no-insert
      :v "M-p" #'my/evil-open-above-no-insert

      :n "C-e" #'move-end-of-line
      :n "C-y" #'yank
      :v "C-e" #'move-end-of-line
      :v "C-y" #'yank

      :n "C-p" #'previous-error
      :n "C-n" #'next-error
      :v "C-p" #'previous-error
      :v "C-n" #'next-error

      :n "C-z" #'evil-undo
      :v "C-z" #'evil-undo
      :i "C-z" #'evil-undo

      :i "C-v" #'yank

      :n "C-b a" #'bookmark-set
      :n "C-b s" #'bookmark-save
      :n "C-b d" #'bookmark-delete
      :n "C-b r" #'bookmark-rename
      :n "C-b l" #'bookmark-bmenu-list

      ;;;Leader Keymaps;;;
      :leader :desc "Jump to Written Text" "SPC" #'avy-goto-char-timer
      :leader :desc "Find Files in Project" "f p" #'projectile-find-file

      ;; Projectile keymaps
      :leader :desc "Configure Project" "p *" #'projectile-configure-project
      :leader :desc "Grep in other project" "p g" #'doom/grep-in-other-project
      :leader :desc "Grep in other project" "p g" #'doom/grep-in-other-project
      :leader :desc "Find file in other project" "p SPC" #'doom/projectile-find-file-in-other-project
      :leader :desc "Find file in other project" "p F" #'doom/projectile-find-file-in-other-project

      ;; Magit Keymaps
      :leader "g p" #'magit-pull
      :leader "g P" #'magit-push
      :leader :desc "Magit Stage All and Commit" "g SPC" #'my-magit-stage-all-and-commit
      :leader :desc "Magit Merge Plain" "g m m" #'magit-merge-plain
      :leader :desc "Magit Merge with Comment" "g m c" #'magit-merge-editmsg
      :leader :desc "Magit Merge Preview" "g m p" #'magit-merge-preview
      :leader :desc "Magit Merge Abort" "g m a" #'magit-merge-abort
      :leader :desc "Magit Add Untracked" "g a" #'magit-stage-untracked
      :leader :desc "Magit Stage this File" "g s" #'magit-file-stage
      :leader :desc "Magit Stage all Modified" "g S" #'magit-stage-modified

      ;; Custom Keymaps
      :leader :desc "project build (auto-detect)" "b" #'my/project-build)
