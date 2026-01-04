;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(load! "modules/doom_variables.el")
(load! "modules/treesitter.el")
(load! "modules/projectile.el")
(load! "modules/org_project.el")
(load! "modules/vertico.el")
(load! "modules/magit.el")
(load! "modules/my_customizations.el")


;;;;NOTE: MY FUNCTON TO TEST STUFF!!;;;;
(defun my-test-butten()
  (interactive)
  (setq x (region-beginning))
  (setq y (region-end))
  (evil-indent x y)
  (evil-visual-make-region x (- y 1)))

(map! :v "<f14>" #'my-test-butten)
;;;;NOTE: MY STUFF END!!;;;;


;; Keymaps ;;
(map! :n "j"   #'evil-next-visual-line
      :n "k"   #'evil-previous-visual-line
      :v "j"   #'evil-next-visual-line
      :v "k"   #'evil-previous-visual-line
      :i "C-l" #'forward-char
      :i "C-h" #'backward-char

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
      :leader "r" #'anzu-query-replace
      :leader "d" #'dired-jump
      :leader "<" #'evil-switch-to-windows-last-buffer
      :leader ">" #'consult-buffer

      ;; Projectile keymaps
      :leader :desc "Configure Project" "p *" #'projectile-configure-project
      :leader :desc "Grep in other project" "p g" #'doom/grep-in-other-project
      :leader :desc "Find file in other project" "p SPC" #'doom/projectile-find-file-in-other-project
      :leader :desc "Find file in other project" "p F" #'doom/projectile-find-file-in-other-project

      ;; Magit Keymaps
      :leader "g p" #'magit-pull
      :leader "g P" #'magit-push
      :leader "g L" #'nil
      :leader :desc "Log this Buffer" "g L l" #'magit-log-buffer-file
      :leader :desc "Log All Branches" "g L L" #'magit-log-all
      :leader :desc "Magit Stage All and Commit" "g SPC" #'my-magit-stage-all-and-commit
      :leader :desc "Magit Merge Plain" "g m m" #'magit-merge-plain
      :leader :desc "Magit Merge with Comment" "g m c" #'magit-merge-editmsg
      :leader :desc "Magit Merge Preview" "g m p" #'magit-merge-preview
      :leader :desc "Magit Merge Abort" "g m a" #'magit-merge-abort
      :leader :desc "Magit Add Untracked" "g a" #'magit-stage
      :leader :desc "Magit Stage this File" "g s" #'magit-file-stage
      :leader :desc "Magit Stage all Modified" "g S" #'magit-stage-modified

      ;; My Custom Function Keymaps
      :leader :desc "project build (auto-detect)" "b" #'edu/project-build)

;; Special Maps
 
;; This is Replace mode <leader> r
(after! replace
  (define-key query-replace-map (kbd "<escape>") 'exit)
  (define-key query-replace-map (kbd "<return>") 'act)
  (define-key query-replace-map (kbd "RET") 'act)
  (define-key query-replace-map (kbd "SPC") 'skip)
  (define-key query-replace-map (kbd "<backspace>") 'undo)
  (define-key query-replace-map (kbd "<delete>") 'undo-all))

;; Vertico Rebinds
(map! :after vertico
      :map vertico-map "C-v" #'+vertico/trigger-open-other-window
      :map vertico-map "M-j" #'next-history-element
      :map vertico-map "M-k" #'previous-history-element)

;; Git and Magit close buffers with ESC instead of Q
(after! magit
  (map! :map magit-status-mode-map    :n [escape] #'+magit/quit)
  (map! :map magit-diff-mode-map      :n [escape] #'+magit/quit)
  (map! :map magit-log-mode-map       :n [escape] #'+magit/quit))

(after! git-timemachine
  (evil-define-minor-mode-key 'normal 'git-timemachine-mode [escape] #'git-timemachine-quit))
