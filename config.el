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

;; Set Compilation Buffer to open with a specific size in lines
(set-popup-rule! "^\\*compilation\\*"
  :side 'bottom
  :size 10)

;; Center buffer after Searching
(after! evil
  (setq evil-search-wrap t)
  (add-hook 'evil-jumps-post-jump-hook #'recenter))

;; D F <space> should now delete the whitespace
(defun edu/cleanup-whitespace-after-df-space (&rest _)
  "Auto-delete extra whitespace after running 'd f <space>'."
  (when (and (eq evil-this-operator 'evil-delete)      ;; 1. It was a delete op
             (looking-at "[ \t]"))                     ;; 2. We are staring at a space
    
    ;; 3. Check if the motion was 'f' (standard or snipe)
    (when (memq evil-this-motion '(evil-find-char evil-snipe-f))
      
      ;; 4. Verify the user actually typed <space> as the target
      ;;    (We check the last key press of the command)
      (let* ((keys (this-command-keys-vector))
             (last-key (aref keys (1- (length keys)))))
        
        (when (= last-key 32) ;; 32 is the ASCII code for SPACE
          (delete-char 1))))))

;; Apply the advice
(advice-add 'evil-delete :after #'edu/cleanup-whitespace-after-df-space)



(defun edu/jump-to-next-brace ()
  "Jump forward to the next { or }."
  (interactive)
  ;; If we are already standing on a brace, step forward one character
  ;; so we don't just 'find' the one we are currently on.
  (when (looking-at "[{}]")
    (forward-char 1))
  
  ;; Search forward. If found, step back 1 so cursor is ON the char.
  (if (re-search-forward "[{}]" nil t)
      (backward-char 1)
    (message "No next brace found")))

(defun edu/jump-to-prev-brace ()
  "Jump backward to the previous { or }."
  (interactive)
  ;; re-search-backward automatically searches text *before* the cursor,
  ;; so we don't usually need to manually adjust point before searching.
  (unless (re-search-backward "[{}]" nil t)
    (message "No previous brace found")))

;; Bindings
(map! :n ")"     #'edu/jump-to-next-brace
      :n "(" #'edu/jump-to-prev-brace)



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
(setq org-directory "D:/GitProjects/DiscipleOfPermanence")

;; Detect build.bat in room and auto-build
(after! projectile
  (defun edu/project-build ()
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

;; Open and Run or just Run the project's .sln in Visual Studio
(after! projectile
  (defvar edu/vs-process nil "Holds the running Visual Studio process object.")

  (defun edu/debug-and-runin-visualstudio ()
    "If VS is open (cached), tell it to Debug.Start via PowerShell.
     If VS is closed, launch it and run Debug.Start."
    (interactive)
    (if-let* ((root (projectile-project-root))
              (solutions (directory-files root nil "\\.sln\\'"))
              (solution (car solutions))
              (path (expand-file-name solution root))
              ;; Visual Studio expects backslashes for path comparison
              (win-path (replace-regexp-in-string "/" "\\\\" path)))
        
        ;; BRANCH A: Process is already running
        (if (process-live-p edu/vs-process)
            (progn
              (message "Visual Studio is open. Sending 'Debug.Start' signal...")
              ;; We call PowerShell to find the DTE object for this solution and press F5
              (call-process "powershell" nil nil nil
                            "-NoProfile" "-Command"
                            (concat
                             "$target = '" win-path "';"
                             "try {"
                             ;; Grab the active VS instance
                             "  $dte = [System.Runtime.InteropServices.Marshal]::GetActiveObject('VisualStudio.DTE');"
                             ;; Check if it holds our specific solution
                             "  if ($dte.Solution.FullName -eq $target) {"
                             "    $dte.MainWindow.Activate();"
                             "    $dte.ExecuteCommand('Debug.Start');"
                             "  } else {"
                             "    Write-Host 'Open VS instance is looking at a different solution.';"
                             "  }"
                             "}"
                             "catch { Write-Host 'Could not connect to VS COM interface.'; }")))

          ;; BRANCH B: Process is dead or nil -> Launch new
          (let ((default-directory root))
            (message "Launching Visual Studio for %s..." solution)
            (setq edu/vs-process
                  (start-process "visual-studio" nil 
                                 "devenv" 
                                 path 
                                 "/Command" 
                                 "Debug.Start"))))
      
      (user-error "No .sln found in project root")))

  (map! :leader (:prefix "p" :desc "Visual Studio Debug" "v" #'edu/debug-and-runin-visualstudio)))

;; Ctrl V will open Search Result in Other Window
(defun +vertico/open-candidate-other-window (candidate)
  "Opens the candidate in another window, supporting Consult metadata."
  (interactive "sCandidate: ")
  ;; 1. Try to get the real location from Consult properties (if available)
  (let ((cons (get-text-property 0 'consult-location candidate)))
    (if cons
        ;; If it's a consult candidate (grep/ripgrep/line), use its internal jump logic
        (let ((current-prefix-arg t)) ;; Emulate C-u to force other-window logic if supported
           ;; We manually split and jump because consult-jump usually reuses window
           (consult--jump cons))
      
      ;; 2. Fallback to your Regex logic for simple file paths
      (let* ((clean-cand (substring-no-properties candidate))
             (file clean-cand)
             (line nil))
        (when (string-match "^\\([^:]+\\):\\([0-9]+\\):" clean-cand)
          (setq file (match-string 1 clean-cand))
          (setq line (string-to-number (match-string 2 clean-cand))))
        
        (find-file-other-window (expand-file-name file))
        (when line
          (goto-char (point-min))
          (forward-line (1- line))
          (recenter)
          (pulse-momentary-highlight-one-line (point)))))))

;; 2. The Trigger: Ensure Embark is ready before binding the variable
(defun +vertico/trigger-open-other-window ()
  "Trigger embark to act on the current candidate with our custom function."
  (interactive)
  ;; FIX: Ensure embark is loaded so 'embark-prompter' is recognized as a dynamic variable
  (require 'embark)
  (let ((embark-prompter (lambda (&rest _) #'+vertico/open-candidate-other-window)))
    (embark-act)))

;; Projectile Recursive Discovery to include Submodules
(setq projectile-auto-discover t)

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


(after! git-timemachine
  (evil-define-minor-mode-key 'normal 'git-timemachine-mode
    [escape] #'git-timemachine-quit))

;; Disable the DOOM default S key behavior in Normal mode (snipe)
;; (remove-hook 'doom-first-input-hook #'evil-snipe-mode)
;; (after! evil-snipe
;;   (evil-snipe-mode -1)
;;   (evil-snipe-override-mode -1))


;; Keeps Visual Selection after evil-indent '='
(defun keep-visual-after-indent()
  (interactive)
  (setq x evil-visual-beginning)
  (setq y evil-visual-end)
  (evil-indent x y)
  (evil-visual-make-region x (- y 1)))

(map! :v "=" #'keep-visual-after-indent)

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
