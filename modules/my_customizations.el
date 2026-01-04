;; Force Evil Normal State after jumping to a bookmark.
(defun edu/force-normal-after-bookmark (&rest _)
  (when (evil-visual-state-p)
    (evil-normal-state)))

(advice-add #'bookmark-jump :after #'edu/force-normal-after-bookmark)


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



;; Jump to next {}!
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
(map! :n ")"  #'edu/jump-to-next-brace
      :n "("  #'edu/jump-to-prev-brace)


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


;; Keeps Visual Selection after evil-indent '='
(defun keep-visual-after-indent()
  (interactive)
  (setq x evil-visual-beginning)
  (setq y evil-visual-end)
  (evil-indent x y)
  (evil-visual-make-region x (- y 1)))

(map! :v "=" #'keep-visual-after-indent)
