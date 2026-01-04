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
