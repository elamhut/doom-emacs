;; Magit Stage all Modified and Commit with message
(defun my-magit-stage-all-and-commit (message)
  (interactive "sCommit message: ")
  (magit-stage-modified)
  (magit-commit-create `("-m" ,message)))

