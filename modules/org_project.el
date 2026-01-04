(setq org-project-capture-default-backend
  (make-instance 'org-project-capture-project-backend))

(setq org-project-capture-projects-file "~/.doom.d/org/projects.org")

(setq org-project-capture-strategy
      (make-instance 'org-project-capture-combine-strategies
		     :strategies (list (make-instance 'org-project-capture-single-file-strategy)
				       (make-instance 'org-project-capture-per-project-strategy))))


(map! :leader :desc "Capture Project TODO" "p t" #'org-project-capture-project-todo-completing-read)
