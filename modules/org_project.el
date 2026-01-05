(setq org-project-capture-projects-file "~/.doom.d/org/projects.org")

(map! :leader :desc "Capture Project TODO" "p t" #'org-project-capture-project-todo-completing-read)
