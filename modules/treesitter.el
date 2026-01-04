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
