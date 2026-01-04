;; Projectile Recursive Discovery to include Submodules
(setq projectile-auto-discover t)


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
