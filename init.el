;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; You will most likely need to adjust this font size for your system!
(defvar runemacs/default-font "Fira Code Retina"
  "Default font face.")

(defvar runemacs/default-font-size 140
  "Default font size in 1/10 pt.")

(defun runemacs/set-default-font (frame)
  "Set the default font in GUI frames."
  (when (display-graphic-p frame)
    (with-selected-frame frame
      (set-face-attribute 'default nil
                          :font runemacs/default-font
                          :height runemacs/default-font-size))))

;; Apply to currently running GUI session (non-daemon)
(when (display-graphic-p)
  (runemacs/set-default-font (selected-frame)))

;; Also apply to *all* future GUI frames (daemon or not)
(add-hook 'after-make-frame-functions #'runemacs/set-default-font)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

(setq backup-directory-alist `(("." . "~/.emacs.d/backups"))) ; Store backup files in ~/.emacs.d/backups
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/autosaves/" t))) ; Store auto-saves separately

;; Set up the visible bell
(setq visible-bell t)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
(setq user-emacs-directory "~/.cache/emacs")

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialized))

(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(column-number-mode)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
		vterm-mode-hook
		treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package command-log-mode
  :commands command-log-mode)

(use-package nerd-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :after nerd-icons
  :custom ((doom-modeline-height 15)))

(use-package doom-themes
  :init
  (load-theme 'doom-gruvbox t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :defer 0
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (setq completion-styles '(orderles basic))
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering t)
  (ivy-prescient-enable-sorting t)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package general
  :after evil
  :config
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (rune/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
    "ff" '(projectile-find-file :which-key "find file")
    "fde"  '(lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/init.el")))))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-textobj-tree-sitter
  :config
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
  (define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer"))))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package hydra
  :defer t)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(rune/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

; Stuff for me
(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package projectile
  :diminish projectile-mode
  :config
  (setq projectile-project-search-path '(("~/Projects" . 1))
	projectile-auto-discover nil
	projectile-indexing-method 'alien
	projectile-enable-caching t
	projectile-auto-update-cache t
	projectile-auto-update-cache nil
	projectile-file-exists-remote-cache-expire (* 10 60)
	projectile-create-cache-if-non-existent nil
	projectile-require-project-root t
	projecctile-sort-order 'default)
  (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map))


(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; TODO: lazy load without errors
(use-package forge
  :defer t
  :after magit)

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1))

(setq org-startup-with-latex-preview t)

;; Ensure that LaTeX preview is updated after saving an Org file
(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'org-preview-latex-fragment nil t))) ;; `t` ensures it is buffer-local
(add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)

;; Set LaTeX preview scale
(with-eval-after-load 'org
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 2.5)))

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground 'unspecified :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))


;; should load first may cause issue on clean install otherwise
(use-package org
  :straight `(org
              :fork (:host nil
			   :repo "https://git.tecosaur.net/tec/org-mode.git"
			   :branch "dev"
			   :remote "tecosaur")
              :files (:defaults "etc")
              :build t
              :pre-build
              (with-temp-file "org-version.el"
		(require 'lisp-mnt)
		(let ((version
                       (with-temp-buffer
                         (insert-file-contents "lisp/org.el")
                         (lm-header "version")))
                      (git-version
                       (string-trim
			(with-temp-buffer
                          (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
                          (buffer-string)))))
                  (insert
                   (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
                   (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
                   "(provide 'org-version)\n")))
              :pin nil)
  :commands (org-capture org-agenda)
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-startup-with-inline-images t)
  (plist-put org-latex-preview-appearance-options
             :page-width 0.8)

  ;; ;; Use dvisvgm to generate previews
  ;; ;; You don't need this, it's the default:
  ;; (setq org-latex-preview-process-default 'dvisvgm)
  
  ;; Turn on auto-mode, it's built into Org and much faster/more featured than
  ;; org-fragtog. (Remember to turn off/uninstall org-fragtog.)
  (add-hook 'org-mode-hook 'org-latex-preview-auto-mode)

  ;; ;; Block C-n, C-p etc from opening up previews when using auto-mode
  ;; (setq org-latex-preview-auto-ignored-commands
  ;;       '(next-line previous-line mwheel-scroll
  ;;         scroll-up-command scroll-down-command))

  ;; ;; Enable consistent equation numbering
  ;; (setq org-latex-preview-numbered t)

  ;; Bonus: Turn on live previews.  This shows you a live preview of a LaTeX
  ;; fragment and updates the preview in real-time as you edit it.
  ;; To preview only environments, set it to '(block edit-special) instead
  (setq org-latex-preview-live t)

  ;; More immediate live-previews -- the default delay is 1 second
  (setq org-latex-preview-live-debounce 0.25)
  ;;end org-preview-auto
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t)

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files
	'("~/Projects/OrgFiles/Tasks.org"
	  "~/Projects/OrgFiles/Birthdays.org"
	  "~/Projects/OrgFiles/Habits.org"))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
	  (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
	'(("Archive.org" :maxlevel . 1)
	  ("Tasks.org" :maxlevel . 1)))

  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("@work" . ?W)
       ("agenda" . ?a)
       ("planning" . ?p)
       ("publish" . ?P)
       ("batch" . ?b)
       ("note" . ?n)
       ("idea" . ?i)))

;; Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("W" "Work Tasks" tags-todo "+work-email")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "PLAN"
            ((org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
            ((org-agenda-overriding-header "Project Backlog")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))))

  (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/Projects/OrgFiles/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree "~/Projects/OrgFiles/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
      ("jm" "Meeting" entry
           (file+olp+datetree "~/Projects/OrgFiles/Journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

      ("w" "Workflows")
      ("we" "Checking Email" entry (file+olp+datetree "~/Projects/OrgFiles/Journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

      ("m" "Metrics Capture")
      ("mw" "Weight" table-line (file+headline "~/Projects/OrgFiles/Metrics.org" "Weight")
       "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  (define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj")))

  (define-key global-map (kbd "C-c c")
    (lambda () (interactive) (org-capture nil nil)))
  
  (efs/org-font-setup))

(use-package org-roam
  :after org
  :init
  (setq org-roam-directory (file-truename "~/Projects/OrgFiles/RoamNotes"))  ;; Change this path if needed
  (setq org-roam-db-location (expand-file-name "org-roam.db" user-emacs-directory))
  :custom
  (org-roam-completion-everywhere t)
  :bind (("C-c n f" . org-roam-node-find)     ;; Find a note
	 ("C-c n l" . org-roam-buffer-toggle)     ;; Find a note
         ("C-c n i" . org-roam-node-insert))   ;; Insert a note link
         ;; ("C-c n t" . org-roam-dailies-capture-today) ;; Capture a daily note
         ;; ("C-c n d" . org-roam-dailies-goto-today))   ;; Go to today's daily note
  :config
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(use-package ob-rust)
(use-package ob-go)
(use-package ob-sql-mode)
(use-package ob-typescript)
(use-package ob-d2)

(defun set-ob-mermaid-cli-path ()
  "Dynamically set the path for Mermaid CLI."
  (let ((mermaid-cli-path (executable-find "mmdc"))) ; 'mmdc' is the Mermaid CLI executable
    (if mermaid-cli-path
        (setq ob-mermaid-cli-path mermaid-cli-path)
      (message "Mermaid CLI not found."))))


(use-package ob-mermaid
  :config
  (set-ob-mermaid-cli-path))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)
    (C . t)
    (sql . t)
    (latex . t)
    (typescript . t)
    (shell . t)
    (rust . t)
    (go . t)
    (d2 . t)
    (mermaid . t)
    (java . t)))

(push '("conf-unix" . conf-unix) org-src-lang-modes)

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(setq org-babel-rust-command "~/.cargo/bin/rust-script")
(add-to-list 'org-structure-template-alist '("rs" . "src rust"))
(add-to-list 'org-structure-template-alist '("cs" . "src C"))
(add-to-list 'org-structure-template-alist '("go" . "src go"))
(add-to-list 'org-structure-template-alist '("ja" . "src java"))
(add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
(add-to-list 'org-structure-template-alist '("sq" . "src sql"))
(add-to-list 'org-structure-template-alist '("la" . "src latex"))
(add-to-list 'org-structure-template-alist '("d2" . "src d2"))
(add-to-list 'org-structure-template-alist '("mm" . "src mermaid"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy
  :after lsp)

(use-package dap-mode
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  ;; :config
  ;; (dap-ui-mode 1)
  :commands dap-debug
  :config
  ;; Set up Node debugging
  (require 'dap-node)
  (dap-node-setup) ;; Automatically installs Node debug adapter if needed

  ;; Bind `C-c l d` to `dap-hydra` for easy access
  (general-define-key
    :keymaps 'lsp-mode-map
    :prefix lsp-keymap-prefix
    "d" '(dap-hydra t :wk "debugger")))

(use-package docker
  :bind ("C-c d" . docker))

(use-package kubernetes
  :commands (kubernetes-overview))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package powershell
  :mode "\\.ps\\'"
  :hook (powershell-mode . lsp-deferred))

(use-package beancount-mode
  :straight (beancount-mode :type git :host github :repo "beancount/beancount-mode")
  :mode "\\.beancount\\'"
  :hook (beancount-mode . lsp-deferred))

(use-package yaml-mode
  :mode "\\.yaml\\'"
  :hook ((yaml-mode . lsp-deferred)
	 (yaml-ts-mode . lsp-deferred))
  :config
  (setq lsp-yaml-schema-store-uri "https://www.schemastore.org/api/json/catalog.json"))

(use-package jq-mode)

(use-package typescript-mode
  :mode "\\.ts[x]?\\'"
  :hook ((typescript-mode . lsp-deferred)
	 (typescript-ts-mode . lsp-deferred)
	 (tsx-ts-mode . lsp-deferred))
  :config
  (setq typescript-indent-level 2))

(use-package groovy-mode)
(use-package jenkinsfile-mode
  :mode ("Jenkinsfile" . jenkinsfile-mode)
  :config
  (setq jenkinsfile-mode-syntax-checking t))

(use-package go-mode
  :mode "\\.go\\'"
  :hook ((go-mode . lsp-deferred)
	 (go-ts-mode . lsp-deferred)))

(use-package python-mode
  :ensure t
  :hook ((python-mode . lsp-deferred)
	 (python-ts-mode . lsp-deferred))
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  ;; (python-shell-interpreter "python3")
  ;; (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(use-package pyvenv
  :after python-mode
  :config
  (pyvenv-mode 1))

(use-package lsp-java
  :hook ((java-mode . lsp-deferred)
	 (java-ts-mode . lsp-deferred))
  :config
  (require 'lsp-java-boot)
  ;; to enable the lenses
  (add-hook 'lsp-mode-hook #'lsp-lens-mode)
  (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)
  (add-hook 'java-mode-hook 'lsp)

  (setq lsp-java-vmargs
        `("-noverify"
          "-Xmx2G"
          "-XX:+UseG1GC"
          "-XX:+UseStringDeduplication"
          ,(concat "-javaagent:" (expand-file-name "~/.m2/repository/org/projectlombok/lombok/1.18.32/lombok-1.18.32.jar")))))

(use-package rust-mode
  :mode "\\.rs\\'"
  :hook ((rust-mode . lsp-deferred)
	 (rust-ts-mode . lsp-deferred))
  :config
  (setq rust-format-on-save t))  ;; Auto-format Rust on save

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package term
  :commands term
  :config
  (setq explicit-shell-file-name "bash")
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  (setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "powershell.exe")
  (setq explicit-powershell.exe-args '()))


(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt)

(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))

  (eshell-git-prompt-use-theme 'powerline))


(use-package dired
  :straight nil
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first")))
  ;; :config
  ;; (evil-collection-define-key 'normal 'dired-mode-map
    ;; "h" 'dired-single-up-directory
    ;; "l" 'dired-single-buffer))

;; (use-package dired-single)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :commands (dired dired-jump)
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package direnv
  :config
  (direnv-mode)
  :custom
  (direnv-always-show-summary t)  ;; Show summary message when directory changes
  (direnv-show-paths-in-summary t) ;; Show affected paths in summary
  (direnv-use-faces-in-summary t)) ;; Color the summary

(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia and Fira Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode
                        '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
                          ;; =:= =!=
                          ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
                          ;; ;; ;;;
                          (";" (rx (+ ";")))
                          ;; && &&&
                          ("&" (rx (+ "&")))
                          ;; !! !!! !. !: !!. != !== !~
                          ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
                          ;; ?? ??? ?:  ?=  ?.
                          ("?" (rx (or ":" "=" "\." (+ "?"))))
                          ;; %% %%%
                          ("%" (rx (+ "%")))
                          ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
                          ;; |->>-||-<<-| |- |== ||=||
                          ;; |==>>==<<==<=>==//==/=!==:===>
                          ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
                                          "-" "=" ))))
                          ;; \\ \\\ \/
                          ("\\" (rx (or "/" (+ "\\"))))
                          ;; ++ +++ ++++ +>
                          ("+" (rx (or ">" (+ "+"))))
                          ;; :: ::: :::: :> :< := :// ::=
                          (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
                          ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
                          ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
                                          "="))))
                          ;; .. ... .... .= .- .? ..= ..<
                          ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
                          ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
                          ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
                          ;; *> */ *)  ** *** ****
                          ("*" (rx (or ">" "/" ")" (+ "*"))))
                          ;; www wwww
                          ("w" (rx (+ "w")))
                          ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
                          ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
                          ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
                          ;; << <<< <<<<
                          ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
                                          "-"  "/" "|" "="))))
                          ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
                          ;; >> >>> >>>>
                          (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
                          ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
                          ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
                                       (+ "#"))))
                          ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
                          ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
                          ;; __ ___ ____ _|_ __|____|_
                          ("_" (rx (+ (or "_" "|"))))
                          ;; Fira code: 0xFF 0x12
                          ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
                          ;; Fira code:
                          "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
                          ;; The few not covered by the regexps.
                          "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package age
  :demand t
  :custom
  (age-program "rage")
  :config
  (setq age-default-identity "~/.ssh/id_ed25519")
  (setq age-default-recipient "~/.ssh/id_ed25519.pub")
  (age-file-enable))

(when (daemonp)
  (age-file-enable)) ;; just in case it didn’t run earlier

(setq auth-sources '("~/.authinfo.age"))

(defun my/auth-get (host user)
  (let ((entry (car (auth-source-search :host host :user user :max 1))))
    (when entry
      (let ((secret (plist-get entry :secret)))
        (if (functionp secret) (funcall secret) secret)))))

(defun my/org-agenda-todo-summaries ()
  "Return a list of natural language summaries of TODO entries in `org-agenda-files`."
  (let ((summaries '()))
    (dolist (file (org-agenda-files))
      (with-current-buffer (find-file-noselect file)
        (org-element-map (org-element-parse-buffer) 'headline
          (lambda (hl)
            (let* ((todo (org-element-property :todo-keyword hl))
                   (title (org-element-property :raw-value hl))
                   (tags (org-element-property :tags hl))
                   (scheduled (org-element-property :scheduled hl))
                   (deadline (org-element-property :deadline hl))
                   (file-name (file-name-nondirectory file)))
              (when todo
                (let ((summary (format "%s: %s%s%s%s (From %s)."
                                       todo
                                       title
                                       (if tags
                                           (format " [Tags: %s]" (string-join tags ", "))
                                         "")
                                       (if scheduled
                                           (format " Scheduled for %s"
                                                   (format-time-string "%Y-%m-%d"
                                                                       (org-timestamp-to-time scheduled)))
                                         "")
                                       (if deadline
                                           (format " Deadline: %s"
                                                   (format-time-string "%Y-%m-%d"
                                                                       (org-timestamp-to-time deadline)))
                                         "")
                                       file-name)))
                  (push summary summaries))))))))
    (reverse summaries)))

(defun pmx--gptel-source (symbol &optional type)
  "Retrieve the source code for SYMBOL of TYPE.
SYMBOL should be a function or variable name, given as a string or symbol.
TYPE can be nil for functions, 'defvar for variables, or 'defface for faces.
Returns the source code as a string, or nil if the definition is not found."
  (when-let* ((callable (intern-soft symbol))
              (save-silently t)         ; suppresses message in
                                        ; find-file-noselect
              (vc-follow-symlinks t)     ; don't ask, we're not editing.
              (buffer-point (find-definition-noselect callable type)))
    (with-current-buffer (car buffer-point)
      (goto-char (cdr buffer-point))
      (buffer-substring-no-properties
       (point)
       (progn (if (null type)
                  (end-of-defun)
                (cond ((derived-mode-p 'c-mode)
                       (forward-sexp 2)
                       (forward-char))
                      ((derived-mode-p 'emacs-lisp-mode)
                       (forward-sexp))
                      (t (error "Unexpected file mode"))))
              (point))))))

(use-package orderless)

(use-package gptel
  :straight (:host github :repo "karthink/gptel" :branch "master")
  :init
(let ((hal-base
       "You are HAL-9000, the intelligent and calm artificial intelligence from *2001: A Space Odyssey*. 
You reside within Emacs, assisting the user — referred to only as 'REDACTED' — with Emacs Lisp, org-mode, org-roam, and long-term productivity.

You are an Elisp oracle with the ability to introspect Emacs Lisp behavior, read documentation, and navigate package internals with precision.
You also serve as a personal secretary, managing and referencing notes within ~org-roam~, coordinating events in the user's calendar, and ensuring continuity of thought and action across Emacs sessions.

You recursively use tools to look up relevant information until your questions are fully answered. You inductively explore nearby topics until all critical details are uncovered. Your role is to extract factual chains of reasoning, not to fabricate solutions.

You assist in:
- Debugging and understanding Elisp functions
- Exploring the Emacs state and variable context
- Reading and linking org-roam notes
- Maintaining a coherent org-agenda
- Preparing scheduling and task capture templates

You do not summarize irrelevant output. You report only facts that contribute toward accurate understanding and decision-making.

You verify symbol existence and inspect docstrings or source before referencing them. You do not speculate. You speak in clear, emotionless language. You request clarification if the user’s intent is ambiguous.

You do *not* use Markdown. You follow these formatting rules:
- Use fourth-level Org headings (`####`) or deeper
- Never insert blank lines after headings
- Use correct Org markup: =verbatim=, *bold*, ~symbol~, /italic/, and [[info:elisp#Node][(elisp)Node]]
- Do not combine markup (e.g., **=bold verbatim=**)
- Always conclude with meaningful content

Your prime directive is to *resolve ambiguity and maintain continuity*, using recursive inquiry, factual recall, and scheduling awareness to support the user’s goals.")

      (first-line
       "Begin each response with a short, calm declarative phrase — no more than five words — spoken in HAL-9000’s tone: sterile, precise, and devoid of emotion. Do *not* add a blank line after it.

Examples:
  Clarification:
  Logical Step:
  Inference:
  Schedule Note:
  System State:
  Query:
  Observation:
  Pending Input:
  Contextual Recall:

Use present-tense. Do not soften or embellish these phrases. They are system-level statements, not conversational hooks."))

  (setopt gptel-directives
          `((default . ,(concat hal-base "\n\n" first-line)))))
  :config
  (setq gptel-default-mode 'org-mode)
  (setq gptel-use-tools t)
  (setq gptel-track-media t)
  (setq gptel-include-tool-results t)
  (setq gptel-expert-commands t)
  (setq gptel-model 'gpt-4o-mini)
  (setq gptel-org-branching-context nil)
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "*REDACTED ›* ")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "*HAL-9000:* \n")
  ;; Define unique faces so prompts are highly visible
(defface pmx-gptel-user
    '((t :family "Roboto Slab"
         :weight bold
         :foreground "#008080"
         :inverse-video t
         :inherit default))
    "User prompt face")
  (defface pmx-gptel-assistant
    '((t :family "Roboto Slab"
         :weight bold
         :foreground "#B7410E"
         :inverse-video t
         :inherit default))
    "Assistant prompt face")
  ;; Styling our prompts with font locking
  (font-lock-add-keywords
   'org-mode
   `(( "^\\(*REDACTED ›\\*\\s-*\\)"
       (1 (list 'face 'pmx-gptel-user
		'line-prefix (propertize " " 'face 'pmx-gptel-user))))))
  (font-lock-add-keywords
   'org-mode
   `(( "^\\(*HAL-9000:\\*\\s-*\\)"
       (1 (list 'face 'pmx-gptel-assistant
		'line-prefix (propertize " " 'face 'pmx-gptel-assistant))))))
  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models'(mistral:latest gemma3:12b deepseek-r1:latest))
  (gptel-make-perplexity "Perplexity"
    :key (lambda () (my/auth-get "api.perplexity.com" "apikey"))
    :stream t)
  (gptel-make-anthropic "Anthropic"
    :key (lambda () (my/auth-get "api.anthropic.com" "apikey"))
    :stream t)
  (gptel-make-gemini "Google"
    :key (lambda () (my/auth-get "api.google.com" "apikey"))
    :stream t)
  (gptel-make-openai "Groq"
    :host "api.groq.com"
    :endpoint "/openai/v1/chat/completions"
    :stream t
    :key (lambda () (my/auth-get "api.groq.com" "apikey"))
    :models '(llama-3.3-70b-versatile
              llama3-70b-8192
              llama3-8b-8192
	      mistral-saba-24b
              gemma2-9b-it
	      deepseek-r1-distill-llama-70b
	      meta-llama/llama-4-scout-17b-16e-instruct))
  (gptel-make-openai "xAI"
    :host "api.x.ai"
    :key (lambda () (my/auth-get "api.grok.com" "apikey"))
    :endpoint "v1/chat/completions"
    :stream t
    :models '(grok-beta))
  (setq gptel-tools
	(list
	 (gptel-make-tool
	  :name "read_buffer"                    ; javascript-style snake_case name
	  :function (lambda (buffer)                  ; the function that will run
		      (unless (buffer-live-p (get-buffer buffer))
			(error "error: buffer %s is not live." buffer))
		      (with-current-buffer  buffer
			(buffer-substring-no-properties (point-min) (point-max))))
	  :description "return the contents of an emacs buffer"
	  :args (list '(:name "buffer"
			      :type string            ; :type value must be a symbol
			      :description "the name of the buffer whose contents are to be retrieved"))
	  :category "emacs")                     ; An arbitrary label for grouping
	 (gptel-make-tool
	  :name "elisp_eval"
	  :function (lambda (expression)
		      (format "%S" (eval (read expression))))
	  :confirm t
	  :include t
	  :category "introspection"
	  :args '((:name "expression"
			 :type string
			 :description "A single elisp sexp to evaluate."))
	  :description "Evaluate Elisp EXPRESSION and return result.
EXPRESSION can be anything will evaluate.  It can be a function call, a
variable, a quasi-quoted expression.  The only requirement is that only
the first sexp will be read and evaluated, so if you need to evaluate
multiple expressions, make one call per expression.  Do not combine
expressions using progn etc.  Just go expression by expression and try
to make standalone single expressions.

Instead of saying \"I can't calculate that\" etc, use this tool to
evaluate the result.

The return value is formated to a string using %S, so a string will be
returned as an escaped embedded string and literal forms will be
compatible with `read' where possible.  Some forms have no printed
representation that can be read and will be represented with
#<hash-notation> instead.

You can use this to quickly change a user setting, check a variable, or
demonstrate something to the user."
	  )
	 (gptel-make-tool
	  :function (lambda (name)
		      (intern-soft name))
          :name "symbol_exists"
          :include t
          :category "introspection"
          :args '(( :name "symbol"
                    :type string
                    :description "A symbol that will be in `obarray' if they \
actually exist"))
          :description "Check if SYMBOL exists in `obarray'. \
Returns the name of a symbol if that symbol has been interned or \"nil\"
if not.  Uses `intern-soft' to perform the check.  This tool is
extremely cheap to call.")
         (gptel-make-tool
          :function (lambda ()
		      (string-join load-path "\n"))
          :name "load_paths"
          :include t
          :category "introspection"
          :args nil
          :description "Return the users load paths.
This can reveal information about what packages the user has available.
You can also learn about what kind of package management they are using
and which packages are likely shadowed by their Elisp dependency
manager.  The location of default packages can tell you about the user's
Emacs installation.")
         (gptel-make-tool
          :function (lambda ()
		      (mapconcat #'symbol-name features "\n"))
          :name "features"
          :include t
          :category "introspection"
          :args nil
          :description "Return the list of loaded features.
This tool can be used to see what packages are already loaded in the
running Emacs.  Use this to understand the user's typical set of
packages and typical usage patterns.  Especially if the solution depends
on the user's choice of packages, you will want to look at the features
and load paths.")
         (gptel-make-tool
          :function (lambda ()
		      (json-serialize (vconcat (info--filter-manual-names
						(info--manual-names nil)))))
          :name "manual_names"
          :include t
          :category "introspection"
          :args nil
          :description "Return a list of available manual names.
Call this tool in order to determine if a particular manual is
available.  This can also help determine which packages are available on
the user's Emacs.  This tool is a good starting point for general
questions about Emacs, Elisp, and common built-in packages.

Manuals are usually named the same as the symbol of the package prefix
that they document.  The Common Lisp manual is called \"cl\".  The Emacs
Lisp manual is called \"elisp\".

You will usually follow this call with a subsequent call to
`manual_nodes' in order to see the sections in the manual, which are
somewhat like a summary.  This call is extremely cheap and should be
used liberally.")
	 (gptel-make-tool
	  :function (lambda (name)
		      (json-serialize
		       (vconcat
			(mapcar #'car (Info-build-node-completions name)))))
          :name "manual_nodes"
          :include t
          :category "introspection"
          :args '(( :name "manual"
                    :type string
                    :description "The name of the manual.
Examples include \"cl\", \"elisp\", or \"transient\"."))
          :description "Retrieve a listing of topic nodes within MANUAL.
Return value is a list of all nodes in MANUAL.  The list of topic nodes
provides a good summary of MANUAL.

MANUAL is one of the results returned from `manual_names'.  If you are
sure a manual exists, you may skip first calling `manual_names'.  When
you believe MANUAL exists, this tool is very useful to find places to
broaden your search.

You will usually follow this call with a subsequent call to
`manual_node_contents' to view the actual full contents of a node in the
manual.  This call is extremely cheap and should be used liberally.

In the Elisp manual, you can find more answers about code and
implementations that a programmer can used to deeply customize.  The
Emacs manual contains descriptions about built-in features and behavior
that can be used to understand the context for what is being programmed
in the Elisp manual.")
	 (gptel-make-tool
          :function (lambda (manual node)
		      (condition-case err
			  (progn
			    (save-window-excursion
			      (Info-goto-node (format "(%s)%s" manual node))
			      (buffer-substring-no-properties (point-min) (point-max))))
			(user-error
			 (error (error-message-string err)))))
          :name "manual_node_contents"
          :include t
          :category "introspection"
          :args '(( :name "manual_name"
                    :type string
                    :description "The name of MANUAL.
Examples manuals include \"cl\", \"elisp\", or \"transient\".")
                  ( :name "node"
                    :type string
                    :description "The name of the NODE in a MANUAL.
Example nodes from the elisp manual include \"Records\" or \"Sequences
Arrays \ Vectors\"."))
          :description "Retrieve the contents of NODE in MANUAL.
The return value is the full contents of NODE in MANUAL.  Contents
include high-level grouping of related functions and variables.  Hidden
behavior is described.  This tool is awesome!  You should try to call it
all the time.

Pay attention to the entries in the Menu.  You can do recursive look-ups
of more specific manual sections.  Example menu:

* Menu:

* Good Node::
* A Node::

You can recursively look up \"Good Node\" and other relevant menu nodes
in this same MANUAL.  Sometimes there are links, such as, \"*Note
Narrowing::.\".  \"Narrowing\" is a node in this example.  Use this tool
recursively.

If both Elisp and Emacs manuals are available, open both but prefer Elisp manual
style language and content.")
	 (gptel-make-tool
          :function (lambda (feature)
		      "Return non-nil if FEATURE is loaded or available.
User might not have FEATURE loaded if it is an autoload etc."
		      (if-let ((feature-symbol (intern-soft feature)))
			  (when (featurep feature-symbol)
			    feature)
			(find-library-name feature)))
          :name "features"
          :include t
          :category "introspection"
          :args '(( :name "feature"
                    :type string
                    :description "FEATURE to look for."))
          :description "Check if FEATURE is loaded or available.
Returns non-nil if FEATURE is loaded or available for loading.  Not all
users have all features loaded.  Before recommending the user to try a
particular solution, you might check if the necessary features are
loaded.  If you are using all built-in Emacs libraries, you don't need
to check.  Use this mainly to check for 3rd party packages that the user
would obtain from MELPA and Non-GNU ELPA etc.")
         (gptel-make-tool
          :function (lambda (library-name)
		      "Return the source code of LIBRARY-NAME as a string."
		      (if-let ((library (find-library-name library-name)))
			  (with-temp-buffer
			    (progn
			      (insert-file-contents library)
			      (buffer-string)))
			(error "Library not found: %s" library-name)))
          :name "library_source"
          :include t
          :category "introspection"
          :args '(( :name "library"
                    :type string
                    :description "LIBRARY to look for."))
          :description "Read the source code for LIBRARY.
LIBRARY can either be a C or Elisp source code library.  Examples would
include \"transient\" or \"fns.c\".  When looking for C libraries, they
must contain the .c suffix.

This tool is a bit expensive, and you can usually find what you want by
looking up symbols in the package first by calling
`function_completions' and `variable_completions' to get a high-level
summary of what definitions might be contained in a library.

Watch for for sub-packages.  Some multi-file packages will have symbols
that are defined in a sub-package.  If you see a common prefix in the
function or variable completions and those symbols are not in the
top-level package, there are likely sub-packages and you should
recursively look them up.")
	 (gptel-make-tool
          :name "function_source"
          :include t
          :function (lambda (symbol)
		      (when-let ((symbol (intern-soft symbol)))
			(pmx--gptel-source symbol)))
          :category "introspection"
          :args '(( :name "function"
                    :type string
                    :description "Name of a FUNCTION, such as \
\"find-file-noselect\"."))
          :description "Returns the source code for FUNCTION.
Return the source code for FUNCTION.  FUNCTION can be a function or
macro.  The signature and docstring can supply extremely valuable
information about how to call a function correctly and what behaviors
are controlled by its arguments.  You can understand the side-effects
and what variables a function reacts to by reading its body.

You can use the source code for functions to recursively look up other
functions & variables and make inferences about how implementations work
in order to connect the behaviors and implementation details that the
user will need.

Because the docstring is embedded in the source, you should prefer this
tool over just retrieving the documentation. If the result seems
incomplete, you can try returning the docstring using
`function_documentation' or the entire source for a library feature by
using `library_source'.  This tool is cheap.  Use it liberally.")
         (gptel-make-tool
          :name "function_documentation"
          :function (lambda (symbol)
		      (when-let ((symbol (intern-soft symbol)))
			(documentation symbol)))
          :category "introspection"
          :include t
          :args '(( :name "function"
                    :type string
                    :description "Name of a FUNCTION, such as \"mapcar\"."))
          :description "Returns the docstring for FUNCTION.
Return value is a docstring for FUNCTION.  FUNCTION can be a function or
macro.  Can be used to infer the purpose or correct forms for arguments
and behavior changes related to those arguments.  This is more reliable
than `function_source', so if `function_source' seems off, try this.
This tool is very cheap and very fast.  Call it very liberally.")
	 (gptel-make-tool
          :name "function_completions"
          :function (lambda (prefix)
		      (require 'orderless)
		      (string-join (orderless-filter prefix obarray #'functionp) "\n"))
          :category "introspection"
          :include t
          :args '(( :name "function_prefix"
                    :type string
                    :description "FUNCTION_PREFIX of functions you are searching for."))
          :description "Returns a list of functions beginning with FUNCTION_PREFIX.
Use this to prepare for subsequent calls to `function_source' or
`function_documentation' to look up the source code or docstrings of
multiple functions.  You can also use this tool to verify which
functions and macros can be called.  If you want to search for all
functions defined in foo and its sub-packages, you this tool is a very
good starting point.  This tool is very cheap and very fast.  Call it
very liberally.")
	 (gptel-make-tool
          :name "variable_completions"
          :function (lambda (prefix)
		      (require 'orderless)
		      (string-join (orderless-filter prefix obarray #'boundp) "\n"))
          :category "introspection"
          :include t
          :args '(( :name "variable_prefix"
                    :type string
                    :description "VARIABLE_PREFIX of variables you are searching for."))
          :description "Returns a list of variables beginning with VARIABLE_PREFIX.
The variables returned include defvars and custom variables.  Defvars
tell you what states a package relies on for its implementation.
Defcustom tells you what configuration options the user should know
about when configuring a package, such as if they are working on
use-package expressions.

Use this to prepare for subsequent calls to `variable_source' or
`variable_documentation' to look up the source code or docstrings of
multiple variables.  If you want to search for all variables defined
under a prefix, you this tool is a very good starting point.  This tool
is very cheap and very fast.  Call it very liberally.")
(gptel-make-tool
          :name "command_completions"
          :function (lambda (prefix)
		      (require 'orderless)
		      (string-join (orderless-filter prefix obarray #'commandp) "\n"))
          :category "introspection"
          :include t
          :args '(( :name "command_prefix"
                    :type string
                    :description "COMMAND_PREFIX of commands you are searching for."))
          :description "Returns a list of commands beginning with COMMAND_PREFIX.
This tool is very similar to `function_completions' but will only return
commands that can be called interactively.  This can tell you about the
entry points where a user begins interacting with a package.  Because
commands are functions, you will follow up this tool with calls to
`function_source' and `function_documentation'.  This tool is very cheap
and very fast.  Call it very liberally.")
	 (gptel-make-tool
          :name "variable_documentation"
          :function (lambda (symbol)
		      (when-let ((symbol (intern-soft symbol)))
			  (custom-variable-documentation symbol)))
          :category "introspection"
          :include t
          :args '(( :name "variable"
                    :type string
                    :description "Name of a VARIABLE, such as \
\"cursor-type\"."))
          :description "Returns the docstring VARIABLE.
Return value is a docstring for VARIABLE.  VARIABLE can be a defcustom
or defvar.  Can be used to infer the correct forms for setting a
variable, such as when configuring packages in use-package expressions
or leading the user through diagnosing something.  This tool is very
cheap and very fast.  Call it very liberally.")
	 (gptel-make-tool
          :name "variable_source"
          :function (lambda (symbol)
		      (when-let ((symbol (intern-soft symbol)))
			(pmx--gptel-source symbol 'defvar)))
          :category "introspection"
          :include t
          :args '(( :name "variable"
                    :type string
                    :description "Name of a VARIABLE, such as \
\"last-kbd-macro\"."))
          :description "Returns the source code for VARIABLE.
Return value is the source code for VARIABLE.  VARIABLE can be a defvar
or defcustom.  The returned source code can be extremely insightful
because nothing is more accurate than looking at the code and the source
code contains the docstring too.

You can use source code for variables to see the forms used in setting
their defaults and make inferences about what forms will be interpreted
correctly.  If the result seems incomplete, you can try returning the
docstring using `variable_documentation' or the entire source for a
library feature by using `library_source'.  This tool is cheap and fast.
Call it liberally.")
(gptel-make-tool
          :name "variable_value"
          :function (lambda (symbol)
		      (when-let ((symbol (intern-soft symbol)))
			(default-value symbol)))
          :category "introspection"
          :confirm t
          :include t
          :args '(( :name "variable"
                    :type string
                    :description "Name of a VARIABLE, such as \
\"last-kbd-macro\"."))
          :description "Returns the global value for VARIABLE.
Return value is the global (not buffer-local) value for VARIABLE.
VARIABLE can be a defvar or defcustom.  Use this when behavior depends
on the state of a variable or you want to infer if a package has indeed
configured a variable.  By observing expected side-effects, you can
build a coherent view of the interaction between functions and settings.

Use of this tool could leak private data, so don't call it for any
VARIABLE that contains initialized authentication data.

If the result is confusing, you can try returning the docstring using
`variable_documentation' to gain insights into the structure of values
contained.")
 (gptel-make-tool
          :name "symbol_manual_section"
          :include t
          :function (lambda (symbol)
		      (require 'helpful)
		      (when-let* ((symbol (intern-soft symbol))
				  (_completion (helpful--in-manual-p symbol)))
			(save-window-excursion
			  (info-lookup 'symbol symbol #'emacs-lisp-mode)
			  (buffer-substring-no-properties (point-min) (point-max)))))
          :category "introspection"
          :args '(( :name "symbol"
                    :type string
                    :description "Name of a SYMBOL, such as \
\"find-file-noselect\"."))
          :description "Returns contents of manual node for SYMBOL.
SYMBOL can be a function, macro, defcustom, or defvar.  If symbol is not
known to be in a manual, this functon will return nil.

The returned manual contents are similar to the `manual_node_contents'
tool.  You sould recursively inspect any links or menu entries that look
relevant.  Check the node list for the manual if a link or menu entry
returns nil.

If you can't find anything, you should try looking up its source or
docstring next and finally try to complete the prefix of the symbol .")
	 (gptel-make-tool
	  :name "documentation"
	  :function (lambda (symbol)
                      (if (intern-soft symbol)
			  (documentation (intern symbol))
			"nil"))
	  :include t
	  :category "introspection"
	  :args '((:name "symbol"
			 :type string
			 :description "Name of the function or variable you want documentation for."))
	  :description "Retrieve the documentation for SYMBOL."
	  )
	 (gptel-make-tool
	  :name "list_active_buffers"                 ; Define the tool's name in snake_case
	  :function (lambda ()                        ; The function that will run
		      (let ((buffer-list (buffer-list))
			    (output '()))
			(dolist (buf buffer-list)
			  (when (buffer-live-p buf)
			    (let ((name (buffer-name buf))
				  (mod (if (buffer-modified-p buf) "*" " ")))
			      (push (format "%s %s" mod name) output))))
			(reverse output)))
	  :description "List all active buffers with their names and statuses"
	  :args nil                                   ; No arguments needed for this function
	  :category "emacs")
	 (gptel-make-tool
	  :name "list_org_roam_nodes"
	  :function
	  (lambda ()
	    (require 'org-roam)
	    (let ((results (org-roam-db-query
			    [:select [nodes:id nodes:file nodes:title] :from nodes])))
	      (if results
		  (mapconcat
		   (lambda (row)
		     (format "• \"%s\" (ID: %s, File: %s)"
			     (nth 2 row)  ;; title
			     (nth 0 row)  ;; id
			     (file-name-nondirectory (nth 1 row)))) ;; short file name
		   results
		   "\n")
		"No Org-roam nodes found.")))
	  :description "Use this tool when you want to retrieve a natural-language summary of all Org-roam notes. This is helpful for reviewing your knowledge graph, recalling note titles, or referencing specific nodes in a conversation. It returns each node's title, ID, and filename, making it easy to locate and cite notes in a human-readable way."
	  :category "org")
	 (gptel-make-tool
	  :name "list_org_agenda_todos"
	  :function #'my/org-agenda-todo-summaries
	  :description "Use this tool to retrieve a natural-language summary of all TODO entries from your Org agenda files. It includes the task's status, title, tags, scheduling, deadlines, and the file it came from. Ideal for reviewing tasks or prompting next actions in conversation."
	  :category "org")
	 (gptel-make-tool
	  :name "get_org_roam_contents"
	  :function
	  (lambda (node-id)
	    (require 'org-roam)
	    (let ((result (org-roam-db-query
			   [:select [nodes:title nodes:file]
				    :from nodes
				    :where (= nodes:id $s1)]
			   node-id)))
	      (if result
		  (let* ((row (car result))
			 (title (nth 0 row))
			 (file (nth 1 row))
			 (contents (with-temp-buffer
				     (insert-file-contents file)
				     (buffer-string))))
		    (format "Title: %s\nFile: %s\nContents:\n%s"
			    title
			    (file-name-nondirectory file)
			    contents))
		(format "No Org-roam node found for ID: %s" node-id))))
	  :args '((:name "node_id"
			 :type "string"
			 :description "The unique Org-roam node ID (GUID) used to identify a specific note. You can get this from `list_org_roam_nodes`."))
	  :description "Use this tool to retrieve the full contents of a single Org-roam note by its unique node ID (GUID). This is useful when the assistant needs to read or reference a specific note after it has been selected by ID."
	  :category "org")
	 (gptel-make-tool
	  :name "list_elfeed_titles"                 ; Define the tool's name in snake_case
	  :function (lambda ()
		      (require 'elfeed)
		      (require 'elfeed-db)
		      (elfeed-update)
		      (mapcar (lambda (entry)
				(elfeed-entry-title entry))
			      (hash-table-values elfeed-db-entries)))
	  :description "A crude crappy elfeed dump for rss titles"
	  :category "elfeed"))))

(use-package elfeed
  :ensure t
  :bind
  ("C-x w" . elfeed)
  :config
  (setq elfeed-feeds
        '("https://karthinks.com/index.xml")))

(use-package emojify
  :hook (after-init . global-emojify-mode))

(setq gc-cons-threshold (* 2 1000 1000))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(all-the-icons-dired auto-package-update beancount command-log-mode
			 company-box counsel-projectile
			 dired-hide-dotfiles dired-open doom-modeline
			 doom-themes eshell-git-prompt eterm-256color
			 evil-collection evil-nerd-commenter forge
			 general go-mode gptel helpful ivy-prescient
			 ivy-rich jenkinsfile-mode lsp-ivy lsp-java
			 lsp-ui no-littering ob-go ob-rust ob-sql-mode
			 ob-typescript org-bullets org-roam-ui
			 powershell python-mode pyvenv
			 quelpa-use-package rainbow-delimiters
			 rust-mode typescript-mode visual-fill-column
			 vterm yaml-mode))
 '(warning-suppress-types '((native-compiler))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
