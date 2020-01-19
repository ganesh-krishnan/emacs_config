;; Inhibit startup message
(setq inhibit-startup-message t)

;; Save clipboard in kill ring
(setq save-interprogram-paste-before-kill t)

;; Enable recursive minibuffer
(setq enable-recursive-minibuffers t)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "http://stable.melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")))

(package-initialize)

;; Load custom lisp
(add-to-list 'load-path "~/emacs_config/lisp/")

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Download try-package
(use-package try
  :ensure t)

;; Delight
(use-package delight
  :ensure t
  )

;; Trim whitespace
(use-package ws-butler
  :ensure t
  :delight
  :config
  (add-hook 'prog-mode-hook #'ws-butler-mode)
  )


;; Enable tramp
(use-package tramp
  :ensure t
  )

;; Install which-key
(use-package which-key
  :delight
  :ensure t
  :config
  (which-key-mode)
  )

;; Install Zenburn theme
(use-package zenburn-theme
  :ensure t
  )

;; Setup SQL - this is because of a quirk with Postgres
(setenv "DYLD_LIBRARY_PATH" "/usr/local/Cellar/postgresql/9.5.4_1/lib/")

(use-package sql-indent
  :ensure t
  )

;; Underscores in database names mess up the prompt
(add-hook 'sql-interactive-mode-hook
	  (lambda ()
	    (setq sql-prompt-regexp "^[_[:alpha:]]*[=][#>] ")
	    (setq sql-prompt-cont-regexp "^[_[:alpha:]]*[-][#>] ")
	    (toggle-truncate-lines t))) ;; Truncate long line output

(defun my-sql-connect (product connection)
  ;; remember to set the sql-product, otherwise, it will fail for the first time
  ;; you call the function
  (setq sql-product product)
  (sql-connect connection))


;; Projectile config
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-file-exists-remote-cache-expire nil)
  :delight '(:eval (concat " " (projectile-project-name)))
  )

(use-package helm-ag
  :ensure t
  )

;; Uncomment lines below to turn on helm-projectile
(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on)
  )

;; Swiper-helm
(use-package swiper-helm
  :ensure t
  :bind (("C-s" . swiper))
  )

;; Avy
(use-package avy
  :ensure t
  :bind (("C-'" . avy-goto-char-timer)
	 ("M-g g" . avy-goto-line))
  )

(use-package ess-site
  :ensure ess
  :config
  (ess-toggle-underscore nil)
  )

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc")
  )

(use-package polymode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
  )

;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
	 ("C->". mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C-<". mc/mark-all-like-this))
  )

;; Expand region
(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region)
  )

;; Autocomplete
(use-package auto-complete
  :ensure t
  :delight
  :config
  (ac-config-default)
  (add-to-list 'ac-modes 'sql-interactive-mode)
  (add-to-list 'ac-modes 'sql-mode)
  (setq ac-modes (delq 'python-mode ac-modes))
  )

;; Hungry Delete
(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode)
  )

;; Ace Window keybindings and customizations
(use-package ace-window
  :ensure t
  :bind
  (("M-s" . ace-window))
  :config
  (setq aw-dispatch-always t)
  )

;; Purpose Mode
(use-package window-purpose
  :ensure t
  :init
  (purpose-mode)
  :config
  (add-to-list 'purpose-user-mode-purposes '(python-mode . py))
  (add-to-list 'purpose-user-mode-purposes '(inferior-python-mode . py-repl))
  (purpose-compile-user-configuration)
  (define-key purpose-mode-map (kbd "C-x C-f") nil)
  )

;; Turn on winner mode
(use-package winner
  :ensure t
  :init
  (winner-mode)
  )

;; Exec-path from shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))
  )

;; Eshell auto-completion stuff
(defun ac-pcomplete ()
  ;; eshell uses `insert-and-inherit' to insert a \t if no completion
  ;; can be found, but this must not happen as auto-complete source
  (flet ((insert-and-inherit (&rest args)))
    ;; this code is stolen from `pcomplete' in pcomplete.el
    (let* (tramp-mode ;; do not automatically complete remote stuff
           (pcomplete-stub)
           (pcomplete-show-list t) ;; inhibit patterns like * being deleted
           pcomplete-seen pcomplete-norm-func
           pcomplete-args pcomplete-last pcomplete-index
           (pcomplete-autolist pcomplete-autolist)
           (pcomplete-suffix-list pcomplete-suffix-list)
           (candidates (pcomplete-completions))
           (beg (pcomplete-begin))
           ;; note, buffer text and completion argument may be
           ;; different because the buffer text may bet transformed
           ;; before being completed (e.g. variables like $HOME may be
           ;; expanded)
           (buftext (buffer-substring beg (point)))
           (arg (nth pcomplete-index pcomplete-args)))
      ;; we auto-complete only if the stub is non-empty and matches
      ;; the end of the buffer text
      (when (and (not (zerop (length pcomplete-stub)))
                 (or (string= pcomplete-stub ; Emacs 23
                              (substring buftext
                                         (max 0
                                              (- (length buftext)
                                                 (length pcomplete-stub)))))
                     (string= pcomplete-stub ; Emacs 24
                              (substring arg
                                         (max 0
                                              (- (length arg)
                                                 (length pcomplete-stub)))))))
        ;; Collect all possible completions for the stub. Note that
        ;; `candidates` may be a function, that's why we use
        ;; `all-completions`.
        (let* ((cnds (all-completions pcomplete-stub candidates))
               (bnds (completion-boundaries pcomplete-stub
                                            candidates
                                            nil
                                            ""))
               (skip (- (length pcomplete-stub) (car bnds))))
          ;; We replace the stub at the beginning of each candidate by
          ;; the real buffer content.
          (mapcar #'(lambda (cand) (concat buftext (substring cand skip)))
                  cnds))))))

(defvar ac-source-pcomplete
  '((candidates . ac-pcomplete)))

(add-hook 'eshell-mode-hook
	  #'(lambda ()
	      (setq ac-sources '(ac-source-pcomplete))
               ;; Helm completion with pcomplete
               (setq eshell-cmpl-ignore-case t)
               (eshell-cmpl-initialize)
               (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
               ;; Helm lisp completion
               (define-key eshell-mode-map [remap eshell-complete-lisp-symbol] 'helm-lisp-completion-at-point)
               ;; Helm completion on eshell history.
               (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)))

(add-to-list 'ac-modes 'eshell-mode)

;; Magit Keybinding
(global-set-key (kbd "C-x g") 'magit-status)

(use-package github-browse-file
  :ensure t
  )

;; Recentf to open recently viewed files
(recentf-mode 1)
(setq-default recent-save-file "~/.emacs.d/recentf")

;; Helm stuff
(use-package helm
  :delight
  :ensure t
  :config
  (progn
    (require 'helm-config)
    )
  (setq helm-mini-default-sources '(helm-source-buffers-list
				    helm-source-recentf
				    helm-source-bookmarks
				    helm-source-buffer-not-found))
  :bind (("C-x C-f" . helm-find-files)
	 ("M-y" . helm-show-kill-ring)
	 ("C-x b" . helm-mini))
  )

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(defun spacemacs//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))


(add-hook 'helm-minibuffer-set-up-hook
          'spacemacs//helm-hide-minibuffer-maybe)

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)

(helm-mode 1)
(put 'scroll-left 'disabled nil)

;; ob-ipython
(use-package ob-ipython
  :ensure t
  )

(defun ob-ipython--collect-json ()
  ;; hacks here
  (when (re-search-forward "{" nil t)
    (backward-char))
  ;; hacks end
  (let ((json-array-type 'list))
    (let (acc)
      (while (not (= (point) (point-max)))
        (setq acc (cons (json-read) acc))
        (forward-line))
      (nreverse acc))))

(advice-add 'ob-ipython--collect-json :before
            (lambda (&rest args)
              (when (re-search-forward "{" nil t)
                (backward-char))))

;; scimax
(load "scimax-ob")
(load "scimax-org-babel-ipython-upstream")
(setq ob-ipython-exception-results nil)

(use-package csv
  :ensure t
  )

;; Smartrep
;; Omit prefix keys - useful for ein
(use-package smartrep
  :ensure t
  )

;; EIN
(use-package ein
  :ensure t
  :config
  (setq ein:use-auto-complete-superpack t)
  (setq ein:use-smartrep t)
  (setq ein:completion-backend 'ein:use-ac-jedi-backend)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ein . t)
     ))
  (require 'ein-connect)
  )

;; Stan
(use-package stan-mode
  :ensure t
  )

(use-package stan-snippets
  :ensure t
  )

;; realgud Debugger
(use-package realgud
  :ensure t
  :commands (realgud:gdb
	     realgud:ipdb
	     realgud:pdb))

;; Python stuff
(setq python-shell-enable-font-lock nil)

;; Elpy
(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  :config
  (setq elpy-rpc-backend "jedi")
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "--simple-prompt --matplotlib tk")
  (add-hook 'inferior-python-mode-hook 'company-mode)

  (defun start-lore ()
    (interactive)
    (run-python "lore console --simple-prompt")
    )

  (defun elpy-shell-send-paragraph ()
    "Send the current paragraph to the python shell."
    (interactive)
    (if (python-info-current-line-empty-p)
  	(message "Not in a paragraph")
      (let ((beg (progn (backward-paragraph) (point)))
  	    (end (progn (forward-paragraph) (point))))
  	(elpy-shell-get-or-create-process)
  	(python-shell-send-string (elpy-shell--region-without-indentation beg end))
  	(elpy-shell-display-buffer))))
  (defun elpy-shell-send-region-or-buffer nil
    "Sends from python-mode buffer to a python shell, intelligently."
    (interactive)
    (elpy-shell-get-or-create-process)
    (cond ((region-active-p)
  	   (setq deactivate-mark t)
  	   (python-shell-send-region (region-beginning) (region-end))
  	   (python-nav-forward-block)
  	   ) (t (elpy-shell-send-paragraph))))
  :bind (:map python-mode-map
	      ("C-c C-l" . python-shell-send-buffer)
	      ("C-c l" . start-lore))
  )

;; Yasnippet
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

;; Git Auto-Commit
(use-package git-auto-commit-mode
  :ensure t
  :config
  (setq gac-debounce-interval 300))


;;Org stuff
    (progn
      (fset 'gk/listify
	    (lambda (&optional arg) "Keyboard macro."
	      (interactive "p")
	      (kmacro-exec-ring-item
	       (quote ([33554435 33554435 1 42 42 32 79 80 84 73 79 78 83 32 7 5] 0 "%d")) arg))))

;; Org work timer stuff
(use-package org-pomodoro
  :ensure t
  :init
  (setq org-pomodoro-finished-sound-p t)
  (setq org-pomodoro-finished-sound "/Users/ganeshkrishnan/.emacs.d/elpa/org-pomodoro-20171108.1314/resources/bell_multiple.wav")
  (setq org-pomodoro-keep-killed-pomodoro-time t)
  )

(use-package org-clock-convenience
  :ensure t
    :bind (:map org-agenda-mode-map
   	   ("<S-up>" . org-clock-convenience-timestamp-up)
   	   ("<S-down>" . org-clock-convenience-timestamp-down)
   	   ("C-c C-f" . org-clock-convenience-fill-gap)
   	   ("C-c C-b" . org-clock-convenience-fill-gap-both)))

(use-package org-ql
  :ensure t
  :defer f
  )
(require 'org-ql-search)
(load "org-ql-gtd")
(load "org-ql-agenda")

(defun gk/create-logbook-entry ()
  (interactive)
  (catch 'active-pomodoro
    (when (org-pomodoro-active-p)
      (message "Pomodoro is active. Please end pomodoro before adjusting clocking")
      (throw 'active-pomodoro "Pomodoro is active"))
    (let ((org-clock-out-remove-zero-time-clocks nil))
      (if (derived-mode-p 'org-agenda-mode)
	  (progn (org-agenda-clock-in)
		 (org-agenda-clock-out))
	(org-clock-in)
	(org-clock-out)))))

(defun gk/org-pomodoro-ask (n)
  (interactive "nHow many minutes? ")
  (setq org-pomodoro-length n)
  (org-pomodoro)
  )

(defun gk/org-pomodoro-kill ()
  (interactive)
  (when (org-pomodoro-active-p)
    (org-pomodoro-kill)
    (message "Pomodoro ended")))


(defun gk/save-all-agenda-buffers ()
  "Function used to save all agenda buffers that are
currently open, based on `org-agenda-files'."
  (interactive)
  (save-current-buffer
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (when (member (buffer-file-name)
                    (mapcar 'expand-file-name (org-agenda-files)))
        (save-buffer)))))

; (advice-add 'org-refile :after 'gk/save-all-agenda-buffers)
(advice-add 'org-agenda-quit :before 'gk/save-all-agenda-buffers)

(defun gk/org-pomodoro-toggle-sounds ()
  (interactive)
  (cond ((eq org-pomodoro-play-sounds t)
	 (setq org-pomodoro-play-sounds nil))
	((eq org-pomodoro-play-sounds nil)
	 (setq org-pomodoro-play-sounds t)))
  )

(defun gk/cmp-org-heading-levels (a b)
  (let* ((a-pos (get-text-property 0 'org-marker a))
	 (b-pos (get-text-property 0 'org-marker b))
	 (level-a
	  (org-with-point-at a-pos
	    (nth 1 (org-heading-components))))
	 (level-b
	  (org-with-point-at b-pos
	    (nth 1 (org-heading-components))))
	 )
    (cond ((if level-a (and level-b (< level-a level-b)) level-b) -1)
	  ((if level-b (and level-a (< level-b level-a)) level-a) +1))))


(defun gk/habits ()
  '(tags "HABIT=\"current\""
	 ((org-agenda-overriding-header "Habits to Reinforce")
	  (org-agenda-files '("~/org/habits.org")))))

(defun gk/agendablock-inbox ()
  '(tags-todo "/-DONE|-CANCELED"
	 ((org-agenda-overriding-header "Inbox")
	  (org-agenda-files '("~/org/inbox.org"))
	 )))

(defun gk/waiting-fors ()
  '(tags-todo "/+WAITING"
	 ((org-agenda-overriding-header "Waiting Fors")
	  (org-agenda-skip-function (org-query-select "headline" (org-query-gtd-waiting-task)))
	  )))

(defun gk/habitual-tasks ()
  '(tags "DAILY_REVIEW_TASKS=\"t\""
	 ((org-agenda-overriding-header "Habitual Tasks")
	  (org-agenda-files '("~/org/housekeep.org")))))

(defun gk/available-and-visible-tasks ()
  '(tags-todo "/+TODO|+NEXT"
	      ((org-agenda-overriding-header "Current Items")
	       (org-agenda-files gk/project-agenda-files)
	       (org-agenda-skip-function (org-query-select "headline" (org-query-gtd-available-and-visible-task)))
	       (org-agenda-prefix-format '((tags . "%(concat \"[\"(org-format-outline-path (list (car (org-get-outline-path)))) \"] \")"))))))

(defun gk/no-context ()
  '(tags-todo "-{@home\\|@work\\|@computer\\|@errands\\|@anywhere}"
	      ((org-agenda-overriding-header "Tasks with No Context")
	       (org-agenda-files gk/project-agenda-files)
	       (org-agenda-skip-function (org-query-select "headline" (org-query-gtd-available-task))))))

(defun gk/power-lens ()
  '(tags "HABIT=\"power\""
	 ((org-agenda-overriding-header "Power Lens of Focus")
	  (org-agenda-files '("~/org/habits.org")))))

(defun gk/other-daily-tasks ()
  '(tags-todo "DAILY_REVIEW=\"t\""
	      ((org-agenda-overriding-header "Other Tasks")
	       (org-agenda-files '("~/org/housekeep.org"))
	       (org-agenda-skip-function (org-query-select "headline" (org-query-gtd-scheduled-before-today)))
	       (org-agenda-cmp-user-defined 'gk/cmp-org-heading-levels)
	       (org-agenda-sorting-strategy '(user-defined-down)))))

(defun gk1/other-daily-tasks1 ()
  `(org-ql-block '(and (todo "SUBTASK")
		       (property "DAILY_REVIEW" "t")
		       `,(org-query-gtd-scheduled-before-today))
		 ((org-ql-block-header "Other Tasks")
		  (org-agenda-files '("~/org/housekeep.org")))))

(defun gk1/other-daily-tasks2 ()
  `(org-ql-block '(and (todo "NEXT")
		       (property "DAILY_REVIEW" "t")
		       `,(org-query-gtd-scheduled-before-today))
		 ((org-ql-block-header "Other Tasks")
		  (org-agenda-files '("~/org/housekeep.org")))))

(defun gk/weekly-review ()
  '(search "Do Weekly Review" ((org-agenda-files '("~/org/housekeep.org"))
			       (org-agenda-overriding-header "Weekly Review")
			       (org-agenda-skip-function
				(org-query-select "headline" (org-query-gtd-available-task))))))

(defun gk/agendablock-active-projects-with-next ()
  '(tags-todo "/+PROJ"
              ((org-agenda-overriding-header "Active projects with a next task")
               (org-agenda-skip-function (org-query-select "headline" (org-query-gtd-project-armed)))
               (org-tags-match-list-sublevels 't)
	       )))

(defun gk/weekly-tasks-grp1 ()
  '(tags-todo "WEEKLY_REVIEW_GRP1=\"t\""
	      ((org-agenda-overriding-header "Default Review")
	       (org-agenda-files '("~/org/housekeep.org"))
	       (org-agenda-skip-function (org-query-select "headline" (org-query-gtd-scheduled-before-today))))))

(defun gk/weekly-tasks-grp2 ()
  '(tags-todo "WEEKLY_REVIEW_GRP2=\"t\""
	      ((org-agenda-overriding-header "Other Tasks")
	       (org-agenda-files '("~/org/housekeep.org"))
	       (org-agenda-skip-function (org-query-select "headline" (org-query-gtd-scheduled-before-today))))))

(defun gk/stuck-projects ()
  '(tags-todo "/+PROJ"
	      ((org-agenda-overriding-header "Current Stuck Projects")
	       (org-agenda-files gk/project-agenda-files)
	       (org-agenda-skip-function (org-query-select "headline" (org-query-gtd-stuck-project))))))

(defun gk/active-current-visible-projects ()
  '(tags-todo "/+PROJ"
	      ((org-agenda-overriding-header "Active Current Projects")
	       (org-agenda-files gk/project-agenda-files)
	       (org-agenda-skip-function (org-query-select "headline" (org-query-gtd-active-current-visible-project))))))

(defun gk/active-deferred-projects ()
  '(tags-todo "/+PROJ"
	      ((org-agenda-overriding-header "Active Deferred Projects")
	       (org-agenda-files gk/project-agenda-files)
	       (org-agenda-skip-function (org-query-select "headline" (org-query-gtd-active-deferred-project))))))

(defun gk/someday-projects ()
  '(tags-todo "/+PROJ"
	      ((org-agenda-overriding-header "Someday/Maybe Projects")
	       (org-agenda-files gk/project-agenda-files)
	       (org-agenda-skip-function (org-query-select "headline" (org-query-gtd-someday-project))))))

(defun gk/suspended-projects ()
  '(tags-todo "/+PROJ"
	      ((org-agenda-overriding-header "Suspended Projects")
	       (org-agenda-files gk/project-agenda-files)
	       (org-agenda-skip-function (org-query-select "headline" (org-query-gtd-suspended-project))))))

(defun gk/tasks-to-archive ()
  '(todo "DONE|CANCELED"
	 ((org-agenda-overriding-header "Tasks to Archive"))))


(defun gk/work-tasks-for-today ()
  '(tags-todo "{@work\\|@computer}/+TODO|+NEXT"
	      ((org-agenda-overriding-header "Work Tasks for Today")
	       (org-agenda-files gk/project-agenda-files)
	       (org-agenda-skip-function (org-query-select "headline" (org-query-gtd-available-task))))))

(defun gk/meetings-for-today ()
  '(todo "TODO"
	 ((org-agenda-overriding-header "Meetings for Today")
	  (org-agenda-files (list "~/org/calendar.org")))))

(defun gk/home-tasks-for-today ()
  '(tags-todo "{@home\\|@computer}/+TODO|+NEXT"
	      ((org-agenda-overriding-header "Home Tasks for Today")
	       (org-agenda-skip-function (org-query-select "headline" (org-query-gtd-available-task))))))

(defun gk/tasks-for-today ()
    '(todo "TODO|NEXT"
	   ((org-agenda-overriding-header "Tasks for Today")
	    (org-agenda-files gk/project-agenda-files)
	    (org-agenda-skip-function (org-query-select "headline" (org-query-gtd-available-task))))))

(defun gk/review-clock-report ()
  '(tags-todo "PLACEHOLDER"
	      ((org-agenda-overriding-header "Review clock report for week - Not yet ready"))))

(defun gk/review-frequently-rescheduled ()
  '(tags-todo "PLACEHOLDER"
	      ((org-agenda-overriding-header "Review highly rescheduled tasks - Not yet ready"))))

(defun gk/org-entry-properties-inherit-deadline (orig-fun &optional pom which)
  "Call ORIG-FUN with POM, but if WHICH is `SCHEDULED' or `DEADLINE' do it recursively."

  (if (or (string= which "DEADLINE") (string= which "SCHEDULED"))
      (org-with-point-at pom
	(let (value)
	  (while (not (or (setq value (funcall orig-fun (point) which))
			  (not (org-up-heading-safe)))))
	  value))
    (funcall orig-fun pom which)))
(advice-add 'org-entry-properties :around #'gk/org-entry-properties-inherit-deadline)

(if (file-exists-p "~/org/work.org")
    (setq org-agenda-files (list "~/org/work.org"
				 "~/org/housekeep.org"
				 "~/org/spiritual.org"
				 "~/org/career.org"
				 "~/org/habits.org"
				 "~/org/other.org"
				 "~/org/inbox.org"
				 "~/org/calendar.org")))

(setq gk/project-agenda-files (list "~/org/work.org"
				    "~/org/housekeep.org"
				    "~/org/spiritual.org"
				    "~/org/career.org"
				    "~/org/habits.org"
				    "~/org/other.org"))

(defun gk/save-all-agenda-buffers ()
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (member (buffer-file-name)
                    (mapcar 'expand-file-name (org-agenda-files t)))
        (if (and (buffer-modified-p) (buffer-file-name))
            (save-buffer))))))

;; save all the agenda files after each capture
(run-with-idle-timer 1 30 'gk/save-all-agenda-buffers)

;; (setq org-agenda-todo-ignore-with-date t)
(global-set-key (kbd "C-c C-x t") 'gk/org-pomodoro-ask)
(global-set-key (kbd "C-c C-w") 'org-refile)
(global-set-key (kbd "C-c C-x k") 'gk/org-pomodoro-kill)
(global-set-key (kbd "C-c C-x s") 'gk/org-pomodoro-toggle-sounds)
(define-key org-mode-map (kbd "C-c l") 'org-store-link)
(define-key org-mode-map (kbd "C-c C-x l") 'gk/create-logbook-entry)
(define-key org-agenda-mode-map (kbd "C-c C-x l") 'gk/create-logbook-entry)
(load "org-query")
(load "org-query-gtd")
(load "org-subtask-reset")
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(setq org-archive-location "diary.org::datetree/* Archived Tasks")
(setq org-log-done t)
(setq org-log-reschedule nil)
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-columns-default-format "%40ITEM(Task) %10TODO %17EFFORT(Estimated Effort){:} %CLOCKSUM")
(setq org-refile-targets '((nil :maxlevel . 9)
			   ("~/org/supplementary/reference.org" :maxlevel . 9)
			   (org-agenda-files :maxlevel . 9)))
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path 'file)              ; Show full paths for refiling
(setq org-enforce-todo-dependencies t)
(setq org-agenda-dim-blocked-tasks nil)
(setq org-log-into-drawer t)
(setq org-stuck-projects '("/+PROJ-DONE-CANCELED" ("NEXT") nil ""))

(setq org-agenda-custom-commands
      `(("N" "Daily Review"
	 ((search "Do Daily Review" ((org-agenda-files '("~/org/housekeep.org"))
				     (org-agenda-overriding-header "Daily Review")
				     (org-agenda-skip-function
				      (org-query-select "headline" (org-query-gtd-available-task)))))
	  ,(gk1/habits)
	  ,(gk1/habitual-tasks)
	  ,(gk/waiting-fors)
	  ,(gk/agendablock-inbox)
	  ,(gk/no-context)
	  ,(gk/power-lens)
	  ,(gk/available-and-visible-tasks)
	  ,(gk1/other-daily-tasks1)
	  ,(gk1/other-daily-tasks2))
	 nil)
	("W" "Weekly Review"
	 (,(gk/weekly-review)
	  ,(gk/habits)
	  ,(gk/agendablock-inbox)
	  ,(gk/no-context)
	  ,(gk/weekly-tasks-grp1)
	  ,(gk/stuck-projects)
	  ,(gk/active-deferred-projects)
	  ,(gk/active-current-visible-projects)
	  ,(gk/someday-projects)
	  ,(gk/suspended-projects)
	  ,(gk/available-and-visible-tasks)
	  ,(gk/power-lens)
	  ,(gk/weekly-tasks-grp2)
	  ,(gk/review-clock-report)
	  ,(gk/review-frequently-rescheduled)
	  ,(gk/weekly-review)
	  ,(gk/tasks-to-archive))
	 nil)
	("w" "Work Tasks"
	 (,(gk/work-tasks-for-today)
	  ,(gk/meetings-for-today))
	 nil)
	("h" "Home Tasks" (,(gk/home-tasks-for-today))
	 nil)
	("C" "Current Tasks" (,(gk/tasks-for-today))
	 nil)
	))

(setq org-capture-templates
      '(("t" "Task" entry (file "~/org/inbox.org") "* TODO %i%?")
	("i" "Interruption" entry (file+datetree "~/org/supplementary/diary.org") "* %?" :clock-in t :clock-resume t)
	("r" "Reference" entry (file "~/org/supplementary/reference.org") "* %i%?")
	("m" "Meeting" entry (file+headline "~/org/calendar.org" "Meetings") "* TODO %i%?")
	("w" "Weekly Meeting" entry (file+datetree "~/org/supplementary/reviews.org")
	 (file "~/org/templates/weekly_meeting.org"))))

;; Modify org-pomodoro-finished to not start breaks
(defun org-pomodoro-finished ()
  "Is invoked when a pomodoro was finished successfully.
This may send a notification, play a sound and start a pomodoro break."
  (unless org-pomodoro-clock-break
    (org-clock-out nil t))
  (org-pomodoro-reset)
  (org-pomodoro-update-mode-line)
  (org-agenda-maybe-redo)
  (org-pomodoro-maybe-play-sound :pomodoro)
  (run-hooks 'org-pomodoro-finished-hook))

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-src-preserve-indentation t)
(setq org-confirm-babel-evaluate nil)
(setq org-src-window-setup 'current-window)
(custom-theme-set-faces 'zenburn
                        `(org-level-1 ((t (:inherit outline-1 :height 1.4))))
			`(org-level-2 ((t (:inherit outline-2 :height 1.3))))
			`(org-level-3 ((t (:inherit outline-3 :height 1.2))))
			`(org-level-4 ((t (:inherit outline-4 :height 1.1)))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (ein . t)
   (stan .t)
   (dot . t)
   (ipython . t)
   (latex . t)
   ))

(use-package ox-gfm
  :ensure t
  )

(use-package org-noter
  :ensure t
  )

(if (file-exists-p "~/emacs_config/org-mind-map.el")
    (load "~/emacs_config/org-mind-map.el"))

;; aucTEX
(use-package tex
  :defer t
  :ensure auctex
  )

;; dumb-jump
(use-package dumb-jump
  :ensure t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'helm)
  :ensure)

;; Hydra
(use-package hydra
  :ensure t
  :config
  (if (file-exists-p "~/emacs_config/hydras.el")
      (load "~/emacs_config/hydras.el")
    )
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ess-eval-visibly nil)
 '(flycheck-lintr-linters "with_defaults(line_length_linter(120))")
 '(package-selected-packages
   (quote
    (git-auto-commit-mode dash recently org-snooze org-ql csv sql-indent org-noter yasnippet window-purpose python use-package delight persp-projectile perspective Perspective websocket request org-pomodoro smartrep smartparens undo-tree avy ws-butler github-browse-file ox-gfm hydra hyrda hungry-delete realgud elpy ess ess-site dumb-jump helm-ag ein ob-ipython which-key swiper-helm jedi swiper flycheck zenburn-theme tabbar try auto-complete ace-window magit multiple-cursors exec-path-from-shell helm-projectile helm projectile)))
 '(safe-local-variable-values
   (quote
    ((gac-debounce-interval . 180)
     (pyvenv-workon . staffing))))
 '(tramp-verbose 3 nil (tramp)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Misc stuff
(setq auto-window-vscroll nil) ; Fix slow next line issue (https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746)
(global-hl-line-mode 1)
(set-face-background 'hl-line "#000000")

;;;; Dired stuff
(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook 'auto-revert-mode)

(if (file-exists-p "~/.emacs.d/.emacs_local.el")
    (load "~/.emacs.d/.emacs_local.el"))

;;;; Ediff
(setq ediff-split-window-function (quote split-window-horizontally))

;;; Interaction log
(load "interaction-log")
