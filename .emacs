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
  (ws-butler-global-mode)
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

(if (file-exists-p "~/emacs_config/sql-indent.el")
    (progn
      (load "~/emacs_config/sql-indent.el")))

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
;; (use-package ob-ipython
;;   :ensure t
;;   :config
;;   (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((ipython . t)
;;    ;; other languages..
;;    ))
;;   )

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


;; Perspective
(use-package perspective
  :ensure t
  :init
  (persp-mode)
  )

;; Perspective Projectile
;(use-package persp-projectile
;  :ensure t
;)

;; Eyebrowse
(use-package eyebrowse
  :ensure t
  :init
  (eyebrowse-mode t)
  )

;; Yasnippet
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

;;Org stuff
    (progn
      (fset 'gk/listify
	    (lambda (&optional arg) "Keyboard macro."
	      (interactive "p")
	      (kmacro-exec-ring-item
	       (quote ([33554435 33554435 1 42 42 32 79 80 84 73 79 78 83 32 7 5] 0 "%d")) arg)))
      (define-key org-mode-map (kbd "C-c c") 'gk/listify))

;; Org work timer stuff
(use-package org-pomodoro
  :ensure t
  :init
  (setq org-pomodoro-finished-sound-p t)
  (setq org-pomodoro-finished-sound "/Users/ganeshkrishnan/.emacs.d/elpa/org-pomodoro-20171108.1314/resources/bell_multiple.wav")
  ;; (setq org-pomodoro-ticking-sound-p t)
  ;; (setq org-pomodoro-ticking-sound "/Users/ganeshkrishnan/.emacs.d/elpa/org-pomodoro-20171108.1314/resources/ocean_waves.wav")
  (setq org-pomodoro-keep-killed-pomodoro-time t)
  )

(defun gk/org-pomodoro-ask (n)
  (interactive "nHow many minutes? ")
  (setq org-pomodoro-length n)
  (org-pomodoro)
  )

(defun gk/org-pomodoro-toggle-sounds ()
  (interactive)
  (cond ((eq org-pomodoro-play-sounds t)
	 (setq org-pomodoro-play-sounds nil))
	((eq org-pomodoro-play-sounds nil)
	 (setq org-pomodoro-play-sounds t)))
  )

(define-key org-mode-map (kbd "C-c C-x t") 'gk/org-pomodoro-ask)
(define-key org-mode-map (kbd "C-c C-x s") 'gk/org-pomodoro-toggle-sounds)

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

(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(if (file-exists-p "~/org/work.org")
    (setq org-agenda-files (list "~/org/work.org")))
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
   ))

(use-package ox-gfm
  :ensure t
  )

(use-package ob-async
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
    (window-purpose python use-package delight persp-projectile perspective Perspective websocket request org-pomodoro smartrep smartparens undo-tree avy ws-butler github-browse-file ox-gfm hydra hyrda hungry-delete realgud elpy ess ess-site dumb-jump helm-ag ein ob-ipython which-key swiper-helm jedi swiper flycheck zenburn-theme tabbar try auto-complete ace-window magit multiple-cursors exec-path-from-shell helm-projectile helm projectile)))
 '(safe-local-variable-values (quote ((pyvenv-workon . staffing))))
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
